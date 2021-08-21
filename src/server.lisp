;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DIDIT; Base: 10 -*-

#|

Copyright (C) 2021  Anthony Green <green@moxielogic.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with this program.  If not, see
<http://www.gnu.org/licenses/>.

|#

;; Top level for didit

(markup:enable-reader)

(in-package :didit)

;; ----------------------------------------------------------------------------
;; Get the version number at compile time.  This comes from
;; APP_VERSION (set on the linux container build commandline), or from
;; git at compile-time.  Use UNKNOWN if all else fails.

;; This can come from build time...
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defparameter +didit-git-version+
    (inferior-shell:run/ss
     "git describe --tags --dirty=+ || git rev-parse --short HEAD || echo UNKNOWN")))

;; But this must come from runtime...
(defparameter +didit-version+
  (let ((v +didit-git-version+))
    (if (equal v "UNKNOWN")
 	(or (uiop:getenv "APP_VERSION") v)
 	v)))

;; ----------------------------------------------------------------------------
;; Find the directory in which we are installed.  This is used to
;; serve up static content.

(defun didit-root ()
  (fad:pathname-as-directory
   (make-pathname :name nil
                  :type nil
                  :defaults #.(or *compile-file-truename* *load-truename*))))

;; ----------------------------------------------------------------------------
;; Machinery for managing the execution of the server.

(defvar *shutdown-cv* (bt:make-condition-variable))
(defvar *server-lock* (bt:make-lock))

(defvar *etcd* nil)

;; ----------------------------------------------------------------------------
;; Current state

(defvar *current-state* nil)
(defvar *current-state-lock* (bt:make-lock))

(defun get-current-state ()
  (bt:with-lock-held (*current-state-lock*)
    *current-state*))

(defun set-current-state ()
  (bt:with-lock-held (*current-state-lock*)
    (setf *current-state* state)))

;; ----------------------------------------------------------------------------
;; Default configuration.  Overridden by external config file.
;; Config files are required to be in TOML format.

(defvar *config* nil)
(defvar *default-config* nil)
(defparameter +default-config-text+
"server-uri = \"http://localhost:8080\"
config-repo = \"FIXME\"
root-dir = \"/tmp/var/didit/\"
")

;; ----------------------------------------------------------------------------
;; The URI of the server.  Define this in your config.ini files.  Use
;; this is you are generating responses that point back to this
;; application.

(defvar *server-uri* nil)

;; ----------------------------------------------------------------------------
;; Initialize prometheus values.

(defparameter *http-requests-counter* nil)
(defparameter *http-request-duration* nil)

(defun initialize-metrics ()
  (unless *didit-registry*
    (setf *didit-registry* (prom:make-registry))
    (let ((prom:*default-registry* *didit-registry*))
      (setf *http-requests-counter*
            (prom:make-counter :name "http_requests_total"
                               :help "Counts http request by type"
                               :labels '("method" "app")))
      (setf *http-request-duration*
	    (prom:make-histogram :name "http_request_duration_milliseconds"
                                 :help "HTTP requests duration[ms]"
                                 :labels '("method" "app")
                                 :buckets '(10 25 50 75 100 250 500 750 1000 1500 2000 3000)))
      #+sbcl
      (prom.sbcl:make-memory-collector)
      #+sbcl
      (prom.sbcl:make-threads-collector)
      (prom.process:make-process-collector))))

;; ----------------------------------------------------------------------------
;; API routes

(defparameter *didit-registry* nil)

;; Readiness probe.  Always ready by default, but this can be as
;; complex as required.
(easy-routes:defroute health ("/health") ()
  "ready")

(easy-routes:defroute reload ("/reload") ()
  (load-config *etcd*))

(defvar *random-state-lock*
  (bt:make-lock "Random state lock"))
(setf *random-state* (make-random-state t))

(defun random-hex-string ()
  (bt:with-lock-held (*random-state-lock*)
    (format nil "~:@(~36,8,'0R~)" (random (expt 36 8) *random-state*))))

(easy-routes:defroute oneshot ("/oneshot/:prefix") (alert minutes)
  (let ((token (random-hex-string)))
    (multiple-value-bind
	  (second minute)
        (decode-universal-time (+ (get-universal-time) (+ 60 (* 60 (parse-integer minutes)))))
      (setf (gethash (format nil "/didit/~A/~A" prefix token) *didit-table*)
            (make-didit
             :name "oneshot"
             :alert (gethash (format nil "~A/~A" prefix alert) *alerts-table*)
             :token token
             :oneshot t
             :scheduler-task (scheduler:create-scheduler-task
                              *scheduler*
                              (format nil "~A * * * * (didit:check-didit \"/didit/~A/~A\")"
                                      minute prefix token)))))
    (format nil "~A/didit/~A/~A" *server-uri* prefix token)))

(decode-universal-time (+ (get-universal-time) (* 60 122)))

(markup:deftag page-template (children &key title)
   <html>
     <head>
       <meta charset="utf-8" />
       <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no" />
       <title>,(progn title)</title>
       <link rel="stylesheet" href="css/didit.css" />
     </head>
     <body>
     ,@(progn children)
     </body>
   </html>)

;; Render the home page.
(easy-routes:defroute index ("/") ()
  (markup:write-html
   <page-template title="didit">
   This is the index page of my new app, version ,(progn +didit-version+).
   </page-template>))

;; ----------------------------------------------------------------------------
;; HTTP server control

(defparameter *handler* nil)

(defvar *hunchentoot-server* nil)

(defun do-version ()
  +didit-version+)

(defparameter +didit-dispatch-table+
  (list
   (hunchentoot:create-folder-dispatcher-and-handler
    "/images/" (fad:pathname-as-directory
                (make-pathname :name "static/images"
                               :defaults (didit-root))))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/js/" (fad:pathname-as-directory
            (make-pathname :name "static/js"
                           :defaults (didit-root))))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/css/" (fad:pathname-as-directory
             (make-pathname :name "static/css"
                            :defaults (didit-root))))
   (hunchentoot:create-prefix-dispatcher "/didit" 'do-didit)))

(defclass exposer-acceptor (prom.tbnl:exposer hunchentoot:acceptor)
  ())

(defclass application (easy-routes:easy-routes-acceptor)
  ((exposer :initarg :exposer :reader application-metrics-exposer)
   (mute-access-logs :initform t :initarg :mute-access-logs :reader mute-access-logs)
   (mute-messages-logs :initform t :initarg :mute-error-logs :reader mute-messages-logs)))

(defmacro stop-server (&key (handler '*handler*))
  "Shutdown the HTTP handler"
  `(hunchentoot:stop ,handler))

(defun short-hash (s)
  (subseq (ironclad:byte-array-to-hex-string
           (ironclad:digest-sequence
            :sha1 (flexi-streams:string-to-octets s)))
          0 8))

(defvar *alerts-table* (make-hash-table :test 'equal))
(defvar *scheduler* (make-instance 'scheduler:in-memory-scheduler))
(defvar *scheduler-lock* (bt:make-lock))

(defun become-leader (etcd)
  (bt:with-lock-held (*scheduler-lock*)
    (log:info "I am the leader.")
    (setf *etcd* etcd)
    (load-config etcd)))

(defun become-follower (etcd)
  ;; Followers just need to populate etcd when they get URL hits. Look up the slug and mark it as 'done' if it exists.
  (bt:with-lock-held (*scheduler-lock*)
    (log:info "I am a follower.  Clearing all scheduled tasks.")
    (let ((tasks (scheduler:list-scheduler-tasks *scheduler*)))
      (dolist (task tasks)
        (scheduler:delete-scheduler-task *scheduler* task)))))

(defun start-server (&optional (config-ini "/etc/didit/config.ini"))

  (bt:with-lock-held (*server-lock*)

    (setf hunchentoot:*catch-errors-p* t)
    (setf hunchentoot:*show-lisp-errors-p* t)
    (setf hunchentoot:*show-lisp-backtraces-p* t)

    (log:info "Starting didit version ~A" +didit-version+)

    ;; Read the built-in configuration settings.
    (setf *default-config* (cl-toml:parse +default-config-text+))

    (log:info "config.ini file = ~A"
              (alexandria:read-file-into-string config-ini :external-format :latin-1))

    ;; Read the user configuration settings.
    (setf *config*
  	  (if (fad:file-exists-p config-ini)
	      (cl-toml:parse
	       (alexandria:read-file-into-string config-ini
					         :external-format :latin-1))
	      (make-hash-table)))

    (maphash
     (lambda (key value)
       (log:info "config: ~A = ~A" key value))
     *config*)

    (flet ((get-config-value (key)
	     (let ((value (or (gethash key *config*)
			      (gethash key *default-config*)
			      (error "config does not contain key '~A'" key))))
	       ;; Some of the users of these values are very strict
	       ;; when it comes to string types... I'm looking at you,
	       ;; SB-BSD-SOCKETS:GET-HOST-BY-NAME.
	       (if (subtypep (type-of value) 'vector)
		   (coerce value 'simple-string)
		   value))))

      ;; Extract any config.ini settings here.
      (setf *server-uri* (get-config-value "server-uri"))

      ;; This is the directory where we check out policies.  Make sure it
      ;; ends with a trailing '/'.
      ;;
      (setf *config-dir*
	    (let ((dir (get-config-value "root-dir")))
	      (pathname
	       (if (str:ends-with? "/" dir)
		   (str:concat dir "config/")
		   (str:concat dir "/config/")))))

      ;; Initialize prometheus
      (initialize-metrics)

      (bt:make-thread (lambda () (scheduler:start-scheduler *scheduler*)))

      (cl-etcd:with-etcd (etcd (gethash "etcd" *config*) :on-leader #'become-leader :on-follower #'become-follower)

        (log:info "Starting server")

        (setf hunchentoot:*dispatch-table* +didit-dispatch-table+)
        (setf prom:*default-registry* *didit-registry*)
        (setf *print-pretty* nil)
        (setf *handler* (let ((exposer (make-instance 'exposer-acceptor :registry *didit-registry* :port (parse-integer (get-config-value "prometheus-port")))))
                          (hunchentoot:start (make-instance 'application
                                                            :document-root #p"./"
                                                            :port (parse-integer (get-config-value "server-port"))
                                                            :exposer exposer))))

        (bt:condition-wait *shutdown-cv* *server-lock*)))))

(defmethod hunchentoot:start ((app application))
  (hunchentoot:start (application-metrics-exposer app))
  (call-next-method))

(defmethod hunchentoot:stop ((app application) &key soft)
  (call-next-method)
  (hunchentoot:stop (application-metrics-exposer app) :soft soft))

(defmethod hunchentoot:acceptor-dispatch-request ((app application) request)
  (let ((labels (list (string-downcase (string (hunchentoot:request-method request)))
		      "didit_app")))
    (log:info *http-requests-counter*)
    (prom:counter.inc *http-requests-counter* :labels labels)
    (prom:histogram.time
     (prom:get-metric *http-request-duration* labels)
     (call-next-method))))
