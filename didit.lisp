;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DIDIT; Base: 10 -*-
;;;
;;; Copyright (C) 2021  Anthony Green <green@moxielogic.com>
;;;
;;; Didit is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your
;;; option) any later version.
;;;
;;; Didit is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Didit; see the file COPYING3.  If not see
;;; <http://www.gnu.org/licenses/>.

;; Top level for didit

(in-package :didit)

(defun print-hash-entry (key value)
  (format t "The value associated with the key ~S is ~S~%" key value))
; (maphash #'print-hash-entry *c*)

;; ----------------------------------------------------------------------------
;; Get the version number at compile time.  This comes from
;; DIDIT_VERSION (set on the linux container build commandline), or
;; from git at compile-time.  Use UNKNOWN if all else fails.

;; This can come from build time...
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defparameter +didit-git-version+
    (inferior-shell:run/ss
     "(test -d .git && git describe --tags --dirty=+) || echo UNKNOWN")))

;; But this must come from runtime...
(defparameter +didit-version+
  (let ((v +didit-git-version+))
    (if (equal v "UNKNOWN")
 	(or (uiop:getenv "DIDIT_VERSION") v)
 	v)))

(defvar *config* nil)
(defvar *default-config* nil)
(defparameter +default-config-text+
"didit-uri = \"http://localhost:8080\"
config-repo = \"http://srv.labdroid.net:3000/green/didit-test\"
root-dir = \"/tmp/var/didit/\"
")

(defvar *didit-uri* nil)
;; Our server....

(defparameter *didit-registry* nil)
(defparameter *http-requests-counter* nil)
(defparameter *http-request-duration* nil)

(defstruct didit
  (scheduler-task nil)
  (name nil :type string)
  (alert nil :type alert)
  (token nil :type string)
  (done nil))

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

(defvar *thread-pool* (thread-pool:make-thread-pool 10))
(defvar *scheduler* (make-instance 'scheduler:in-memory-scheduler))

(defvar *didit-table* (make-hash-table :test 'equal))
(defvar *alerts-table* (make-hash-table :test 'equal))

(defvar *hunchentoot-server* nil)

(defvar *default-port-string* "8080")

(defun do-version ()
  +didit-version+)

(defun do-didit ()
  (bt:with-lock-held (*didit-lock*)
    (let ((didit (gethash (hunchentoot:request-uri*) *didit-table*)))
      (log:info "~A ~A" didit (hunchentoot:request-uri*))
      (if didit
          (setf (didit-done didit) t)
          ;; TODO throw error 500?
          )))
  "didit")

(defparameter +didit-dispatch-table+
  (list
   (hunchentoot:create-prefix-dispatcher "/version" 'do-version)
   (hunchentoot:create-prefix-dispatcher "/didit" 'do-didit)
   (hunchentoot:create-prefix-dispatcher "/webhook" 'do-webhook)))

(defclass exposer-acceptor (prom.tbnl:exposer hunchentoot:acceptor)
  ())

(defclass application (hunchentoot:easy-acceptor)
  ((exposer :initarg :exposer :reader application-metrics-exposer)
   (mute-access-logs :initform t :initarg :mute-access-logs :reader mute-access-logs)
   (mute-messages-logs :initform t :initarg :mute-error-logs :reader mute-messages-logs)))

(defmacro start-server (&key (handler '*handler*) (port 8080))
  "Initialize an HTTP handler"
  `(progn
     (setf *print-pretty* nil)
     (setf hunchentoot:*dispatch-table* +didit-dispatch-table+)
     (setf prom:*default-registry* *didit-registry*)
     (bt:make-thread (lambda () (scheduler:start-scheduler *scheduler*)))
     (let ((exposer (make-instance 'exposer-acceptor :registry *didit-registry* :port 9101)))
       (log:info "About to start hunchentoot")
       (setf ,handler (hunchentoot:start (make-instance 'application
							:document-root #p"./"
							:port ,port
							:exposer exposer))))))

(defmacro stop-server (&key (handler '*handler*))
  "Shutdown the HTTP handler"
  `(hunchentoot:stop ,handler))

(defparameter *disconneted-test* nil)

(defun pull-repo (repo-dirname repo-git-uri)
  "Download the very latest version of REPO-GIT-URI into REPO-DIRNAME
from a git repo."
  (unless *disconneted-test*
    (let ((command (if (fad:directory-exists-p repo-dirname)
                       (format nil "bash -c \"(cd ~A; /usr/bin/git pull)\""
                               repo-dirname)
                       (format nil "GIT_TERMINAL_PROMPT=0 /usr/bin/git clone --depth 1 ~A ~A"
                               repo-git-uri repo-dirname))))
      (dolist (line (inferior-shell:run command))
        (log:info line)))))

(defvar *didit-lock* (bt:make-lock))

(defun check-didit (didit-key)
  "Check whether a didit was done, and send alert if required."
  (log:info "check-didit ~A" didit-key)
  (let ((done? nil))
    (bt:with-lock-held (*didit-lock*)
      (let ((didit (gethash didit-key *didit-table*)))
        (setf done? (didit-done didit))
        (setf (didit-done didit) nil)))
    (unless done?
      (send-alert (didit-alert didit) (didit-name didit)))))

;; Start the web app.

(defun start-didit (&rest interactive)
  "Start the web application and have the main thread sleep forever,
  unless INTERACTIVE is non-nil."

  (setf hunchentoot:*show-lisp-errors-p* t)
  (setf hunchentoot:*show-lisp-backtraces-p* t)

  ;; We never git commit, but git will complain even when cloning if
  ;; we run in a container without a known user unless...
  (sb-posix:setenv "GIT_COMMITTER_NAME" "didit" 1)
  (sb-posix:setenv "GIT_COMMITTER_EMAIML" "didit@example.com" 1)

  (log:info "Starting didit version ~A" +didit-version+)

  ;; Read the built-in configuration settings.
  (setf *default-config* (cl-toml:parse +default-config-text+))
  (log:info +default-config-text+)

  ;; Read the user configuration settings.
  (setf *config*
	(if (fad:file-exists-p "/etc/didit/config.ini")
	    (cl-toml:parse
	     (alexandria:read-file-into-string "/etc/didit/config.ini"
					       :external-format :latin-1))
	    (make-hash-table)))

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

    (setf *server-uri* (or (uiop:getenv "DIDIT_URI")
			   (get-config-value "didit-uri")))
    (unless (didit.util:valid-url? *server-uri*)
      (error "didit-uri is not valid URL: ~A" *didit-uri*))

    (log:info "About to initialize config-dir")

    ;; This is the directory where we check out policies.  Make sure it
    ;; ends with a trailing '/'.
    ;;
    (setf *config-dir*
	  (let ((dir (get-config-value "root-dir")))
	    (pathname
	     (if (str:ends-with? "/" dir)
		 (str:concat dir "config/")
		 (str:concat dir "/config/")))))

    ;; Pulling git-hosted config
    (pull-repo *config-dir* (get-config-value "config-repo"))

    ;; Load the config.ini file
    (let* ((config (let ((repo.ini-filename
                           (merge-pathnames *config-dir* "repos.ini")))
                     (if (fad:file-exists-p repo.ini-filename)
                         (cl-toml:parse
                          (alexandria:read-file-into-string repo.ini-filename
                                                            :external-format :latin-1))
                         (make-hash-table))))
           (repos (gethash "repos" config)))
      (when repos
        (maphash
         (lambda (key value)
           (let* ((repo (gethash "repo" value))
                  (prefix (gethash "prefix" value))
                  (repo-dirname (str:concat (namestring (get-config-value "root-dir"))
                                            (subseq (ironclad:byte-array-to-hex-string
                                                     (ironclad:digest-sequence
                                                      :sha1 (flexi-streams:string-to-octets repo)))
                                                    0 8))))
             (pull-repo repo-dirname repo)
             (let* ((didit.ini-filename (concatenate 'string repo-dirname "/didit.ini"))
                    (didit.ini (if (fad:file-exists-p didit.ini-filename)
                                   (cl-toml:parse
                                    (alexandria:read-file-into-string didit.ini-filename
                                                                      :external-format :latin-1))
                                   (make-hash-table))))
               (maphash #'print-hash-entry didit.ini)
               ;; Process all of the alerts entries
               (let ((alerts (gethash "alerts" didit.ini)))
                 (maphash (lambda (key value)
                            (setf (gethash (format nil "~A/~A" prefix key) *alerts-table*)
                                  (make-instance (read-from-string
                                                  (str:concat "didit:alert/" (gethash "type" value))) :config value)))
                          alerts))
               ;; Process all of the didit entries
               (let ((didits (gethash "didit" didit.ini)))
                 (maphash (lambda (key value)
                            (let ((cron (gethash "cron" value))
                                  (token (gethash "token" value)))
                              (log:info "~A ~A" cron token)
                              (if (not (= 5 (length (split-sequence:split-sequence #\Space cron))))
                                  (error "Invalid schedule format: ~A" cron))
                              (log:info "/didit/~A/~A" prefix token)
                              (setf (gethash (format nil "/didit/~A/~A" prefix token) *didit-table*)
                                    (make-didit
                                     :name (gethash "name" value)
                                     :alert (gethash (format nil "~A/~A" prefix (gethash "alert" value)) *alerts-table*)
                                     :token token
                                     :scheduler-task (scheduler:create-scheduler-task
                                                      *scheduler*
                                                      (format nil "~A (didit:check-didit \"/didit/~A/~A\")" cron prefix token))))
                              (log:info ">> ~A" (gethash (format nil "/didit/~A/~A" prefix token) *didit-table*))))
                          didits)
                 (log:info didits)))))
         repos)))

    (log:info "About to initialize metrics")

    (initialize-metrics)

    (log:info "About to start server")

    (thread-pool:start-pool *thread-pool*)

    (let ((srvr (start-server)))
      ;; If SLEEP-FOREVER? is NIL, then exit right away.  This is used by the
      ;; testsuite.
      (log:info "About to enter sleep loop")
      (loop
        (sleep 3000)))))

(defun stop-didit ()
  "Stop the web application."
  (hunchentoot:stop *hunchentoot-server*))

(defmethod hunchentoot:start ((app application))
  (hunchentoot:start (application-metrics-exposer app))
  (call-next-method))

(defmethod hunchentoot:stop ((app application) &key soft)
  (call-next-method)
  (hunchentoot:stop (application-metrics-exposer app) :soft soft))

(defmethod hunchentoot:acceptor-dispatch-request ((app application) request)
  (let ((labels (list (string-downcase (string (hunchentoot:request-method request)))
		      "didit")))
    (prom:counter.inc *http-requests-counter* :labels labels)
    (prom:histogram.time
     (prom:get-metric *http-request-duration* labels)
     (call-next-method))))
