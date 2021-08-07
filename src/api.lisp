(in-package :didit)

(defstruct didit
  (scheduler-task nil)
  (name nil :type string)
  (alert nil :type alert)
  (token nil :type string)
  (done nil))

(defvar *thread-pool* (thread-pool:make-thread-pool 10))
(defvar *scheduler* (make-instance 'scheduler:in-memory-scheduler))
;(setf *scheduler* (make-instance 'scheduler:in-memory-scheduler))
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
        (setf (didit-done didit) nil)
        (unless done?
          (send-alert (didit-alert didit) (didit-name didit)))))))
