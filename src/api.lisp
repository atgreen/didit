;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DIDIT; Base: 10 -*-
;;;
;;; Copyright (C) 2021  Anthony Green <green@moxielogic.com>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(in-package :didit)

(defstruct didit
  (scheduler-task nil)
  (name nil :type string)
  (alert nil :type alert)
  (token nil :type string)
  (done nil))

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
