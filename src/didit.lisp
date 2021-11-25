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

(in-package :didit)

(defstruct didit
  (scheduler-task nil)
  (name nil :type string)
  (oneshot nil)
  (alert nil :type alert)
  (token nil :type string))

(defvar *didit-lock* (bt:make-lock "didit-lock"))
(defvar *didit-table* (make-hash-table :test 'equal))

(defun do-didit ()
  (log:info "do-didit!")
  (let ((request-uri (hunchentoot:request-uri*)))
    (bt:with-lock-held (*didit-lock*)
      (log:info "do-didit ~A" request-uri)
      (if (cl-etcd:get-etcd request-uri *etcd*)
        (progn
          (log:info "didit: ~A" request-uri)
          (setf (cl-etcd:get-etcd (str:concat "done" request-uri) *etcd*) "didit")
          (format nil "~A~%" request-uri "\n"))
        ;; return 404?
        ))))

(bt:with-lock-held (*didit-lock*)
  (log:info "Got lock!")
  555)

(defun check-didit (didit-key)
  "Check whether a didit was done, and send alert if required."
  (log:info "check-didit ~A" didit-key)
  (bt:with-lock-held (*didit-lock*)
    (let ((done? (cl-etcd:get-etcd (str:concat "done" didit-key) *etcd*))
          (didit (gethash didit-key *didit-table*)))
      (log:info "    done? = ~A" done?)
      (if done?
          (cl-etcd:delete-etcd (str:concat "done" didit-key) *etcd*)
          (send-alert (didit-alert didit) (didit-name didit)))
      (if (didit-oneshot didit)
          ;; TODO this isn't quite right yet
          (scheduler:delete-scheduler-task *scheduler* (didit-scheduler-task didit))))))
