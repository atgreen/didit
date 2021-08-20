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
  (alert nil :type alert)
  (token nil :type string))

(defvar *hunchentoot-server* nil)

(defun do-version ()
  +didit-version+)

(defun do-didit ()
  (bt:with-lock-held (*didit-lock*)
    (if (cl-etcd:get-etcd (hunchentoot:request-uri*) *etcd*)
        (progn
          (log:info "didit: ~A" (hunchentoot:request-uri*))
          (setf (cl-etcd:get-etcd (str:concat "done" (hunchentoot:request-uri*)) *etcd*) "didit")
          (format nil "~A~%" (hunchentoot:request-uri*) "\n"))
        ;; return 404?
        )))

(defparameter *disconneted-test* nil)

(defvar *didit-lock* (bt:make-lock))

(defun check-didit (didit-key)
  "Check whether a didit was done, and send alert if required."
  (log:info "check-didit ~A" didit-key)
  (bt:with-lock-held (*didit-lock*)
    (let ((done? (cl-etcd:get-etcd (str:concat "done" didit-key) *etcd*))
          (didit (gethash didit-key *didit-table*)))
      (format t ">> ~A~%" done?)
      (if done?
          (cl-etcd:delete-etcd (str:concat "done" didit-key) *etcd*)
          (send-alert (didit-alert didit) (didit-name didit))))))
