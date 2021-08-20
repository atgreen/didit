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

(in-package #:didit)

(defclass alert ()
  ((config :initarg :config :reader config)))

#|
===========================================================================
Slack Alerts

These use slack webhooks.
===========================================================================
|#

(defclass alert/slack (alert)
  ((webhook-url :reader webhook-url)))

(defmethod initialize-instance :after ((alert alert/slack) &key)
  (with-slots (config webhook-url) alert
    (setf webhook-url (gethash "webhook-url" config))))

(defmethod send-alert ((alert alert/slack) message)
  (log:info "alert/slack: ~A" message)
  (multiple-value-bind (a b c d e f g)
      (drakma:http-request (webhook-url alert)
                           :method :post
                           :content-type "application/json"
                           :redirect 100
                           :content (json:encode-json-to-string
                                     `((:TEXT . ,(str:concat "ALERT: " message)))))
    (log:info a)))

#|
===========================================================================
AWX/Tower Alerts

We use the tower-cli tool.
===========================================================================
|#

(defclass alert/tower (alert)
  ((host :reader host)
   (username :reader username)
   (password :reader password)
   (job-template :reader job-template)))

(defmethod initialize-instance :after ((alert alert/tower) &key)
  (with-slots (config host username password job-template) alert
    (setf host (gethash "host" config))
    (setf username (gethash "username" config))
    (setf password (gethash "password" config))
    (setf job-template (gethash "job-template" config))))

(defmethod send-alert ((alert alert/tower) message)
  (log:info "alert/tower: ~A" message)
  (with-slots (host username password job-template) alert
    (let ((command (format nil "tower-cli job launch --no-input -h ~A -u ~A -p ~A -J ~A"
                           host username password job-template)))
      (dolist (line (inferior-shell:run command))
        (log:info line)))))
