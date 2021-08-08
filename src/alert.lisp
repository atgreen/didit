(in-package #:didit)

(defclass alert ()
  ((config :initarg :config :reader config)))

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

(defclass alert/tower (alert)
  ((host :reader host)
   (username :reader username)
   (password :reader password)
   (job-template :reader job-template)))

(defmethod initialize-instance :after ((alert alert/tower) &key)
  (with-slots (host username password job-template) alert
    (setf host (gethash "host" config))
    (setf username (gethash "username" config))
    (setf password (gethash "password" config))
    (setf job-template (gethash "job-template" config))))

(defmethod send-alert ((alert alert/tower) message)
  (log:info "alert/tower: ~A" message)
  (with-slots (host username password job-template) alert
    (let ((command (format nil "tower-cli --no-input -h ~A -u ~A -p ~A -J ~A"
                           host username password job-template)))
      (dolist (line (inferior-shell:run command))
        (log:info line)))))
