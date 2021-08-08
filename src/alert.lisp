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
