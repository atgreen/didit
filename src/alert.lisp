(in-package #:didit)

(defclass alert ()
  ((config :initarg :config :reader config)))

(defclass alert/slack (alert)
  ((webhook-url :reader webhook-url)))

(defmethod initialize-instance :after ((alert alert/slack) &key)
  (with-slots (config channel token) alert
    (setf webhook-url (gethash "webhook-url" config))))

(defmethod send-alert ((alert/slack alert) message)
  (log:info "webhook ~A" (webhook-url alert))
  (log:info "ALERT: ~A" message)
  (multiple-value-bind (a b c d e f g)
      (drakma:http-request (webhook-url alert)
                           :method :post
                           :redirect 100
                           :parameters
                           `((:text . ,message)))
    (let ((result (flexi-streams:octets-to-string a :external-format :utf-8)))
      (log:info result))))
