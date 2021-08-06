(in-package #:didit)

(defclass alert ()
  ((config :initarg :config :reader config)))

(defclass alert/slack (alert)
  ((token :reader token)
   (channel :reader channel)))

(defmethod initialize-instance :after ((alert alert/slack) &key)
  (with-slots (config channel token) alert
    (setf channel (gethash "channel" config))
    (setf alert (gethash "token" config))))

(defmethod send-alert ((alert/slack alert) message)
  (log:info "ALERT: ~A" message))
