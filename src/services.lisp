(defpackage ida-bot.services
  (:use :cl :ida-bot.util)
  (:export

   :service-enabled
   :service-id
   :define-service
   :search-for-service
   :start-service
   :stop-service
   :start-services
   :stop-services))
   
(in-package :ida-bot.services)

(defvar *services* nil
  "list containing all of our services")

(defclass service ()
  ((id :initarg :id
       :reader service-id)
   (function :initarg :function
             :reader service-function)
   (enabled :initarg :enabled
            :accessor service-enabled-p)
   (running :initform nil
            :accessor service-running-p)))

(defmethod (setf service-enabled) (set-enabled (srv service))
  (setf (slot-value srv 'enabled) set-enabled)

  (if set-enabled
    (start-service srv)
    (stop-service srv)))

(defmethod start-service ((srv service))
  (setf (service-running srv) t)
  (-> (:name (service-id srv))
      (funcall (service-function srv))))

(defmethod stop-service ((srv service))
  (setf (service-running srv) nil)
  (bt:join-thread (find-if (lambda (th)
                             (search (service-id srv) (bt:thread-name th)))
                           (bt:all-threads))))

(defmacro define-service ((id &key enabled) &body body)
  (if (member id *services* :key #'service-id :test #'equal)
      (format t "Service with ID '~A' already exists. Not loading current service~&" id)
      `(prog1 (push (make-instance 'service
                                   :id ,id
                                   :enabled ,enabled
                                   :function
                                   (lambda ()
                                     ,@body))
                    *services*))))

(defun search-for-service (id)
  (member id *services* :key #'service-id :test #'equal))

(defun start-services ()
  (mapcar #'start-service
          (remove-if-not #'(lambda (s)
                             (or (service-enabled-p s)
                                 (service-running-p s)))
                         *services*)))

(defun stop-services ()
  (mapcar #'stop-service (remove-if-not #'service-running-p *services*)))
