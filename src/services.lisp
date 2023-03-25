(defpackage ida-bot.services
  (:use :cl :ida-bot.util)
  (:import-from :ida-bot.conditions
                :thread-should-end)
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
   (wants-stream :initarg :wants-stream
                 :reader :service-wants-stream-p)
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
  (setf (slot-value srv 'running) t)
  (-> (:name (service-id srv))
      (funcall (service-function srv))))

(defmethod stop-service ((srv service))
  (setf (slot-value srv 'running) nil)
  (flet ((end-thread () (signal 'thread-should-end)))
    (let ((thread (find-if (lambda (th)
                             (search (service-id srv) (bt:thread-name th)))
                           (bt:all-threads))))
      (bt:interrupt-thread thread #'end-thread)
      (bt:join-thread thread))))

(defmacro define-service ((id &key start-immediately start-with-stream) &body body)
  (if (member id *services* :key #'service-id :test #'equal)
      (log:warn "Service with ID '~A' already exists. Not loading current service~&" id)
      `(let ((service (make-instance 'service
                                     :id ,id
                                     :enabled nil
                                     :wants-stream ,start-with-stream
                                     :function
                                     (lambda ()
                                       ,@body))))
         (push service *services*)
         ,(when start-immediately `(setf (service-enabled service) t))
         service)))

(defun search-for-service (id)
  (member id *services* :key #'service-id :test #'equal))

(defun start-services (&key stream-dependent)
  "starts services

if STREAM-DEPENDENT is non-nil, only starts services that are flagged as START-WITH-STREAM"
  (let ((services (if stream-dependent
                      (remove-if-not #'service-wants-stream-p *services*)
                      (remove-if #'service-wants-stream-p *services*))))
    (mapcar #'start-service services)))


(defun stop-services (&key stream-dependent all)
  "stops running services

if ALL is non-nil all services are stopped
if STREAM-DEPENDENT is non-nil only stops services that are flagged as START-WITH-STREAM"
  (let ((services (if stream-dependent
                      (remove-if-not #'service-wants-stream-p *services*)
                      (if all *services*
                          (remove-if #'service-wants-stream-p *services*)))))
    (mapcar #'stop-service (remove-if-not #'service-running-p services))))
