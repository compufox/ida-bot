(defpackage ida-bot.handler
  (:use :cl :ida-bot.util)
  (:export :define-handler
           :run-handlers))
(in-package :ida-bot.handler)

(defvar *handlers* nil
  "list of defined handlers")

(defclass handler ()
  ((id :initarg :id
       :reader handler-id )
   (priority :initarg :priority
             :reader handler-priority)
   (function :initarg :function
             :reader handler-function)))

(defmacro define-handler ((id &key (type :chat) priority) &body body)
  (if (member id *handlers* :key #'handler-id :test #'equal)
      (format t "A handler with the id '~A' already exists. Not loading current handler" id)
      `(prog1
           (push (make-instance 'handler
                                :id ,id
                                :priority ,(or priority (1+ (length *handlers*)))
                                :function
                                (lambda (it)
                                  (let ((event-type (agetf it "type"))
                                        (event-data (agetf it "eventData")))
                                    (when (check-type-symbol ,type event-type)
                                      ,@body))))
                 *handlers*)
         (setf *handlers* (sort *handlers* #'< :key #'handler-priority)))))

(defun run-handlers (incoming-message)
  (loop :for hndlr :in *handlers*
        :do (funcall (handler-function hndlr) incoming-message)))
                                   
