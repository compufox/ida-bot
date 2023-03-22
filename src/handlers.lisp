(defpackage ida-bot.handler
  (:use :cl :ida-bot.util)
  (:import-from :ida-bot.conditions
                :dependency-not-found)
  (:export :define-handler
   :run-handlers
   :*handler-data*))
(in-package :ida-bot.handler)

(defvar *handler-data* nil
  "")

(defvar *handlers* nil
  "list of defined handlers")

(defclass handler ()
  ((id :initarg :id
       :reader handler-id )
   (priority :initarg :priority
             :reader handler-priority)
   (dependencies :initarg :deps
                 :reader handler-deps)
   (function :initarg :function
             :reader handler-function)))

(defmacro define-handler ((id &key (type :chat) priority depends-on) &body body)
  (if (member id *handlers* :key #'handler-id :test #'equal)
      (log:warn "Handler with id '~A' already loaded, skipping loading.~%")
      (if (and depends-on (not (find depends-on *handlers* :key #'handler-id :test #'equal)))
          (signal 'dependency-not-found :dep-id id)
          `(prog1
               (push (make-instance 'handler
                                    :id ,id
                                    
                                    ;; TODO: needs to factor in dependencies
                                    :priority (or ,priority (1+ (length *handlers*)))
                                    :deps (ensure-list ,depends-on)
                                    :function
                                    (lambda (it)
                                      (let* ((event-type (agetf it "type"))
                                             (event-data (agetf it "eventData"))
                                             (*handler-data* event-data))
                                        (when (check-type-symbol ,type event-type)
                                          ,@body))))
                     *handlers*)
             (sort-handlers)))))

(defun sort-handlers ()
  (setf *handlers* (sort *handlers* #'< :key #'handler-priority)))

(defun run-handlers (incoming-message)
  (loop :for hndlr :in *handlers*
        :do (funcall (handler-function hndlr) incoming-message)))
                                   
