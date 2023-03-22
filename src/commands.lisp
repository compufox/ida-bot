(defpackage ida-bot.commands
  (:use :cl :ida-bot.util)
  (:export :define-command
   :process-commands
   :*command-data*))

(in-package :ida-bot.commands)

;; here is where i need to write the command loader
;; this should ideally load a directory with all
;; 

(defvar *command-data* nil
  "")

(defvar *commands* nil
  "list of commands")

(defclass bot-command ()
  ((command :reader command-string
            :initarg :command)
   (function :accessor command-function
             :initarg :function)))

(defmacro define-command ((command &key (type :chat)) &body body)
  "create a command COMMAND"
  (let ((cmd (str:concat "!" command)))
    
    ;; ensure each command has unique commands
    (if (member command *commands* :key #'command-string :test #'equal)
        (log:warn "A command with the trigger '~A' already exists. Not loading current command.~%" command)
        `(prog1
             (push (make-instance 'bot-command :command ,cmd
                                               :function
                                               (lambda (it)
                                                 (let* ((event-type (agetf it "type"))
                                                        (event-data (agetf it "eventData"))
                                                        (*command-data* event-data))
                                                   (when (and (check-type-symbol ,type event-type)
                                                              (str:starts-with-p ,cmd (agetf event-data "body")))
                                                     ,@body))))
                   *commands*)))))

(defun process-commands (message)
  "process each command based on priority"
  (loop :for cmd :in *commands*
        :do (funcall (command-function cmd) message)))
      
    
