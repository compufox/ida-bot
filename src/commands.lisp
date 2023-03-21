(defpackage ida-bot.commands
  (:use :cl :ida-bot.util)
  (:export :define-command
           :process-commands))

(in-package :ida-bot.commands)

;; here is where i need to write the command loader
;; this should ideally load a directory with all
;; 

(defvar *commands* nil
  "list of commands")

(defclass bot-command ()
  ((priority :accessor command-priority
             :initarg :priority)
   (type :reader command-type
         :initarg :type)
   (command :reader command-string
            :initarg :command)
   (function :accessor command-function
             :initarg :function)))

(defmacro define-command ((command &key priority (type :chat)) &body body)
  "create a command COMMAND"
  (let ((pri (or priority (1+ (length *commands*))))
        (cmd (str:concat "!" command)))
    
    ;; ensure each command has unique commands
    (unless (some #'(lambda (x)
                      (string= command (command-string x)))
                  *commands*)
      `(prog1
           (push (make-instance 'bot-command :command ,cmd
                                             :priority ,pri
                                             :type ,type
                                             :function
                                             (lambda (it)
                                               (let ((type (agetf it "type"))
                                                     (data (agetf it "eventData")))
                                                 (when (and (check-type-symbol ,type type)
                                                            (str:starts-with-p ,cmd (agetf data "body")))
                                                   ,@body))))
                 *commands*)
         (setf *commands* (sort *commands* #'< :key #'command-priority))))))

(defun process-commands (message)
  "process each command based on priority"
  (loop :for cmd :in *commands*
        :do (funcall (command-function cmd) message)))

(in-package :cl-user)
(defpackage ida-bot.extension
  (:use :cl :ida-bot.util :ida-bot.commands
        :ida-bot.actions :ida-bot.config)
  (:export :load-commands))
(in-package :ida-bot.extension)

(defun load-commands ()
  "loads all commands from subdirectory"
  (unless (uiop:directory-exists-p "./commands/")
    (format t "The command directory doesn't exist. Please create it and fill it with lisp commands"))

  (loop :for file :in (uiop:directory-files "./commands/" "*.lisp")
        :do (format t "Loading file ~A~%" file)
            (load file)))
      
    
