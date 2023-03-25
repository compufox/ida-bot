(defpackage ida-bot.commands
  (:use :cl :ida-bot.util :ida-bot.moderator)
  (:export :define-command
   :process-commands
   :*command-data*
   :*command-message*))
(in-package :ida-bot.commands)

(defvar *command-data* nil
  "alist structure containing the webhook eventdata payload")

(defvar *command-message* ""
  "string containing the command chat message minus the command string itself")

(defvar *commands* (make-hash-table :test 'equal)
  "hash-table of commands

key is command string
value is command function")

(defmacro define-command ((command &key moderator-only) &body body)
  "create a command COMMAND"
  (let ((cmd (str:concat "!" command)))
    
    ;; ensure each command has unique commands
    (if (member command *commands* :key #'command-string :test #'equal)
        (log:warn "A command with the trigger '~A' already exists. Not loading current command.~%" command)
        `(setf (gethash ,cmd *commands*)
               #'(lambda (it)
                   (let* ((event-type (agetf it "type"))
                          (event-data (agetf it "eventData"))
                          (*command-data* event-data))
                     (when (and (check-type-symbol :chat event-type)
                                (str:starts-with-p ,cmd (agetf event-data "body")))
                       (let ((*handler-message* (str:replace-first ,cmd "" (agetf "body" event-data))))
                         ,@(if moderator-only
                               `((when (moderator-p (agetf (agetf event-data "user") "id"))
                                   ,@body))
                               `(,@body))))))))))

(defun process-commands (message)
  "process each command based on priority"
  (loop :for cmd :in *commands*
        :do (funcall (gethash cmd *commands*) message)))
    
