(defpackage ida-bot.commands
  (:use :cl :ida-bot.util)
  (:export :define-command
   :process-commands
   :*command-data*
   :*command-message*))
(in-package :ida-bot.commands)

(defvar *command-data* nil
  "alist structure containing the webhook eventdata payload")

(defvar *command-message* ""
  "string containing the command chat message minus the command string itself")

(defvar *moderator-only-message* "Only moderators have permission to use this command"
  "message sent to non-moderator users when they try to run a command marked as moderator only")

(defvar *authenticated-only-message* "Only authenticated users have permission to use this command"
  "message sent to unauthenticated users when they try to run a command marked as authenticated users only")

(defvar *commands* (make-hash-table :test 'equal)
  "hash-table of commands

key is command string
value is command function")

(defmacro define-command ((command &key moderator-only authenticated-only) &body body)
  "create a command COMMAND

if MODERATOR-ONLY is non-nil then the command can only be ran by users marked as moderators
if AUTHENTICATED-ONLY is non-nil then the command can only be ran by chatters who are authenticated"
  (let ((cmd (str:concat "!" command)))
    
    ;; ensure each command has unique commands
    (if (gethash cmd *commands*)
        (log:warn "A command with the trigger '~A' already exists. Not loading current command.~%" command)
        `(setf (gethash ,cmd *commands*)
               #'(lambda (it)
                   (let* ((event-type (agetf it "type"))
                          (event-data (agetf it "eventData"))
                          (*command-data* event-data))
                     (when (and (check-type-symbol :chat event-type)
                                (str:starts-with-p ,cmd (agetf event-data "body")))
                       (let ((*command-message* (str:replace-first ,cmd "" (agetf event-data "body"))))
                         ,@(if moderator-only
                               `((if (member "MODERATOR" (agetf (agetf event-data "user") "scopes")
                                             :test #'string=)
                                     (progn ,@body)
                                     (ida-bot.actions:send-system-chat-to-client (agetf (agetf event-data "user") "clientId")
                                                                                 *moderator-only-message*)))
                               (if authenticated-only
                                   `((if (agetf (agetf event-data "user") "authenticated")
                                         (progn ,@body)
                                         (ida-bot.actions:send-system-chat-to-client (agetf (agetf event-data "user") "clientId")
                                                                                     *authenticated-only-message*)))
                                     `(,@body)))))))))))

(defun process-commands (message)
  "process each command based on priority"
  (loop :for func :being :the :hash-value :of *commands*
        :do (funcall func message)))
    
