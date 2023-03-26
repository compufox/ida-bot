(in-package :cl-user)
(defpackage ida-bot.util
  (:use :cl)
  (:import-from :bt
   :make-thread)
  (:import-from :ida-bot.conditions
                :thread-should-end)
  (:export

   :agetf
   :check-type-symbol
   :->
   :after
   :after-every
   :parse-time
   :ensure-list
   :log-info
   :log-debug
   :quit-app))

(in-package ida-bot.util)

(declaim (inline agetf))

(defun agetf (place indicator &optional default)
  (or (cdr (assoc indicator place :test #'equal))
      default))

(defun check-type-symbol (tsym tstr)
  (or (and (eq tsym :chat) (string= tstr "CHAT"))
      (and (eq tsym :name-changed) (string= tstr "NAME_CHANGED"))
      (and (eq tsym :user-joined) (string= tstr "USER_JOINED"))
      (and (eq tsym :stream-started) (string= tstr "STREAM_STARTED"))
      (and (eq tsym :stream-stopped) (string= tstr "STREAM_STOPPED"))
      (and (eq tsym :visibility-update) (string= tstr "VISIBILITY-UPDATE"))))

(defmacro -> ((&key name) &body body)
  (let ((n (or name (string (gensym)))))
    `(bt:make-thread #'(lambda ()
                         (block :thread
                           (handler-case
                               ,@body
                             (thread-should-end ()
                               (return-from :thread))
                             (error (e)
                               (log:error "encountered error on thread ~A: ~A~%" ,name e)))))
                     :name ,n)))

(defmacro after ((amount duration) &body body)
  "runs BODY after AMOUNT of DURATION"
  `(prog2 (sleep (parse-time ,amount ,duration))
       ,@body))

(defmacro after-every ((amount duration &key run-immediately) &body body)
  "runs BODY after every AMOUNT of DURATION

if RUN-IMMEDIATELY is non-nil, runs BODY once before waiting for next invocation"
  `(loop ,@(when run-immediately `(:initially ,@body))
         :do (sleep (parse-time ,amount ,duration))
         ,@body))

(defun parse-time (amount duration)
  "parses AMOUNT of DURATION into seconds"
  (* amount (cond
	     ((or (eq duration :seconds)
		  (eq duration :second))
	      1)
	     ((or (eq duration :minutes)
		  (eq duration :minute))
	      60)
	     ((or (eq duration :hours)
		  (eq duration :hour))
	      3600)
	     ((or (eq duration :days)
		  (eq duration :day))
	      86400)
	     ((or (eq duration :weeks)
		  (eq duration :week))
	      604800)
	     (t (error "unknown duration")))))
  
(defun ensure-list (obj)
  (typecase obj
    (list obj)
    (t (list obj))))

(defmacro log-info (&rest args)
  `(when (log:info) (log:info ,@args)))

(defmacro log-debug (&rest args)
  `(when (log:debug) (log:debug ,@args)))

(defun quit-app (code &rest args)
  "quits the app with specified CODE

ARGS, if passed, are treated as parameters for FORMAT and are used as such"
  (when args
    (funcall #'format `(t ,@args)))

  (uiop:quit code))
  
