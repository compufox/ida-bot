(in-package :cl-user)
(defpackage ida-bot.util
  (:use :cl)
  (:import-from :bt
                :make-thread)
  (:export

   :agetf
   :check-type-symbol
   :->))

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
    `(bt:make-thread #'(lambda () ,@body) :name ,n)))
