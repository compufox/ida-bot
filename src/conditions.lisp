(defpackage ida-bot.conditions
  (:use :cl)
  (:export

   :thread-should-end
   :dependency-not-found
   :unmet-dependency-id))
(in-package :ida-bot.conditions)

(define-condition thread-should-end () ()
  (:documentation ""))
  
(define-condition dependency-not-found ()
  ((dependency-id :initarg :dep-id
                  :reader unmet-dependency-id))
  (:documentation ""))
