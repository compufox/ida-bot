(in-package :cl-user)
(defpackage ida-bot.web
  (:use :cl :caveman2)
  (:import-from :ida-bot.util
   :agetf)
  (:import-from :ida-bot.commands
   :process-commands)
  (:import-from :ida-bot.handler
   :run-handlers
   :define-handler
   :*handler-data*)
  (:export :*web*))
(in-package :ida-bot.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Handlers that starts/stops stream dependent services
(define-handler ("service-starter" :type :stream-started)
  (log:info "starting stream-dependent services...")
    (ida-bot.services:start-services :stream-dependent t))

(define-handler ("service-ender" :type :stream-stopped)
  (log:info "stopping stream-dependent services...")
    (ida-bot.services:stop-services :stream-dependent t))

;;
;; Routing rules

@route POST "/webhook"
(defun parse-webhooks (&key _parsed)
  (format t "~A~%" _parsed)
  (run-handlers _parsed)
  (process-commands _parsed))
  

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
