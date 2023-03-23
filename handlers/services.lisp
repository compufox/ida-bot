(in-package :ida-bot.extension)

;; starts our services that are flagged as being used for the stream
(define-handler ("service-starter" :type :stream-started)
    (start-services :stream-dependent t))

;; stops our services that are flagged as being used for the stream
(define-handler ("service-ender" :type :stream-stopped)
    (stop-services :stream-dependent t))
