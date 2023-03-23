(in-package :ida-bot.extension)

;; Defines a new service that posts the message of the day every 45 minutes
;; since :start-with-stream is non-nil, this service is started on demand
;; when the stream starts and will end when the stream stops
(define-service ("MOTD Poster" :start-with-stream t)
  (after-every (45 :minutes :run-immediately t)
     (send-chat (env :motd))))
