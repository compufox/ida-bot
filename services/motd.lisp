(in-package :ida-bot.extension)

(define-service ("MOTD Poster" :enabled t)
  (after-every (45 :minutes :run-immediately t)
     (send-chat (env :motd))))
