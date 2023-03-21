(in-package :ida-bot.extension)

(define-command ("motd")
  (send-chat (env :motd)))
