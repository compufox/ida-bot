(defpackage :ida-bot.actions
  (:use :cl :ida-bot.util)

  (:import-from :ida-bot.config :env)
  (:import-from :drakma :http-request)
  (:import-from :com.inuoe.jzon :stringify)
  (:import-from :alexandria :alist-hash-table)
  
  (:export

   :send-chat
   :moderate-chat
   :moderate-user))
(in-package :ida-bot.actions)

(defun send-chat (message)
  "sends a chat message to the configured server"
  (owncast-request "/api/integrations/chat/send"
                   `(("body" . ,message))))
  

(defun moderate-chat (message-id set-visible)
  "moderates a chat message with id MESSAGE-ID, setting it visible based on SET-VISIBLE"
  (owncast-request "/api/chat/messagevisibility"
                   `(("visible" . ,set-visible)
                     ("idArray" . (,message-id)))))

(defun moderate-user (user-id set-enabled)
  "moderates a user with USER-ID, enabling them based on SET-ENABLED"
  (owncast-request "/api/chat/users/setenabled"
                   `(("userId" . ,user-id)
                     ("enabled" . ,set-enabled))))

(defun owncast-request (api-path payload)
  "runs the owncast request"
  (http-request (str:concat (env :stream-url) api-path)
                :method :post
                :content-type "application/json"
                :accept "application/json"
                :content (stringify (alist-hash-table payload))
                :additional-headers `(("Authorization" . ,(str:concat "Bearer " (env :access-token))))))
          
