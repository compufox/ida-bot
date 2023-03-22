(defpackage :ida-bot.actions
  (:use :cl :ida-bot.util)

  (:import-from :ida-bot.config :env)
  (:import-from :drakma :http-request)
  (:import-from :com.inuoe.jzon :stringify)
  (:import-from :alexandria :alist-hash-table)
  
  (:export

   :send-chat
   :send-system-chat
   :send-system-chat-to-client
   :moderate-chat
   :moderate-user))
(in-package :ida-bot.actions)


(defun send-chat (message)
  "sends a chat message to the configured server"
  (owncast-request "/api/integrations/chat/send"
                   `(("body" . ,message))))

(defun send-system-chat (message)
  (owncast-request "/api/integrations/chat/system"
                   `(("body" . ,message))))

(defun send-system-chat-to-client (client-id message)
  (owncast-request (str:concat "/api/integrations/chat/system/client/" client-id)
                   `(("body" . ,message))))

(defun moderate-chat (message-id set-visible)
  "moderates a chat message with id MESSAGE-ID, setting it visible based on SET-VISIBLE"
  (owncast-request "/api/integrations/chat/messagevisibility"
                   `(("visible" . ,(when set-visible t))
                     ("idArray" . (,message-id)))))

(defun moderate-user (user-id set-enabled)
  "moderates a user with USER-ID, enabling them based on SET-ENABLED"
  (owncast-request "/api/chat/users/setenabled"
                   `(("userId" . ,user-id)
                     ("enabled" . ,(when set-enabled t)))))

(defun owncast-request (api-path payload)
  "runs the owncast request"
  (http-request (str:concat (env :stream-url) api-path)
                :method :post
                :content-type "application/json"
                :accept "application/json"
                :content (stringify (alist-hash-table payload))
                :additional-headers `(("Authorization" . ,(str:concat "Bearer " (env :access-token))))))
          
