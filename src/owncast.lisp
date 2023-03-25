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

(defun owncast-request (api-path &key payload (method :post) (content-type "application/json"))
  "runs the owncast request"
  (multiple-value-bind (resp status) (http-request (str:concat (env :stream-url) api-path)
                                                   :method method
                                                   :content-type content-type
                                                   :accept "application/json"
                                                   :content (when payload (stringify (alist-hash-table payload)))
                                                   :additional-headers `(("Authorization" . ,(str:concat "Bearer " (env :access-token)))))
    (let ((response (coerce (map 'vector #'code-char resp) 'string)))
      (values (alexandria:hash-table-alist (com.inuoe.jzon:parse response))
              status))))

(defun set-stream-title (new-title)
  (owncast-request "/api/integrations/streamtitle"
                   :payload 
                   `(("value" . ,new-title))))

;; chat functions 
(defun send-chat (message)
  "sends a chat message to the configured server"
  (owncast-request "/api/integrations/chat/send"
                   :payload 
                   `(("body" . ,message))))

(defun send-system-chat (message)
  (owncast-request "/api/integrations/chat/system"
                   :payload 
                   `(("body" . ,message))))

(defun send-chat-action (message &optional author)
  (owncast-request "/api/integrations/chat/action"
                   :payload
                   (append `(("body" . ,message))
                           (when author `(("author" . ,author))))))

(defun send-system-chat-to-client (client-id message)
  (owncast-request (str:concat "/api/integrations/chat/system/client/" client-id)
                   :payload 
                   `(("body" . ,message))))

;; moderation functions
(defun moderate-chat (message-id set-visible)
  "moderates a chat message with id MESSAGE-ID, setting it visible based on SET-VISIBLE"
  (owncast-request "/api/integrations/chat/messagevisibility"
                   :payload 
                   `(("visible" . ,(when set-visible t))
                     ("idArray" . (,message-id)))))
#|
;; this uses a non-integration api method and as such doesnt work! :upside_down:
(defun moderate-user (user-id set-enabled)
  "moderates a user with USER-ID, enabling them based on SET-ENABLED"
  (owncast-request "/api/chat/users/setenabled"
                   :payload 
                   `(("userId" . ,user-id)
                     ("enabled" . ,(when set-enabled t)))))
|#

(defun get-connected-clients ()
  (owncast-request "/api/integrations/clients" :method :get))

(defun get-chat-backlog ()
  (owncast-request "/api/integrations/chat" :method :get))

(defun get-information ()
  (owncast-request "/api/config" :method :get))

(defun get-current-status ()
  (owncast-request "/api/status" :method :get))
