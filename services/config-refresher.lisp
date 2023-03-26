(in-package :ida-bot.extension)

(multiple-value-bind (opts args) (opts:get-opts)

  (defvar *config-modified-time* (file-write-date (getf opts :config))
    "variable that contains the last modified time for the config file")

  ;; defines a service that checks every 5 minutes (configurable) if the config file has 
  ;; been updated and reloads it, so we have a fresh version in memory
  (define-service ("config-refresher" :start-immediately t)
    (after-every ((env :refresh-config 5) :minutes)
      ;; check to see if the file's ATIME is newer than our cached version
      (when (> (file-write-date (getf opts :config)) *config-modified-time*)
        (setf *config-modified-time* (file-write-date (getf opts :config)))
        (ida-bot.config:load-config (getf opts :config))))))
