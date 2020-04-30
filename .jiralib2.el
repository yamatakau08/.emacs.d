(require 'jiralib2)

(setq jiralib2-url company-jira-url)

;; for debug
;(setq request-log-level     'debug)
;(setq request-message-level 'debug)

;; since request-backed curl doesn't work, switch to url-retrive works fine.
;(setq request-backend 'url-retrieve)
