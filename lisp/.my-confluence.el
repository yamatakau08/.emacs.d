(use-package my-confluence
  :load-path "my-confluence"

  :after (ox-confluence)

  :custom
  ;; company-jira-auth-url,company-confluece-url variable set in .private.el
  (my-confluence-auth-url company-jira-auth-url)
  (my-confluence-url      company-confluence-url)
  (my-confluence-username "0000910700")

  :demand t
  :bind (:map dired-mode-map ; demand t is needed
              ("e" . my-confluence-create-or-update-attachment))
  )

(provide '.my-confluence)
