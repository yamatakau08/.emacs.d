(use-package my-confluence
  ;; :if (company-network-p) ; don't check if my pc is in company network

  :load-path "my-confluence"

  :custom
  ;; company-jira-auth-url,company-confluece-url variable set in .private.el
  (my-confluence-auth-url company-jira-auth-url)
  (my-confluence-url      company-confluence-url)
  (my-confluence-user-login-name "0000910700")

  :config
  (define-key dired-mode-map "e" #'my-confluence-create-or-update-attachment)

  ;; :bind (:map dired-mode-map
  ;;             ("e" . my-confluence-create-or-update-attachment))
  )
