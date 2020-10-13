(use-package my-confluence
  ;; :if (company-network-p)
  :load-path "~/.emacs.d/my-confluence"
  :custom
  ;; company-jira-auth-url,company-confluece-url variable set in .private.el
  (my-confluence-auth-url company-jira-auth-url)
  (my-confluence-url      company-confluence-url)
  (my-confluence-user-login-name "0000910700"))
