(use-package gtasks
  :ensure t

  :custom
  (gtasks-client-id (plist-get (car (auth-source-search :host "gtasks")) :client-id))
  (gtasks-client-secret (auth-source-pick-first-password :host "gtasks"))

  :config
  (setq gtasks--redirect-uri "http://localhost")

  )

(provide '.gtasks)
