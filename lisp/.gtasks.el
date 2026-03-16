(use-package gtasks
  :ensure t

  :config
  (setq gtasks--redirect-uri "http://localhost")

  (setopt gtasks-client-id (plist-get (car (auth-source-search :host "gtasks")) :client-id))
  (setopt gtasks-client-secret (auth-source-pick-first-password :host "gtasks"))

  )

(provide '.gtasks)
