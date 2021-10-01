(use-package dired-narrow
  :ensure t

  :bind
  (:map dired-mode-map
	("n" . dired-narrow))

  )

(provide '.dired-narrow)

