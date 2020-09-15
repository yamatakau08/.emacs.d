(use-package dired-posframe
  :ensure t
  :custom
  (dired-posframe-file-size-limit (* 10 1024 1024))

  :bind
  (:map dired-mode-map
	("z" . dired-posframe-show) ; don't work,but dired-posframe-show works.
	)
  )
