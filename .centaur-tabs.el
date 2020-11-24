(use-package centaur-tabs
  :demand

  :config
  (centaur-tabs-mode t)

  :bind
  ("C-," . centaur-tabs-backward)
  ("C-." . centaur-tabs-forward)
  )
