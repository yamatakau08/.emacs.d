(use-package cc-mode
  :custom
  (c-basic-offset 4)
  :hook
  (c-mode-common . (lambda () (c-set-style "k&r"))))
