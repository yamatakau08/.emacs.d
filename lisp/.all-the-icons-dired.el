(use-package all-the-icons-dired
  :ensure t

  :after (all-the-icons)

  :hook (dired-mode . all-the-icons-dired-mode))

(provide '.all-the-icons-dired)
