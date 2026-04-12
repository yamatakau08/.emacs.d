(use-package esqlite
  :unless (eq window-system 'w32) ; Windows 11 doesn't have echo, tentative use this condition
  :ensure t)

(provide '.esqlite)

