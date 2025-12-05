(use-package dired-aux
  :config
  (add-to-list 'shell-command-guess-functions #'shell-command-guess-open t) ; The argument t of add-to-list is required 
  )
(provide '.dired-aux)
