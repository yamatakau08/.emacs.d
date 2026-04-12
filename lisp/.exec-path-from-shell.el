(use-package exec-path-from-shell
  :unless (eq window-system 'w32)
  :ensure t
  :config (exec-path-from-shell-initialize)
 )

(provide '.exec-path-from-shell)
