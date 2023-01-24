(use-package flycheck
  :ensure t
  :unless (eq (window-system) 'w32) ; on Windows environment, flycheck makes the editing too slow.
  :hook (ruby-mode . flycheck-mode)
  )

(provide '.flycheck)
