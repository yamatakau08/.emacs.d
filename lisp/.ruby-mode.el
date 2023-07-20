(use-package ruby-mode
  :custom
  ;; to supress "#coding: utf-8" is automatically inserted at top of the ruby script after saving
  (ruby-insert-encoding-magic-comment nil)

  :bind
  (:map ruby-mode-map
	("C-M-d" . delete-trailing-whitespace)) ; originally assinged smie-down-list in ruby-mode-map
  )

(provide '.ruby-mode)
