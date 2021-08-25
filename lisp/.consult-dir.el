(use-package consult-dir
  :ensure t

  :bind
  (("C-x C-d" . consult-dir)
   :map minibuffer-local-completion-map
   ("C-x C-d" . consult-dir)
   ;;("C-x C-j" . consult-dir-jump-file)) ; comment because C-x C-j binds SKK
   )
  )

(provide '.consult-dir)

