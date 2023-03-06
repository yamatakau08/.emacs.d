(use-package compat

  :ensure t

  :config
  ;; https://github.com/minad/consult/issues?q=%22compat-string-width%22
  ;; to suppress consult--display-width: Symbol’s function definition is void: compat-string-width
  ;; add compat-string-width manually
  ;; https://github.com/blester125/dotfiles/blob/master/doom.d/config.el#L47
  (defun compat-string-width (STRING &optional FROM TO)
    (string-width STRING FROM TO))
  )

(provide '.compat)
