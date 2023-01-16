(use-package orderless
  :ensure t

  :init
  (setq completion-category-defaults nil)

  :after migemo

  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-migemo))
  ;; https://github.com/oantolin/orderless#component-separator-regexp
  (orderless-component-separator #'orderless-escapable-split-on-space) ; escpae space

  :config
  ;; supported emacs-jp slack
  ;;(setq orderless-matching-styles '(orderless-migemo))
  ;;(setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-migemo)) ; move to :custom block
  (defalias 'orderless-migemo #'migemo-get-pattern)

  (defun orderless-migemo (component)
    (let ((pattern (migemo-get-pattern component)))
      (condition-case nil
          (progn (string-match-p pattern "") pattern)
	(invalid-regexp nil))))
  )

(provide '.orderless)
