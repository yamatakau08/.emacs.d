(use-package orderless
  :ensure t

  :init
  (setq ; completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))

  :custom
  (completion-styles '(orderless))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-migemo))

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
