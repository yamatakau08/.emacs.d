(use-package orderless
  :ensure t

  :init
  (setq completion-category-defaults nil)

  ;;:after migemo
  ;; comment because without migemo, orderless doesn't work well

  :custom
  (completion-styles '(orderless basic)) ; see orderless https://github.com/oantolin/orderless#overview
  ;; modified the follwoing but doesn't work.
  ;;(completion-styles '(add-to-list 'completion-styles 'orderless))
  ;; conao3 pointed move to :config section (add-to-list 'completion-styles 'orderless)
  ;; that works but, since the order of completion-styles depends on loading package order that can't control
  ;; I follow the orderless overview

  (completion-category-overrides '((file (styles basic partial-completion))))

  ;; add condition "(if (executable-find "cmigemo") ...", orderless-migemo affect orderless work.
  (orderless-matching-styles (if (executable-find "cmigemo")
				 '(orderless-literal orderless-regexp orderless-migemo)
			       '(orderless-literal orderless-regexp)
			       ))
  ;; https://github.com/oantolin/orderless#component-separator-regexp
  (orderless-component-separator #'orderless-escapable-split-on-space) ; escpae space

  :config
  ;;(add-to-list 'completion-styles 'orderless) ; conao3 advice, see the comment :custom section

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
