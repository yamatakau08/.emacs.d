(use-package company
  :ensure t
  :init (global-company-mode t)
  :bind  (:map company-active-map
	       ("C-n" . company-select-next)
	       ("C-p" . company-select-previous)))
