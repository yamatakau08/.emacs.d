(use-package frog-jump-buffer
  :ensure t
  :bind (("C-x b" . frog-jump-buffer)
	 ("C-x c" . frog-jump-buffer-other-window)))

(custom-set-variables
 '(frog-jump-buffer-default-filter 'frog-jump-buffer-filter-recentf))
