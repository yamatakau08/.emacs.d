(use-package frog-jump-buffer
  :ensure t

  :custom
  (frog-jump-buffer-default-filter 'frog-jump-buffer-filter-recentf)
  (frog-jump-buffer-max-buffers 20)

  :config
  ;; redfine original function
  ;; fixed https://github.com/waymondo/frog-jump-buffer/issues/16
;;  (defun frog-jump-buffer-find-or-create-buffer (res)
;;    "Switch to buffer, or if closed, find and create it first."
;;    (let ((buffer (if frog-jump-buffer-include-virtual-buffers
;;		      (find-file (assoc-default res (frog-jump-buffer-recentf-buffers)))
;;                    res)))
;;      (if frog-jump-buffer-target-other-window
;;          ;;(switch-to-buffer-other-window buffer) ; original
;;	  (progn
;;	    (message "[debug] frog-jump-buffer-find-or-create-buffer 1")
;;            (switch-to-buffer-other-window nil)) ; my modification
;;	(switch-to-buffer buffer))))

  :bind (;;("C-x b" . frog-jump-buffer)
	 ("C-x c" . frog-jump-buffer-other-window)))
