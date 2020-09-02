(use-package helm
  :ensure t

  :custom
  ;; https://github.com/syohex/emacs-helm-ag#enable-helm-follow-mode-by-default
  ;; in helm-ag candidate buffer, selected search result with ctl-n or ctl-p
  ;; other window follow that action and shows the part selected in candidate buffer
  ;; need to execcute C-c C-f in helm minibuffer to enable helm-follow-mode
  (helm-follow-mode-persistent t)

  :config
  (helm-migemo-mode t) ; enable migemo

  ;; found define another function is useful
  ;;(defun advice-around:helm-mini (orig-func &rest args)
  ;; (helm-posframe-enable)
  ;;  (apply orig-func args)
  ;;  (helm-posframe-disable))
  ;; (advice-add 'helm-mini :around  #'advice-around:helm-mini)

  (defun helm-mini-posframe ()
    (interactive)
    (helm-posframe-enable)
    (helm-mini)
    (helm-posframe-disable))

  :bind (("C-x b" . helm-mini) ; not to assign for frog-jump-buffer
	 ("C-x f" . helm-find-files)
	 ;;("C-x o" . helm-buffers-list))
	 ;;("C-s" . helm-occur) ; Since simple C-s and C-r is useful, don't use helm-occur
	 ;;("C-r" . helm-occur) ;
	 )
  )
