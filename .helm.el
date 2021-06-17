(use-package helm
  :ensure t

  :custom
  ;; https://github.com/syohex/emacs-helm-ag#enable-helm-follow-mode-by-default
  ;; in helm-ag candidate buffer, selected search result with ctl-n or ctl-p
  ;; other window follow that action and shows the part selected in candidate buffer
  ;; need to execcute C-c C-f in helm minibuffer to enable helm-follow-mode
  (helm-follow-mode-persistent t)

  :config
  (if (featurep 'migemo)
      ;; enable migemo
      (helm-migemo-mode t))

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

  :bind (;;("C-x b" . helm-mini) ; comment this bind, because use consult-buffer
	 ("C-x f" . helm-find-files)
	 ;;("C-x f" . helm-multi-files)
	 ;;("C-x f" . helm-mini)
	 ;;("M-x"   . helm-M-x) ; comment, because use "vertico" alternatively.
	 ;;("C-x o" . helm-buffers-list))
	 ;;("C-s" . helm-occur) ; Since simple C-s and C-r is useful, don't use helm-occur
	 ;;("C-r" . helm-occur) ;
	 )
  )
