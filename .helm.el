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

;(require 'helm-config) ; https://github.com/emacs-helm/helm/wiki#if-installed-from-source
;(require 'helm) ; helm-config を require すると、helm-migiemo-mode を設定するとエラーになるので、(require 'helm) する

;;
;(when (featurep 'helm)
;  ;;(global-set-key (kbd "C-x b") 'helm-mini) ; assing frog-jump-buffer
;  (global-set-key (kbd "C-x f") 'helm-find-files)
;  ;; (global-set-key (kbd "C-x o") 'helm-buffers-list) ; to tentative setting
;;;
;;(when (featurep 'helm-occur)
;  ;; replace C-s/C-r to helm-occur
;  ;; firstly I replace C-s/C-r with helm-ag-this-file.
;  ;; but it's not available in buffers e.g. *GNU Emacs* is not file buffers
;  ;; helm-ag-this-file: Wrong type argument: stringp, nil
;  ;; it's better to use helm-occur in this case.
;  ;(define-key global-map (kbd "C-s") 'helm-occur)
;  ;(define-key global-map (kbd "C-r") 'helm-occur)
;;  )
;  )
