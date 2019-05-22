(use-package helm-ag
  :ensure t)

;;; https://github.com/syohex/emacs-helm-ag#enable-helm-follow-mode-by-default
;;; in helm-ag candidate buffer, selected search result with ctr-n or ctr-p
;;; other window follow that action and shows the part you selected in candidate buffer
(setq helm-follow-mode-persistent t)
