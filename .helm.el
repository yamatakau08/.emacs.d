;;; to https://kumaroot.readthedocs.io/ja/latest/emacs-use-package.html#id2
(use-package helm
  :ensure t)

;(require 'helm-config) ; https://github.com/emacs-helm/helm/wiki#install
(require 'helm) ; helm-config を require すると、helm-migiemo-mode を設定するとエラーになるので、(require 'helm) する

;;
(when (featurep 'helm)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x f") 'helm-find-files)
  ;; (global-set-key (kbd "C-x o") 'helm-buffers-list) ; to tentative setting

  ;; replace C-s/C-r to helm-occur
  ;; Since helm-occur can search string but it use grep is slow and can not edit search result,
  ;; use helm-ag-this-file, but helm-ag-this-file can't search buffers e.g. *GNU Emacs*
  ;; and helm-ag-buffers can't search pattern in only one buffer,
  ;; it's better to use helm-occur in case of simply search
  (define-key global-map (kbd "C-s") 'helm-occur)
  (define-key global-map (kbd "C-r") 'helm-occur)
  )

;; enable migemo
(helm-migemo-mode 1)

;;; https://github.com/syohex/emacs-helm-ag#enable-helm-follow-mode-by-default
;;; in helm-ag candidate buffer, selected search result with ctr-n or ctr-p
;;; other window follow that action and shows the part selected in candidate buffer
;;; need to execcute C-c C-f in helm minibuffer to enable helm-follow-mode
(setq helm-follow-mode-persistent t)
