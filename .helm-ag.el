(use-package helm-ag
  :ensure t)

;;; to install "ag"
;;; $ brew install ag, ag is installed in /usr/local/bin on mac
;;; unfortunately, exec-path doesn't include /usr/local/bin even if that path is set in .bash_profile.
;;; tentative add /usr/local/bin in exec-path
(if (eq system-type 'darwin)
    (add-to-list 'exec-path "/usr/local/bin"))

;;; https://github.com/syohex/emacs-helm-ag#enable-helm-follow-mode-by-default
;;; in helm-ag candidate buffer, selected search result with ctr-n or ctr-p
;;; other window follow that action and shows the part you selected in candidate buffer
(setq helm-follow-mode-persistent t)
