;(require 'helm-config) ; https://github.com/emacs-helm/helm/wiki#install
(require 'helm) ; helm-config を require すると、helm-migiemo-mode を設定するとエラーになるので、(require 'helm) する

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x f") 'helm-find-files)
;(global-set-key (kbd "C-x o") 'helm-buffers-list) ; to tentative setting

(helm-migemo-mode 1)

