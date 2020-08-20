(use-package tabbar
  :ensure t
  :init (tabbar-mode 1)
  :config (custom-set-variables `(tabbar-buffer-groups-function nil)) ; defvar var is available.
  :bind (("C-." . tabbar-forward-tab)))
