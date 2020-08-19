(use-package tabbar
  :ensure t
  :config
  (tabbar-mode 1)
  (custom-set-variables `(tabbar-buffer-groups-function nil)) ; defvar var is available?

  :bind (("C-." . tabbar-forward-tab)))
  
