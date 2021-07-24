;; init.el for debug using
(setq debug-on-error nil)
(setq debug-on-quit  nil)

(load-file "~/.emacs.d/private.el")
(load-file "~/.emacs.d/my-network-type.el")
(load-file "~/.emacs.d/.package.el")

(add-to-list 'load-path "~/.emacs.d/lisp")

(require '.run-assoc)
