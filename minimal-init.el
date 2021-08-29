;; init.el for debug using
(setq debug-on-error  nil)
(setq debug-on-signal nil)
(setq debug-on-quit   nil)

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'private)
(require 'my-network-type)
(require '.package)
