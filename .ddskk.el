; install ddskk on Mac refer the following page
; http://yoppa.org/blog/6162.html
;(add-to-list 'load-path "~/.emacs.d/elpa/ddskk-20170709.839")
;(require 'skk-autoloads) ; XEmacs でパッケージとしてインストールした場合は不要

(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj"    'skk-auto-fill-mode)
(global-set-key "\C-xt"    'skk-tutorial)

;(setq skk-tut-file "~/mylisp/ddskk/etc/SKK.tut")
