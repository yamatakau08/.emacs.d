;;; ruby-mode.el in c:/winbin/emacs-26.2-x86_64/share/emacs/26.2/lisp/progmodes/

;; to supress "#coding: utf-8" is automatically inserted at top of the ruby script.rb
;(with-eval-after-load "ruby-mode" ; 'ruby-mode is also ok
;  (custom-set-variables '(ruby-insert-encoding-magic-comment nil))
;;  (setq ruby-insert-encoding-magic-comment nil)
;  )

;; since without with-eval-after-load is also available, use this.
(custom-set-variables '(ruby-insert-encoding-magic-comment nil))
