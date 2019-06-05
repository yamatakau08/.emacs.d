;;; to https://kumaroot.readthedocs.io/ja/latest/emacs-use-package.html#id2
(use-package howm
  :ensure t)

(require 'howm) ; to check if howm package is installed.

;(setq howm-menu-lang 'ja)
;(global-set-key "\C-c,," 'howm-menu)
;(autoload 'howm-menu "howm-mode" "Hitori Otegaru Wiki Modoki" t)

;; during using howm ,helm and tramp
;; sometimes happen (error "No buffer name *howmM:"),
;; so change the buffer name of howm to "howmM:" remove '*' at the top of buffer name.
;; to monitor if this modification is effective.
(custom-set-variables
 '(howm-menu-name-format "howm:%s"))
