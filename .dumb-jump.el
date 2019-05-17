(use-package dumb-jump
  :ensure t)

;;; https://github.com/jacktasia/dumb-jump/blob/master/README.md#debugging-a-jump
;(setq dumb-jump-debug t) ; to put out debug message of dumb-jump

(setq dumb-jump-selector 'helm)

(if (eq system-type 'windows-nt) ; on mingw64
    ;; since 'git-grep and 'gnu-grep doesn't work, use 'ag
    ;;(setq dumb-jump-prefer-searcher 'ag)
    ;; since the above dumb-jump-prefer-searcher doesn't effect, use dumb-jump-force-searcher
    (setq dumb-jump-force-searcher  'ag))

;;; the following advice is experimentally to change args regexes dumb-jump-generate-git-grep-command
;;; but dumb-jump executes grep twice, so this advice doesn't effect.
;;; just for an advice :arround example
;(defun my-dumb-jump-regexes-args (orig-func &rest args)
;  (message "debug my-dumb-jump-regexes-args")
;  (dolist (arg args) (message "%s" arg))
;  (message "\n3rd arg: %s\n" (nth 3 args))
;;  (setcar (cdr (cdr (cdr args))) "???")
;  (dolist (arg args) (message "%s" arg))
;  (apply orig-func args))
;(advice-add 'dumb-jump-generate-git-grep-command :around 'my-dumb-jump-regexes-args)
