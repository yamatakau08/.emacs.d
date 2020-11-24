(use-package dumb-jump
  :ensure t

  :init
  ;; https://github.com/jacktasia/dumb-jump#basic
  ;; how to write :hook in use-package ?
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

  :custom
  ;; https://github.com/jacktasia/dumb-jump/blob/master/README.md#debugging-a-jump
  ;;(dumb-jump-debug t) ; to put out debug message of dumb-jump

  ;; default 2s, over 2s, even if there are results, don't display anything.
  (dumb-jump-max-find-time 60)

  (dumb-jump-selector 'helm)

  :config
  (if (eq system-type 'windows-nt) ; on mingw64
      ;; since 'git-grep and 'gnu-grep doesn't work, use 'ag.
      ;; since (setq dumb-jump-prefer-searcher 'ag) doesn't effect, use dumb-jump-force-searcher.
      (setq dumb-jump-force-searcher  'ag))
  )

;;; dumb-jump-fallback-regex initial value is "\\bJJJ\\j"
;;; Since without "\\b" can search on shell-mode, remove "\\b" from it.
;;; works fine with dumb-jump-prefer-searcher nil case on mingw64
;;(setq dumb-jump-fallback-regex "JJJ\\j")

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

;;; .dumbjump
;;; on msys, use path format drive letter likely c:
;;; +c:/msys64/mingw64/lib/ruby/2.6.0
