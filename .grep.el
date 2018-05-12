;;; for grep-find
(cond
 ((eq window-system 'w32)
  (setq find-program "/cygdrive/c/cygwin/bin/find")
;(setq grep-command "lgrep -n -Os")
;(setq grep-program "lgrep")
))

;;; for color-grep
(require 'color-grep)

;;; for igrep
(autoload 'igrep "igrep"
   "*Run `grep` PROGRAM to match REGEX in FILES..." t)
(autoload 'igrep-find "igrep"
   "*Run `grep` via `find`..." t)
(autoload 'igrep-visited-files "igrep"
   "*Run `grep` ... on all visited files." t)
(autoload 'dired-do-igrep "igrep"
   "*Run `grep` on the marked (or next prefix ARG) files." t)
(autoload 'dired-do-igrep-find "igrep"
   "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
(autoload 'Buffer-menu-igrep "igrep"
  "*Run `grep` on the files visited in buffers marked with '>'." t)
(autoload 'igrep-insinuate "igrep"
  "Define `grep' aliases for the corresponding `igrep' commands." t)

(setq igrep-find-program "~/bin/find")
;(setq igrep-find-program "/cygwin/bin/find")

;; for grep-find
(setq grep-find-command "find ../.. -type f -name '*.[ch]' -print0 | xargs -0 -e grep -ns ")

