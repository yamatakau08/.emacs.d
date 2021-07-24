(use-package whitespace
  :config
  (global-whitespace-mode t) ; should be in :config, in :init doesn't effect.

  ;;(delq 'lines whitespace-style)      ; delete lines check over 80 column set by whitespace-line-column
  ;;(delq 'spaces whitespace-style)     ; not to highlite SPACEs and HARD SPACEs are visualized via faces.
  ;;(delq 'space-mark whitespace-style) ; not to display "table.SPACEs" and HARD SPACEs are visualized via faces.
  ;;(delq 'newline-mark whitespace-style) ; NEWLINEs are visualized via display table.

  (dolist (val '(lines spaces space-mark newline-mark))
    (delq val whitespace-style)))

(provide '.whitespace)


