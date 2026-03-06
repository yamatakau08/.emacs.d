(use-package whitespace
  :config
  (global-whitespace-mode t) ; should be in :config, in :init doesn't effect.

  ;; whitespace-style
  ;; 'lines        ; delete lines check over 80 column set by whitespace-line-column
  ;; 'spaces       ; not to highlite SPACEs and HARD SPACEs are visualized via faces.
  ;; 'space-mark   ; not to display "table.SPACEs" and HARD SPACEs are visualized via faces.
  ;; 'newline-mark ; NEWLINEs are visualized via display table.
  ;; 'indentation  ; visualize `tab-width' or more SPACEs at beginning of line, if `indent-tabs-mode' (which see) is non-nil

  (setq whitespace-style
        (seq-difference whitespace-style
                        '(lines spaces space-mark newline-mark indentation)))
  )

(provide '.whitespace)
