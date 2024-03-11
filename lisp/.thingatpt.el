(use-package thingatpt
  :config
  ;; add ' ' space and '　' zenkaku space, '\\' to get windows file path
  ;; Since :custom section doesn't reflect the setting, use :config section and setq function.
  ;; When the variable is set by defvar function, should use setq function
  (setq thing-at-point-file-name-chars "-~/[:alnum:]_.${}#%,: 　\\")

  (defun my-thing-at-point-filename ()
    "Even if cursor is on '\"' or '<' character is preceding file name, returns filename"
    (save-excursion
      (skip-chars-forward "\"<")
      (thing-at-point 'filename t)))

  )

;; on my mac
;; "~/Documents/QRチケット印刷 _ スワチケ2入口41段181番.pdf"
;: if path has ' ' space or '　' zenkaku space, (things-at-point 'filename) can return path correctly,

(provide '.thingatpt)
