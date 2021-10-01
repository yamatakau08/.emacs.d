(use-package bookmark+
  ;;https://www.emacswiki.org/emacs/BookmarkPlus#h5o-35
  :straight t

  ;; :config
  ;; (defun my-bmkp-jump-file-externally (file-name)
  ;;   (consult-file-externally (expand-file-name file-name)))
)

(defun my-bmkp-jump-file-externally (bookmark-entry)
  "refer consult-file-externally"
  (let ((file-name (let-alist bookmark-entry .filename)))
    ;;(w32-shell-execute "open" (expand-file-name file-name)))) ; no need to expand-file-name
    (w32-shell-execute "open" file-name)))

(provide '.bookmark+)
