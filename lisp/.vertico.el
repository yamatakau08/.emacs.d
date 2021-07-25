(use-package vertico
  :ensure t

  :custom
  (vertico-count 20)
  (vertico-cycle nil)

  :init
  (vertico-mode))

;; https://github.com/minad/vertico#extensions
;; put vertico-directory.el in ~/.emacs.d/lisp/
(let ((srcurl "https://raw.githubusercontent.com/minad/vertico/main/extensions/vertico-directory.el")
      (file "~/.emacs.d/lisp/vertico-directory.el"))
  (unless (file-exists-p file)
    (my-git-source1-get srcurl file)))

(use-package vertico-directory
  :after (vertico)

  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
	      ("C-l" . vertico-directory-delete-word) ; same key assign of helm
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
)

(provide '.vertico)
