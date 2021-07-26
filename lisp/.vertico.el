(use-package vertico
  :ensure t

  :custom
  (vertico-count 20)
  (vertico-cycle nil)

  :init
  (vertico-mode)

  ;; :bind
  ;; (:map vertico-map
  ;; 	("C-z" . grugrut/up-dir))

  ;; :config
  ;; (defun grugrut/up-dir ()
  ;;     "一つ上の `/' まで辿って削除する."
  ;;     (interactive)
  ;;     (let* ((orig (minibuffer-contents))
  ;;            (orig-dir (file-name-directory orig))
  ;;            (up-dir (if orig-dir (file-name-directory (directory-file-name orig-dir))))
  ;;            (target (if (and up-dir orig-dir) up-dir orig)))
  ;; 	(message "orig: %s" orig)
  ;;       (delete-minibuffer-contents)
  ;;       (insert target)))
  )

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
