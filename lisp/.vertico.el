(use-package vertico
  ;;:ensure t ; use straight to install extensions is not registerd in elpa
  ;; :straight (:files (:defaults "extensions/*")
  ;;                       :includes (vertico-buffer
  ;;                                  vertico-directory
  ;;                                  vertico-flat
  ;;                                  vertico-indexed
  ;;                                  vertico-mouse
  ;;                                  vertico-quick
  ;;                                  vertico-repeat
  ;;                                  vertico-reverse))

  :straight (:files (:defaults "extensions/*"))

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

;; install the vertico extensios files which are not registerd in elpa, by straight
;; https://github.com/raxod502/straight.el/issues/819#issuecomment-882039946
;; finally move in "(use-package vertico ..." block
;;(straight-use-package '( vertico :files (:defaults "extensions/*")
;;                         :includes (vertico-buffer
;;                                    vertico-directory
;;                                    vertico-flat
;;                                    vertico-indexed
;;                                    vertico-mouse
;;                                    vertico-quick
;;                                    vertico-repeat
;;                                    vertico-reverse)))

(use-package vertico-directory
  :after (vertico)

  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
	      ("C-l" . vertico-directory-delete-word) ; same key assign of helm
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  ;; comment following setting, this show "nc: sg: su:" ... when execute find-file then specifi "/" etc.
  ;;:hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
)

(provide '.vertico)
