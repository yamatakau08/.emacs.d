;;; sdic-mode 用の設定
;;; for Windows Emacs
(setq sdic-default-coding-system  'utf-8) 
(setq sdicf-default-coding-system 'utf-8)

(setq load-path (cons "~/.emacs.d/sdic/lisp" load-path))

(autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
(global-set-key "\C-cW" 'sdic-describe-word) ; original mapping C-cw
(autoload 'sdic-describe-word-at-point "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
(global-set-key "\C-cw" 'sdic-describe-word-at-point) ; original mapping C-cW

;;; work arround for
;;; eval-buffer: Symbol’s value as variable is void: default-fill-column
;;; refer http://suzuki.tdiary.net/20161226.html
(setq default-fill-column (default-value 'fill-column))

;;; for dic setting
;;; http://pogin.hatenablog.com/entry/20110418/1303062923
(setq sdic-eiwa-dictionary-list
      ;;英和検索で使用する辞書
      '(
;	(sdicf-client "~/.emacs.d/dict/gene-euc.sdic")   ; Mac/Linux?
	(sdicf-client "~/.emacs.d/dict/gene-utf8.sdic")  ; Win+cygwin,Mac
	)
      ;; 和英検索で使用する辞書
      sdic-waei-dictionary-list
      '(
;	(sdicf-client "~/.emacs.d/dict/jgene-euc.sdic")  ; Mac/Linux?
	(sdicf-client "~/.emacs.d/dict/jgene-utf8.sdic" (strategy direct)) ; Win+cygwin,Mac
	)
)

;;;
(setq sdic-anki-push-deck "英語")

(defun my-sdic-register-item (from to)
  "push note into Anki through AnkiConnect"
  (interactive
   (let ((from (read-string "From: " (sdic-word-at-point)))
         (to (read-string "To: ")))
     (list from to)))
;  (my-anki-editor-push-note sdic-anki-push-deck from to) ; my function defined in .anki-editor.el
  (my-anki-connect-push-note sdic-anki-push-deck from to) ; my function defined in anki-connect.el
  )

;;; register my function
(setq sdic-mode-hook
      '(lambda ()
	 (define-key sdic-mode-map "r" 'my-sdic-register-item)))
