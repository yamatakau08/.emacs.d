;;; sdic-mode 用の設定
;;; for Windows Emacs
(setq sdic-default-coding-system  'utf-8)
(setq sdicf-default-coding-system 'utf-8)

(add-to-list 'load-path "~/.emacs.d/sdic/lisp")

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
;;(add-to-list 'load-path "~/.emacs.d/my-anki-connect")
;;(require 'my-anki-connect)

(add-to-list 'load-path "~/.emacs.d/my-anki-browse")
(require 'my-anki-browse)

(defun my-sdic-register-item ()
  "push note into Anki through AnkiConnect"
  (interactive)
  (let ((deck "英語") ; fixed
	(front (read-string "Front: "
			    (if (region-active-p)
				(buffer-substring (region-beginning) (region-end))
			      (thing-at-point 'word))))
	(back  (read-string "Back : " )))
    (my-anki-connect-push-note "英語" front back)))

;;; register my function
(setq sdic-mode-hook
      '(lambda ()
	 (define-key sdic-mode-map "r" 'my-sdic-register-item)))
