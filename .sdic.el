;;; sdic-mode 用の設定
(setq load-path (cons "~/.emacs.d/sdic/lisp" load-path))
(autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)
(global-set-key "\C-t" 'sdic-describe-word)
(autoload 'sdic-describe-word-at-point "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
(global-set-key "\C-cW" 'sdic-describe-word-at-point)

;;; work arround for
;;; eval-buffer: Symbol’s value as variable is void: default-fill-column
;;; refer http://suzuki.tdiary.net/20161226.html
(setq default-fill-column (default-value 'fill-column))

;;; for dic setting
;;; http://pogin.hatenablog.com/entry/20110418/1303062923
(setq sdic-eiwa-dictionary-list
      ;;英和検索で使用する辞書
      '((sdicf-client "~/.emacs.d/dict/gene.sdic"))
      ;; 和英検索で使用する辞書
;      sdic-waei-dictionary-list
;     '((sdicf-client "/usr/share/dict/jedict.sdic"))
)
