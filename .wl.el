;;; set t in trouble
(setq elmo-imap4-debug t)
(setq elmo-pop3-debug  t)

;;; need to use openssl in https://github.com/wanderlust/wanderlust/blob/master/utils/ssl.el
;;; Since package doesn't install ssl.el, install manualy.
(load "ssl.el")

;;; for imap connection
;;; ssl-program-name default value is "openssl"
;;; should be OpenSSL Ver 1.1 above to use proxy option
;;; you can check openssl version with $openssl version
;;; explicitly set because cannot judge which kernel is used cygwin or msys2 minge64.
(if (eq system-type 'windows-nt)
    ;; on cygwin
    ;; (setq ssl-program-name "/cygdrive/c/winbin/OpenSSL-Win64/bin/openssl.exe")
    ;; on msys2 mingw64, 
    (setq ssl-program-name "openssl")
  (setq ssl-program-name "openssl"))

(setq elmo-imap4-default-stream-type 'ssl) ; Infoより、これを設定した場合は、.foldersに '!' を付けなくてもよい

(setq elmo-network-stream-type-alist
      '(("!" ssl ssl open-ssl-stream)
	("!!" starttls nil open-network-stream)
	("!socks" socks socks socks-open-network-stream)
	("!direct" direct nil open-network-stream)))

(if (company-network-p)
    (setq ssl-program-arguments
	  '("s_client"
	    "-quiet"
	    "-connect"
	    (format "%s:%s" host service)
	    "-proxy"
	    (format "%s:%s" wl-proxy-server wl-proxy-port)
	    ))
  ;; for private network
  (setq ssl-program-arguments
	'("s_client"
	  "-quiet"
	  "-connect"
	  (format "%s:%s" host service)
	  )))

;;; In case of openssl ver 1.1 above my own compiled on cygwin environment, need to "-crlf"
;;; It's the best to user Windows openssl until openssl ver1.1 above on cygin is released
;(when (eq system-type 'windows-nt) ; may be cygwin is better
;  (nconc ssl-program-arguments '("-crlf"))) ; need this option for Gmail through proxy environment

;;; https://github.com/fumiyas/home-dot-files/blob/master/.wl#L142
(setq wl-user-mail-address-list
      (list wl-from isp-smtp-posting-user gmail-smtp-posting-user))
;(setq wl-user-mail-address-list
;      `(,wl-from ,isp-smtp-posting-user ,gmail-smtp-posting-user)) ; care backquote '`' before '(' and ',' to evaluate the argument

;;; select correct email address when we _start_ writing a draft.
(add-hook 'wl-mail-setup-hook 'wl-draft-config-exec)
;;; don't apply the templates when sending the draft otherwise
;;; choosing another template with C-c C-j won't have any effect
(remove-hook 'wl-draft-send-hook 'wl-draft-config-exec)

(setq wl-draft-config-alist
      '(((string-match isp-local-domain wl-draft-parent-folder)
         (template . isp-local-domain)
         (wl-smtp-posting-user      . isp-smtp-posting-user)
         (wl-smtp-posting-server    . isp-smtp-posting-server)
         (wl-local-domain           . isp-local-domain)
         (wl-smtp-authenticate-type . isp-smtp-authenticate-type)
         (wl-smtp-posting-port      . isp-smtp-posting-port)
         (wl-smtp-connection-type   . isp-smtp-connection-type)
	 )
        (
	 (string-match gmail-local-domain wl-draft-parent-folder)
         (template . gmail-local-domain)
         (wl-smtp-posting-user      . gmail-smtp-posting-user)
         (wl-smtp-posting-server    . gmail-smtp-posting-server)
         (wl-smtp-authenticate-type . gmail-smtp-authenticate-type)
         (wl-smtp-posting-port      . gmail-smtp-posting-port)
         (wl-local-domain           . gmail-local-domain)
	 (wl-smtp-connection-type   . gmail-smtp-connection-type) ; http://tototoshi.hatenablog.com/entry/20100602/1275486312
	 )))

(setq wl-template-alist
      `((,isp-local-domain ; care backquote '`' before '(' and ',' to evaluate the argument
         (wl-from . isp-smtp-posting-user)
         ("From"  . wl-from))
	(,gmail-local-domain
         (wl-from . gmail-smtp-posting-user)
         ("From"  . wl-from))
	))

;;; for summary
(setq wl-summary-line-format "%n%T%P %Y/%M/%D(%W)%h:%m %t%[%17(%c %f%) %] %s") ; %Y 年追加

;;; サマリーモードに入った際に、日付逆順でソート
(defun my-wl-summary-mode-hook ()
  (interactive)
  (wl-summary-sort-by-date t)
  (beginning-of-buffer) ; sort後、bufferのトップにカーソルを移動
)

;;; http://www.ss.scphys.kyoto-u.ac.jp/person/yasui/emacs/mail.html
;;; より、サマリモードに入った直後は、wl-summary-prepared-hook にする事で、正常動作
(add-hook  'wl-summary-prepared-hook 'my-wl-summary-mode-hook) 
;(add-hook 'wl-summary-mode-hook     'my-wl-summary-mode-hook)

;;; http://www.kaisei.org/person/waasuke/2012/12/05/elmo_message_fetch_confirm/
;;; "Wanderlustで大きいサイズのメールも確認なくフェッチ" より
(setq elmo-message-fetch-confirm nil)

;;; 大きなメッセージを分割して送信しない
(setq mime-edit-split-message nil)

;;;
;;; 通常の設定では、表示できない文字対応
;;;
;;; gb2312(中国からのメール)の場合に必要
;;; Windows環境?で、Rictiy dimishedフォントを用いていると、summary bufferでのsubject一部文字化けしてしまうので、
;;; init.el 中の font-setting.el をloadしている所をコメントアウトする
(when (coding-system-p 'gbk)
  (define-coding-system-alias 'cn-gb-2312 'gbk)
  (define-coding-system-alias 'gb2312 'gbk))

;;; 丸付数字が表示されない対応
;;; "丸付き数字" "はしごだか"が入った JISメールを読むための設定
(coding-system-put 'iso-2022-jp :decode-translation-table
       '(cp51932-decode japanese-ucs-cp932-to-jis-map))

;;; "丸付き数字" "はしごだか"が入った JISメールを送るための設定
;;; 以下設定をしない場合は、本来の utf-8 で送付 (消極 Windows派になる)
(coding-system-put 'iso-2022-jp :encode-translation-table
      '(cp51932-encode))

;;; charset の判定する際に cp932 を sjis より優先順位を上げておくことで
;;; 機種依存文字を表示できるようにする (charset と coding-system の優先度設定)。
(if (>= emacs-major-version 23)
    (progn
      (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201 'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
      (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)))

;;; http://d.hatena.ne.jp/buzztaiki/20071030/1193765910
;;; IMAP 日本語フォルダ文字化け対策
(setq elmo-imap4-use-modified-utf7 t)
