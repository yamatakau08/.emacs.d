(use-package wl
  :ensure wanderlust ; when t Error (use-package): Failed to install wl: Package ‘wl-’ is unavailable

  :init
  ;; select correct email address when we _start_ writing a draft.
  (add-hook 'wl-mail-setup-hook 'wl-draft-config-exec)
  ;; don't apply the templates when sending the draft otherwise
  ;; choosing another template with C-c C-j won't have any effect
  (remove-hook 'wl-draft-send-hook 'wl-draft-config-exec)

  ;; http://www.ss.scphys.kyoto-u.ac.jp/person/yasui/emacs/mail.html
  ;; より、サマリモードに入った直後は、wl-summary-prepared-hook にする事で、正常動作
  (add-hook  'wl-summary-prepared-hook 'my-wl-summary-mode-hook)
  ;;(add-hook 'wl-summary-mode-hook     'my-wl-summary-mode-hook)

  :custom
  (if nil ; set t the followings when debug
      (elmo-imap4-debug t)
      (elmo-pop3-debug  t))

  (elmo-imap4-default-stream-type 'ssl) ; Infoより、これを設定した場合は、.foldersに '!' を付けなくてもよい
  (wl-summary-line-format "%n%T%P %Y/%M/%D(%W)%h:%m %t%[%17(%c %f%) %] %s") ; add %Y
  (ssl-program-arguments
   '("s_client"
     "-quiet"
     "-connect"
     (format "%s:%s" host service)))

  ;; https://github.com/fumiyas/home-dot-files/blob/master/.wl#L142
  (wl-user-mail-address-list (list wl-from isp-smtp-posting-user gmail-smtp-posting-user))
  ;;(wl-user-mail-address-list `(,wl-from ,isp-smtp-posting-user ,gmail-smtp-posting-user)) ; care backquote '`' before '(' and ',' to evaluate the argument

  (wl-draft-config-alist
      '(((string-match isp-local-domain wl-draft-parent-folder)
         (template . isp-local-domain)
         (wl-smtp-posting-user      . isp-smtp-posting-user)
         (wl-smtp-posting-server    . isp-smtp-posting-server)
         (wl-local-domain           . isp-local-domain)
         (wl-smtp-authenticate-type . isp-smtp-authenticate-type)
         (wl-smtp-posting-port      . isp-smtp-posting-port)
         (wl-smtp-connection-type   . isp-smtp-connection-type))
        ((string-match gmail-local-domain wl-draft-parent-folder)
         (template . gmail-local-domain)
         (wl-smtp-posting-user      . gmail-smtp-posting-user)
         (wl-smtp-posting-server    . gmail-smtp-posting-server)
         (wl-smtp-authenticate-type . gmail-smtp-authenticate-type)
         (wl-smtp-posting-port      . gmail-smtp-posting-port)
         (wl-local-domain           . gmail-local-domain)
	 (wl-smtp-connection-type   . gmail-smtp-connection-type) ; http://tototoshi.hatenablog.com/entry/20100602/1275486312
	 )))

  (wl-template-alist
      `((,isp-local-domain ; care backquote '`' before '(' and ',' to evaluate the argument
         (wl-from . isp-smtp-posting-user)
         ("From"  . wl-from))
	(,gmail-local-domain
         (wl-from . gmail-smtp-posting-user)
         ("From"  . wl-from))
	))

  ;; http://www.kaisei.org/person/waasuke/2012/12/05/elmo_message_fetch_confirm/
  ;; "Wanderlustで大きいサイズのメールも確認なくフェッチ" より
  (elmo-message-fetch-confirm nil)

  :config
  (setq elmo-network-stream-type-alist
	'(("!" ssl ssl open-ssl-stream)
	  ("!!" starttls nil open-network-stream)
	  ("!socks" socks socks socks-open-network-stream)
	  ("!direct" direct nil open-network-stream)))

  ;; http://d.hatena.ne.jp/buzztaiki/20071030/1193765910
  ;; IMAP 日本語フォルダ文字化け対策
  (setq elmo-imap4-use-modified-utf7 t)

  ;; 大きなメッセージを分割して送信しない
  (setq mime-edit-split-message nil)

  ;;; サマリーモードに入った際に、日付逆順でソート
  (defun my-wl-summary-mode-hook ()
    (interactive)
    (wl-summary-sort-by-date t)
    (beginning-of-buffer) ; sort後、bufferのトップにカーソルを移動
    )
  )

;;; should be defun before being called.
;;; https://qiita.com/tadsan/items/17d32514b81f1e8f208a
(defun my-wl-source-get ()
  "get wanderlust source from wanderlust github repository"
  (interactive)
  (let* ((repository-name "wanderlust")
	 (default-directory (expand-file-name "~/.emacs.d"))
	 (repository "https://github.com/wanderlust/wanderlust.git")
	 ;; https://qiita.com/tadsan/items/17d32514b81f1e8f208a#%E3%81%AE%E5%B1%95%E9%96%8B
	 (destdir    (expand-file-name (concat default-directory "/" repository-name))))
    (if (file-directory-p destdir)
	;; https://qiita.com/tadsan/items/17d32514b81f1e8f208a#default-directory%E3%81%AB%E6%B0%97%E3%82%92%E9%85%8D%E3%82%8B
	(progn
	  ;; (message "default-dirrectory:%s, directory:%s exists" default-directory destdir)
	  (message "pulling %s into %s" repository-name destdir)
	  (shell-command-to-string
	   (mapconcat #'shell-quote-argument
		      (list "git" "pull")
		      " "))
	  )
      ;; else
      (message "cloning %s into %s" repository-name destdir)
      (shell-command-to-string
       (mapconcat #'shell-quote-argument
		  (list "git" "clone" repository destdir)
		  " "))
      (message "done cloning %s into %s" repository-name destdir)
      )))

;;; when launch wl puts out errro
;;; "wl: Cannot open load file: No such file or directory, ssl"
;;; need to use openssl in https://github.com/wanderlust/wanderlust/blob/master/utils/ssl.el
;;; Since package doesn't install ssl.el, install manualy.
(let ((file (expand-file-name "~/.emacs.d/wanderlust/utils/ssl.el")))
  (if (not (file-exists-p file))
      (my-wl-source-get))
  (require 'ssl file))

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

(if (eq (my-get-network-type) 'company)
    (nconc ssl-program-arguments
	   '("-proxy" (format "%s:%s" wl-proxy-server wl-proxy-port))))

;;; In case of openssl ver 1.1 above my own compiled on cygwin environment, need to "-crlf"
;;; It's the best to user Windows openssl until openssl ver1.1 above on cygin is released
;(when (eq system-type 'windows-nt) ; may be cygwin is better
;  (nconc ssl-program-arguments '("-crlf"))) ; need this option for Gmail through proxy environment

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

;;; 1. When show the mail with attched file name is ATT00001.txt, error happened file-missing ("Opening input file" "No such file or directory" "c:/yama/ATT00001.txt")
;;; 2. When show the mail, wl error (args-out-of-range "" 0 4)
;;; redfine wl-message-buffer-display to get the backtrace
;(eval-after-load "wl-message"
;  '(defun wl-message-buffer-display (folder number display-type
;					    &optional force-reload unread)
;     (let* ((msg-id (ignore-errors
;		      (elmo-message-field folder number 'message-id)))
;	    (fname (elmo-folder-name-internal folder))
;	    (hit (wl-message-buffer-cache-hit (list fname number msg-id)))
;	    (redisplay nil)
;	    entity)
;       (when (and hit (not (buffer-live-p hit)))
;	 (wl-message-buffer-cache-delete (list fname number msg-id))
;	 (setq hit nil))
;       (if hit
;	   (progn
;	     ;; move hit to the top.
;	     (wl-message-buffer-cache-sort
;	      (wl-message-buffer-cache-entry-make (list fname number msg-id) hit))
;	     (with-current-buffer hit
;	       ;; Rewind to the top page
;	       (widen)
;	       (goto-char (point-min))
;	       (ignore-errors (wl-message-narrow-to-page))
;	       (setq entity wl-message-buffer-mime-entity)
;	       (unless (eq wl-message-buffer-cur-display-type display-type)
;		 (setq redisplay t))))
;	 ;; delete tail and add new to the top.
;	 (setq hit (wl-message-buffer-cache-add (list fname number msg-id)))
;	 (setq redisplay t))
;       (when (or force-reload redisplay)
;	 (with-current-buffer hit
;	   (when (or force-reload
;		     (null entity)
;		     (not (elmo-mime-entity-display-p
;			   entity
;			   (if (wl-message-mime-analysis-p display-type)
;			       'mime
;			     'as-is)))
;		     (if (wl-message-display-no-merge-p display-type)
;			 (elmo-mime-entity-reassembled-p entity)
;		       (elmo-mime-entity-fragment-p entity)))
;	     (setq entity (elmo-message-mime-entity
;			   folder
;			   number
;			   (wl-message-get-original-buffer)
;			   (and wl-message-auto-reassemble-message/partial
;				(not (wl-message-display-no-merge-p
;				      display-type)))
;			   force-reload
;			   unread
;			   (not (wl-message-mime-analysis-p display-type)))))
;	   (unless entity
;	     (error "Cannot display message %s/%s" fname number))
;	   (wl-message-display-internal entity display-type))
;	 ) ;; will not be used
;       hit))
;  )

;;; 1. When show the mail with attched file name is ATT00001.txt, error happened file-missing ("Opening input file" "No such file or directory" "c:/yama/ATT00001.txt")
;;; redfine the following functions to take the backtrace by support wl mailing list.
;;; Since the this fixing applied the package, comment the following.
;(eval-after-load "mime-tnef"
;  '(progn
;     (defun mime-tnef-insert-file (file data)
;       (let*  ((guess (mime-find-file-type file))
;	       (type (nth 0 guess))
;	       (subtype (nth 1 guess))
;	       (parameters (nth 2 guess))
;	       (encoding "8bit")
;	       (disposition-type (nth 4 guess))
;	       (disposition-params (nth 5 guess)))
;	 (setq parameters
;	       (concat
;		(when (consp parameters)
;		  (mime-tnef-insert-file-parameters parameters file data))
;		(when disposition-type
;		  (concat "\n" "Content-Disposition: " disposition-type
;			  (mime-edit-insert-file-parameters
;			   disposition-params file)))))
;	 (insert
;	  ;; multibyte buffer is needed for non-ASCII filename.
;	  (with-temp-buffer
;	    (mime-edit-insert-tag type subtype parameters)
;	    (mime-edit-define-encoding encoding)
;	    (goto-char (point-min))
;	    (mime-tnef-translate-single-part-tag)
;	    (buffer-string)))
;	 (insert data "\n")
;	 ))
;
;     (defun mime-tnef-insert-file-parameters (params file data)
;       (let (charset
;	     result)
;	 (dolist (elt params result)
;	   (setq result
;		 (cons
;		  (cons
;		   (car elt)
;		   (if (eq (cdr elt) 'charset)
;		       (or charset
;			   (let ((codings (detect-coding-string data)))
;			     (while codings
;			       (when (coding-system-to-mime-charset
;				      (car codings))
;				 (setq charset
;				       (symbol-name
;					(coding-system-to-mime-charset
;					 (car codings)))
;				       codings nil))
;			       (setq codings (cdr codings)))
;			     charset))
;		     (cdr elt)))
;		  result)))
;	 (mime-edit-insert-file-parameters (nreverse result) file)))
;     ))

;;; 2. When show the mail, wl error (args-out-of-range "" 0 4)
;;; redfine the following functions supported by wl mailing list Kazuhiro Ito
(eval-after-load "mime-tnef"
  '(defun mime-tnef-parse (string)
     (catch :done
       (unless (and (> (length string) 6)
		    (equal (string-make-unibyte (substring string 0 4))
			   (string-make-unibyte "\x78\x9f\x3e\x22")))
	 (message "Input data does not seem to be MS-TNEF format")
	 (throw :done nil))
       (mime-tnef-debug "TNEF Key: %04x\n" (mime-tnef-2bytes string 4))
       (let ((length (length string))
	     (read 6)
	     result object
	     lvl-type name type
	     sum
	     start end data-length)
	 (while (< read length)
	   (setq lvl-type (mime-tnef-byte string read)
		 name (mime-tnef-2bytes string (1+ read))
		 type (mime-tnef-2bytes string (+ read 3))
		 data-length (mime-tnef-4bytes string (+ read 5))
		 start (+ read 9)
		 end (+ start data-length)
		 object `(
			  (lvl-type . ,(or (cdr (assq lvl-type
						      mime-tnef-lvl-types-table))
					   lvl-type))
			  (name . ,(or (cdr (assq name mime-tnef-names-table))
				       name))
			  (type . ,(or (cdr (assq type mime-tnef-types-table))
				       type))
			  (start . ,start)
			  (end . ,end)
			  ;; (length . ,(mime-tnef-4bytes string (+ read 5)))
			  )
		 read (+ read data-length 9 2)
		 sum 0)
	   (dotimes (i data-length)
	     (setq sum (+ sum (mime-tnef-byte string (- read 3 i)))))
	   (unless (eq (mime-tnef-2bytes string (- read 2)) (% sum 65536))
	     (message "Checksum mismatch, TNEF may be corrupted"))
	   (setq result (cons object result)))
	 (cons (nreverse result) string))))
  )
