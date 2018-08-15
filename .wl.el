;; set t in trouble
(setq elmo-imap4-debug t)
(setq elmo-pop3-debug  t)

;; need to use openssl in https://github.com/wanderlust/wanderlust/blob/master/utils/ssl.el
;; Since package doesn't install ssl.el, install manualy.
(load "ssl.el")

;; for imap connection
(setq ssl-program-name "openssl") ; should be OpenSSL 1.1.1-pre6 (beta) 1 May 2018 to use proxy

(setq elmo-imap4-default-stream-type 'ssl)

(setq elmo-network-stream-type-alist
      '(("!" ssl ssl open-ssl-stream)
	("!!" starttls nil open-network-stream)
	("!socks" socks socks socks-open-network-stream)
	("!direct" direct nil open-network-stream)))

(if (check-private-network)
    ;; for private network
    (setq ssl-program-arguments
	  '("s_client"
	    "-quiet"
	    "-connect"
	    (format "%s:%s" host service)
	    ))
  ;; for company network
  (setq ssl-program-arguments
	'("s_client"
	  "-quiet"
	  "-connect"
	  (format "%s:%s" host service)
	  "-proxy"
	  (format "%s:%s" wl-proxy-server wl-proxy-port)
	  )))

(when (eq system-type 'windows-nt) ; may be cygwin is better
  (nconc ssl-program-arguments '("-crlf"))) ; need this option for Gmail through proxy environment

;; https://github.com/fumiyas/home-dot-files/blob/master/.wl#L142
(setq wl-user-mail-address-list
      (list wl-from isp-smtp-posting-user gmail-smtp-posting-user))
;(setq wl-user-mail-address-list
;      `(,wl-from ,isp-smtp-posting-user ,gmail-smtp-posting-user)) ; care backquote '`' before '(' and ',' to evaluate the argument

;; select correct email address when we _start_ writing a draft.
(add-hook 'wl-mail-setup-hook 'wl-draft-config-exec)
;; don't apply the templates when sending the draft otherwise
;; choosing another template with C-c C-j won't have any effect
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

;; for summary
(setq wl-summary-line-format "%n%T%P %Y/%M/%D(%W)%h:%m %t%[%17(%c %f%) %] %s") ; %Y 年追加

;; サマリーモードに入った際に、日付逆順でソート
(defun my-wl-summary-mode-hook ()
  (interactive)
  (wl-summary-sort-by-date t)
  (beginning-of-buffer) ; sort後、bufferのトップにカーソルを移動
)

;; http://www.ss.scphys.kyoto-u.ac.jp/person/yasui/emacs/mail.html
;; より、サマリモードに入った直後は、wl-summary-prepared-hook にする事で、正常動作
(add-hook  'wl-summary-prepared-hook 'my-wl-summary-mode-hook) 
;(add-hook 'wl-summary-mode-hook     'my-wl-summary-mode-hook)
