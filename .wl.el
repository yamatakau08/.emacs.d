;; set t in trouble
(setq elmo-imap4-debug t)
(setq elmo-pop3-debug  t)

;; for imap connection
(setq ssl-program-name "openssl") ; should be OpenSSL 1.1.1-pre6 (beta) 1 May 2018 to use proxy

(setq elmo-imap4-default-stream-type 'ssl)

(setq elmo-network-stream-type-alist
      '(("!" ssl ssl open-ssl-stream)
	("!!" starttls nil open-network-stream)
	("!socks" socks socks socks-open-network-stream)
	("!direct" direct nil open-network-stream)))

(setq ssl-program-arguments
      '("s_client"
	"-quiet"
	"-connect"
	(format "%s:%s" host service)
	"-proxy"
	(format "%s" wl-proxy-server)
	"-crlf" ; need this option for Gmail through proxy environment
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
