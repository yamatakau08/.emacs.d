;; for imap connection
(cond 
  ((equal ssl-program-name "openssl")
   (setq elmo-imap4-default-port 993) ; imap over ssl
   (setq ssl-program-arguments '("s_client" "-quiet" "-host" host "-port" service))
   )
  ((equal ssl-program-name "gnutls-cli")
   (setq elmo-imap4-default-port 993)
;  (setq ssl-program-arguments '("-p" service host))
   (setq ssl-program-arguments '("--insecure" "-p" service host))
   )
  )

;; for summary
(setq wl-summary-line-format "%n%T%P %Y/%M/%D(%W)%h:%m %t%[%17(%c %f%) %] %s") ; %Y 年追加

; サマリーモードに入った際に、日付逆順でソート
(defun my-wl-summary-mode-hook ()
  (interactive)
  (wl-summary-sort-by-date t)
  (beginning-of-buffer) ; sort後、bufferのトップにカーソルを移動
)
; http://www.ss.scphys.kyoto-u.ac.jp/person/yasui/emacs/mail.html
; より、サマリモードに入った直後は、wl-summary-prepared-hook にする事で、正常動作
(add-hook  'wl-summary-prepared-hook 'my-wl-summary-mode-hook) 
;(add-hook 'wl-summary-mode-hook     'my-wl-summary-mode-hook)

