;;; to avoid the following error message in company network on Emacs 27.0
;;; in case, M-x pakage-refresh-contents
;;; https://emacs-jp.slack.com/archives/C1B5WTJLQ/p1547097797720600
;;; Importing package-keyring.gpg...done
;;; error in process sentinel: if: Error retrieving: http://melpa.org/packages/archive-contents (error connection-failed "failed with code 110" :host "melpa.org" :service 80)
(if (eq (my-get-network-type) 'company)
    (let ((proxy-server-port (format "%s:%s" wl-proxy-server wl-proxy-port)))
      (setq url-proxy-services
	    `(("http"  . ,proxy-server-port)
	      ("https" . ,proxy-server-port)))))
