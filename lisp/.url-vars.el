;;; to avoid the following error message in company network on Emacs 27.0
;;; in case, M-x pakage-refresh-contents
;;; Importing package-keyring.gpg...done
;;; error in process sentinel: if: Error retrieving: http://melpa.org/packages/archive-contents (error connection-failed "failed with code 110" :host "melpa.org" :service 80)
;; (if (eq (my-network-type) 'company)
;;     (let ((proxy-server-port (format "%s:%s" wl-proxy-server wl-proxy-port)))
;;       (setq url-proxy-services
;; 	    `(("http"  . ,proxy-server-port)
;; 	      ("https" . ,proxy-server-port)))))

;; (use-package url-vars
;;   :custom
;;   (url-proxy-services `(("http"  . ,(when (eq (my-network-type) 'company)
;; 				      (format "%s:%s" wl-proxy-server wl-proxy-port)))
;; 			("https" . ,(when (eq (my-network-type) 'company)
;; 				      (format "%s:%s" wl-proxy-server wl-proxy-port)))))
;; )
					;
;; don't use use-package, because my emacs 27.0.50 on Linux has Bad request
(custom-set-variables
 '(url-proxy-services `(("http"  . ,(when (eq (my-network-type) 'company)
				      (format "%s:%s" wl-proxy-server wl-proxy-port)))
			("https" . ,(when (eq (my-network-type) 'company)
				      (format "%s:%s" wl-proxy-server wl-proxy-port))))))

(provide '.url-vars)
