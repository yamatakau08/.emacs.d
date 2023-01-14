;; to avoid the following error message in company network on Emacs 27.0 ,Linux
;; in case, M-x pakage-refresh-contents
;; Importing package-keyring.gpg...done
;; error in process sentinel: if: Error retrieving: http://melpa.org/packages/archive-contents (error connection-failed "failed with code 110" :host "melpa.org" :service 80)

;; need to load before use-package
(if (eq (my-network-type) 'company)
    (custom-set-variables
     '(url-proxy-services `(("http"  . ,(when (eq (my-network-type) 'company)
					  (format "%s:%s" proxy-server proxy-port)))
			    ("https" . ,(when (eq (my-network-type) 'company)
					  (format "%s:%s" proxy-server proxy-port)))))))

(provide '.url-vars)
