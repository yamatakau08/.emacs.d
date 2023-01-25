;; to avoid the following error message in company network on Emacs 27.0 ,Linux
;; in case, M-x pakage-refresh-contents
;; Importing package-keyring.gpg...done
;; error in process sentinel: if: Error retrieving: http://melpa.org/packages/archive-contents (error connection-failed "failed with code 110" :host "melpa.org" :service 80)

;; need to load before use-package
(if (eq (my-network-type) 'company)
    (custom-set-variables
     '(url-proxy-services `(("http"  . ,(format "%s:%s" proxy-server proxy-port))
			    ;; google translate package needs "http" of url-proxy-services under proxy environment
			    ;; comment https
			    ;; curl backend of use-package "(url-retrieve-synchronously "https://orgmode.org/elpa/archive-contents")"
			    ;; takes much time 5minute to finish
			    ;;("https" . ,(format "%s:%s" proxy-server proxy-port))
			    ))))

(provide '.url-vars)
