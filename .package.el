;; https://www-he.scphys.kyoto-u.ac.jp/member/shotakaha/dokuwiki/doku.php?id=toolbox:emacs:package:start
;(require 'package)

; Unnecessary call on Emacs 27
; https://github.com/jkitchin/scimax/issues/194#issuecomment-380470596
(if (< emacs-major-version 27)
    (package-initialize))
;;; https://github.com/jkitchin/scimax/issues/194#issuecomment-385437906
;(unless package--initialized 
;  (package-initialize t))

;;; to avoid the following error message in company network on Emacs 27.0
;;; in case, M-x pakage-refresh-contents
;;; https://emacs-jp.slack.com/archives/C1B5WTJLQ/p1547097797720600
;;; Importing package-keyring.gpg...done
;;; error in process sentinel: if: Error retrieving: http://melpa.org/packages/archive-contents (error connection-failed "failed with code 110" :host "melpa.org" :service 80)
(unless (check-private-network)
  (let ((proxy-server-port (format "%s:%s" wl-proxy-server wl-proxy-port)))
    (setq url-proxy-services
	  `(("http"  . ,proxy-server-port)
	    ("https" . ,proxy-server-port)))))

;;; in case, M-x list-packages with the above settings
;;; https://emacs-jp.slack.com/archives/C1B5WTJLQ/p1547100014738900
;;; Error while verifying signature archive-contents.sig:
;;;
;;; Command output:
;;; gpg: no valid OpenPGP data found.
(setq package-check-signature nil)

;;; defaultでは、("gnu" . "https://elpa.gnu.org/packages/") しかなかったので、以下を追加 
(if  (check-private-network)
    ;; for private network
    (add-to-list 'package-archives
		 '("melpa" . "https://melpa.org/packages/")
		 '("org"   . "https://orgmode.org/elpa/"))
  ;; for company network
  ;; Since suddenly cannot connect to ("gnu" . "https://elpa.gnu.org/packages/")
  ;; use setq, not add-to-list
  (setq package-archives
	'(("org"   . "https://orgmode.org/elpa/")
	  ("melpa" . "https://melpa.org/packages/"))))

;; no effect! need to study
;; avoid to write custom-set-variables setting automatically in init.el
;; https://ymotongpoo.hatenablog.com/entry/2017/11/07/000921
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

