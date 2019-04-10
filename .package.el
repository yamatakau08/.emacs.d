;;; https://www-he.scphys.kyoto-u.ac.jp/member/shotakaha/dokuwiki/doku.php?id=toolbox:emacs:package:start
;(require 'package)

;;; Unnecessary call on Emacs 27
;;; https://github.com/jkitchin/scimax/issues/194#issuecomment-380470596
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
(if (company-network-p)
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

(if (company-network-p)
  ;; Since suddenly cannot connect to ("gnu" . "https://elpa.gnu.org/packages/")
  ;; use setq, not add-to-list
  (setq package-archives
	'(("org"   . "https://orgmode.org/elpa/")
	  ("melpa" . "https://melpa.org/packages/")))

  ;; for private network
  ;; if package-initialize is not executed, the followin add-to-list make emacs fail
;  (add-to-list 'package-archives
;	       '("melpa" . "https://melpa.org/packages/")
;	       '("org"   . "https://orgmode.org/elpa/"))
  ;; add-to-list時、自宅でも Failed to download ‘(melpa . https://melpa.org/packages/)’ archive. になる場合があったので、setqにする
    (setq package-archives
	  '(("gnu" . "https://elpa.gnu.org/packages/")
	    ("org"   . "https://orgmode.org/elpa/")
	    ("melpa" . "https://melpa.org/packages/")))
)

;;; if use-package is not installed, install "use-package"
;;; https://emacs.stackexchange.com/questions/39250/error-package-use-package-is-unavailable
(package-refresh-contents) ; without this line, happen package-compute-transaction: Package ‘use-package-’ is unavailable
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; no effect! need to study
;; avoid to write custom-set-variables setting automatically in init.el
;; https://ymotongpoo.hatenablog.com/entry/2017/11/07/000921
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

