;; https://www-he.scphys.kyoto-u.ac.jp/member/shotakaha/dokuwiki/doku.php?id=toolbox:emacs:package:start
(require 'package) ;; need require to execute package's functions

;; need to set package-archives before executing "package-initialize"
;; when include ("gnu"   . "https://elpa.gnu.org/packages/") and use setq for pacakge-archives in company network,
;; since failed to get the list from "gnu", delete from package-archives.
;; setq is no effect, use custom-set-variables
(custom-set-variables
 '(package-archives
   '(("org"   . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu"   . "https://elpa.gnu.org/packages/")
     ;;("celpa" . "https://celpa.conao3.com/packages/")
     )))

;; in case, M-x list-packages with the above settings
;; https://emacs-jp.slack.com/archives/C1B5WTJLQ/p1547100014738900
;; Error while verifying signature archive-contents.sig:
;;
;; Command output:
;; gpg: no valid OpenPGP data found.
(custom-set-variables '(package-check-signature nil))

;; Unnecessary call on Emacs 27
;; https://github.com/jkitchin/scimax/issues/194#issuecomment-380470596
;; (if (< emacs-major-version 27)
;;     (package-initialize))
(package-initialize) ; but need to call, because when load minimal init.el on Emacs 28.5 on Windows have error without this.

;; https://github.com/jkitchin/scimax/issues/194#issuecomment-385437906
;;(with-eval-after-load 'package
;;  (unless package--initialized
;;    (package-initialize t)))
(unless package--initialized
    (package-initialize t))

;; install "use-package"
;; https://emacs.stackexchange.com/questions/39250/error-package-use-package-is-unavailable
(cond
 ((eq (my-network-type) 'company)
  (setenv "HTTP_PROXY"  wl-proxy-server)
  (setenv "HTTPS_PROXY" wl-proxy-server)

  (package-refresh-contents) ; without this line, happen package-compute-transaction: Package ‘use-package-’ is unavailable
  (unless (package-installed-p 'use-package)
    (package-install 'use-package)))

 ((eq (my-network-type) 'private)
  ;; set to nil to avoid miss match shell environment
  (setenv "HTTP_PROXY"  nil)
  (setenv "HTTPS_PROXY" nil)
  (package-refresh-contents) ; without this line, happen package-compute-transaction: Package ‘use-package-’ is unavailable
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))))

;; use-package
(use-package use-package
  :custom
  (use-package-verbose t)
  (use-package-expand-minimally t) ; to show minimal steps when executing macrostep
  (use-package-compute-statistics t)
  (package-quickstart t)
  )

(provide '.package)
