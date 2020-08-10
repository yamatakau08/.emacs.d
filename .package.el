;;; https://www-he.scphys.kyoto-u.ac.jp/member/shotakaha/dokuwiki/doku.php?id=toolbox:emacs:package:start
(require 'package)

;; need to set package-archives before executing "package-initialize"
;; when include ("gnu"   . "https://elpa.gnu.org/packages/") and use setq for pacakge-archives in company network,
;; since failed to get the list from "gnu", delete from package-archives.
;; setq is no effect, use custom-set-variables
(custom-set-variables
 '(package-archives
   '(("org"   . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("celpa" . "https://celpa.conao3.com/packages/"))))

;;; Unnecessary call on Emacs 27
;;; https://github.com/jkitchin/scimax/issues/194#issuecomment-380470596
(if (< emacs-major-version 27)
    (package-initialize))
;;; https://github.com/jkitchin/scimax/issues/194#issuecomment-385437906
;(with-eval-after-load 'package
;  (unless package--initialized
;    (package-initialize t)))
(unless package--initialized
    (package-initialize t))

;;; in case, M-x list-packages with the above settings
;;; https://emacs-jp.slack.com/archives/C1B5WTJLQ/p1547100014738900
;;; Error while verifying signature archive-contents.sig:
;;;
;;; Command output:
;;; gpg: no valid OpenPGP data found.
(setq package-check-signature nil)

;;; if use-package is not installed, install "use-package"
;;; https://emacs.stackexchange.com/questions/39250/error-package-use-package-is-unavailable
(if (my-get-network-type)
    (package-refresh-contents)) ; without this line, happen package-compute-transaction: Package ‘use-package-’ is unavailable
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
