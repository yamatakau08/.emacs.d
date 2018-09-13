;; https://www-he.scphys.kyoto-u.ac.jp/member/shotakaha/dokuwiki/doku.php?id=toolbox:emacs:package:start
;(require 'package)

;;; https://github.com/jkitchin/scimax/issues/194#issuecomment-385437906
(package-initialize)
;(unless package--initialized 
;  (package-initialize t)) ; Unnecessary call on Emacs 27

; defaultでは、("gnu" . "http://elpa.gnu.org/packages/") しかなかったので、以下を追加 
(add-to-list 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 '("org"   . "http://orgmode.org/elpa/"))

;; no effect! need to study
;; avoid to write custom-set-variables setting automatically in init.el
;; https://ymotongpoo.hatenablog.com/entry/2017/11/07/000921
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

