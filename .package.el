;; https://www-he.scphys.kyoto-u.ac.jp/member/shotakaha/dokuwiki/doku.php?id=toolbox:emacs:package:start
;(require 'package)
(package-initialize)

; defaultでは、("gnu" . "http://elpa.gnu.org/packages/") しかなかったので、以下を追加 
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")) 
(add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/"))
