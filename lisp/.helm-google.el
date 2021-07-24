;;; https://blog.vifortech.com/posts/emacs-tls-fix/
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

;;; http://emacs.rubikitch.com/helm-google/
(require 'xml)
(setq helm-google-tld "co.jp")
;;; Google側のサイトデザイン変更で動かなくなったらAPI検索に切替える
;;; (setq helm-google-search-function 'helm-google-api-search)
;;; ブラウザをEWWにすればEmacs内で完結する
(setq browse-url-browser-function 'eww-browse-url)

(provide '.helm-google)

