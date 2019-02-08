;(add-to-list 'load-path "~/.emacs.d/elpa/search-web-20150312.403")
(require 'search-web) ; need require

;- search-web-default-browser
;- search-web-in-emacs-browser
;- search-web-external-browser

; http://kawamuray.hatenablog.com/entry/2013/11/03/180543
; http://no-maddojp.hatenablog.com/entry/2015/01/23/235238
;(setq search-web-engine "google ja")
(setq search-web-engine "eijiro")

(defadvice w3m-browse-url (around w3m-browse-url-popwin activate)
   (save-window-excursion ad-do-it)
   (unless (get-buffer-window "*w3m*")
      (pop-to-buffer "*w3m*")))

(defadvice eww-render (around eww-render-popwin activate)
  (save-window-excursion ad-do-it)
  (unless (get-buffer-window "*eww*")
    (pop-to-buffer "*eww*")))

;(push "*eww*" popwin:special-display-config)
;(push "*w3m*" popwin:special-display-config)
