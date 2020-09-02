(use-package search-web
  :custom
  ;; http://kawamuray.hatenablog.com/entry/2013/11/03/180543
  ;; http://no-maddojp.hatenablog.com/entry/2015/01/23/235238
  ;;(search-web-engine "google ja")
  (search-web-engine "eijiro")

  :config
  (defadvice w3m-browse-url (around w3m-browse-url-popwin activate)
    (save-window-excursion ad-do-it)
    (unless (get-buffer-window "*w3m*")
      (pop-to-buffer "*w3m*")))

  (defadvice eww-render (around eww-render-popwin activate)
    (save-window-excursion ad-do-it)
    (unless (get-buffer-window "*eww*")
      (pop-to-buffer "*eww*")))
  )
