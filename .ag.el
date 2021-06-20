(use-package ag
  :ensure t

  :custom
  (ag-arguments (list "--smart-case" "--stats" "--ignore-case" "--color-match"))
  ;; https://miyazakikenji.wordpress.com/2014/02/02/emacs%E3%81%A7%E4%B8%80%E6%8B%AC%E6%A4%9C%E7%B4%A2%E3%81%A8%E4%B8%80%E6%8B%AC%E7%BD%AE%E6%8F%9B/
  (ag-highlight-search t) ; No effect
  ;;(ag-reuse-window 'nil)   ; 現在のウィンドウを検索結果表示に使う
  ;;(ag-reuse-buffers 'nil)) ; 現在のバッファを検索結果表示に使う
)
