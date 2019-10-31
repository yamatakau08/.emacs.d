;;; customize ffap to get the file path in which has ' ' and '　' .
;;; helm-find-files (C-x f) use ffap can handle that as file

;;; modify ffap-string-at-point-mode-alist 'file
;;(with-eval-after-load "ffap"
(with-eval-after-load "ffap"
  ;; callf: refer https://qiita.com/kawabata@github/items/9a1a1e211c57a56578d8#%E6%B1%8E%E5%A4%89%E6%95%B0%E3%82%92%E6%89%B1%E3%81%86%E3%83%9E%E3%82%AF%E3%83%AD%E3%81%A8%E3%81%9D%E3%81%AE%E4%BE%8B
  (callf concat (cadr (assoc 'file ffap-string-at-point-mode-alist)) " 　") ; add ' ' space and '　' zenkau space for the file path have them
)

;;; test file path pattern
;;; M-: (ffapthing-at-point 'filename)
;;; file path should be within "file path"
;;; on windows
;;; "\\jps00004944\share_data\SQA1-HA\10_Test_Project_FY17\NewAudio\02_検証計画書\Schedule-NewAudio-Evaluation.pptx"
;;; "\\jps00004944\SQA_PostSkips\工数フォルダトップ\FY2019\SQA2_山口　尊広\SQA2_山口　尊広_社員工数メトリクス表_v0080.xlsm"
;;; -> when ffap-string-at-point-mode-alist 'file is default value
;;;    helm-find-files shows in minibuffer \\jps00004944\SQA_PostSkips\工数フォルダトップ\FY2019
;;;    (ffap-string-at-point 'file) returns \\jps00004944\SQA_PostSkips\工数フォルダトップ\FY2019\SQA2_山口　
;;; -> add ' ' space and '　' zenkau space in ffap-string-at-point-mode-alist 'file
;;;
;;; on my mac
;;; "~/Documents/QRチケット印刷 _ スワチケ2入口41段181番.pdf"
