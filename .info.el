(use-package info
  :config
  ;; effect environment variable INFOPATH
  ;; note adding info directory in Info-default-directory-list doesn't effect
  ;; use Info-directory-list

  ;; on Windows Emacs ftp sitepackage
  ;; Though there are info for emacs in "c:/msys64/usr/share/info" which in Info-directory-list,
  ;; Info can't show emacs lisp info and so on
  ;; add c:/winbin/emacs-28.0.50/x86_64/share/info/ manually.
  ;; The following setting is also available on other platforms Mac,Linux... maybe
  (add-to-list 'Info-directory-list (format "%s../share/info" (invocation-directory)))
  )
