;; note adding info directory in Info-default-directory-list doesn't effect
;; use Info-directory-list
;;(add-to-list 'Info-default-directory-list "/mingw64/share/info/")

;; on mingw64 environment /usr/share/info is /msys64/usr/share/info
;; and emacs.info is in /msys64/mingw64/share/info
;; so set /mingw64/share/info.
(add-to-list 'Info-directory-list "/mingw64/share/info/")
;; both "c:/winbin/emacs-26.2-x86_64/share/info" or "/c/winbin/emacs-26.2-x86_64/share/info" are OK
(add-to-list 'Info-directory-list "c:/winbin/emacs-26.2-x86_64/share/info")
