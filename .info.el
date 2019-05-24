;; note adding info directory in Info-default-directory-list doesn't effect
;; use Info-directory-list
;;(add-to-list 'Info-default-directory-list "/mingw64/share/info/")

;; on mingw64 environment /usr/share/info is /msys64/usr/share/info
;; and emacs.info is in /msys64/mingw64/share/info
;; so set /mingw64/share/info.
(add-to-list 'Info-directory-list "/mingw64/share/info/")
