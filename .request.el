(use-package request
  :ensure request)

;; Important!
;; On msys2 mingw64 emacs environment
;; When request-backend is curl, request's GET method has the following error and doesn't work.
;; {"errorMessages":["Unexpected character ('u' (code 117)): expected a valid value (number, String, array, object, 'true', 'false' or 'null')\n at [Source: org.apache.catalina.connector.CoyoteInputStream@461e5b9e; line: 1, column: 2]"]}
;;
;; So, I tried to use url-retrieve as request-backend
;; (setq request-backend 'url-retrieve)
;; it works to a certain degree.
;;
;; But GET/PUT has multibyte characters, have the following error
;; GET response data can't be displayed as readable character.
;; PUT have the following error
;; Debugger entered--Lisp error: (error \"error: (error Multibyte text in HTTP request: PUT https://www.tool.company.biz/confluence/rest/api/content/...
;;
;; I suspect it cause in curl command itsefl on msys2 with referring https://github.com/ahungry/org-jira/issues/49#issuecomment-303870919
;;
;; Finally I download curl command windows native from https://curl.haxx.se/dlwiz/
;; and specified it as request-curl command.
;; It WORKS FINE and SOLVED the above all cumbersome problem.

(custom-set-variables
 '(request-curl
   (if (eq system-type 'windows-nt)
       (let ((curl "/c/winbin/curl-7.69.1-win64-mingw/bin/curl.exe"))
	 (if (executable-find curl)
	     curl
	   (error "install curl from https://curl.haxx.se/dlwiz/")
	   "curl"))
     "curl")))
