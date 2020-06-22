;; About Confluence restapi
;; refer the followings.
;; example      https://developer.atlassian.com/server/confluence/confluence-rest-api-examples/
;; api referece https://developer.atlassian.com/cloud/confluence/rest/
;;
;; e.g. api url "expand" keyword is used in the function in this program
;; /rest/api/content/%s?expand=body.storage
;; refer https://developer.atlassian.com/cloud/confluence/rest/#api-api-content-id-get
;; explains
;; GET /wiki/rest/api/content/{id}?
;; seems to be base url.
;; There is "expand" item and explains
;; "body" returns the body of the content in different formats, including the editor format, view format, and export format.
;; in this case
;; concat "?expand=" with baseurl.
;; Then additional ".storage"
;; the page explains the following.
;; If you need to expand nested entities, use the . dot notation.
;; at https://developer.atlassian.com/cloud/confluence/rest/#using
;;
;; other
;; https://www.ricksoft.jp/blog/archives/2984/

(require 'request)
(require 'shr) ; for showing the content with shr-render-buffer
(require 'ox-confluence) ; to convert org to confluence wiki? markup

;; for debug
;(setq request-log-level     'debug)
;(setq request-message-level 'debug)

(defgroup my-confluence nil
  "My confluence rest api interface"
  :group 'tools) ; set from finder-known-keywords need to (require 'finder)

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
;; I download curl command windows native from https://curl.haxx.se/dlwiz/
;; and specified it as request-curl command.
;; It WORKS FINE and SOLVED the avobe all cumbersome problem.

(setq request-curl
      (if (eq system-type 'windows-nt)
	  (progn
	    (let ((msys-curl-command "/c/winbin/curl-7.69.1-win64-mingw/bin/curl.exe"))
	      (if (executable-find msys-curl-command) msys-curl-command "curl")))
	"curl"))

(defcustom my-confluence-url "https://www.tool.company.biz/confluence"
  "Confluence url"
  :group 'my-confluence
  :type  'string)

(defcustom my-confluence-auth-url nil
  "set confluence authentication rest api url,
e.g. \"https://www.tool.company.biz/jira3/rest/auth/1/session\"
If Confluence work with JIRA, probaly use JIRA authentication url for rest api.
Probably you can get the auth url after login JIRA, and replace base url \"www.tool.company.biz/jira3\" to your site url"
  :group 'my-confluence
  :type  'string)

(defcustom my-confluence-user-login-name nil
  "Username for login Confluence as same as JIRA login in my company"
  :group 'my-confluence
  :type  'string)

(defvar my-confluence--session nil
  "Contains the cookie of the active session.")

(defun my-confluence-get-cookie (&optional username password)
  "Get a session cookie from Jira
https://developer.atlassian.com/server/jira/platform/cookie-based-authentication/
https://developer.atlassian.com/cloud/jira/platform/jira-rest-api-cookie-based-authentication/"

  (interactive)

  (message "[my-confluence] -get-cookie getting cookie ...")

  (let ((xusername (or username
		       my-confluence-user-login-name
		       (read-string "Confluence Username: ")))
	(xpassword (or password
		       (read-passwd "Confluence Password: "))))
    (setq my-confluence--session nil) ; clear the last cookie
    (request
      my-confluence-auth-url
      :sync t
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :data (json-encode
	     `(("username" . ,xusername)
	       ("password" . ,xpassword)))
      :parser 'json-read ; parse-error occurs without json-encode at ":data" part
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (setq my-confluence--session
			;;(format "username=id:%s=%s" ; jirlib2.el pattern
			;; it's ok without "username=id:" above.
			;; https://developer.atlassian.com/server/jira/platform/cookie-based-authentication/
			;; shows cookie like JSESSIONID=6E3487971234567896704A9EB4AE501F
			(format "%s=%s"
				(cdr (assoc 'name  (car data)))
				(cdr (assoc 'value (car data)))))
		  (message "[my-confluence] -get-cookie cookie: %s" my-confluence--session)))
      ;; &allow-other-keys : fail
      ;; &allow-other-keys&rest : fail document error
      ;; &key data *error-thrown &rest _ fail
      :status-code '((401 . (lambda (&key data &allow-other-keys &rest _)
			      (message "[my-confluence] -get-cookie %s %s" (let-alist data .errorMessages) (plist-get _ :error-thrown)))))
      :error (cl-function
	      ;; https://tkf.github.io/emacs-request/ regacy document
	      ;; "&allow-other-keys&rest _" is document error
	      ;;(lambda (&key error-thrown &allow-other-keys&rest _)
	      ;; correct one is
	      ;; https://github.com/tkf/emacs-request/blob/master/README.rst#examples
	      (lambda (&rest args &key error-thrown &allow-other-keys)
		(message "[my-confluence] -get-cookie error: %s" error-thrown))))
    my-confluence--session))

(defun my-confluence-get-content-body-storage-by-id (pageId)
  "Get Confluence content specified pageId"
  (interactive "spageId: ")
  (unless my-confluence--session
    (my-confluence-get-cookie))
  (my-confluence--get-content-body-storage-by-id pageId))

(defun my-confluence-delete-content (pageId)
  "Delete Confluence page specified pageId"
  (interactive "spageId: ")
  (unless my-confluence--session
    (my-confluence-get-cookie))
  (my-confluence--delete-content pageId))

(defun my-confluence-get-content-info-by-id (pageId)
  "Get Confluence content info specified content-id"
  (interactive "spageId: ")
  (unless my-confluence--session
    (my-confluence-get-cookie))
  (let ((content-info (my-confluence--get-content-info-by-id pageId)))
    (unless content-info ; retry due to cookie is expired or pageId doesn't exist
      (message "[my-confluence] -get-content-info-by-id retry due to pageId(%s) not found or session expired..." pageId)
      (my-confluence-get-cookie)
      (setq content-info (my-confluence--get-content-info-by-id pageId)))
    (message "[my-confluence] -get-content-info-by-id: %s" content-info)
    content-info))

(defun my-confluence-update-content-with-org-buffer ()
  "Update Confluence content with org buffer."
  (interactive)
  (let ((org-buffer-file-name (buffer-file-name)))
    (my-confluence-update-content org-buffer-file-name)))

(defun my-confluence-update-content-with-org-file (org-file-name)
  "Update Confluence content with org file."
  (interactive "forg-file: ")

  (if (string= "org" (file-name-extension org-file-name))
      (progn
	(with-temp-buffer
	  (insert-file-contents org-file-name)
	  (org-mode) ; important to enable org-mode, jk-org-kwds function the following works in ONLY org-mode
	  (my-confluence-update-content org-file-name) ; in this buffer, should execute, because jk-org-kwds works in org-mode buffer
	  ))
;      (progn
;	;(find-file-noselect org-file-name)
;	(find-file org-file-name)
;	(my-confluence-update-content org-file-name))
    (message "[my-confluence] -update-content-with-org-file: Not org file")))

(defun my-confluence-update-content (org-buffer-file-name)
  "Update Confluence page specified by pageId in org file.
if pageId is not specified in org file, prompt to ask pageId."

  (if (string= "org" (file-name-extension org-buffer-file-name))
      (let* ((cfw (format "%s.cfw" (file-name-sans-extension org-buffer-file-name))) ; cfw: ConFluence Wiki
	     pageId content-info
	     status version title type body)

	;; function jk-org-kwd is defined in ~/.emacs.d/.org.el
	;; read keyword #+PAGEID: value in org file
	(setq pageId (jk-org-kwd "PAGEID"))

	(unless pageId
	  (setq pageId (read-string "pageId: ")))

	(org-export-to-file 'confluence cfw nil nil nil t nil) ; export org to confluence format file

	(unless my-confluence--session
	  (my-confluence-get-cookie))

	(if my-confluence--session
	    (progn
	      (setq content-info (my-confluence-get-content-info-by-id pageId))
	      (if content-info
		  (progn
		    (setq status  (let-alist content-info .status)
			  version (let-alist content-info .version)
			  title   (let-alist content-info .title)
			  type    (let-alist content-info .type)
			  body    (my-confluence-convert-content-body cfw))
		    (message "[my-confluence] -update-content updating pageId(%s)" pageId)
		    (my-confluence--update-content content-info body))
		(message "[my-confluence] -update-content not found pageId(%s) or session expired!" pageId))))
	)
    (message "[my-confluence] -update-content not in org buffer or file")))

(defun my-confluence-convert-content-body (wiki-file)
  "Convert confluence-wiki file format to html format is inside body"

  (interactive "fconfluence wiki format file: ")

  (unless my-confluence--session
    (my-confluence-get-cookie))

  (let ((body (with-temp-buffer
		(insert-file-contents wiki-file)
		(buffer-substring-no-properties (point-min) (point-max)))))
    (message "[my-confluence] -convert-content-body converting %s" wiki-file)
    (my-confluence--convert-content-body body)))

(defun my-confluence-markdownxhtmlconverter (markdown-file)
  "Convert markdown file to xhtml and return the string converted
markdown: normal markdown or confluence markdown ? need to study
Confluence Wiki markup rules: https://confluence.atlassian.com/confcloud/confluence-wiki-markup-938044804.html
"
  (interactive "fmarkdown-file: ")

  (let ((body (with-temp-buffer
		(insert-file-contents markdown-file)
		(buffer-substring-no-properties (point-min) (point-max)))))
    (my-confluence--markdownxhtmlconverter body))

;  (let ((body (my-confluence--markdownxhtmlconverter " * one\n * two")))
;    (message "[my-confluence] -markdownxhtmlconvert converted: %s" (format "%s" body)))
  )

(defun my-confluence-browse ()
  "browse confluence page"
  (interactive)
  (let ((pageId (or (jk-org-kwd "PAGEID")
		    (read-string "pageId: "))))
    (cond
     ((eq system-type 'windows-nt)
      (w32-shell-execute "open" (format "%s/pages/viewpage.action?pageId=%s" company-confluence-url pageId))))))

;; private

;; need to study!
(defun my-confluence-auth-url-success (key data allow-other-keys)
  (switch-to-buffer "*request-result*")
  (erase-buffer)
  (insert (format "%s" data)))

(defun my-confluence--get-content-info-by-id (content-id)
  "The backend of getting Confluence content info specified content-id
https://developer.atlassian.com/cloud/confluence/rest/#api-api-content-id-get"

  (message "[my-confluence] --get-content-info-by-id getting content info id(%s) ..." content-id)

  (let (content-info)
    (request
      (format "%s/rest/api/content/%s" my-confluence-url content-id)
      :sync t
      :type "GET"
      :headers `(("Content-Type" . "application/json") ("cookie" . ,my-confluence--session))
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys) ; pass &key argument name should be data
		  ;;(lambda (&key data &rest _ &allow-other-keys) ; fail
		  ;;(lambda (&key data) ; fail without &allow-other-keys
		  (setq content-info
			(list (assoc 'id data)
			      (assoc 'status data)
			      `(version . ,(let-alist data .version.number)) ; for content update
			      (assoc 'title data)
			      (assoc 'type data)))
		  ))
      ;; Even if content specified contet-id exists, somehow respond 404
      ;; On the other hand, there is no content specified content-id, respond 404
      ;; It's hard to distinguish whether the contet exists or not!
      :status-code '((404 . (lambda (&rest _)
			      (message "[my-confluence] --get-content-info-by-id status-code: 404")))))
    content-info
  ))

(defun my-confluence--get-content-body-storage-by-id (pageId)
  "The backend of getting Confluence page
https://developer.atlassian.com/cloud/confluence/rest/#api-api-content-get
https://developer.atlassian.com/server/confluence/confluence-rest-api-examples/#manipulating-content"

  (message "[my-confluence] --get-content-body-storage-by-id retriving pageId(%s) ..." pageId)

  (request
    ;; to get content, url is specified ?expand=body.storage
    ;; is described https://developer.atlassian.com/server/confluence/confluence-rest-api-examples/#manipulating-content
    ;; body.???? ???? should set the format is described
    ;; https://developer.atlassian.com/cloud/confluence/rest/#api-api-content-get
    ;; explains body returns the body of the content in different formats, including the editor format, view format, and export format
    (format "%s/rest/api/content/%s?expand=body.storage" my-confluence-url pageId)
    :sync t
    :type "GET"
    :headers `(("Content-Type" . "application/json") ("cookie" . ,my-confluence--session))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(let ((bufname (format "Confluence pageId: %s" pageId)))
		  (switch-to-buffer bufname)
		  (erase-buffer)
		  (insert (let-alist data .body.storage.value))

		  ;; render html
		  (shr-render-buffer bufname) ; make *html* buffer with rendering
		  (setq buffer-read-only t))  ; set read-only *html* buffer http://emacs.rubikitch.com/inhibit-read-only/
		))
    :status-code '((404 . (lambda (&rest _)
			    (message "[my-confluence] --get-content-body-storage-by-id: 404")
			    )))
    ))

(defun my-confluence--update-content (content-info body)
  "The backend of updating Confluence page
https://developer.atlassian.com/server/confluence/confluence-rest-api-examples/ \"Update a page\"
https://developer.atlassian.com/cloud/confluence/rest/#api-api-content-id-put"

  (let ((uci (copy-alist content-info))
	pageId) ; uci: update content info
    ;; get id
    (setq pageId (let-alist uci .id))
    ;; increment version
    (setcdr (assoc 'version uci)  `((number . ,(+ (let-alist uci .version) 1))))
    ;; add body
    (add-to-list 'uci `(body (storage (value . ,body) (representation . "storage"))) t) ; (*) representaiion should set "storage" to update content

    ;; "representation" "storage","view","wiki" ...
    ;; https://developer.atlassian.com/server/confluence/confluence-rest-api-examples/#converting-content
    ;; See "Convert wiki markup to storage format", "representation":"wiki"

    ;; (*) when set "view" in "representation", faced error!
    ;; {"statusCode":500,"data":{"authorized":false,"valid":true,"errors":[],"successful":false},"message":"com.atlassian.confluence.api.service.exceptions.ServiceException: java.lang.UnsupportedOperationException: Cannot convert from view to storage"}

    (message "[my-confulence] --update-contennt uci: %s" uci) ; for debug

    (request
      (format "%s/rest/api/content/%s" my-confluence-url pageId)
      :sync t
      :type "PUT"
      :headers `(("Content-Type" . "application/json") ("cookie" . ,my-confluence--session))
      :data    (json-encode uci)
      :parser  'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (message "[my-confluence] --update-content success pageId(%s)" pageId)))
      :error   (cl-function
		(lambda (&rest args &key error-thrown &allow-other-keys)
		  (message "[my-confluence] --update-content error: %S" error-thrown))))))

(defun my-confluence--get-content-list (spaceKey)
  "The backend of getting Confluence page by pageId
https://developer.atlassian.com/cloud/confluence/rest/#api-api-content-get"

  (request
    (format "%s/rest/api/content" my-confluence-url)
    :sync t
    :type "GET"
    :headers `(("Content-Type" . "application/json") ("cookie" . ,my-confluence--session))
    :data (json-encode `(("spaceKey" . ,spaceKey)))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(switch-to-buffer "*request-result*")
		(erase-buffer)
		(insert (format "%s" data))
		))
    ))

(defun my-confluence--get-space-list ()
  "The backend of getting Confluence page by pageId
https://developer.atlassian.com/cloud/confluence/rest/#api-api-content-id-get"

  (request
    (format "%s/rest/api/space" my-confluence-url)
    :sync t
    :type "GET"
    :headers `(("Content-Type" . "application/json") ("cookie" . ,my-confluence--session))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(switch-to-buffer "*request-result*")
		(erase-buffer)
		(my-confluence--parse-spaces data)
		))
    ))

(defun my-confluence--parse-spaces (data)
  "parse spaces list"
  (let ((v (cdr (assq 'results data))))
    (dotimes (i (length v))
      (let ((result (aref v i)))
	(insert (format "key:%s name:%s\n" (let-alist result .key) (let-alist result .name)))))))

(defun my-confluence--delete-content (pageId)
  "The backend of deleting Confluence page
https://developer.atlassian.com/cloud/confluence/rest/#api-api-content-id-delete"

  (request
    (format "%s/rest/api/content/%s" my-confluence-url pageId)
    :sync t
    :type "DELETE"
    :headers `(("Content-Type" . "application/json") ("cookie" . ,my-confluence--session))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(switch-to-buffer "*request-result*")
		(erase-buffer)
		(insert (format "%s" data))))))

(defun my-confluence--convert-content-body (wiki-content)
  "Convert arg is wiki-content to storage format
This api WORKS but I don't understand how to use response data
https://developer.atlassian.com/server/confluence/confluence-rest-api-examples/#converting-content
https://developer.atlassian.com/cloud/confluence/rest/#api-api-contentbody-convert-to-post"

  (let (converted)
    (request
      (format "%s/rest/api/contentbody/convert/storage" my-confluence-url)
      :sync t
      :type "POST"
      :headers `(("Content-Type" . "application/json") ("cookie" . ,my-confluence--session))
      :data (json-encode `(("value" . ,wiki-content)
			   ("representation" . "wiki")))
      :parser 'json-read
;    :success (cl-function
;	      (lambda (&key data &allow-other-keys)
;		(switch-to-buffer "*request-result*")
;		(erase-buffer)
;		(insert (format "%s" data))))
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (setq converted (let-alist data .value)))))
    converted))

(defun my-confluence--markdownxhtmlconverter (markdown-content)
  "Convert markdown to xhtml
This is not documented.
I found this api in the ATTASSIAN Communitythe follwoing.
https://community.atlassian.com/t5/Answers-Developer-Questions/How-do-you-post-markdown-using-confluences-rest-API/qaq-p/492056"

;; curl -XPOST -u <user>:<password> \
;; https://.../rest/tinymce/1/markdownxhtmlconverter \
;; -H 'Content-Type: application/json' \
;; -d '{"wiki": " * one\n * two"}'

  (let ((body))
    (request
      (format "%s/rest/tinymce/1/markdownxhtmlconverter" my-confluence-url)
      :sync t
      :type "POST"
      :headers `(("Content-Type" . "application/json") ("cookie" . ,my-confluence--session))
      :data (json-encode `(("wiki" . ,markdown-content)))
      ;;:parser 'json-read ;; !no need to parse
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (switch-to-buffer "*request-result*")
		  (erase-buffer)
		  (insert (format "%s" data))
		  (setq body data))
		))
    body)
  )

;;
(provide 'my-confluence)
