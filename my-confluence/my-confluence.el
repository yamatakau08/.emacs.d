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
;; Finally I download curl command windows native from https://curl.haxx.se/dlwiz/
;; and specified it as request-curl command.
;; It WORKS FINE and SOLVED the above all cumbersome problem.

;(setq request-curl
;      (if (eq system-type 'windows-nt)
;	  (progn
;	    (let ((msys-curl-command "/c/winbin/curl-7.69.1-win64-mingw/bin/curl.exe"))
;	      (if (executable-find msys-curl-command) msys-curl-command "curl")))
;	"curl"))

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

(defcustom my-confluence-username nil
  "Username for login Confluence as same as JIRA login in my company"
  :group 'my-confluence
  :type  'string)

(defcustom my-confluence-password nil
  "Password to use when logging in to JIRA. Not recommended to set this for security."
  :type 'string)

(defvar my-confluence--session nil
  "Contains the session information.")

(defvar my-confluence--cookie nil
  "Contains the cookie of the active session.")

(defun my-confluence--build-basic-auth-token ()
  "Build the base64-encoded auth token from `helm-jira-username' and `helm-jira-password'."
  (base64-encode-string (format "%s:%s" my-confluence-username my-confluence-password)))

(defun my-confluence-get-cookie (&optional username password)
  "Get a session cookie from Jira
https://developer.atlassian.com/server/jira/platform/cookie-based-authentication/
https://developer.atlassian.com/cloud/jira/platform/jira-rest-api-cookie-based-authentication/"

  (interactive)
  (message "[my-confluence] -get-cookie getting cookie ...")

  (if (not my-confluence-auth-url)
      (message "[my-confluence] set my-confluence-auth!")
    (let* ((xusername (or username
			  my-confluence-username
			  (read-string "Confluence Username: ")))
	   (xpassword (or password
			  (read-passwd (format "Confluence Password for %s: " xusername)))))
      (setq my-confluence--session nil) ; clear the last session infromation
      (setq my-confluence--cookie  nil) ; clear the last cookie
      (request
	my-confluence-auth-url
	:sync t
	:type "POST"
	:headers '(("Content-Type" . "application/json"))
	:data (json-encode
	       `(("username" . ,xusername)
		 ("password" . ,xpassword)))
	;;:parser 'json-read ; parse-error occurs without json-encode at ":data" part
	:parser 'my-confluence--parser
	:success (cl-function
		  (lambda (&key data &allow-other-keys)
		    (setq my-confluence--session data)
		    (setq my-confluence--cookie
			  ;; https://developer.atlassian.com/server/jira/platform/cookie-based-authentication/#step-1--create-a-new-session-using-the-jira-rest-api
			  ;; shows cookie like JSESSIONID=6E3487971234567896704A9EB4AE501F
			  (format "%s=%s" (let-alist data .session.name) (let-alist data .session.value)))
		    (message "[my-confluence] -get-cookie cookie: %s" my-confluence--cookie)))
	:error (cl-function
		(lambda (&rest ret &key data symbol-status error-thrown &allow-other-keys)
		  (message "[my-confluence] -get-cookie error: %s" error-thrown)))))))

(defun my-confluence-get-content-body-storage-by-id (pageId)
  "Get Confluence content specified pageId"
  (interactive "spageId: ")
  (my-confluence--get-content-body-storage-by-id pageId))

(defun my-confluence-delete-content (pageId)
  "Delete Confluence page specified pageId"
  (interactive "spageId: ")
  (my-confluence--delete-content pageId))

(defun my-confluence-get-content-info-by-id (pageId)
  "Get Confluence content info specified content-id"
  (interactive "spageId: ")
  (let ((content-info (my-confluence--get-content-info-by-id pageId)))
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
	  (org-mode) ; important to enable org-mode, jk-org-kwd function the following works in ONLY org-mode
	  (my-confluence-update-content org-file-name) ; in this buffer, should execute, because jk-org-kwd works in org-mode buffer
	  ))
;      (progn
;	;(find-file-noselect org-file-name)
;	(find-file org-file-name)
;	(my-confluence-update-content org-file-name))
    (message "[my-confluence] -update-content-with-org-file: Not org file")))

(defun my-confluence-update-content (org-buffer-file-name)
  "Update Confluence page specified by pageId in org file.
if pageId is not specified in org file, prompt to ask pageId."

  (if (not (string= "org" (file-name-extension org-buffer-file-name)))
      (message "[my-confluence] -update-content not in org buffer or file")

    (let* ((cfw (format "%s.cfw" (file-name-sans-extension org-buffer-file-name))) ; cfw: ConFluence Wiki
	   (pageId (or (jk-org-kwd "PAGEID") ;; read keyword #+PAGEID: in org file
		       (read-string "pageId: ")))
	   content-info
	   status version title type body)

      (org-export-to-file 'confluence cfw nil nil nil t nil) ; export into confluence format file

      (setq content-info (my-confluence-get-content-info-by-id pageId))
      (if (not content-info)
	  (message "[my-confluence] -update-content not found pageId(%s)" pageId)

	(setq status  (let-alist content-info .status)
	      version (let-alist content-info .version)
	      title   (let-alist content-info .title)
	      type    (let-alist content-info .type)
	      body    (my-confluence-convert-content-body cfw))
	(message "[my-confluence] -update-content updating pageId(%s)" pageId)
	(my-confluence--update-content content-info body)))))

(defun my-confluence-convert-content-body (wiki-file)
  "Convert confluence-wiki file format to html format is inside body"
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
    (my-confluence--markdownxhtmlconverter body)))

(defun my-confluence-browse ()
  "browse confluence page specified #+PAGEID in org file"
  (interactive)
  (let ((pageId (jk-org-kwd "PAGEID")))
    (if pageId
	(browse-url-default-browser
	 (format "%s/pages/viewpage.action?pageId=%s" company-confluence-url pageId)))))

(defun my-confluence--get-content-info-by-id (content-id)
  "The backend of getting Confluence content info specified content-id
https://developer.atlassian.com/cloud/confluence/rest/#api-api-content-id-get"

  (let (content-info)
    (my-confluence--request
     (format "%s/rest/api/content/%s" my-confluence-url content-id)
     ;;:parser 'json-read
     :parser 'my-confluence--parser
     :success (cl-function
	       (lambda (&key data &allow-other-keys)
		 (setq content-info
		       (list (assoc 'id data)
			     (assoc 'status data)
			     `(version . ,(let-alist data .version.number)) ; for content update
			     (assoc 'title data)
			     (assoc 'type data)))))
     :status-code '(;; Even if token is expired, confluence doesn't return 401. this part is for safety.
		    (401 . (lambda (&rest _)
			     (message "[my-confluence][debug] --get-content-info-by-id status-code: 401")
			     (setq my-confluence--cookie nil)
			     (my-confluence--get-content-info-by-id content-id))) ; try again
		    ;; Even if the page specified content-id exists, when token is expired, get respond 404
		    ;; On the other hand, there is no content specified content-id, respond 404
		    ;; It's hard to distinguish whether the contet really exists or not!
		    (404 . (lambda (&rest _)
			     (message "[my-confluence][debug] --get-content-info-by-id status-code: 404")))))
    content-info))

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
    :headers `(("Content-Type" . "application/json") ("cookie" . ,my-confluence--cookie))
    ;;:parser 'json-read
    :parser 'my-confluence--parser
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

  (let* ((uci (copy-alist content-info)) ; uci: update content info
	 (pageId (let-alist uci .id)))   ; get id
    ;; increment version
    (setcdr (assoc 'version uci)  `((number . ,(+ (let-alist uci .version) 1))))
    ;; add body
    (add-to-list 'uci `(body (storage (value . ,body) (representation . "storage"))) t) ; (*) representaiion should set "storage" to update content

    ;; "representation","storage","view","wiki" ...
    ;; refer https://developer.atlassian.com/server/confluence/confluence-rest-api-examples/#converting-content
    ;; See "Convert wiki markup to storage format", "representation":"wiki"

    ;; (*) when set "view" in "representation", got error!
    ;; {"statusCode":500,"data":{"authorized":false,"valid":true,"errors":[],"successful":false},"message":"com.atlassian.confluence.api.service.exceptions.ServiceException: java.lang.UnsupportedOperationException: Cannot convert from view to storage"}

    (my-confluence--request
     (format "%s/rest/api/content/%s" my-confluence-url pageId)
     :type "PUT"
     :data    (json-encode uci)
     ;;:parser  'json-read
     :parser 'my-confluence--parser
     :success (cl-function
	       (lambda (&key data &allow-other-keys)
		 (message "[my-confluence] --update-content success pageId(%s)" pageId))))))

(defun my-confluence--delete-content (pageId)
  "The backend of deleting Confluence page
https://developer.atlassian.com/cloud/confluence/rest/#api-api-content-id-delete"
  (my-confluence--request
   (format "%s/rest/api/content/%s" my-confluence-url pageId)
   :type "DELETE"
   :success (cl-function
	     (lambda (&key data &allow-other-keys)
	       (message "[my-confluence] deleted pageId: %s" pageId)))))

(defun my-confluence--convert-content-body (wiki-content)
  "Convert arg is wiki-content to storage format
This api WORKS but I don't understand how to use response data
https://developer.atlassian.com/server/confluence/confluence-rest-api-examples/#converting-content
https://developer.atlassian.com/cloud/confluence/rest/#api-api-contentbody-convert-to-post"

  (let (converted)
    (my-confluence--request
      (format "%s/rest/api/contentbody/convert/storage" my-confluence-url)
      :type "POST"
      :data (json-encode `(("value" . ,wiki-content)
			   ("representation" . "wiki")))
      ;;:parser 'json-read
      :parser 'my-confluence--parser
      ;; :success (cl-function
      ;; 		(lambda (&key data &allow-other-keys)
      ;; 		  (switch-to-buffer "*request-result*")
      ;; 		  (erase-buffer)
      ;; 		  (insert (format "%s" data))))
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (setq converted (let-alist data .value)))))
    converted))

(defun my-confluence--markdownxhtmlconverter (markdown-content)
  "Convert markdown to xhtml
This is not documented.
Found this api in the ATTASSIAN Communitythe follwoing.
https://community.atlassian.com/t5/Answers-Developer-Questions/How-do-you-post-markdown-using-confluences-rest-API/qaq-p/492056"

;; curl -XPOST -u <user>:<password> \
;; https://.../rest/tinymce/1/markdownxhtmlconverter \
;; -H 'Content-Type: application/json' \
;; -d '{"wiki": " * one\n * two"}'

  (let (body)
    (request
      (format "%s/rest/tinymce/1/markdownxhtmlconverter" my-confluence-url)
      :sync t
      :type "POST"
      :headers `(("Content-Type" . "application/json") ("cookie" . ,my-confluence--cookie))
      :data (json-encode `(("wiki" . ,markdown-content)))
      ;;:parser 'json-read ;; !no need to parse
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (switch-to-buffer "*request-result*")
		  (erase-buffer)
		  (insert (format "%s" data))
		  (setq body data))))
    body))

(defun my-confluence-create-or-update-attachment ()
  "Add attachments the files which are marked in Dired in confluence content id you specified"
  ;; Todo non-ascii filename upload

  (interactive)

  (let* ((files (when (> (string-to-number (dired-number-of-marked-files)) 0) (dired-get-marked-files)))
	 (limitsize 40000000) ; upload limit size
	 (uploadfiles
	  (delq nil (mapcar (lambda (file)
			      (if (> (file-attribute-size (file-attributes file)) limitsize)
				  (progn
				    (message "[my-confluence] %s > %s" file limitsize)
				    nil)
				file)) files)))
	 content-id)
    (if files
	(if uploadfiles
	    (progn
	      (setq content-id (read-string "Content Id: ")) ;; need to check if inputed string is nil
	      (message "[my-confluence] uploading in id %s ..." content-id)
	      (my-confluence--request
		;; refer https://developer.atlassian.com/cloud/confluence/rest/api-group-content---attachments/#api-api-content-id-child-attachment-put
		(format "%s/rest/api/content/%s/child/attachment" my-confluence-url content-id)
		:type "POST"  ; for create, success if the file is not already uploaded.
		;;:type "PUT" ; for create or update, always return http error 405
		:headers '(("X-Atlassian-Token" . "no-check"))
		:files (mapcar (lambda (file) `("file" . ,file)) uploadfiles)
		:success (cl-function
			  (lambda (&key data &allow-other-keys)
			    (message "[my-confluence] uploaded in id %s" content-id)))
		:error   (cl-function
			  (lambda (&rest args &key error-thrown &allow-other-keys)
			    ;; if the attachment already existed, have 404 error
			    (message "[my-confluence] Got error: %S" error-thrown)))))
	  (message "[my-confluence] No upload files!"))
      (message "[my-confluence] No marked files!"))))

(defun my-confluence--request-cookie (&rest args)
  "the common request for confluence rest api using cookie"
  (unless my-confluence--cookie
    (call-interactively #'my-confluence-get-cookie)
    (sleep-for 2)) ; to wait my-confluence--get-current-user return type "known".

  ;; once get cookie, even if cookie is modified, request will success. wonder!
  (if my-confluence--cookie ; check after execute my-confluence-get-cookie function
      (apply 'request
	     (append args
		     `(:headers (("Content-Type" . "application/json") ("cookie" . ,my-confluence--cookie)))
		     '(:sync t)
		     `(:error ,(cl-function
				(lambda (&rest response &key data error-thrown &allow-other-keys)
				  (let ((statusCode (let-alist data .statusCode))
					(message    (let-alist data .message)))
				    (message "[my-confluence][error] statusCode:%s %s" statusCode message)))))))
    (message "[my-confluence][error] password incorrect!")))

(defun my-confluence--request (&rest args)
  "the common request for confluence rest api using Basic Authentication"
  (my-confluence--ensure-password)
  (apply 'request
	 (append args
		 `(:headers (("Content-Type" . "application/json") ("Authorization" . ,(my-confluence--build-auth-header))))
		 '(:sync t)
		 `(:error ,(cl-function
			    (lambda (&rest response &key data error-thrown &allow-other-keys)
			      (let ((statusCode (let-alist data .statusCode))
				    (message    (let-alist data .message)))
				(message "[my-confluence][error] statusCode:%s %s" statusCode message))))))))

(defun my-confluence--search-content-by-cql (cql callback)
  "https://developer.atlassian.com/cloud/confluence/rest/api-group-content/#api-api-content-search-get"
  (my-confluence--request
   (format "%s/rest/api/content/search" my-confluence-url)
   ;;:parser 'json-read
   :parser 'my-confluence--parser
   :params `(("cql" . ,cql) ("limit" . 1000)) ; add limit=1000 is from result data, default is 25
   :success (cl-function
	     (lambda (&key data &allow-other-keys)
	       (funcall callback data)))
   ;; 403 https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
   ;; The server understood the request, but is refusing to fulfill it. Authorization will not help and the request SHOULD NOT be repeated.
   ;; assume 403 is that token is expired for search method
   ;; :status-code '((403 . (lambda (&rest _)
   ;; 			   (message "[my-confluence][error] --search-content-by-cql: 403")
   ;; 			   (setq my-confluence--cookie nil)
   ;; 			   (my-confluence--search-content-by-cql cql callback)))))
   ))

(defun my-confluence--get-spaces (callback)
  "The backend of getting Confluence page by pageId
https://developer.atlassian.com/cloud/confluence/rest/api-group-space/#api-api-space-get"
  (my-confluence--request
   (format "%s/rest/api/space" my-confluence-url)
   ;;:parser 'json-read
   :parser 'my-confluence--parser
   :params '(("limit" . 500)) ; add limit=500 is from result data, default is 25
   :success (cl-function
	     (lambda (&key data &allow-other-keys)
	       (if (<= (let-alist data .size) 0) ; assume size is 0, session is expired.
		   (message "[my-confluence] No confluence spaces.")
		 (funcall callback data))))))

;; user
(defun my-confluence--is-token-available ()
  (let ((current-user (my-confluence--get-current-user)))
    (if (string-equal (let-alist current-user .type) "known") t nil)))

(defun my-confluence--get-current-user ()
  "https://developer.atlassian.com/cloud/confluence/rest/api-group-users/#api-api-user-current-get"
  (let (ret)
    (my-confluence--request
     (format "%s/rest/api/user/current" my-confluence-url)
     ;;:parser 'json-read
     :parser 'my-confluence--parser
     :success (cl-function
	       (lambda (&key data &allow-other-keys)
		 (setq ret data))))
    (message "[my-confluence][debug] --get-current-user: %s" ret)
    ret))

(defun my-confluence--build-auth-header ()
  "Build the Authorization-Header for Confluence requests."
  (format "Basic %s" (my-confluence--build-basic-auth-token)))

(defun my-confluence--build-basic-auth-token ()
  "Build the base64-encoded auth token from `my-confluence-username' and `my-confluence-password'."
  (base64-encode-string (format "%s:%s" my-confluence-username my-confluence-password)))

(defun my-confluence--read-password ()
  "Read a new value for `my-confouence-password'."
  (setq my-confluence-password (read-passwd (format "Confluence-Password for %s: " my-confluence-username)))
  nil)

(defun my-confluence--ensure-password ()
  "Ensures that `my-confluence-password' is set."
  (if (not my-confluence-password)
      (my-confluence--read-password)
    my-confluence-password))

(defalias 'my-confluence--parser 'my-confluence--my-parser)

(defun my-confluence--my-parser ()
  "Since proxy server spec was changed on 2022/09/14, need to custom parser instead of normarl parser \"json-read\""
  (end-of-buffer)
  (backward-list)
  (json-read))

;; http://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/
;; function jk-org-kwd gets the propertie specifed by args.
;; your original propertiy is also available.
;; To utilize my original #+PAGEID: 123456 property in org file for Confluence page update
;; property should be BIG CHARATER
;; the following function is available in org BUFFER, means it's not available in with-temp-buffer with org-mode.

;; http://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/
(defun jk-org-kwds ()
  "parse the buffer and return a cons list of (property . value)
from lines like:
#+PROPERTY: value"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (keyword)
      (cons (org-element-property :key   keyword)
            (org-element-property :value keyword)))))

(defun jk-org-kwd (KEYWORD)
  "get the value of a KEYWORD in the form of #+KEYWORD: value"
  (cdr (assoc KEYWORD (jk-org-kwds))))

;;
(provide 'my-confluence)
