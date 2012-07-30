
    ;; identica-http.el
    ;; Copyright (C) 2012  Giménez, Christian N.

    ;; This program is free software: you can redistribute it and/or modify
    ;; it under the terms of the GNU General Public License as published by
    ;; the Free Software Foundation, either version 3 of the License, or
    ;; (at your option) any later version.

    ;; This program is distributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ;; GNU General Public License for more details.

    ;; You should have received a copy of the GNU General Public License
    ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

    ;; Martes 24 De Julio Del 2012    


;; I'm not the only authors. These functions was done by lot of people.
;;
;; See identica-mode.el for information about who made this! :)

;; PURPOSE:
;; __________
;;
;; Here comes everything needed for connecting throught HTTP to identi.ca (or any status.net server).
;; 
;; Incudes everything related with authentication and connection to the microblogging server.
;;
;; Interface, or any interaction with the user must be done in the proper library. 
;; Avoid that as much as possible here!
;; 
;; This library shouldn't decode anything neither. 
;; This library just know how to connect and retrieve information!
;; ________________________________________
;;

;; TAKE A LOOK:
;; (1) A parentezised string as parameter?

(require 'url)
(require 'url-http)
(require 'identica-common-things)

(defvar identica-http-buffer nil
  "Pointer to the current http response buffer.")

(defvar identica-http-initialized nil
  "If identica-http variables has initialized. 
You can initialize variables using `identica-http-init-variables'.

Any function can use this variables to see if identica-http has all information available, and if not, call
themself the function `identica-http-init-variables'.")

;; Connection data.
(defcustom identica-username nil
  "Your identi.ca username.  If nil, you will be prompted."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'identica-mode)

(defcustom identica-password nil
  "Your identi.ca password.  If nil, you will be prompted."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'identica-mode)

(defcustom identica-auth-mode "password"
  "Authorization mode used, options are password and oauth."
  :type 'string
  :group 'identica-mode)

(defcustom statusnet-server "identi.ca"
  "Statusnet instance url."
  :type 'string
  :group 'identica-mode)

(defcustom statusnet-port 80
  "Port on which StatusNet instance listens."
  :type 'integer
  :group 'identica-mode)

(defcustom identica-http-get-timeout 10
  "Controls how long to wait for a response from the server."
  :type 'integer
  :group 'identica-mode)

(defcustom statusnet-server-textlimit 140
  "Number of characters allowed in a status."
  :type 'integer
  :group 'identica-mode)

(defstruct (statusnet-account
	    (:conc-name sn-account-))
  "Container for account information."
  server ; string
  port ; integer
  username ; string
  auth-mode ; string, either "password" or "oauth"
  password ; string
  textlimit ; integer
  oauth-data ; statusnet-account-oauth-data
  last-timeline-retrieved ; string
)

(defvar sn-current-account nil
  "A pointer to the statusnet account being processed.")

(defvar statusnet-accounts nil
  "A list of login credentials for statusnet instances.")


;; Variables and structures for OAUTH 

;;workaround for url-unhex-string bug that was fixed in emacs 23.3
(defvar identica-unhex-broken nil
  "Predicate indicating broken-ness of `url-unhex-string'.

If non-nil, indicates that `url-unhex-string' is broken and
must be worked around when using oauth.")

(defvar identica-mode-oauth-consumer-key
  "53e8e7bf7d1be8e58ef1024b31478d2b")

(defvar identica-mode-oauth-consumer-secret
  "1ab0876f14bd82c4eb450f720a0e84ae")

(defcustom statusnet-request-url
  "https://identi.ca/api/oauth/request_token"
  "Statusnet oauth request_token url."
  :type 'string
  :group 'identica-mode)

(defcustom statusnet-access-url
  "https://identi.ca/api/oauth/access_token"
  "Statusnet oauth access_token url."
  :type 'string
  :group 'identica-mode)

(defcustom statusnet-authorize-url
  "https://identi.ca/api/oauth/authorize"
  "Statusnet authorization url."
  :type 'string
  :group 'identica-mode)

(defvar oauth-access-token nil)

(defstruct (statusnet-oauth-data
	    (:conc-name sn-oauth-))
  "The oauth configuration associated with a statusnet account."
  consumer-key ; string
  consumer-secret ; string
  request-url ; string
  access-url ; string
  authorize-url ; string
  access-token ; string
)



(defun identica-get-status-url (id)
  "Generate status URL."
  (format "https://%s/notice/%s" (sn-account-server sn-current-account) id))

(defun identica-get-context-url (id)
  "Generate status URL."
  (format "https://%s/conversation/%s" (sn-account-server sn-current-account) id))

(defun identica-get-config-url ()
  "Generate configuration URL."
  (format "http://%s/api/statusnet/config.xml" (sn-account-server sn-current-account)))



					;____________________
					; Proxy
(defvar identica-proxy-use nil)
(defvar identica-proxy-server nil)
(defvar identica-proxy-port 8080)
(defvar identica-proxy-user nil)
(defvar identica-proxy-password nil)

(defun identica-toggle-proxy ()
  "Toggle whether identica-mode uses a proxy."
  (interactive)
  (setq identica-proxy-use
	(not identica-proxy-use))
  (message "%s %s"
	   "Use Proxy:"
	   (if identica-proxy-use
	       "on" "off")))

;;;
;;; Basic HTTP functions
;;;

(defun identica-set-proxy (&optional url username passwd server port)
  "Sets the proxy authentication variables as required by url library.
When called with no arguments, it reads `identica-mode' proxy
variables to get the authentication parameters.URL is either a string
or parsed URL.  If URL is non-nil and valid, proxy authentication
values are read from it.  The rest of the arguments can be used to
directly set proxy authentication.  This function essentially adds
authentication parameters from one of the above methods to the double
alist `url-http-proxy-basic-auth-storage' and sets `url-using-proxy'."
  (let* ((href (if (stringp url)
		   (url-generic-parse-url url)
		 url))
	 (port (or (and href (url-port href))
		   port identica-proxy-port))
	 (port (if (integerp port) (int-to-string port) port))
	 (server (or (and href (url-host href))
		     server identica-proxy-server))
	 (server (and server
		      (concat server (when port (concat ":" port)))))
	 (file (if href (let ((file-url (url-filename href)))
			  (cond
			   ((string= "" file-url) "/")
			   ((string-match "/$" file-url) file-url)
			   (t (url-basepath file-url))))
		 "Proxy"))
	 (password (or (and href (url-password href))
		       passwd identica-proxy-password))
	 (auth (concat (or (and href (url-user href))
			   username identica-proxy-user)
		       (and password (concat ":" password)))))
    (when (and identica-proxy-use
	       (not (string= "" server))
	       (not (string= "" auth)))
      (setq url-using-proxy server)
      (let* ((proxy-double-alist
	      (or (assoc server
			 url-http-proxy-basic-auth-storage)
		  (car (push (cons server nil)
			     url-http-proxy-basic-auth-storage))))
	     (proxy-auth-alist (assoc file proxy-double-alist)))
	(if proxy-auth-alist
	    (setcdr proxy-auth-alist (base64-encode-string auth))
	  (setcdr proxy-double-alist
		  (cons (cons file
			      (base64-encode-string auth))
			(cdr-safe proxy-double-alist))))))))

					; ____________________
					; authentication

(defun identica-set-auth (&optional url username passwd server port)
  "Sets the authentication parameters as required by url library.
If URL is non-nil and valid, it reads user authentication
parameters from url.  If URL is nil, Rest of the arguments can be
used to directly set user authentication.
When called with no arguments, user authentication parameters are
read from identica-mode variables `(sn-account-username sn-current-account)'
`(sn-account-password sn-current-account)' `(sn-account-server sn-current-account)'
 `(sn-account-port sn-current-account)'.
The username and password can also be set on ~/.authinfo,
~/.netrc or ~/.authinfo.gpg files for better security.
In this case `(sn-account-password sn-current-account)' should
not be predefined in any .emacs or init.el files, only
`(sn-account-username sn-current-account)' should be set."
  (unless (sn-account-username sn-current-account)
    (identica-ask-credentials))
  (let* ((href (if (stringp url)
		   (url-generic-parse-url url)
		 url))
	 (port (or (and href (url-port href))
		   port (sn-account-port sn-current-account)))
	 (port (if (integerp port) (int-to-string port) port))
	 (server (or (and href (url-host href))
		     server (sn-account-server sn-current-account)))
	 (servername server)
	 (server (and server
		      (concat server (when port (concat ":" port)))))
	 (file (if href (let ((file-url (url-filename href)))
			  (cond
			   ((string= "" file-url) "/")
			   ((string-match "/$" file-url) file-url)
			   (t (url-basepath file-url))))
		 "Identi.ca API"))

	 (auth-user (if (functionp 'auth-source-search)
		       (plist-get (car (auth-source-search :host servername :max 1)) :user)
		     (auth-source-user-or-password "login" server "http")))
	 (auth-pass (if (functionp 'auth-source-search)
			(if (functionp (plist-get (car (auth-source-search :host servername :max 1)) :secret))
			    (funcall (plist-get (car (auth-source-search :host servername :max 1)) :secret))
			  (plist-get (car (auth-source-search :host servername :max 1)) :secret))
		      (auth-source-user-or-password "password" server "http")))
	 (password (or auth-pass (and href (url-password href))
		       passwd (sn-account-password sn-current-account)))
	 (auth (concat (or auth-user (and href (url-user href))
			   username (sn-account-username sn-current-account))
		       (and password (concat ":" password)))))
    (when (and (not (string= "" server))
	       (not (string= "" auth)))
      (let* ((server-double-alist
	      (or (assoc server
			 url-http-real-basic-auth-storage)
		  (car (push (cons server nil)
			     url-http-real-basic-auth-storage))))
	     (api-auth-alist (assoc file server-double-alist)))
	(if api-auth-alist
	    (setcdr api-auth-alist (base64-encode-string auth))
	  (setcdr server-double-alist
		  (cons (cons file
			      (base64-encode-string auth))
			(cdr-safe server-double-alist))))))))

(defun identica-initialize-oauth ()
  "Get authentication token unless we have one stashed already.
Shamelessly stolen from yammer.el"
  (let ((filename (concat "~/." (sn-account-server sn-current-account) "-"
			  (sn-account-username sn-current-account) "-oauth-token")))
    (when (file-exists-p filename)
      (save-excursion
	(find-file filename)
	(let ((str (buffer-substring (point-min) (point-max))))
	  (if (string-match "\\([^:]*\\):\\(.*\\)"
			    (buffer-substring (point-min) (point-max)))
	      (setf (sn-oauth-access-token (sn-account-oauth-data sn-current-account))
		    (make-oauth-access-token
		     :consumer-key (sn-oauth-consumer-key (sn-account-oauth-data sn-current-account))
		     :consumer-secret (sn-oauth-consumer-secret (sn-account-oauth-data sn-current-account))
		     :auth-t (make-oauth-t
			      :token (match-string 1 str)
			      :token-secret (match-string 2 str))))))
	(save-buffer)
	(kill-this-buffer)))
    (unless (sn-oauth-access-token (sn-account-oauth-data sn-current-account))
      (setf (sn-oauth-access-token (sn-account-oauth-data sn-current-account))
	    (oauth-authorize-app (sn-oauth-consumer-key (sn-account-oauth-data sn-current-account))
				 (sn-oauth-consumer-secret (sn-account-oauth-data sn-current-account))
				 (sn-oauth-request-url (sn-account-oauth-data sn-current-account))
				 (sn-oauth-access-url (sn-account-oauth-data sn-current-account))
				 (sn-oauth-authorize-url (sn-account-oauth-data sn-current-account))))
      (save-excursion
	(find-file filename)
	(end-of-buffer)
	(let ((token (oauth-access-token-auth-t (sn-oauth-access-token (sn-account-oauth-data sn-current-account)))))
	  (insert (format "%s:%s\n"
			  (oauth-t-token token)
			  (oauth-t-token-secret token))))
	(save-buffer)
	(kill-this-buffer))))
  (sn-oauth-access-token (sn-account-oauth-data sn-current-account)))

(defun identica-percent-encode (str &optional coding-system)
  "Make a symbol translation for URLs parameters. 

For example: 
% is translated into %25, space character is translated into +, etc.

\"(%s hola mundo áéíóú)\" will be translated into:
\"%28%25s+hola+mundo+%c3%a1%c3%a9%c3%ad%c3%b3%c3%ba%29\"

Observe that translation depends on authentication mode(variable `sn-current-account').
"
  (if (equal (sn-account-auth-mode sn-current-account) "oauth")
      (oauth-hexify-string str)
    (when (or (null coding-system)
	      (not (coding-system-p coding-system)))
      (setq coding-system 'utf-8))
    (mapconcat
     (lambda (c)
       (cond
	((identica-url-reserved-p c)
	 (char-to-string c))
	((eq c ? ) "+")
	(t (format "%%%x" c))))
     (encode-coding-string str coding-system)
     "")))

(defun identica-make-url (server method-class method &optional parameters)
  "Make a string containing the URL to connect to the indicater SERVER at that METHOD-CLASS and METHOD.
You can give PARAMETERS for HTTP GET optionaly in a list of list format. For example:
  '((\"id\" \"19923\")(\"name\" \"juan bonachón\"))

This will return http://server/api/methods.xml?id=19923&name=juan%20bonachón.

A simbol translation is done for the PARAMETERS using `identica-percent-encode'."
  
  (format "http://%s/api/%s%s.xml"
	  server
	  (when (not (string-equal method-class "none"))
	    (concat method-class "/" ))
	  method
	  (when parameters
	    (concat "?"
		    (mapconcat
		     (lambda (param-pair)
		       (format "%s=%s"
			       (identica-percent-encode (car param-pair))
			       (identica-percent-encode (cdr param-pair)))) ;; (1) Becareful here! cdr returns a parentezised string like "(hi)"!
		     parameters
		     "&")))))

					; ____________________
					; Error Checking 

(defvar identica-http-error-data nil
  "If there is an error in HTTP, this variable is non-nil and it has the information
and explanation of the error.")

(defun identica-http-check-errors (status)
  "Check for HTTP errors and return a string explaining what happened.
If no error found, return nil.

The variable `identica-http-error-data' is stored if errors found, if not is setted to nil."
  (let ((error-object (assoc-workaround :error status)))
    (if error-object
	(let ((error-data (format "%s" (caddr error-object)))) ;; We have an error!
	  (cond
	   ((string= error-data "deleted\n")  
	    (setq identica-http-error-data "no info... \"deleted\"?"))
	   ((and (string= error-data "404") method 
		 (= 13 (string-match "/" method)))
	    (setq identica-http-error-data ("No Such User: %s" (substring method 14))))
	   (t
	    (setq identica-http-error-data (format "Identica-Mode: Network error:%s" status)))))
      (setq identica-http-error-data nil))
    identica-http-error-data));; No error found.
      

					; ____________________
					; Getting data

(defvar identica-http-get-sentinel nil
  "This is used for `identica-http-get-nothing-sentinel'. If this variable has a function, it will be called after
the sentinel finish checking all the HTTP errors and no errors found.

The function here, will need to recieve the following arguments:
method-class method parameters arg1 arg2 ...

Where (arg1, arg2, ...) is the list with arguments passed to the `identica-http-get' function in SENTINEL-ARGUMENTS parameter.

Method-class, method and parameters is the information used for connecting to the server.")

(defun identica-http-get-nothing-sentinel (status &rest parameters)
  "This sentinel checks for HTTP errors and then calls the function stored at `identica-http-post-sentinel'
if no erros found."
  (if (identica-http-check-errors status)      
      (message identica-http-error-data)
    (when identica-http-get-sentinel
      (apply identica-http-get-sentinel parameters)
      )
    )
  )

(defun identica-http-get
  (method-class method &optional parameters sentinel sentinel-arguments)
  "Basic function which communicates with server.

METHOD-CLASS and METHOD are parameters for getting dents messages and
other information from server as specified in api documentation.

PARAMETERS arguments specify the additional parameters required by
the above METHOD.  It is specified as an alist with parameter name and its corresponding value.

SENTINEL represents the callback function to be called after the http response is completely retrieved.
If SENTINEL is nil, the following sentinel is used:`identica-http-get-nothing-sentinel'.

SENTINEL-ARGUMENTS is the list of arguments (if any) of the SENTINEL
procedure.

The variable `sn-current-account' gives the following important information:
* Server to connect.
* Port to connect.
* Username and password if necessary
* If oauth is used for authentication(instead of username and pass.)
"
  (identica-http-init-variables)
  (or sentinel (setq sentinel 'identica-http-get-nothing-sentinel))
  (let* ((server (sn-account-server sn-current-account))
	 (auth-mode (sn-account-auth-mode sn-current-account))
	 (url (identica-make-url server method-class method parameters))
	 (url-show-status nil))
    (identica-set-proxy)
    (unless (equal auth-mode "none")
      (if (equal auth-mode "oauth")
          (or (sn-oauth-access-token (sn-account-oauth-data sn-current-account))
              (identica-initialize-oauth))
        (identica-set-auth url)))
    (when (get-buffer-process identica-http-buffer)
      (delete-process identica-http-buffer)
      (kill-buffer identica-http-buffer))
    (setq identica-http-buffer
          (identica-url-retrieve url sentinel method-class
                                 method parameters sentinel-arguments))
    ))

(defun identica-url-retrieve
  (url sentinel method-class method parameters sentinel-arguments &optional unhex-workaround)
  "Call url-retrieve or oauth-url-retrieve dsepending on the mode.
Apply url-unhex-string workaround if necessary.

The SENTINEL will recieve the following argument:
A list with the METHOD-CLASS METHOD PARAMETERS and SENTINEL-ARGUMENTS, everything appended.

For example: 
if you call:
  (`identica-url-retrieve' \"identi.ca\" my-sentinel \"statuses\" \"friends_timeline\" '(\"login\" \"t\") '(param1 param2))

The sentinel will need the following definition:
  (defun my-sentinel (status method-class method parameters arg1 arg2) ... )


SENTINEL-ARGUMENTS is a list of elements, but the sentinel will recieve a list like described above with no sublist as elements(everything will be apened forming one list).
"
  (if (and (equal (sn-account-auth-mode sn-current-account) "oauth")
	   (sn-oauth-access-token (sn-account-oauth-data sn-current-account)))
      (if unhex-workaround
	  (flet ((oauth-extract-url-params
		  (req)
		  "Modified oauth-extract-url-params using w3m-url-decode-string to work around
bug in url-unhex-string present in emacsen previous to 23.3."
		  (let ((url (oauth-request-url req)))
		    (when (string-match (regexp-quote "?") url)
		      (mapcar (lambda (pair)
				`(,(car pair) . ,(w3m-url-decode-string (cadr pair))))
			      (url-parse-query-string (substring url (match-end 0))))))))
	    (identica-url-retrieve url sentinel method-class method parameters sentinel-arguments))
	(oauth-url-retrieve (sn-oauth-access-token (sn-account-oauth-data sn-current-account)) url sentinel
			    (append (list method-class method parameters)
				    sentinel-arguments)))
    (url-retrieve url sentinel
		  (append (list method-class method parameters)
			  sentinel-arguments))))

					; ____________________
					; Posting data

(defvar identica-http-post-sentinel nil
    "This is used for `identica-http-post-nothing-sentinel'. If this variable has a function, it will be called after
the sentinel finish checking all the HTTP errors and no errors found.")

(defun identica-http-post-nothing-sentinel (status &rest parameters)
  "This sentinel checks for HTTP errors and then calls the function stored at `identica-http-post-sentinel'
if no erros found."
  (if (identica-http-check-errors status)
      (message identica-http-error-data)
    (when identica-http-post-sentinel
      (apply identica-http-post-sentinel parameters)
      )
    )
  ;; This is not necessary because it has to be in the function stored in `identica-http-post-sentinel'.
  ;; (identica-display-success-messages
  ;;  (message (or success-message "Success: Post")))
  )
(defun identica-http-post
  (method-class method &optional parameters sentinel sentinel-arguments)
  "Send HTTP POST request to statusnet server.
METHOD-CLASS must be one of Identica API method classes(statuses, users or direct_messages).
METHOD must be one of Identica API method which belongs to METHOD-CLASS.
PARAMETERS is alist of URI parameters. ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6"
  (identica-http-init-variables)
  (or sentinel (setq sentinel 'identica-http-post-default-sentinel))
  (let ((url-request-method "POST")
	(url (concat "http://"(sn-account-server sn-current-account) "/api/" method-class "/" method ".xml"
		     (when parameters
		       (concat "?"
			       (mapconcat
				(lambda (param-pair)
				  (format "%s=%s"
					  (identica-percent-encode (car param-pair))
					  (identica-percent-encode (cdr param-pair))))
				parameters
				"&")))))
	(url-package-name "emacs-identicamode")
	(url-package-version identica-mode-version)
	;; (if (assoc `media parameters)
	;; (url-request-extra-headers '(("Content-Type" . "multipart/form-data")))
	(url-request-extra-headers '(("Content-Length" . "0")))
	(url-show-status nil))
    (identica-set-proxy)
    (if (equal (sn-account-auth-mode sn-current-account) "oauth")
	(or (sn-oauth-access-token (sn-account-oauth-data sn-current-account))
	    (identica-initialize-oauth))
      (identica-set-auth url))
    (when (get-buffer-process identica-http-buffer)
      (delete-process identica-http-buffer)
      (kill-buffer identica-http-buffer))
    (identica-url-retrieve url sentinel method-class method parameters
			   sentinel-arguments identica-unhex-broken)))


					; ____________________
					; Initialization
(defun identica-autoload-oauth ()
  "Autoloads oauth.el when needed."
  (autoload 'oauth-authorize-app "oauth")
  (autoload 'oauth-hexify-string "oauth")
  (autoload 'make-oauth-access-token "oauth"))


(defun identica-http-init-variables()
  "Init any variable necessary to make identica-http works fine and happy. If `identica-http-initialized' is t then, do nothing!.

You have to use this functions first if you want to use any identica-http's function.

 (Why I don't call this function at loading time? That's a good question...) 

`identica-http-initialized' is setted to t when this function finish so we don't initialize twice!
"
  (unless identica-http-initialized
    ;; Create an account object based on the various custom variables.
    ;; Insert it into the statusnet accounts list.
    (setq statusnet-accounts
	  (cons (make-statusnet-account
		 :server statusnet-server
		 :port statusnet-port
		 :username identica-username
		 :auth-mode identica-auth-mode
		 :password identica-password
		 :textlimit statusnet-server-textlimit
		 :oauth-data (if (string= identica-auth-mode "oauth")
				 (make-statusnet-oauth-data
				  :consumer-key identica-mode-oauth-consumer-key
				  :consumer-secret identica-mode-oauth-consumer-secret
				  :request-url statusnet-request-url
				  :access-url statusnet-access-url
				  :authorize-url statusnet-authorize-url
				  :access-token nil)
			       nil)
		 :last-timeline-retrieved nil)
		statusnet-accounts))
    (setq sn-current-account (car statusnet-accounts))
    
    (identica-autoload-oauth)
    (setq identica-http-initialized t)
    )
  )

;;
;; If you want to call this when loading identica-http, uncomment the following line:
;; (identica-http-init-variables)

(provide 'identica-http)