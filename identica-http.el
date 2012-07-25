
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

(defvar identica-http-buffer nil
  "Pointer to the current http response buffer.")

(defcustom identica-auth-mode "password"
  "Authorization mode used, options are password and oauth."
  :type 'string
  :group 'identica-mode)

(defcustom identica-http-get-timeout 10
  "Controls how long to wait for a response from the server."
  :type 'integer
  :group 'identica-mode)



;;; Proxy
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

(defun identica-http-get
  (server auth-mode method-class method &optional parameters sentinel sentinel-arguments)
  "Basic function which communicates with server.
METHOD-CLASS and METHOD are parameters for getting dents messages and
other information from SERVER as specified in api documentation.
Third optional arguments specify the additional parameters required by
the above METHOD.  It is specified as an alist with parameter name and
its corresponding value SENTINEL represents the callback function to
be called after the http response is completely retrieved.
SENTINEL-ARGUMENTS is the list of arguments (if any) of the SENTINEL
procedure."
  (or sentinel (setq sentinel 'identica-http-get-default-sentinel))
  (let ((url (concat "http://" server "/api/"
		     (when (not (string-equal method-class "none"))
		       (concat method-class "/" ))
		     method ".xml"
		     (when parameters
		       (concat "?"
			       (mapconcat
				(lambda (param-pair)
				  (format "%s=%s"
					  (identica-percent-encode (car param-pair))
					  (identica-percent-encode (cdr param-pair))))
				parameters
				"&")))))
	(url-package-name "emacs-identica-mode")
	(url-package-version identica-mode-version)
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
                                 method parameters sentinel-arguments auth-mode))
    (set-buffer identica-buffer)
    (set-buffer identica-buffer)
    (identica-set-mode-string t)))

(defun identica-url-retrieve
  (url sentinel method-class method parameters sentinel-arguments &optional auth-mode unhex-workaround)
  "Call url-retrieve or oauth-url-retrieve dsepending on the mode.
Apply url-unhex-string workaround if necessary."
  (if (and (equal auth-mode "oauth")
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
	    (identica-url-retrieve url sentinel method-class method parameters sentinel-arguments auth-mode))
	(oauth-url-retrieve (sn-oauth-access-token (sn-account-oauth-data sn-current-account)) url sentinel
			    (append (list method-class method parameters)
				    sentinel-arguments)))
    (url-retrieve url sentinel
		  (append (list method-class method parameters)
			  sentinel-arguments))))

(defun identica-http-post
  (method-class method &optional parameters sentinel sentinel-arguments)
  "Send HTTP POST request to statusnet server.
METHOD-CLASS must be one of Identica API method classes(statuses, users or direct_messages).
METHOD must be one of Identica API method which belongs to METHOD-CLASS.
PARAMETERS is alist of URI parameters. ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6"
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
			   sentinel-arguments (sn-account-auth-mode sn-current-account) identica-unhex-broken)))

(defun identica-get-status-url (id)
  "Generate status URL."
  (format "https://%s/notice/%s" (sn-account-server sn-current-account) id))

(defun identica-get-context-url (id)
  "Generate status URL."
  (format "https://%s/conversation/%s" (sn-account-server sn-current-account) id))

(defun identica-get-config-url ()
  "Generate configuration URL."
  (format "http://%s/api/statusnet/config.xml" (sn-account-server sn-current-account)))


