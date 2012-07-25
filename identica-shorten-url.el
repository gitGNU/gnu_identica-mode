
    ;; identica-shorten-url.el
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


;; PURPOSE:
;; __________
;;
;; This library must give support to identica-mode for shorten url in various common services.
;; 
;; The supported url-shortening services are listed in `identica-urlshortening-services-map'.
;;
;; ____________________
;;

(defvar identica-urlshortening-services-map
  '((tinyurl . "http://tinyurl.com/api-create.php?url=")
    (toly    . "http://to.ly/api.php?longurl=")
    (google . "http://ggl-shortener.appspot.com/?url=")
    (ur1ca . "http://ur1.ca/?longurl=")
    (tighturl . "http://2tu.us/?save=y&url=")
    (isgd . "http://is.gd/create.php?format=simple&url="))
  "Alist of tinyfy services.")

(defcustom identica-urlshortening-service 'ur1ca
  "The service to use for URL shortening.
Values understood are ur1ca, tighturl, tinyurl, toly, google and isgd."
  :type 'symbol
  :group 'identica-mode)

(defun identica-tinyurl-unjson-google (result)
  "Gets only the URL from JSON URL tinyfying service results.

Google's shortening service, goo.gl, returns shortened URLs as a
JSON dictionary. This function retrieves only the URL value from
this dictionary, only if identica-urlshortening-service is 'google."
  (if (eq identica-urlshortening-service 'google)
      (cdr (assoc 'short_url (json-read-from-string result)))
    result))

(defun identica-ur1ca-get (api longurl)
  "Shortens url through ur1.ca free service 'as in freedom'."
  (let* ((url-request-method "POST")
	 (url-request-extra-headers
	  '(("Content-Type" . "application/x-www-form-urlencoded")))
	 (url-request-data (url-hexify-string longurl))
	 (buffer (url-retrieve-synchronously (concat api (url-hexify-string longurl)))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (prog1
	  (if (eq api "ur1ca")
	      (if (search-forward-regexp "Your .* is: .*>\\(http://ur1.ca/[0-9A-Za-z].*\\)</a>" nil t)
		  (match-string-no-properties 1)
		(error "URL shortening service failed: %s" longurl))
	    (if (search-forward-regexp "\\(http://[0-9A-Za-z/].*\\)" nil t)
		(match-string-no-properties 1)
	      (error "URL shortening service failed: %s" longurl)))
	(kill-buffer buffer)) )))

(defun identica-shortenurl-get (longurl)
  "Shortens url through a url shortening service."
  (let ((api (cdr (assoc identica-urlshortening-service
			 identica-urlshortening-services-map))))
    (unless api
      (error "`identica-urlshortening-service' was invalid.  try one of %s"
	     (mapconcat (lambda (x)
			  (symbol-name (car x)))
			identica-urlshortening-services-map ", ")
	     "."))
    (if longurl
	(if (not (eq identica-urlshortening-service 'google))
	    (identica-ur1ca-get api longurl)
	  (let ((buffer (url-retrieve-synchronously (concat api longurl))))
	    (with-current-buffer buffer
	      (goto-char (point-min))
	      (prog1
		  (identica-tinyurl-unjson-google
		   (if (search-forward-regexp "\n\r?\n\\([^\n\r]*\\)" nil t)
		       (match-string-no-properties 1)
		     (error "URL shortening service failed: %s" longurl)))
		(kill-buffer buffer))))
	  nil))))

(defun identica-expand-shorturl (url)
  "Return the redirected URL, or the original url if not found."
  (let ((temp-buf (get-buffer-create "*HTTP headers*")))
    (set-buffer temp-buf)
    (erase-buffer)
    (goto-char 0)
    (let*
        ((url (replace-regexp-in-string "http://" "" url))
         (host (substring url 0 (string-match "/" url)))
         (file (if (string-match "/" url)
                   (substring url (string-match "/" url))
                 "/"))
         (tcp-connection (open-network-stream "Identica URLExpand"
                                              temp-buf host 80))
         (request (concat "GET http://" url " HTTP/1.1\r\n"
			  "Host:" host "\r\n"
			  "User-Agent: " (identica-user-agent) "\r\n"
			  "Authorization: None\r\n"
			  "Accept-Charset: utf-8;q=0.7,*;q=0.7\r\n\r\n")))
      (set-marker (process-mark tcp-connection) (point-min))
      (set-process-sentinel tcp-connection 'identica-http-headers-sentinel)
      (process-send-string tcp-connection request)
      (sit-for 2)
      (let ((location (identica-get-location-from-header (concat "http://" host file) tcp-connection)))
        (delete-process tcp-connection)
        (kill-buffer temp-buf)
        location))))

(defun identica-http-headers-sentinel (process string)
  "Process the results from the efine network connection."
  )

(defun identica-get-location-from-header (url process)
  "Parse HTTP header."
  (let ((buffer)
	(headers)
        (location))
    (setq buffer (get-buffer-create "*HTTP headers*"))
    (set-buffer buffer)
    (goto-char 0)
    (setq location
	  (if (search-forward-regexp "^Location: \\(http://.*?\\)\r?$" nil t)
	      (match-string-no-properties 1)
	    url))
    (replace-regexp-in-string "\r" "" location)))

