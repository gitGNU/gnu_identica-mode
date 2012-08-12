
    ;; identica-translator.el
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

    ;; Miércoles 25 De Julio Del 2012    


;; PURPOSE:
;; __________
;;
;; This library intends to transform the XML+HTTP retrieved into information structures usable for the interface. 
;;
;; ____________________

(require 'identica-common-things)

(defvar identica-timeline-data nil
  "This variable will store the timeline information. We can say that is where the cache is.")

(defvar identica-new-dents-count 0
  "Number of new tweets when `identica-new-dents-hook' is run.")

(defvar identica-new-dents-hook nil
  "Hook run when new twits are received.

You can read `identica-new-dents-count' to get the number of new
tweets received when this hook is run.")

(defvar identica-display-max-dents nil
  "How many dents to keep on the displayed timeline.

If non-nil, dents over this amount will bre removed.")



;; HTTP sentinel should be part of identica-http, 
;; the problem here is that there is high coupling(things from idenica-http and lots of things from here...)
(defun identica-http-get-default-sentinel-original
  (&optional status method-class method parameters success-message)
  (debug-print (window-buffer))
  (let ((error-object (assoc-workaround :error status))
	(active-p (eq (window-buffer) (identica-buffer))))
    (cond (error-object
	   (let ((error-data (format "%s" (caddr error-object))))
	     (when (cond
		    ((string= error-data "deleted\n") t)
		    ((and (string= error-data "404") method
			  (= 13 (string-match "/" method)))
		     (message "No Such User: %s" (substring method 14))
		     t)
		    ((y-or-n-p
		      (format "Identica-Mode: Network error:%s Retry? "
			      status))
		     (identica-http-get (sn-account-server sn-current-account)
                                        (sn-account-auth-mode sn-current-account)
                                        method-class method parameters)
		     nil))
	       ;; when the network process is deleted by another query
	       ;; or the user queried is not found , query is _finished_
	       ;; unsuccessful and we want to restore identica-method
	       ;; to loose track of this unsuccessful attempt
	       (setq identica-method (sn-account-last-timeline-retrieved sn-current-account)))))
	  ((< (- (point-max) (or (re-search-forward ">\r?\n\r*$" nil t) 0)) 2)
	   ;;Checking the whether the message is complete by
	   ;;searching for > that closes the last tag, followed by
	   ;;CRLF at (point-max)
	   (let ((body (identica-get-response-body)))
	     (if (not body)
		 (identica-set-mode-string nil identica-method (sn-account-server sn-current-account))
	       (setq identica-new-dents-count
		     (+ identica-new-dents-count
			(count t (mapcar
				  #'identica-cache-status-datum
				  (reverse (identica-xmltree-to-status
					    body))))))
					; Shorten the timeline if necessary
	       (identica-shorten-timeline)
	       (if active-p
		   (identica-render-pending-dents)
		 (identica-set-mode-string "pending" identica-method (sn-account-server sn-current-account)))))))))


(defun identica-process-http-buffer (&optional output-variable)
  "Process de HTTP contents of the `current-buffer' and add everything to the cache.

The cache will *not* be cleared!

If OUTPUT-VARIABLE is absent or nil, then use `identica-timeline-data'."
  (let ((body (identica-get-response-body)))
    (unless (not body)
      (setq identica-new-dents-count
	    (+ identica-new-dents-count
	       (count t (mapcar
			 #'identica-cache-status-datum
			 (reverse (identica-xmltree-to-status
				   body))))))
      (identica-shorten-timeline))))

(defun identica-shorten-timeline (&optional timeline-data)
  "Shorten the timeline if necessary. 

If timeline has to many dents, shorten it. 
TIMELINE-DATA is used, it has the same format as `identica-cache-status-datum' return.
If not present or nil, use `identica-timeline-data'."
  (setq timeline-data (or timeline-data identica-timeline-data))
  (if (and identica-display-max-dents
	   (> (safe-length timeline-data)
	      identica-display-max-dents))
      (cl-set-nthcdr identica-display-max-dents
		     timeline-data nil))
  )

(defun identica-get-response-header (&optional buffer)
  "Exract HTTP response header from HTTP response.
BUFFER may be a buffer or the name of an existing buffer.
 If BUFFER is omitted, 'current-buffer' is parsed."
  (or buffer
      (setq buffer (current-buffer)))
  (set-buffer buffer)
  (let ((end (or (and (search-forward-regexp "\r?\n\r?\n" (point-max) t)
		      (match-beginning 0))
		 0)))
    (and (> end 1)
         (buffer-substring (point-min) end))))

(defun identica-get-response-body (&optional buffer)
  "Exract HTTP response body from HTTP response, parse it as XML, and return a XML tree as list.
`buffer' may be a buffer or the name of an existing buffer.
 If `buffer' is omitted, current-buffer is parsed."
  (or buffer
      (setq buffer (current-buffer)))
  (set-buffer buffer)
  (set-buffer-multibyte t)
  (let ((start (save-excursion
		 (goto-char (point-min))
		 (and (re-search-forward "<\\?xml" (point-max) t)
		      (match-beginning 0)))))
    (identica-clean-response-body)
    (and start
         (xml-parse-region start (point-max)))))

(defun identica-clean-weird-chars (&optional buffer)
  (with-current-buffer identica-http-buffer
    (goto-char (point-min))
    (while (re-search-forward "\

?
[0-9a-z]*\

?
?" nil t)
(replace-match ""))
(buffer-string)))

(defun identica-clean-response-body ()
  "Remove weird strings (e.g., 1afc, a or 0) from the response body.
Known Statusnet issue.  Mostly harmless except if in tags."
  (goto-char (point-min))
  (while (re-search-forward "\r?\n[0-9a-z]+\r?\n" nil t)
    (replace-match "")))

(defun identica-status-to-status-datum (status)  
  "Transform a status(dent) in xml parsed tree into a data structure understandable by identica-mode.
This structure is an alist that has the following keys:

id text source created-at truncated favorited
in-reply-to-status-id
in-reply-to-screen-name
conversation-id
user-id user-name user-screen-name user-location
user-description
user-profile-image-url
user-profile-url
user-url
user-protected 

Observe that this was copied by the last part of this function."

  (flet ((assq-get (item seq)
		   (car (cddr (assq item seq)))))
    (let* ((status-data (cddr status))
	   id text source created-at truncated favorited
	   in-reply-to-status-id
	   in-reply-to-screen-name
	   (user-data (cddr (assq 'user status-data)))
	   user-id user-name
	   conversation-id
	   user-screen-name
	   user-location
	   user-description
	   user-profile-image-url
	   user-profile-url
	   user-url
	   user-protected
	   regex-index)

      (setq id (string-to-number (assq-get 'id status-data)))
      (setq text (identica-decode-html-entities
		  (assq-get 'text status-data)))      
      (setq source (identica-decode-html-entities
		    (assq-get 'source status-data)))
      (setq created-at (assq-get 'created_at status-data))
      (setq truncated (assq-get 'truncated status-data))
      (setq favorited (assq-get 'favorited status-data))
      (setq in-reply-to-status-id
	    (identica-decode-html-entities
	     (assq-get 'in_reply_to_status_id status-data)))
      (setq in-reply-to-screen-name
	    (identica-decode-html-entities
	     (assq-get 'in_reply_to_screen_name status-data)))
      (setq conversation-id (or (assq-get 'statusnet:conversation_id status-data) "0"))
      (setq user-id (string-to-number (assq-get 'id user-data)))
      (setq user-name (identica-decode-html-entities
		       (assq-get 'name user-data)))
      (setq user-screen-name (identica-decode-html-entities
			      (assq-get 'screen_name user-data)))
      (setq user-location (identica-decode-html-entities
			   (assq-get 'location user-data)))
      (setq user-description (identica-decode-html-entities
			      (assq-get 'description user-data)))
      (setq user-profile-image-url (assq-get 'profile_image_url user-data))
      (setq user-url (assq-get 'url user-data))
      (setq user-protected (assq-get 'protected user-data))
      (setq user-profile-url (assq-get 'statusnet:profile_url user-data))

      ;; make username clickable
      ;; (identica-add-username-properties user-name)

      ;; ;; make screen-name clickable
      ;; (identica-add-screen-name-properties user-screen-name)
      
      ;; ;; make URI clickable
      ;; (setq text (identica-find-and-add-all-properties text))

      ;; ;; make source pretty and clickable
      ;; (setq source (identica-make-source-pretty source))

      ;; ;; save last update time
      ;; (setq identica-timeline-last-update created-at)

      (mapcar
       (lambda (sym)
	 `(,sym . ,(symbol-value sym)))
       '(id text source created-at truncated favorited
	    in-reply-to-status-id
	    in-reply-to-screen-name
	    conversation-id
	    user-id user-name user-screen-name user-location
	    user-description
	    user-profile-image-url
	    user-profile-url
	    user-url
	    user-protected)))))

(defun identica-xmltree-to-status (xmltree)
  "Change XMLtree parsed by `xml-parse-region' and return in status format.

A status format is an alist with a symbol-name and data."
  (mapcar #'identica-status-to-status-datum
	  ;; quirk to treat difference between xml.el in Emacs21 and Emacs22
	  ;; On Emacs22, there may be blank strings
	  (let ((ret nil) (statuses (reverse (cddr (car xmltree)))))
	    (while statuses
	      (when (consp (car statuses))
		(setq ret (cons (car statuses) ret)))
	      (setq statuses (cdr statuses)))
	    ret)))



(defun identica-decode-html-entities (encoded-str)
  (if encoded-str
      (let ((cursor 0)
	    (found-at nil)
	    (result '()))
	(while (setq found-at
		     (string-match "&\\(#\\([0-9]+\\)\\|\\([A-Za-z]+\\)\\);"
				   encoded-str cursor))
	  (when (> found-at cursor)
	    (push (substring encoded-str cursor found-at) result))
	  (let ((number-entity (match-string-no-properties 2 encoded-str))
		(letter-entity (match-string-no-properties 3 encoded-str)))
	    (cond (number-entity
		   (push
		    (char-to-string
		     (identica-ucs-to-char
		      (string-to-number number-entity))) result))
		  (letter-entity
		   (cond ((string= "gt" letter-entity) (push ">" result))
			 ((string= "lt" letter-entity) (push "<" result))
			 (t (push "?" result))))
		  (t (push "?" result)))
	    (setq cursor (match-end 0))))
	(push (substring encoded-str cursor) result)
	(apply 'concat (nreverse result)))
    ""))

					; ____________________
					; Configuration
					;

(defun identica-retrieve-configuration ()
  "Retrieve the configuration for the current statusnet server."
  (identica-http-get (sn-account-server sn-current-account) (sn-account-auth-mode sn-current-account)
                     "statusnet" "config" nil 'identica-http-get-config-sentinel))

(defun identica-http-get-config-sentinel
  (&optional status method-class method parameters success-message)
  "Process configuration page retrieved from statusnet server."
  (let ((error-object (assoc-workaround :error status)))
    (unless error-object
      (let* ((body (identica-get-response-body))
	     (site (xml-get-children (car body) 'site))
	     (textlimit (xml-get-children (car site) 'textlimit))
	     (textlimit-value (caddar textlimit)))
	(when (> (string-to-number textlimit-value) 0)
	  (setf (sn-account-textlimit sn-current-account) (string-to-number textlimit-value))))))
  (identica-start))

(defun identica-cache-status-datum (status-datum &optional data-var)
  "Cache status datum into data-var(default `identica-timeline-data')
If STATUS-DATUM is already in DATA-VAR, return nil.  If not, return t."
  (when (null data-var)
    (setf data-var 'identica-timeline-data))
  (let ((id (cdr (assq 'id status-datum))))
    (if (or (null (symbol-value data-var))
	    (not (find-if
		  (lambda (item)
		    (eql id (cdr (assq 'id item))))
		  (symbol-value data-var))))
	(progn
	  (set data-var (sort (cons status-datum (symbol-value data-var))
			      'identica-compare-statuses))
	  t)
      nil)))

(defun identica-clear-cache ()
  "Clear the cache data (in other words, set `identica-timeline-data' to nil)."
  (setq identica-timeline-data nil))

(provide 'identica-translator)