
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

(defcustom identica-blacklist '()
  "List of regexes used to filter statuses, evaluated after status formatting is applied."
  :type 'string
  :group 'identica-mode)

(defcustom identica-status-format "%i %s,  %@:\n  %t // from %f%L%r\n\n"
  "The format used to display the status updates."
  :type 'string
  :group 'identica-mode)
;; %s - screen_name
;; %S - name
;; %i - profile_image
;; %d - description
;; %l - location
;; %L - " [location]"
;; %r - in reply to status
;; %u - url
;; %j - user.id
;; %p - protected?
;; %c - created_at (raw UTC string)
;; %C{time-format-str} - created_at (formatted with time-format-str)
;; %@ - X seconds ago
;; %t - text
;; %' - truncated
;; %h - favorited
;; %f - source
;; %# - id

(defun identica-http-get-default-sentinel
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
		 (identica-set-mode-string nil)
	       (setq identica-new-dents-count
		     (+ identica-new-dents-count
			(count t (mapcar
				  #'identica-cache-status-datum
				  (reverse (identica-xmltree-to-status
					    body))))))
					; Shorten the timeline if necessary
	       (if (and identica-display-max-dents
			(> (safe-length identica-timeline-data)
			   identica-display-max-dents))
		   (cl-set-nthcdr identica-display-max-dents
				  identica-timeline-data nil))
	       (if active-p
		   (identica-render-pending-dents)
		 (identica-set-mode-string "pending"))))))))

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
      (add-text-properties
       0 (length user-name)
       `(mouse-face highlight
		    uri ,user-profile-url
		    face identica-username-face)
       user-name)

      ;; make screen-name clickable
      (add-text-properties
       0 (length user-screen-name)
       `(mouse-face highlight
		    face identica-username-face
		    uri ,user-profile-url
		    face identica-username-face)
       user-screen-name)

      ;; make URI clickable
      (setq regex-index 0)
      (while regex-index
	(setq regex-index
	      (string-match "@\\([_[:word:]0-9]+\\)\\|!\\([_[:word:]0-9\-]+\\)\\|#\\([_[:word:]0-9\-]+\\)\\|\\(ur1\.ca/[a-z0-9]+/?\\|https?://[-_.!~*'()[:word:]0-9\;/?:@&=+$,%#]+\\)"
			    text
			    regex-index))
	(when regex-index
	  (let* ((matched-string (match-string-no-properties 0 text))
		 (screen-name (match-string-no-properties 1 text))
		 (group-name (match-string-no-properties 2 text))
		 (tag-name (match-string-no-properties 3 text))
		 (uri (match-string-no-properties 4 text)))
	    (add-text-properties
	     (if (or screen-name group-name tag-name)
		 (+ 1 (match-beginning 0))
	       (match-beginning 0))
	     (match-end 0)
	     (if (or screen-name group-name tag-name)
		 `(mouse-face
		   highlight
		   face identica-uri-face
		   uri ,(if screen-name
			    (concat "https://" (sn-account-server sn-current-account) "/" screen-name)
			  (if group-name
			      (concat "https://" (sn-account-server sn-current-account) "/group/" group-name)
			    (concat "https://" (sn-account-server sn-current-account) "/tag/" tag-name)))
		   uri-in-text ,(if screen-name
				    (concat "https://" (sn-account-server sn-current-account) "/" screen-name)
				  (if group-name
				      (concat "https://" (sn-account-server sn-current-account) "/group/" group-name)
				    (concat "https://" (sn-account-server sn-current-account) "/tag/" tag-name)))
                   tag ,tag-name
                   group ,group-name)
	       `(mouse-face highlight
			    face identica-uri-face
			    uri ,uri
			    uri-in-text ,uri))
	     text))
	  (setq regex-index (match-end 0)) ))


      ;; make source pretty and clickable
      (when (string-match "<a href=\"\\(.*\\)\">\\(.*\\)</a>" source)
	(let ((uri (match-string-no-properties 1 source))
	      (caption (match-string-no-properties 2 source)))
	  (setq source caption)
	  (add-text-properties
	   0 (length source)
	   `(mouse-face highlight
			face identica-uri-face
			source ,source)
	   source)))

      ;; save last update time
      (setq identica-timeline-last-update created-at)

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

(defun identica-render-timeline ()
  (with-current-buffer (identica-buffer)
    (let ((point (point))
	  (end (point-max))
          (wrapped (cond (longlines-mode 'longlines-mode)
                         (visual-line-mode 'visual-line-mode)
                         (t nil)))
	  (stripe-entry nil))

      (setq buffer-read-only nil)
      (erase-buffer)
      (when wrapped (funcall wrapped -1))
      (mapc (lambda (status)
              (let ((before-status (point-marker))
                    (blacklisted 'nil)
                    (formatted-status (identica-format-status
                                       status identica-status-format)))
                (mapc (lambda (regex)
                        (when (string-match-p regex formatted-status)
                          (setq blacklisted 't)))
                      identica-blacklist)
                (unless blacklisted
                  (when identica-enable-striping
                    (setq stripe-entry (not stripe-entry)))
                  (insert formatted-status)
                  (when (not wrapped)
                    (fill-region-as-paragraph
                     (save-excursion (beginning-of-line -1) (point)) (point)))
                  (insert-and-inherit "\n")
                  ;; Apply highlight overlays to status
		  (when (or (string-equal (sn-account-username sn-current-account)
					  (assoc-default 'in-reply-to-screen-name status))
                            (string-match
			     (concat "@" (sn-account-username sn-current-account)
				     "\\([^[:word:]_-]\\|$\\)") (assoc-default 'text status)))
		    (merge-text-attribute before-status (point) 'identica-reply-face :background))
                  (when (and identica-enable-highlighting
			     (memq (assoc-default 'id status) identica-highlighted-entries))
		    (merge-text-attribute before-status (point) 'identica-highlight-face :background))
                  (when stripe-entry
		    (merge-text-attribute before-status (point) 'identica-stripe-face :background))
                  (when identica-oldest-first (goto-char (point-min))))))
            identica-timeline-data)
      (when (and identica-image-stack window-system) (clear-image-cache))
      (when wrapped (funcall wrapped 1))
      (setq buffer-read-only t)
      (debug-print (current-buffer))
      (goto-char (+ point (if identica-scroll-mode (- (point-max) end) 0)))
      (identica-set-mode-string nil)
      (setf (sn-account-last-timeline-retrieved sn-current-account) identica-method)
      (if transient-mark-mode (deactivate-mark)))))

(defun identica-format-status (status format-str)
  (flet ((attr (key)
	       (assoc-default key status))
	 (profile-image
	  ()
	  (let ((profile-image-url (attr 'user-profile-image-url)))
	    (when (string-match "/\\([^/?]+\\)\\(?:\\?\\|$\\)" profile-image-url)
	      (let ((filename (match-string-no-properties 1 profile-image-url))
		    (xfilename (match-string-no-properties 0 profile-image-url)))
		;; download icons if does not exist
		(unless (file-exists-p (concat identica-tmp-dir filename))
		  (if (file-exists-p (concat identica-tmp-dir xfilename))
		      (setq filename xfilename)
		    (setq filename nil)
		    (add-to-list 'identica-image-stack profile-image-url)))
		(when (and identica-icon-mode filename)
		  (let ((avatar (create-image (concat identica-tmp-dir filename))))
		    ;; Make sure the avatar is 48 pixels (which it should already be!, but hey...)
		    ;; For offenders, the top left slice of 48 by 48 pixels is displayed
		    ;; TODO: perhaps make this configurable?
		    (insert-image avatar nil nil `(0 0 48 48)))
		  nil))))))
    (let ((cursor 0)
	  (result ())
	  c
	  found-at)
      (setq cursor 0)
      (setq result '())
      (while (setq found-at (string-match "%\\(C{\\([^}]+\\)}\\|[A-Za-z#@']\\)" format-str cursor))
	(setq c (string-to-char (match-string-no-properties 1 format-str)))
	(if (> found-at cursor)
	    (push (substring format-str cursor found-at) result)
	  "|")
	(setq cursor (match-end 1))

	(case c
	  ((?s)                         ; %s - screen_name
	   (push (attr 'user-screen-name) result))
	  ((?S)                         ; %S - name
	   (push (attr 'user-name) result))
	  ((?i)                         ; %i - profile_image
	   (push (profile-image) result))
	  ((?d)                         ; %d - description
	   (push (attr 'user-description) result))
	  ((?l)                         ; %l - location
	   (push (attr 'user-location) result))
	  ((?L)                         ; %L - " [location]"
	   (let ((location (attr 'user-location)))
	     (unless (or (null location) (string= "" location))
	       (push (concat " [" location "]") result)) ))
	  ((?u)                         ; %u - url
	   (push (attr 'user-url) result))
          ((?U)                         ; %U - profile url
           (push (cadr (split-string (attr 'user-profile-url) "https*://")) result))
	  ((?j)                         ; %j - user.id
	   (push (format "%d" (attr 'user-id)) result))
	  ((?r)                         ; %r - in_reply_to_status_id
	   (let ((reply-id (attr 'in-reply-to-status-id))
		 (reply-name (attr 'in-reply-to-screen-name)))
	     (unless (or (null reply-id) (string= "" reply-id)
			 (null reply-name) (string= "" reply-name))
	       (let ((in-reply-to-string (format "in reply to %s" reply-name))
		     (url (identica-get-status-url reply-id)))
		 (add-text-properties
		  0 (length in-reply-to-string)
		  `(mouse-face highlight
			       face identica-uri-face
			       uri ,url)
		  in-reply-to-string)
		 (push (concat " " in-reply-to-string) result)))))
	  ((?p)                         ; %p - protected?
	   (let ((protected (attr 'user-protected)))
	     (when (string= "true" protected)
	       (push "[x]" result))))
	  ((?c)                     ; %c - created_at (raw UTC string)
	   (push (attr 'created-at) result))
	  ((?C) ; %C{time-format-str} - created_at (formatted with time-format-str)
	   (push (identica-local-strftime
		       (or (match-string-no-properties 2 format-str) "%H:%M:%S")
		       (attr 'created-at))
		      result))
	  ((?@)                         ; %@ - X seconds ago
	   (let ((created-at
		  (apply
		   'encode-time
		   (parse-time-string (attr 'created-at))))
		 (now (current-time)))
	     (let ((secs (+ (* (- (car now) (car created-at)) 65536)
			    (- (cadr now) (cadr created-at))))
		   time-string url)
	       (setq time-string
		     (cond ((< secs 5) "less than 5 seconds ago")
			   ((< secs 10) "less than 10 seconds ago")
			   ((< secs 20) "less than 20 seconds ago")
			   ((< secs 30) "half a minute ago")
			   ((< secs 60) "less than a minute ago")
			   ((< secs 150) "1 minute ago")
			   ((< secs 2400) (format "%d minutes ago"
						  (/ (+ secs 30) 60)))
			   ((< secs 5400) "about 1 hour ago")
			   ((< secs 84600) (format "about %d hours ago"
						   (/ (+ secs 1800) 3600)))
			   (t (format-time-string "%I:%M %p %B %d, %Y" created-at))))
	       (setq url (identica-get-status-url (attr 'id)))
	       ;; make status url clickable
	       (add-text-properties
		0 (length time-string)
		`(mouse-face highlight
			     face identica-uri-face
			     uri ,url)
		time-string)
	       (push time-string result))))
	  ((?t)                         ; %t - text
	   (push                   ;(clickable-text)
	    (attr 'text)
	    result))
	  ((?')                         ; %' - truncated
	   (let ((truncated (attr 'truncated)))
	     (when (string= "true" truncated)
	       (push "..." result))))
	  ((?f)                         ; %f - source
	   (push (attr 'source) result))
          ((?F)                         ; %F - ostatus-aware source
           (push (if (string= (attr 'source) "ostatus")
                     (cadr (split-string (attr 'user-profile-url) "https*://"))
                   (attr 'source)) result))
	  ((?#)                         ; %# - id
	   (push (format "%d" (attr 'id)) result))
	  ((?x)                         ; %x - conversation id (conteXt) - default 0
	   (push (attr 'conversation-id) result))
	  ((?h)
	   (let ((likes (attr 'favorited)))
	     (when (string= "true" likes)
	       (push (propertize "❤" 'face 'identica-heart-face) result))))
	  (t
	   (push (char-to-string c) result))))
      (push (substring format-str cursor) result)
      (let ((formatted-status (apply 'concat (nreverse result))))
	(add-text-properties 0 (length formatted-status)
			     `(username, (attr 'user-screen-name)
                                         id, (attr 'id)
                                         text, (attr 'text)
                                         profile-url, (attr 'user-profile-url)
                                         conversation-id, (attr 'conversation-id))
			     formatted-status)
	formatted-status))))

