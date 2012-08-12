
    ;; identica-interface.el
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

    ;; Lunes 30 De Julio Del 2012    


;; PURPOSE:
;; ____________________
;;
;; Change interface and make dents visible.
;;
;; ____________________
;;

(require 'identica-translator)
(require 'identica-common-things)
(require 'identica-icon-mode)

(defcustom identica-blacklist '()
  "List of regexes used to filter statuses, evaluated after status formatting is applied."
  :type 'string
  :group 'identica-mode)

(defcustom identica-status-format "%i %s,  %@:\n  %t // from %f%L%r\n\n"
  "The format used to display the status updates.
%s - screen_name
%S - name
%i - profile_image
%d - description
%l - location
%L - \" [location]\"
%r - in reply to status
%u - url
%j - user.id
%p - protected?
%c - created_at (raw UTC string)
%C{time-format-str} - created_at (formatted with time-format-str)
%@ - X seconds ago
%t - text
%' - truncated
%h - favorited
%f - source
%# - id
"
  :type 'string
  :group 'identica-mode)


(defvar identica-interface-username nil
  "This is the username of the current statusnet user. 

Usable for adding needed properties to status that are replies.")

(defun identica-interface-init-variables (username)
  "Initialize identica-interface module.

USERNAME is the user screen name of the actual session."
  (setq identica-interface-username username))

(defvar identica-buffer "*identica*")

(defun identica-buffer (&optional method)
  "Create a buffer for use by identica-mode.
Initialize the global method with the default, or with METHOD, if present."
  (unless method
    (setq method "friends_timeline"))
  (get-buffer-create identica-buffer))

;; This the original render-timeline function, I'll keep it for reference.
;;
;; TODO: Erase `identica-render-timeline-orig' function when the new ones does the same!
;;
(defun identica-render-timeline-orig ()
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
      (identica-set-mode-string nil identica-method (sn-account-server sn-current-account))
      (setf (sn-account-last-timeline-retrieved sn-current-account) identica-method)
      (if transient-mark-mode (deactivate-mark)))))

(defun identica-status-is-in-blacklist (formated-status)
  "Checks if the FORMATED-STATUS is in the blacklist by checking each regex in the `identica-blacklist'."
  (let ((blacklisted nil))
    (mapc (lambda (regex)
	    (when (string-match-p regex formatted-status)
	      (setq blacklisted 't)))
	  identica-blacklist)
    blacklisted))

(defun identica-mark-all-icons-for-download ()
  "Mark all profile icons for download.

Only will be marked for download those icons that aren't in the temporary directory."
  (dolist (status identica-timeline-data)
    (identica-icon-set-for-download (assoc-default 'user-profile-image-url status))))

(defun identica-download-all-icons ()
  "Find and download all profile image icons."
  (identica-mark-all-icons-for-download)
  (identica-icon-get-icons))

(defun identica-refresh-icon-at-point ()
  "Download the icon of the image at current point, if it was downloaded, download again.
Insert the recently downloaded icon at position replacing the old one.

If no image is at current point, do nothing."
  (let ((url (identica-get-text-value-profile-image))
	(inhibit-read-only t))
    (when url
      ;; Set everything for download the icon.
      (setq identica-icon-finish-sentinel 'identica-replace-image-at-point-2)
      (setq identica-icon-finish-sentinel-parameters (list url (point)))
      (identica-icon-set-for-download url t)
      (identica-icon-get-icons t))))         
  
(defun identica-replace-image-at-point-2 (url position)
  "Replace the image at POSITION. The image must be downloaded."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char position)
      (delete-char 1)
      (insert (identica-format-profile-image url)))))

(defun identica-render-timeline (&optional buffer clean-first)
  "Render all the timeline getting all the information from `identica-timeline-data'.

BUFFER is the buffer where insert all the information.
If BUFFER is nil or not present, use the `identica-buffer'.

A filter is applied as a blacklist of dents."
  (or buffer (setq buffer identica-buffer))
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (when clean-first 
	(erase-buffer))
      (dolist (status identica-timeline-data)
	(let ((formated-status (identica-format-status status identica-status-format)))
	  (unless (identica-status-is-in-blacklist formated-status)	  
	    (insert (identica-find-and-add-all-properties formated-status))))))))
  
(defun identica-format-status (status format-str)
  "Create a string with information from STATUS formatted acording to FORMAT-STR. 

*This function only generates the strings, doesn't insert it in any buffer!*"
  (let ((cursor 0)
	(result '())
	c
	found-at)

    ;; Follow each "%?" element in FORMAT-STR and elaborate a list called "result" in the order given by that format.
    (while (setq found-at (string-match "%\\(C{\\([^}]+\\)}\\|[A-Za-z#@']\\)" format-str cursor))
      (setq c (string-to-char (match-string-no-properties 1 format-str)))
      (if (> found-at cursor)
	  (push (substring format-str cursor found-at) result)
	"|")
      (setq cursor (match-end 1))
      
      (push (identica-format-token c status) result))
    
    
    (push (substring format-str cursor) result)
    
    ;; Mix everything from result into a string and return it.
    (let ((formatted-status (apply 'concat (nreverse result))))
      (flet ((attr (key)
		   (assoc-default key status)))

	(add-text-properties 0 (length formatted-status)
			     `(username, (attr 'user-screen-name)
					 id, (attr 'id)
					 text, (attr 'text)
					 profile-url, (attr 'user-profile-url)
					 in-reply-to-screen-name, (attr 'in-reply-to-screen-name)
				       conversation-id, (attr 'conversation-id))
			     formatted-status)
	formatted-status))))


(defun identica-format-token (token status)
  "Depending on the TOKEN given, return a string that represents the part of the status.

Take a look at the variable `identica-status-format'.

STATUS must be a status data, one element taken from the result of `identica-timeline-data'."
  (flet ((attr (key)
	       (assoc-default key status)))

    (case token
      ((?s)                         ; %s - screen_name
       (identica-format-user-name (attr 'user-screen-name)))
      ((?S)                         ; %S - name
       (identica-format-user-name (attr 'user-name)))
      ((?i)                         ; %i - profile_image
       (identica-format-profile-image (attr 'user-profile-image-url)) ;; TODO:
       )
      ((?d)                         ; %d - description
       (identica-format-user-description (attr 'user-description)))
      ((?l)                         ; %l - location
       (identica-format-location-2 (attr 'user-location)))
      ((?L)                         ; %L - " [location]"
       (identica-format-location-1 (attr 'user-location)))
      ((?u)                         ; %u - url
       (attr 'user-url))
      ((?U)                         ; %U - profile url
       (identica-format-user-profile-url (attr 'user-profile-url)))
      ((?j)                         ; %j - user.id
       (format "%d" (attr 'user-id)))
      ((?r)                         ; %r - in_reply_to_status_id
       (identica-format-in-reply (attr 'in-reply-to-status-id) (attr 'in-reply-to-screen-name)))
      ((?p)                         ; %p - protected?
       (identica-format-user-protected (attr 'user-protected)))
      ((?c)                     ; %c - created_at (raw UTC string)
       (attr 'created-at))
      ((?C) ; %C{time-format-str} - created_at (formatted with time-format-str)
       (identica-format-created-at (attr 'created-at)))
      ((?@)                         ; %@ - X seconds ago
       (identica-format-seconds-ago (attr 'created-at)))
      ((?t)                        ; %t - text
					;(clickable-text)
	(attr 'text))
      ((?')                         ; %' - truncated
       (identica-format-truncated (attr 'truncated)))
      ((?f)                         ; %f - source
       (identica-format-source (attr 'source)))
      ((?F)                         ; %F - ostatus-aware source
       (identica-ostatus-aware (attr 'source) (attr 'user-profile-url)))
      ((?#)                         ; %# - id
       (format "%d" (attr 'id)))
      ((?x)                         ; %x - conversation id (conteXt) - default 0
       (attr 'conversation-id))
      ((?h)
       (identica-format-favored (attr 'favorited)))
      (t
       (char-to-string token)))))

;;
;; All this formated source will not have face format, that will be the job for `identica-mode' major mode(see identica-major-mode.el file).
;; Here will be only properties that doesn't affect the face and will tell usefull information(like what kind of thing is the text).
;; 
;; Take as an example the created-at date: it has the property "'created-at t". This is usefull to know where each dent part starts and ends.

(defun identica-format-profile-image (url)
  (propertize (identica-icon-get-image-string url)
	      'profile-image t
	      'profile-image-url url))

(defun identica-format-user-name (name)
  "How the user-name will be formated?"
  (propertize name 'user-name t)
  )

(defun identica-format-source (source)
  "How source will be formated?"
  (setq source (identica-make-source-pretty source))
  (add-text-properties 0 (length source)
		       '(source t)
		       source)
  source)

(defun identica-format-location-1 (location)
  "How location will formated?"
  (unless (or (null location) (string= "" location))
    (propertize (concat " [" location "]")
		'location t)))

(defun identica-format-location-2 (location)
  "How location will formated?"
  (propertize location 'location t))

(defun identica-format-user-profile-url (url)
  "How user-profile-url will formated?"
  (identica-icon-set-for-download url)
  (propertize (cadr (split-string url "https*://")) 
	      '('user-profile-url t)))

(defun identica-format-in-reply (reply-id reply-name)
  "How \"in-reply...\" will formated?"
  (unless (or (null reply-id) (string= "" reply-id)
	      (null reply-name) (string= "" reply-name))
    (let ((in-reply-to-string (format "in reply to %s" reply-name))
	  ;; (url (identica-get-status-url reply-id)) ;; TODO:
	  )
      (add-text-properties 0 (length in-reply-to-string)
			   `(mouse-face highlight in-reply t)
			   in-reply-to-string)
      (concat " " in-reply-to-string))))

(defun identica-format-user-protected (protected)
  "How user protected sign will formated?"
  (when (string= "true" protected)
    (propertize "[x]" 'user-protected t)))

(defun identica-format-created-at (created-at)
  "How created at date-time will formated?"
  (propertize (identica-local-strftime
	       (or (match-string-no-properties 2 format-str) "%H:%M:%S")
	       created-at)
	      'created-at t))

(defun identica-format-seconds-ago (status-created-at)
  "How \"some seconds ago\" message will formated?"
  (let ((created-at (apply 'encode-time (parse-time-string status-created-at)))
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
      ;; (setq url (identica-get-status-url (attr 'id))) ;; TODO:
      ;; make status url clickable
      (add-text-properties  0 (length time-string)
			    `(mouse-face highlight seconds-ago t)
			    time-string)
      time-string)))


(defun identica-format-truncated (truncated)
  "How truncated sign will formated?"
  (when (string= "true" truncated)
    (propertize "..." 'truncated t)))

(defun identica-ostatus-aware (source user-profile-url)
  "How ostatus aware will formated?"
  (if (string= source "ostatus")
      (propertize (cadr (split-string user-profile-url "https*://"))
		  'ostatus-aware t)
    (propertize source
		'ostatus aware t)))

(defun identica-format-favored (likes)			 
  "How favored sign will formated?"
  (when (string= "true" likes)
    (propertize "❤" 
		'favored t)))

;;--------------------

(defun identica-find-and-add-screen-name-properties (text)
  "Finds all texts with URI format and add link properties to them."  
  (setq regex-index 0)
  (while regex-index
    (setq regex-index
	  (string-match identica-screen-name-regexp
			text
			regex-index))
    (when regex-index
      (let* ((matched-string (match-string-no-properties 0 text))
	     (screen-name (match-string-no-properties 1 text)))
	(add-text-properties (+ 1 (match-beginning 0))
			     (match-end 0)
			     `(mouse-face highlight screen-name ,screen-name)
			     text))
      (setq regex-index (match-end 0))))
  text)


(defun identica-find-and-add-group-name-properties (text)
  "Finds all texts with URI format and add link properties to them."  
  (setq regex-index 0)
  (while regex-index
    (setq regex-index
	  (string-match identica-group-name-regexp
			text
			regex-index))
    (when regex-index
      (let* ((matched-string (match-string-no-properties 0 text))
	     (group-name (match-string-no-properties 1 text)))
	(add-text-properties (+ 1 (match-beginning 0))
			     (match-end 0)
			     `(mouse-face highlight group ,group-name)
			     text))
      (setq regex-index (match-end 0))))
  text)

(defun identica-find-and-add-tag-name-properties (text)
  "Finds all texts with URI format and add link properties to them."  
  (setq regex-index 0)
  (while regex-index
    (setq regex-index
	  (string-match identica-tag-name-regexp
			text
			regex-index))
    (when regex-index
      (let* ((matched-string (match-string-no-properties 0 text))
	     (tag-name (match-string-no-properties 1 text)))
	(add-text-properties (+ 1 (match-beginning 0))
			     (match-end 0)
			     `(mouse-face highlight tag ,tag-name)
			     text))
      (setq regex-index (match-end 0))))
  text)

(defun identica-find-and-add-http-url-properties (text)
  "Finds all texts with URI format and add link properties to them."  
  (setq regex-index 0)
  (while regex-index
    (setq regex-index
	  (string-match identica-url-regexp
			text
			regex-index))
    (when regex-index
      (let* ((uri (match-string-no-properties 0 text)))	     
	(add-text-properties (+ 1 (match-beginning 0))
			     (match-end 0)
			     `(mouse-face highlight uri ,uri uri-in-text ,uri)
			     text))
      (setq regex-index (match-end 0))))
  text)

(defun identica-find-and-add-reply-properties (text)
  "If text is a status reply to the current user, then add the property 'is-a-reply

To know who is the current user, the variable `identica-interface-username' must be setted, if not this function will do nothing."
  (when identica-interface-username
    (if (string= (get-text-property 1 'in-reply-to-screen-name text)
		 identica-interface-username) ;; the username of the status is the same as the current one.
	(add-text-properties 1 (length text) '(is-a-reply t) text)
      text))
  text)
    

(defun identica-find-and-add-all-properties (text)
  "Finds all important text(like screen-names and tag-names), then add format and link properties to them."  
  (setq text (identica-find-and-add-screen-name-properties text))
  (setq text (identica-find-and-add-group-name-properties text))
  (setq text (identica-find-and-add-tag-name-properties text))
  (setq text (identica-find-and-add-http-url-properties text))
  (setq text (identica-find-and-add-reply-properties text))
  text)

(defun identica-make-source-pretty (source)
  "Remove hyperlinks tags and make the caption clickable."
  (when (string-match "<a href=\"\\(.*\\)\">\\(.*\\)</a>" source)
    (let ((uri (match-string-no-properties 1 source))
	  (caption (match-string-no-properties 2 source)))
      (setq source caption)
      (add-text-properties 0 (length source)
			   `(mouse-face highlight source ,source)
			   source)
      source)))

;; Functions for getting text with some properties

(defun identica-get-text-value (property &optional position)
  "Return the text value of the given PROPERTY.

If POSITION is given, search if that property is in that place."
  (unless position
    (setq position (point)))
  (get-text-property position property))

(defun identica-get-text-value-id ()
  "Return the text value of the property source at current `point'."
  (identica-get-text-value 'id))

(defun identica-get-text-value-username ()
  "Return the text value of the property username at current `point'."
  (identica-get-text-value 'username))

(defun identica-get-text-value-text ()
  "Return the text value of the property text at current `point'."
  (identica-get-text-value 'text))

(defun identica-get-text-value-profile-url ()
  "Return the text value of the property profile-url at current `point'."
  (identica-get-text-value 'profile-url))

(defun identica-get-text-value-conversation-id ()
  "Return the text value of the property conversation-id at current `point'."
  (identica-get-text-value 'conversation-id))

(defun identica-get-text-value-uri ()
  "Return the text value of the property conversation-id at current `point'."
  (identica-get-text-value 'uri))

(defun identica-get-text-value-profile-image ()
  "Return the text value of the property conversation-id at current `point'."
  (identica-get-text-value 'profile-image-url))

(provide 'identica-interface)