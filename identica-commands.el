
    ;; identica-commands.el
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
;; ____________________
;;
;; Alocate lots of functions that can use the user.
;;
;; This functions make something change internally in identica-mode. 
;; 
;; Don't guide from the `interactive' function! Use your wisdom as your guide!
;; Functions that moves cursors to some position, or change something in the interface without making 
;; anything internal depends directly only on the major mode, so they must be there.
;;
;; Examples of functions that can be here:
;; `identica-enable-oauth' Beacuse it changes lots of internal variables and internal behaviour!
;; `identica-icon-mode' Because it start the downloading process!
;; 
;; Examples of funcions that can't be here:
;; `identica-goto-previous-status' This just searchs and moves the cursors. Doesn't need to change anything. 
;;       And only uses the major mode library.
;; ____________________
;;

(require 'identica-common-things)
(require 'identica-translator)
(require 'identica-http)
(require 'identica-major-mode)
(require 'identica-interface)

(defcustom identica-default-timeline "friends_timeline"
  "Default timeline to retrieve."
  :type 'string
  :options '("friends_timeline" "public_timeline" "replies" "user_timeline")
  :group 'identica-mode)

(defun identica-default-method ()
  "Return the default method depending on the value of `identica-default-timeline'."
  identica-default-timeline) ;; There's nothing to process or calculate!

(defun identica-default-method-class ()
  "Return the default method-class depending on the value of `identica-default-timeline'."
  "statuses");;nothing to process!!!
  ;; (cond
  ;;  ((string= identica-default-timeline "friends_timeline")
  ;;   "statuses")
  ;;  ((string= identica-default-timeline "public_timeline")
  ;;   "statuses")
  ;;  ((string= identica-default-timeline "replies")
  ;;   "statuses")
  ;;  ((string= identica-default-timeline "user_timeline")
  ;;   "statuses")
  ;;  ))
   
(defvar identica-current-method nil
  "Current method. This variable is used for `identica-get-timeline' if no parameter METHOD is given.

If nil then use default method given by `identica-default-method'.")

(defvar identica-current-method-class nil
  "Current method class. This variable is used for `identica-get-timeline' if no parameter METHOD-CLASS is given.

If nil the use default method-class given by `identica-default-method-class'.")

(defvar identica-current-parameters nil
  "Current parameters. This variable is used for `identica-get-timeline' if no parameter PARAMETERS is given.")

(defun identica-enable-oauth ()  
  "Enables oauth for identica-mode."
  (interactive)
  (require 'oauth)
  ;Test if we're running on an emacs version with broken unhex and apply workaround.
  (unless (eq (url-unhex-string (url-hexify-string "²")) "²")
    (setq identica-unhex-broken t)
    (require 'w3m))
  (setq identica-auth-mode "oauth"))

(defun identica-mode-version ()
  "Display a message for identica-mode version."
  (interactive)
  (let ((version-string
	 (format "identica-mode-v%s" identica-mode-version)))
    (if (interactive-p)
	(message "%s" version-string)
      version-string)))

(defun identica-icon-mode (&optional arg)
  (interactive)
  (setq identica-icon-mode
	(if identica-icon-mode
	    (if (null arg)
		nil
	      (> (prefix-numeric-value arg) 0))
	  (when (or (null arg)
		    (and arg (> (prefix-numeric-value arg) 0)))
	    (when (file-writable-p identica-tmp-dir)
	      (progn
		(if (not (file-directory-p identica-tmp-dir))
		    (make-directory identica-tmp-dir))
		t)))))
  (identica-current-timeline))

(defun identica-scroll-mode (&optional arg)
  (interactive)
  (setq identica-scroll-mode
	(if (null arg)
	    (not identica-scroll-mode)
	  (> (prefix-numeric-value arg) 0))))


(defun identica-debug-mode ()
  (interactive)
  (setq identica-debug-mode
	(not identica-debug-mode))
  (message (if identica-debug-mode "debug mode:on" "debug mode:off")))

(defun identica-delete-notice ()
  (interactive)
  (let ((id (identica-get-text-value-id))
        (usern (identica-get-text-value-username)))
    (if (string= usern (sn-account-username sn-current-account))
        (when (y-or-n-p "Delete this notice? ")
          (identica-http-post "statuses/destroy" (number-to-string id))
          (identica-get-timeline))
      (message "Can't delete a notice that isn't yours"))))


(defun identica-change-user ()
  (interactive)
  "Interactive function to instantly change user authentication.
Directly reads parameters from user.  This function only sets the
identica-mode variables `(sn-account-username sn-current-account)' and
`(sn-account-password sn-current-account)'.
It is the `identica-set-auth' function that eventually sets the
url library variables according to the above variables which does the
authentication.  This will be done automatically in normal use cases
enabling dynamic change of user authentication."
  (interactive)
  (identica-ask-credentials)
  (identica-get-timeline))

(defun identica-ask-credentials ()
  "Asks for your username and password."
  (setf (sn-account-username sn-current-account)
	(read-string (concat "Username [for " (sn-account-server sn-current-account)
			     ":" (int-to-string (sn-account-port sn-current-account)) "]: ")
		     nil nil (sn-account-username sn-current-account))
	(sn-account-password sn-current-account)
	(read-passwd "Password: " nil (sn-account-password sn-current-account))))

(defun identica-render-pending-dents ()
  (interactive)
  "If at the time an HTTP request for new dents finishes,
identica-buffer is not active, we defer its update, to make sure
we adjust point within the right frame."
  (identica-render-timeline)
  (when (> identica-new-dents-count 0)
    (run-hooks 'identica-new-dents-hook)
    (setq identica-new-dents-count 0))
  (when identica-display-success-messages
    (message "Success: Get")))


(defun identica-update-status-from-edit-buffer-send ()
  (interactive)
  (with-current-buffer "*identica-status-update-edit*"
    (longlines-encode-region (point-min) (point-max))
    (let* ((status (buffer-substring-no-properties (point-min) (point-max)))
           (status-len (length status)))
      (if (< (sn-account-textlimit sn-current-account) status-len)
          (message (format "Beyond %s chars.  Remove %d chars."
			   (sn-account-textlimit sn-current-account)
			   (- status-len (sn-account-textlimit sn-current-account))))
        (if (identica-update-status-if-not-blank identica-update-status-edit-method-class
						 identica-update-status-edit-method status
						 identica-update-status-edit-parameters
						 identica-update-status-edit-reply-to-id)
            (progn
              (erase-buffer)
              (bury-buffer))
          (message "Update failed!"))))))

(defun identica-update-status-from-minibuffer (&optional init-str method-class method parameters reply-to-id)
  (interactive)
  (identica-update-status 'minibuffer init-str method-class method parameters reply-to-id))

(defun identica-update-status-from-edit-buffer (&optional init-str method-class method parameters)
  (interactive)
  (identica-update-status 'edit-buffer init-str method-class method parameters))

(defun identica-update-status-from-edit-buffer-cancel ()
  (interactive)
  (when (or (not identica-update-status-edit-confirm-cancellation)
	    (yes-or-no-p
	     "Really cancel editing this status message (any changes will be lost)?"))
    (erase-buffer)
    (bury-buffer)))

(defun identica-update-status-from-region (beg end)
  (interactive "r")
  (when (> (- end beg) (sn-account-textlimit sn-current-account))
    (setq end (+ beg (sn-account-textlimit sn-current-account))))
  (when (< (- end beg) (sn-account-textlimit sn-current-account))
    (setq beg (+ end (sn-account-textlimit sn-current-account))))
  (identica-update-status-if-not-blank "statuses" "update" (buffer-substring beg end)))

(defun identica-update-status-with-media (attachment &optional init-str method-class method parameters reply-to-id)
  (interactive "f")
  (identica-update-status 'minibuffer nil reply-to-id nil nil `((media . ,(insert-file-contents-literally attachment)))))

(defun identica-shortenurl-replace-at-point ()
  "Replace the url at point with a tiny version."
  (interactive)
  (let ((url-bounds (bounds-of-thing-at-point 'url)))
    (when url-bounds
      (let ((url (identica-shortenurl-get (thing-at-point 'url))))
	(when url
	  (save-restriction
	    (narrow-to-region (car url-bounds) (cdr url-bounds))
	    (delete-region (point-min) (point-max))
	    (insert url)))))))

(defun identica-expand-replace-at-point ()
  "Replace the url at point with a tiny version."
  (interactive)
  (let ((url-bounds (bounds-of-thing-at-point 'url))
        (original-url (thing-at-point 'url)))
    (when url-bounds
      (message (concat "Expanding url: " original-url))
      (let ((uri (identica-expand-shorturl original-url)))
	(when uri
	  (set-buffer (get-buffer identica-buffer))
	  (save-restriction
	    (setq buffer-read-only nil)
	    (narrow-to-region (car url-bounds) (cdr url-bounds))
	    (delete-region (point-min) (point-max))
	    (add-text-properties 0 (length uri)
				 `(mouse-face highlight
					      face identica-uri-face
					      uri ,uri
					      uri-in-text ,uri) uri)
	    (insert uri)
            (message (concat "Expanded Short URL " original-url "to Long URL: " uri))
	    (setq buffer-read-only t)))))))

;;;
;;; Commands
;;;

(defun identica-start (&optional action)
  (interactive)
  (when (null action)
    (setq action #'identica-current-timeline))
  (if identica-timer
      nil
    (setq identica-timer
	  (run-at-time "0 sec"
		       identica-timer-interval
		       #'identica-timer-action action)))
  (set 'identica-active-mode t)
  (identica-update-mode-line))

(defun identica-stop ()
  "Stop Current network activitiy (if any) and the reload-timer."
  (interactive)
  (when (get-buffer-process identica-http-buffer)
    (delete-process identica-http-buffer)
    (kill-buffer identica-http-buffer))
  (setq identica-method (sn-account-last-timeline-retrieved sn-current-account))
  (identica-set-mode-string nil identica-method (sn-account-server sn-current-account))
  (and identica-timer
       (cancel-timer identica-timer))
  (setq identica-timer nil)
  (set 'identica-active-mode nil)
  (identica-update-mode-line))

(defun identica-switch-account ()
  "Update the current account and reload the default timeline."
  (interactive)
  (let ((current-account (member* sn-current-account statusnet-accounts)))
    (setq sn-current-account
	  (if (cdr current-account)
	      (cadr current-account)
	    (car statusnet-accounts))
	  identica-timeline-data nil)
    (identica-current-timeline)))

					; --------------------
					; Timelines
					;

(defun identica-friends-timeline ()
  (interactive)
  (identica-get-timeline "statuses" "friends_timeline"))

(defun identica-replies-timeline ()
  (interactive)
  (identica-get-timeline "statuses" "replies"))

;; (defun identica-direct-messages-timeline ()
;;   (interactive)
;;   (setq identica-method "direct_messages")
;;   (setq identica-method-class "none")
;;   (identica-get-timeline))

(defun identica-public-timeline ()
  (interactive)
  (identica-get-timeline "statuses" "public_timeline"))

(defun identica-group-timeline (&optional group)
  (interactive)
  (unless group
    (setq group (read-from-minibuffer "Group: " nil nil nil nil nil t)))
  (identica-get-timeline "statusnet/groups" (concat "timeline/" group)))

(defun identica-tag-timeline (&optional tag)
  (interactive)
  (unless tag
    (setq tag (read-from-minibuffer "Tag: " nil nil nil nil nil t)))
  (setq identica-method-class "statusnet/tags")
  (identica-get-timeline "statusnet/tags" (concat "timeline/" tag)))

(defun identica-user-timeline (&optional from-user)
  "Retrieve user timeline given its username.

FROM-USER can be an empty string (\"\") meaning that you want to retrieve your own timeline.
If nil, will ask for username in minibuffer."
  (interactive)
  (unless from-user
    (setq from-user (read-from-minibuffer "User [Empty for mine]: "
					  nil nil nil nil nil t)))
  (identica-get-timeline "statuses" (concat "user_timeline/" from-user)))

(defun identica-conversation-timeline ()
  (interactive)
  (let ((context-id (identica-get-text-value-conversation-id)))
    (identica-get-timeline "statusnet" (concat "conversation/" context-id))))

(defun identica-remote-user-timeline ()
  (interactive)
  (let* ((profile (identica-get-text-value-profile-url))
         (username (identica-get-text-value-username))
         (server-url (if (string-match (concat username "/?$") profile)
                         (replace-match "" nil t profile)
                       profile))
         (server (if (string-match "^https?://" server-url)
                     (replace-match "" nil t server-url)
                   server-url)))
    (setq identica-method-class "statuses")
    (setq identica-method (concat "user_timeline/" username))
    (identica-get-timeline server)))

(defun identica-current-timeline (&optional count)
  "Load newer notices, with an argument load older notices, and with a numeric argument load that number of notices."
  (interactive "P")
  (if (> identica-new-dents-count 0)
      (identica-render-pending-dents)
    (identica-get-timeline
     identica-remote-server
     (if count
	 (cons `("count" .
		 ,(int-to-string
		   (if (listp count) identica-statuses-count count)))
	       (if (listp count)
		   `(("max_id" .
		      ,(int-to-string
			(- (assoc-default 'id (car (last identica-timeline-data))) 1))))
		 ()))
       nil))))

					; --------------------

(defun identica-update-status-interactive ()
  (interactive)
  (identica-update-status identica-update-status-method))

(defun identica-direct-message-interactive ()
  (interactive)
  (identica-update-status identica-update-status-method nil nil "direct_messages" "new"))

(defun identica-erase-old-statuses ()
  (interactive)
  (setq identica-timeline-data nil)
  (when (not (sn-account-last-timeline-retrieved sn-current-account))
    (setf (sn-account-last-timeline-retrieved sn-current-account) identica-method))
  (identica-http-get (sn-account-server sn-current-account) (sn-account-auth-mode sn-current-account)
                     "statuses" (sn-account-last-timeline-retrieved sn-current-account)))

(defun identica-follow (&optional remove)
  (interactive)
  (let ((username (identica-get-text-value-username))
	(method (if remove "destroy" "create"))
	(message (if remove "unfollowing" "following")))
    (unless username
      (setq username (read-from-minibuffer "user: ")))
    (if (> (length username) 0)
	(when (y-or-n-p (format "%s %s? " message username))
	  (identica-http-post (format "friendships/%s" method) username)
	  (message (format "Now %s %s" message username)))
      (message "No user selected"))))

(defun identica-unfollow ()
  (interactive)
  (identica-follow t))

(defun identica-group-join (&optional leaving)
  "Simple functions to join/leave a group we are visiting."
  (interactive)
  (setq identica-method-class "statusnet/groups")
  (string-match "\\([^\\]*\\)\\(/.*\\)" identica-method)
  (let ((group-method (replace-match
                       (if leaving "leave"
                         "join") nil nil identica-method 1)))
    (identica-http-post identica-method-class group-method nil)))

(defun identica-group-leave ()
  (identica-group-join t))

(defun identica-favorite ()
  (interactive)
  (when (y-or-n-p "Do you want to favor this notice? ")
    (let ((id (identica-get-text-value-id)))
      (identica-http-post "favorites/create" (number-to-string id))
      (message "Notice saved as favorite"))))

(defun identica-repeat ()
  (interactive)
  (when (y-or-n-p "Do you want to repeat this notice? ")
    (let ((id (identica-get-text-value-id)))
      (identica-http-post "statuses/retweet" (number-to-string id))
      (message "Notice repeated"))))

(defun identica-view-user-page ()
  (interactive)
  (let ((uri (identica-get-text-value-uri)))
    (when uri (browse-url uri))))

(defun identica-redent ()
  (interactive)
  (let ((username (identica-get-text-value-username))
	(id (identica-get-text-value-id))
	(text (replace-regexp-in-string "!\\(\\b\\)" "#\\1" (identica-get-text-value-text))))
    (when username
      (identica-update-status identica-update-status-method
			      (concat identica-redent-format " @" username ": " text) id))))

(defun identica-reply-to-user (all)
  "Open a minibuffer initialized to type a reply to the notice at point.
With no argument, populate with the username of the author of the notice.
With an argument, populate with the usernames of the author and any usernames mentioned in the notice."
  (interactive "P")
  (let ((username (identica-get-text-value-username))
        (notice-text (identica-get-text-value-text))
	(id (identica-get-text-value-id))
        (usernames nil)
	(usernames-string ""))
    (when all
      (setq usernames
	    (mapcar (lambda (string)
		      (when (and (char-equal (aref string 0) ?@)
				 (memq-face identica-uri-face
					    (get-text-property 2 'face string)))
			(concat string " ")))
		    (split-string notice-text))))
    (when username (setq usernames (cons (concat "@" username " ") usernames)))
    (setq usernames (delete-dups usernames))
    (setq usernames (delete (concat "@" (sn-account-username sn-current-account) " ") usernames))
    (setq usernames-string (apply 'concat usernames))
    (identica-update-status identica-update-status-method usernames-string id)))

(defun identica-reply-to-all ()
  (interactive)
  (identica-reply-to-user t))

(defun identica-get-password ()
  (or (sn-account-password sn-current-account)
      (setf (sn-account-password sn-current-account) (read-passwd "password: "))))

(defun identica-toggle-highlight (&optional arg)
  "Toggle the highlighting of entry at 'point'.
With no arg or prefix, toggle the highlighting of the entry at 'point'.
With arg (or prefix, if interactive), highlight the current entry and
un-highlight all other entries."
  (interactive "P")
  (let ((id (identica-get-text-value-id)))
    (setq identica-highlighted-entries
          (if arg (list id)
            (if (memq id identica-highlighted-entries)
                (delq id identica-highlighted-entries)
              (cons id identica-highlighted-entries)))))
  (identica-render-timeline))

(defun memq-face (face property)
  "Check whether FACE is present in PROPERTY."
  (if (listp property)
      (memq face property)
    (eq property face)))

(defun identica-toggle-activate-buffer ()
  (interactive)
  (setq identica-active-mode (not identica-active-mode))
  (if (not identica-active-mode)
      (identica-stop)
    (identica-start)))


;;;###autoload
(defun identica ()
  "Start identica-mode."
  (interactive)
  ;; Initialize everything  
  (switch-to-buffer (identica-buffer))
  ;;(kill-all-local-variables)
  
  (identica-mode-init-variables)
  ;;  (identica-retrieve-configuration) 
  ;; Start the major mode!
  (identica-mode)
  (identica-get-timeline))

(defun identica-kill-buffer-function ()
  (when (eq major-mode 'identica-mode)
    (identica-stop)))


;;
;; This is the old get-timeline function. For simplicity there is a new `identica-get-timeline' function.
;; TODO: Needs to be analized. 
;;
(defun identica-get-timeline-orig (&optional server parameters)
  (setq identica-remote-server server)
  (unless parameters (setq parameters `(("count" . ,(int-to-string identica-statuses-count)))))
  (when (not (eq (sn-account-last-timeline-retrieved sn-current-account) identica-method))
    (setq identica-timeline-last-update nil
	  identica-timeline-data nil))
  (let ((buf (get-buffer identica-buffer)))
    (if (not buf)
	(identica-stop)
      (progn
	(when (not identica-method)
	  (setq identica-method "friends_timeline"))
        (identica-http-get (or server (sn-account-server sn-current-account))
                           (if server "none"
                             (sn-account-auth-mode sn-current-account))
                           identica-method-class identica-method parameters))))
  (identica-get-icons))


(defun identica-process-http-buffer-1 (&rest r)
  "This is a temporary function made for parameters compatibility with the HTTP \"sentinel\" for `identica-http-get-sentinel'
and `identica-process-http-buffer' function."
  (with-current-buffer identica-http-buffer
    (identica-process-http-buffer))
  (with-current-buffer (identica-buffer)
    (erase-buffer)
    (identica-render-timeline)
    (goto-char (point-min))))

(defun identica-get-timeline (&optional method-class method parameters)
  "Update the current method-class, method and parameters and then visit that timeline.

If METHOD-CLASS, METHOD and PARAMETERS is nil or absent then use this variables:

* METHOD-CLASS: `identica-current-method-class'
* METHOD: `identica-current-method'
* PARAMETERS: `identica-current-parameters'

If any of them is not nil, then update the identica-current-? value.
"

  (identica-check-defaults)

  (if method-class
      (setq identica-current-method-class method-class)
    (setq method-class identica-current-method-class))
  (if method
      (setq identica-current-method method)
    (setq method identica-current-method))
  (if parameters
      (setq identica-current-parameters parameters)
    (setq parameters identica-current-parameters))

  (setq identica-http-get-sentinel 'identica-process-http-buffer-1)
  (identica-http-get method-class method parameters)
  (switch-to-buffer (identica-buffer))
  ;; Set the major mode!
  (with-current-buffer (identica-buffer)
    (identica-mode)
    )
  )

(defun identica-check-defaults ()
  "If identica-current-* variables are nil then initialize them with defaults values."
  (unless identica-current-method
    (setq identica-current-method (identica-default-method)))
  (unless identica-current-method-class
    (setq identica-current-method-class (identica-default-method-class)))
  ;; if parameters is nil, nothing to do.
  )

(provide 'identica-commands)
