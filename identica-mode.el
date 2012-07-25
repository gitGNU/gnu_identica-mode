;;; identica-mode.el --- Major mode API client for status.net open microblogging

;; Copyright (C) 2008-2011 Gabriel Saldana
;; Copyright (C) 2009 Bradley M. Kuhn

;; Author: Gabriel Saldana <gsaldana@gmail.com>
;; Last update: 2011-10-20
;; Version: 1.2.1
;; Keywords: identica web
;; URL: http://blog.gabrielsaldana.org/identica-mode-for-emacs/
;; Contributors:
;;     Jason McBrayer <jmcbray@carcosa.net> (minor updates for working under Emacs 23)
;;     Alex Schröder <kensanata@gmail.com> (mode map patches)
;;     Christian Cheng (fixed long standing xml parsing bug)
;;     Carlos A. Perilla from denting-mode
;     Alberto Garcia <agarcia@igalia.com> (integrated patch from twittering-mode for retrieving multiplemethods)
;;     Bradley M. Kuhn <bkuhn@ebb.org> (editing status from edit-buffer rather than minibuffer)
;;     Jason McBrayer <jmcbray@carcosa.net> (replace group tags with hashtags on redents, longlines use)
;;     Sean Neakums (patches of bugs flagged by byte-compiler)
;;     Shyam Karanatt <shyam@swathanthran.in> (several patches and code cleanup, new http backend based on url.el)
;;     Tezcatl Franco <tzk@riseup.net> (ur1.ca support)
;;     Anthony Garcia <lagg@lavabit.com> (fix for icon-mode)
;;     Alexande Oliva <oliva@lsd.ic.unicamp.br> (fix for icon placement on reverse order dents)
;;     Aidan Gauland <aidalgol@no8wireless.co.nz> (variable scope code cleanup)
;;     Joel J. Adamson <adamsonj@email.unc.edu> Added countdown minibuffer-prompt style
;;     Kevin Granade <kevin.granade@gmail.com> (OAuth support)

;;; Commentary:

;; Identica Mode is a major mode to check friends timeline, and update your
;; status on Emacs.

;; identica-mode.el is a major mode for Identica.  Based on the twittering mode
;; version 0.6 by Y.  Hayamizu and Tsuyoshi CHO found at
;; <http://hayamin.com/wiliki.cgi?twittering-mode-en&l=en>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FORCouldn't findSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth floor,
;; Boston, MA 02110-1301, USA.

;; Requirements
;; if using Emacs22 or previous, you'll need json.el
;; get it from http://edward.oconnor.cx/2006/03/json.el
;; json.el is part of Emacs23
;; To use the OAuth support, you need oauth.el
;; Downloadable from http://github.com/psanford/emacs-oauth/

;; If using Oauth with Emacs earlier than 23.3 you'll also need w3m.

;;; Install:

;; You can use M-x customize-group identica-mode to setup all settings or simply
;; add the following to your .emacs or your prefered customizations file

;; (require 'identica-mode)
;; (setq identica-username "yourusername")

;; If you want to use simple authentication add your password
;; (setq identica-password "yourpassword")

;; It is recommended to create a file ~/.authinfo with your login credentials
;; instead of storing your password in plain text, the file should have the
;; following contents:

;; machine servername login yourusername password yourpassword

;; Replace servername with your server (if Identica server use identi.ca)
;; yourusername and yourpassword with your information. If you setup your
;; authinfo file, you don't need to set identica-password variable anywhere

;; If you want to use OAuth authentication add the following
;; (setq identica-auth-mode "oauth")

;; If you want to post from the minibufer without having identica buffer active, add the following global keybinding.
;; Add this to send status updates
;; (global-set-key "\C-cip" 'identica-update-status-interactive)
;; Add this to send direct messages
;; (global-set-key "\C-cid" 'identica-direct-message-interactive)

;; If you want to connect to a custom statusnet server add this and change
;; identi.ca with your server's doman name.

;; (setq statusnet-server "identi.ca")

;; Start using with M-x identica

;; Follow me on identica: http://identi.ca/gabrielsaldana

;;; Code:

(require 'cl)
(require 'xml)
(require 'parse-time)
(require 'longlines)
(require 'url)
(require 'url-http)
(require 'json)
(require 'image)

(defconst identica-mode-version "1.2.1")

;;url-basepath fix for emacs22
(unless (fboundp 'url-basepath)
  (defalias 'url-basepath 'url-file-directory))

(defgroup identica-mode nil
  "Identica Mode for microblogging"
  :tag "Microblogging"
  :link '(url-link http://blog.gabrielsaldana.org/identica-mode-for-emacs/)
  :group 'applications )

(defvar identica-timer nil "Timer object for timeline refreshing will be stored here.  DO NOT SET VALUE MANUALLY.")

(defvar identica-new-dents-count 0
  "Number of new tweets when `identica-new-dents-hook' is run.")

(defvar identica-new-dents-hook nil
  "Hook run when new twits are received.

You can read `identica-new-dents-count' to get the number of new
tweets received when this hook is run.")

(defvar identica-display-max-dents nil
  "How many dents to keep on the displayed timeline.

If non-nil, dents over this amount will bre removed.")

(defcustom identica-idle-time 20
  "Idle time."
  :type 'integer
  :group 'identica-mode)

(defcustom identica-timer-interval 90
  "Timer interval to refresh the timeline."
  :type 'integer
  :group 'identica-mode)

(defcustom identica-username nil
  "Your identi.ca username.  If nil, you will be prompted."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'identica-mode)

(defcustom identica-password nil
  "Your identi.ca password.  If nil, you will be prompted."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'identica-mode)


(defcustom identica-default-timeline "friends_timeline"
  "Default timeline to retrieve."
  :type 'string
  :options '("friends_timeline" "public_timeline" "replies")
  :group 'identica-mode)

(defcustom identica-statuses-count 20
  "Default number of statuses to retrieve."
  :type 'integer
  :group 'identica-mode)

(defcustom identica-display-success-messages nil
  "Display messages when the timeline is successfully retrieved."
  :type 'boolean
  :group 'identica-mode)

(defcustom identica-oldest-first nil
  "If t, display older messages before newer ones."
  :type 'boolean
  :group 'identica-mode)

(defcustom identica-update-status-edit-confirm-cancellation nil
  "If t, ask user if they are sure when aborting editing of an
identica status update when using an edit-buffer"
  :type 'boolean
  :group 'identica-mode)

(defcustom identica-soft-wrap-status t
  "If non-nil, don't fill status messages in the timeline as
paragraphs. Instead, use visual-line-mode or longlines-mode if
  available to wrap messages.  This may work better for narrow
  timeline windows."
  :type 'boolean
  :group 'identica-mode)

(defcustom identica-update-status-method 'minibuffer
  "Method for performaing status updates.

The available choices are:

  'minibuffer  - edit the status update in the minibuffer.
  'edit-buffer - edit the status update in an independent buffer."
  :type '(choice (const :tag "Edit status in minibuffer" minibuffer)
		 (const :tag "Edit status in independent buffer" edit-buffer))
  :group 'identica-mode)

;; Initialize with default timeline
(defvar identica-method identica-default-timeline)
(defvar identica-method-class "statuses")
(defvar identica-remote-server nil)

(defvar identica-scroll-mode nil)
(make-variable-buffer-local 'identica-scroll-mode)

(defvar identica-source "identica-mode")

(defcustom identica-redent-format "♻"
  "The format/symbol to represent redents."
  :type 'string
  :group 'identica-mode)

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

(defvar identica-buffer "*identica*")
(defun identica-buffer (&optional method)
  "Create a buffer for use by identica-mode.
Initialize the global method with the default, or with METHOD, if present."
  (unless method
    (setq method "friends_timeline"))
  (get-buffer-create identica-buffer))

(defvar identica-timeline-data nil)
(defvar identica-timeline-last-update nil)
(defvar identica-highlighted-entries nil
  "List of entry ids selected for highlighting.")

(defcustom identica-enable-highlighting nil
  "If non-nil, set the background of every selected entry to the background
of identica-highlight-face."
  :type 'boolean
  :group 'identica-mode)

(defcustom identica-enable-striping nil
  "If non-nil, set the background of every second entry to the background
of identica-stripe-face."
  :type 'boolean
  :group 'identica-mode)

(defcustom identica-enable-striping nil
  "If non-nil, set the background of every second entry to the background
of identica-stripe-face."
  :type 'boolean
  :group 'identica-mode)

(defun identica-user-agent-default-function ()
  "Identica mode default User-Agent function."
  (concat "Emacs/"
	  (int-to-string emacs-major-version) "." (int-to-string
						   emacs-minor-version)
	  " "
	  "Identica-mode/"
	  identica-mode-version))

(defvar identica-user-agent-function 'identica-user-agent-default-function)

(defun identica-user-agent ()
  "Return User-Agent header string."
  (funcall identica-user-agent-function))

;;; to show image files

(defvar identica-tmp-dir
  (expand-file-name (concat "identicamode-images-" (user-login-name))
		    temporary-file-directory))

(defvar identica-icon-mode nil "You MUST NOT CHANGE this variable directory.  You should change through function'identica-icon-mode'.")
(make-variable-buffer-local 'identica-icon-mode)

(defvar identica-image-stack nil)

(defun identica-image-type (file-name)
  (cond
   ((string-match "\\.jpe?g" file-name) 'jpeg)
   ((string-match "\\.png" file-name) 'png)
   ((string-match "\\.gif" file-name) 'gif)
   (t nil)))

(defun identica-setftime (fmt string uni)
  (format-time-string fmt ; like "%Y-%m-%d %H:%M:%S"
		      (apply 'encode-time (parse-time-string string))
		      uni))
(defun identica-local-strftime (fmt string)
  (identica-setftime fmt string nil))
(defun identica-global-strftime (fmt string)
  (identica-setftime fmt string t))

(defvar identica-debug-mode nil)
(defvar identica-debug-buffer "*identica-debug*")
(defun identica-debug-buffer ()
  (get-buffer-create identica-debug-buffer))
(defmacro debug-print (obj)
  (let ((obsym (gensym)))
    `(let ((,obsym ,obj))
       (if identica-debug-mode
	   (with-current-buffer (identica-debug-buffer)
	     (insert (prin1-to-string ,obsym))
	     (newline)
	     ,obsym)
	 ,obsym))))


(defmacro case-string (str &rest clauses)
  `(cond
    ,@(mapcar
       (lambda (clause)
	 (let ((keylist (car clause))
	       (body (cdr clause)))
	   `(,(if (listp keylist)
		  `(or ,@(mapcar (lambda (key) `(string-equal ,str ,key)) keylist))
		't)
	     ,@body)))
       clauses)))

;; If you use Emacs21, decode-char 'ucs will fail unless Mule-UCS is loaded.
;; TODO: Show error messages if Emacs 21 without Mule-UCS
(defmacro identica-ucs-to-char (num)
  (if (functionp 'ucs-to-char)
      `(ucs-to-char ,num)
    `(decode-char 'ucs ,num)))

(defvar identica-mode-string identica-method)

(defun identica-set-mode-string (loading)
  (with-current-buffer (identica-buffer)
    (let ((timeline-url
	   (concat (or identica-remote-server
		       (sn-account-server sn-current-account))
		   "/" identica-method)))
      (setq mode-name
	    (if loading (concat
			 (if (stringp loading) loading "loading")
			 " " timeline-url "...")
	      timeline-url))
      (debug-print mode-name))))

(defcustom identica-load-hook nil
  "Hook that is run after identica-mode.el has been loaded."
  :group 'identica-mode
  :type 'hook)

(defun merge-text-attribute (start end new-face attribute)
  "Merge the ATTRIBUTE of NEW-FACE into the text between START and END.
If we just add the new face its attributes somehow get overridden by
the attributes of the underlying face, so instead we just add the attribute
we are interested in."
  (while (not (eq start end))
    (let ((bg (face-attribute new-face attribute))
	  (prop (get-text-property start 'face))
          (next-change
           (or (next-single-property-change start 'face (current-buffer))
               end)))
      (if prop
	  (add-text-properties start next-change
			       (list 'face
				     (list prop
					   (list attribute bg))))
        (add-text-properties start next-change
			     (list 'face (list attribute bg))))
      (setq start next-change))))

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

(defun identica-http-post-default-sentinel
  (&optional status method-class method parameters success-message)
  (let ((error-object (assoc-workaround :error status)))
    (cond  ((and
             error-object
	     (y-or-n-p (format "Network error:%s %s Retry? "
			       (cadr error-object)
			       (caddr error-object))))
	    (identica-http-post method-class method parameters nil success-message))
	   (identica-display-success-messages
            (message (or success-message "Success: Post")))))
  (unless (get-buffer-process (current-buffer))
    (kill-buffer (current-buffer))))

(defun identica-compare-statuses (a b)
  "Compare a pair of statuses.
For use as a predicate for sort."
  (< (assoc-default 'id b) (assoc-default 'id a)))

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

(defun identica-percent-encode (str &optional coding-system)
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

(defun identica-url-reserved-p (ch)
  (or (and (<= ?A ch) (<= ch ?z))
      (and (<= ?0 ch) (<= ch ?9))
      (eq ?. ch)
      (eq ?- ch)
      (eq ?_ ch)
      (eq ?~ ch)))
(defun identica-timer-action (func)
  (let ((buf (get-buffer identica-buffer)))
    (if (null buf)
	(identica-stop)
      (funcall func))))

(defun identica-update-status-if-not-blank (method-class method status &optional parameters reply-to-id)
  (if (string-match "^\\s-*\\(?:@[-_a-z0-9]+\\)?\\s-*$" status)
      nil
    (if (equal method-class "statuses")
	(identica-http-post method-class method
			    `(("status" . ,status)
			      ("source" . ,identica-source)
			      ,@(if (assoc `media parameters)
				    `(("media" . ,(cdr (assoc `media parameters))))
				  nil)
			      ,@(if reply-to-id
				    `(("in_reply_to_status_id"
				       . ,(number-to-string reply-to-id))))))
      (identica-http-post method-class method
			  `(("text" . ,status)
			    ("user" . ,parameters) ;must change this to parse parameters as list
			    ("source" . ,identica-source))))

    t))


(defun identica-update-status-edit-in-edit-buffer (init-str msgtype method-class method parameters &optional reply-to-id)
  (let ((buf (get-buffer-create "*identica-status-update-edit*")))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (when (not (equal major-mode 'identica-update-status-edit-mode))
	(progn
	  (identica-update-status-edit-mode)
	  (when identica-soft-wrap-status
	    (when (fboundp 'visual-line-mode)
	      (visual-line-mode t)))
	  (make-local-variable 'identica-update-status-edit-method-class)
	  (make-local-variable 'identica-update-status-edit-method)
	  (make-local-variable 'identica-update-status-edit-parameters)
	  (make-local-variable 'identica-update-status-edit-reply-to-id)
	  (if (> (length parameters) 0)
	      (setq mode-line-format
		    (cons (format "%s(%s) (%%i/%s) " msgtype parameters
				  (sn-account-textlimit sn-current-account))
			  mode-line-format))
	    t (setq mode-line-format
		    (cons (format "%s (%%i/%s) " msgtype (sn-account-textlimit sn-current-account))
			  mode-line-format)))))
      (setq identica-update-status-edit-method-class method-class)
      (setq identica-update-status-edit-method method)
      (setq identica-update-status-edit-parameters parameters)
      (setq identica-update-status-edit-reply-to-id reply-to-id)
      (message identica-update-status-edit-method-class)
      (insert init-str)
      (message "Type C-c C-c to post status update (C-c C-k to cancel)."))))

(defcustom identica-minibuffer-length-prompt-style nil
  "The preferred style of counting characters in the minibuffer.
prompt; \"Down\" counts down from (sn-account-textlimit sn-current-account); \"Up\" counts
  up from 0"
  :type '(choice (const :tag "Down" nil)
		 (const :tag "Up" t))
  :group 'identica-mode)

(defun identica-show-minibuffer-length (&optional beg end len)
  "Show the number of characters in minibuffer."
  (when (minibuffer-window-active-p (selected-window))
    (let* ((status-len (- (buffer-size) (minibuffer-prompt-width)))
	   (mes (format "%d" (if identica-minibuffer-length-prompt-style
				 status-len
			       (- (sn-account-textlimit sn-current-account) status-len)))))
      (if (<= 23 emacs-major-version)
	  (minibuffer-message mes) ; Emacs23 or later
	(minibuffer-message (concat " (" mes ")"))))))

(defun identica-setup-minibuffer ()
  (identica-show-minibuffer-length)
  (add-hook 'post-command-hook 'identica-show-minibuffer-length t t))

(defun identica-finish-minibuffer ()
  (remove-hook 'post-command-hook 'identica-show-minibuffer-length t))

(defun identica-update-status (update-input-method &optional init-str reply-to-id method-class method parameters)
  (when (null init-str) (setq init-str ""))
  (let ((msgtype "")
	(status init-str)
	(not-posted-p t)
	(user nil)
	(map minibuffer-local-map)
	(minibuffer-message-timeout nil))
    (define-key map (kbd "<f4>") 'identica-shortenurl-replace-at-point)
    (if (null method-class)
        (progn (setq msgtype "Status")
               (setq method-class "statuses")
               (setq method "update"))
      (progn (setq msgtype "Direct message")
             (setq method-class "direct_messages")
             (setq parameters (read-from-minibuffer "To user: " user nil nil nil nil t))
             (setq method "new")))
    (cond ((eq update-input-method 'minibuffer)
	   (add-hook 'minibuffer-setup-hook 'identica-setup-minibuffer t)
	   (add-hook 'minibuffer-exit-hook 'identica-finish-minibuffer t)
	   (unwind-protect
	       (while not-posted-p
		 (setq status (read-from-minibuffer (concat msgtype ": ") status nil nil nil nil t))
		 (while (< (+ (sn-account-textlimit sn-current-account) 1) (length status))
		   (setq status (read-from-minibuffer (format (concat msgtype "(%d): ")
							      (- (sn-account-textlimit sn-current-account) (length status)))
						      status nil nil nil nil t)))
		 (setq not-posted-p
		       (not (identica-update-status-if-not-blank method-class method status parameters reply-to-id))))
	     (remove-hook 'minibuffer-setup-hook 'identica-setup-minibuffer)
	     (remove-hook 'minibuffer-exit-hook 'identica-finish-minibuffer)))
          ((eq update-input-method 'edit-buffer)
           (identica-update-status-edit-in-edit-buffer init-str msgtype method-class method parameters reply-to-id))
          (t (error "Unknown update-input-method in identica-update-status: %S" update-input-method)))))

(defun identica-get-timeline (&optional server parameters)
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

(defun identica-get-icons ()
  "Retrieve icons if icon-mode is active."
  (if identica-icon-mode
      (if (and identica-image-stack window-system)
	  (let ((proc
		 (apply
		  #'start-process
		  "wget-images"
		  nil
		  "wget"
		  (format "--directory-prefix=%s" identica-tmp-dir)
		  "--no-clobber"
		  "--quiet"
		  identica-image-stack)))
	    (set-process-sentinel
	     proc
	     (lambda (proc stat)
	       (clear-image-cache)
	       ))))))

(defun assoc-workaround (tag array)
  "Workaround odd semi-associative array returned by url-http."
  (or (assoc tag array)
      (and (equal tag (car array))
	   (cadr array))))

(defun identica-mode-line-buffer-identification ()
  (if identica-active-mode
      identica-modeline-active
    identica-modeline-inactive))



(add-to-list 'minor-mode-alist '(identica-icon-mode " id-icon"))
(add-to-list 'minor-mode-alist '(identica-scroll-mode " id-scroll"))

(defun identica-mode-init-variables ()
  ;; (make-variable-buffer-local 'variable)
  ;; (setq variable nil)
  (make-variable-buffer-local 'identica-active-mode)
  (set-default 'identica-active-mode t) 
  
  ;; make face properties nonsticky
  (nconc text-property-default-nonsticky
	 '((face . t)(mouse-face . t)(uri . t)(source . t)(uri-in-text . t)))

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
  (setq sn-current-account (car statusnet-accounts)))

(provide 'identica-mode)
(add-hook 'identica-load-hook 'identica-autoload-oauth)
(run-hooks 'identica-load-hook)

;;; identica-mode.el ends here
