
    ;; identica-major-mode.el
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


;; Lots of functions come from the original identica-mode. So the authors of those functions are lot of people!
;;
;; See git commit 30ffd973a7cae9e7deae5a075e80f5827af3df2a at savannah. 
;;


(require 'identica-common-things)
;;(require 'identica-mode)

(defvar identica-mode-map (make-sparse-keymap "Identi.ca"))
(defvar menu-bar-identica-mode-menu nil)
(defvar identica-mode-string "")
(defvar identica-icon-mode nil "You MUST NOT CHANGE this variable directory.  You should change through function'identica-icon-mode'.")
(make-variable-buffer-local 'identica-icon-mode)

(defcustom identica-soft-wrap-status t
  "If non-nil, don't fill status messages in the timeline as
paragraphs. Instead, use visual-line-mode or longlines-mode if
  available to wrap messages.  This may work better for narrow
  timeline windows."
  :type 'boolean
  :group 'identica-mode)

;; Menu
(unless menu-bar-identica-mode-menu
  (easy-menu-define
    menu-bar-identica-mode-menu identica-mode-map ""
    '("Identi.ca"
      ["Send an update" identica-update-status-interactive t]
      ["Send a direct message" identica-direct-message-interactive t]
      ["Re-dent someone's update" identica-redent t]
      ["Repeat someone's update" identica-repeat t]
      ["Add as favorite" identica-favorite t]
      ["Follow user" identica-follow]
      ["Unfollow user" identica-unfollow]
      ["--" nil nil]
      ["Friends timeline" identica-friends-timeline t]
      ["Public timeline" identica-public-timeline t]
      ["Replies timeline" identica-replies-timeline t]
      ["User timeline" identica-user-timeline t]
      ["Tag timeline" identica-tag-timeline t]
      ["--" nil nil]
      ["Group timeline" identica-group-timeline t]
      ["Join to this group" identica-group-join t]
      ["Leave this group" identica-group-leave t]
      )))


(defvar identica-mode-syntax-table nil "")

(if identica-mode-syntax-table
    ()
  (setq identica-mode-syntax-table (make-syntax-table))
  ;; (modify-syntax-entry ?  "" identica-mode-syntax-table)
  (modify-syntax-entry ?\" "w"  identica-mode-syntax-table))

(if identica-mode-map
    (let ((km identica-mode-map))
      (define-key km "\C-c\C-f" 'identica-friends-timeline)
      ;;      (define-key km "\C-c\C-i" 'identica-direct-messages-timeline)
      (define-key km "\C-c\C-r" 'identica-replies-timeline)
      (define-key km "\C-c\C-a" 'identica-public-timeline)
      (define-key km "\C-c\C-g" 'identica-group-timeline)
      ;;      (define-ley km "\C-c\C-j" 'identica-group-join)
      ;;      (define-ley km "\C-c\C-l" 'identica-group-leave)
      (define-key km "\C-c\C-t" 'identica-tag-timeline)
      (define-key km "\C-c\C-k" 'identica-stop)
      (define-key km "\C-c\C-u" 'identica-user-timeline)
      (define-key km "\C-c\C-c" 'identica-conversation-timeline)
      (define-key km "\C-c\C-o" 'identica-remote-user-timeline)
      (define-key km "\C-c\C-s" 'identica-update-status-interactive)
      (define-key km "\C-c\C-d" 'identica-direct-message-interactive)
      (define-key km "\C-c\C-m" 'identica-redent)
      (define-key km "\C-c\C-h" 'identica-toggle-highlight)
      (define-key km "r" 'identica-repeat)
      (define-key km "F" 'identica-favorite)
      (define-key km "\C-c\C-e" 'identica-erase-old-statuses)
      (define-key km "\C-m" 'identica-enter)
      (define-key km "R" 'identica-reply-to-user)
      (define-key km "A" 'identica-reply-to-all)
      (define-key km "\t" 'identica-next-link)
      (define-key km [backtab] 'identica-prev-link)
      (define-key km [mouse-1] 'identica-click)
      (define-key km "\C-c\C-v" 'identica-view-user-page)
      (define-key km "q" 'bury-buffer)
      (define-key km "e" 'identica-expand-replace-at-point)
      (define-key km "j" 'identica-goto-next-status)
      (define-key km "k" 'identica-goto-previous-status)
      (define-key km "l" 'forward-char)
      (define-key km "h" 'backward-char)
      (define-key km "0" 'beginning-of-line)
      (define-key km "^" 'beginning-of-line-text)
      (define-key km "$" 'end-of-line)
      (define-key km "n" 'identica-goto-next-status-of-user)
      (define-key km "p" 'identica-goto-previous-status-of-user)
      (define-key km [backspace] 'scroll-down)
      (define-key km " " 'scroll-up)
      (define-key km "G" 'end-of-buffer)
      (define-key km "g" 'identica-current-timeline)
      (define-key km "H" 'beginning-of-buffer)
      (define-key km "i" 'identica-icon-mode)
      (define-key km "s" 'identica-scroll-mode)
      (define-key km "t" 'identica-toggle-proxy)
      (define-key km "\C-k" 'identica-delete-notice)
      (define-key km "\C-c\C-p" 'identica-toggle-proxy)
      nil))

(defvar identica-mode-hook nil
  "Identica-mode hook.")

(defun identica-mode ()
  "Major mode for Identica."
  (interactive)
  (buffer-disable-undo (current-buffer))
  (use-local-map identica-mode-map)
  (setq major-mode 'identica-mode)
  (setq mode-name identica-mode-string)
  (setq mode-line-buffer-identification
	`(,(default-value 'mode-line-buffer-identification)
	  (:eval (identica-mode-line-buffer-identification))))
  (identica-update-mode-line)
  (set-syntax-table identica-mode-syntax-table)
  (set (make-local-variable 'font-lock-keywords)
       identica-mode-font-lock-keywords)
  (font-lock-mode t)
  (if identica-soft-wrap-status
      (if (fboundp 'visual-line-mode)
          (visual-line-mode t)
	(if (fboundp 'longlines-mode)
	    (longlines-mode t))))
  ;; Carefull with this!!! `kill-buffer-hook' is used globally!!! :-S
  ;; `identica-kill-buffer-function' must be reachable, if not you'll have a great emacs problem!
  ;;(add-hook 'kill-buffer-hook 'identica-kill-buffer-function) 
  (run-mode-hooks 'identica-mode-hook))


(defgroup identica-mode-faces nil
  "Identica mode Faces"
  :tag "Identica Mode Faces"
  :group 'identica-mode
  :group 'faces
  )

(defvar identica-username-face 'identica-username-face)
(defvar identica-tagname-face 'identica-tagname-face)
(defvar identica-groupname-face 'identica-groupname-face)
(defvar identica-uri-face 'identica-uri-face)
(defvar identica-reply-face 'identica-reply-face)
(defvar identica-stripe-face 'identica-stripe-face)
(defvar identica-highlight-face 'identica-highlight-face)
(defvar identica-heart-face 'identica-heart-face)
(defvar identica-redent-face 'identica-redent-face)


(defface identica-username-face
  `((t nil)) "" :group 'identica-mode-faces)
(set-face-attribute 'identica-username-face nil :underline t)

(defface identica-groupname-face
  `((t nil)) "" :group 'identica-mode-faces)
(set-face-attribute 'identica-groupname-face nil :underline t)

(defface identica-tagname-face
  `((t nil)) "" :group 'identica-mode-faces)
(set-face-attribute 'identica-tagname-face nil :underline t)

(defface identica-reply-face
  `((t nil)) "" :group 'identica-mode-faces)
(set-face-attribute 'identica-reply-face nil :background "DarkSlateGray")

(defface identica-stripe-face
  `((t nil)) "" :group 'identica-mode-faces)
(set-face-attribute 'identica-stripe-face nil :background "LightSlateGray")

(defface identica-highlight-face
  `((t nil)) "" :group 'identica-mode-faces)
(set-face-attribute 'identica-highlight-face nil :background "SlateGray")

(defface identica-uri-face
  `((t nil)) "" :group 'identica-mode-faces)
(set-face-attribute 'identica-uri-face nil :underline t)

(defface identica-heart-face
  `((t nil)) "" :group 'identica-mode-faces)
(set-face-attribute 'identica-heart-face nil :foreground "firebrick1" :height 2.0)  

(defface identica-redent-face
  `((t nil)) "" :group 'identica-mode-faces)
(set-face-attribute 'identica-redent-face nil :foreground "firebrick1" :height 2.0)  


(defface identica-username-dent-face 
  '((t :weight bold
       :slant italic
       :foreground "spring green"
       ))
  "Face for the username part in a dent message."
  :group 'identica-mode-faces)

(defface identica-source-dent-face 
  '((t :slant italic
       :foreground "spring green"
       ))
  "Face for the username part in a dent message."
  :group 'identica-mode-faces)

(defface identica-location-dent-face 
  '((t :foreground "spring green"
       ))
  "Face for the username part in a dent message."
  :group 'identica-mode-faces)

(defface identica-in-reply-dent-face 
  '((t :underline t
       :foreground "spring green"
       ))
  "Face for the username part in a dent message."
  :group 'identica-mode-faces)

(defface identica-protected-dent-face 
  '((t :weight bold
       :foreground "spring green"
       ))
  "Face for the username part in a dent message."
  :group 'identica-mode-faces)

(defface identica-created-at-dent-face   
  '((t :weight bold
       :slant italic
       :foreground "spring green"
       ))
  "Face for the username part in a dent message."
  :group 'identica-mode-faces)

(defface identica-seconds-ago-dent-face 
  '((t :inherit identica-created-at-dent-face
       ))
  "Face for the username part in a dent message."
  :group 'identica-mode-faces)

(defface identica-truncated-dent-face 
  '((t :weight bold
       :foreground "spring green"
       ))
  "Face for the username part in a dent message."
  :group 'identica-mode-faces)


;; taken from diaspora.el
(defun identica-check-is-property (limit property)
  "Return t if the symbol property given by PROPERTY is in any of the text's properties between current `point' up to LIMIT.
Set `match-data' with the beginning and end position of the first text founded with that property.

Create a new function like `identica-check-is-message-separator' so you can use this function with a font-lock property."
  ;; Point is on a link-to-publication text!
  (let ((beg-pos (text-property-any (point) limit property t))
	(end-pos 0)
	)
    (if beg-pos	
	(progn
	  (goto-char beg-pos)
	  (setq end-pos 
		(next-single-property-change (point) property nil limit)) ;;find the last char where the property is false.    
	    
	  ;; Set match-data
	  (set-match-data (list beg-pos end-pos))
	  t
	  )
      nil
      )
    )
  )

(defun identica-check-is-user-name (limit)  
  "Return t if the text from the current point up to the limit has the property user-name setted to t."
  (identica-check-is-property limit 'user-name))

(defun identica-check-is-source (limit)  
  "Return t if the text from the current point up to the limit has the property user-name setted to t."
  (identica-check-is-property limit 'source))

(defun identica-check-is-location (limit)  
  "Return t if the text from the current point up to the limit has the property user-name setted to t."
  (identica-check-is-property limit 'location))

(defun identica-check-is-in-reply (limit)  
  "Return t if the text from the current point up to the limit has the property user-name setted to t."
  (identica-check-is-property limit 'in-reply))

(defun identica-check-is-protected (limit)  
  "Return t if the text from the current point up to the limit has the property user-name setted to t."
  (identica-check-is-property limit 'user-protected))

(defun identica-check-is-created-at (limit)  
  "Return t if the text from the current point up to the limit has the property user-name setted to t."
  (identica-check-is-property limit 'created-at))

(defun identica-check-is-seconds-ago (limit)  
  "Return t if the text from the current point up to the limit has the property user-name setted to t."
  (identica-check-is-property limit 'seconds-ago))

(defun identica-check-is-truncated (limit)  
  "Return t if the text from the current point up to the limit has the property user-name setted to t."
  (identica-check-is-property limit 'truncated))

(defun identica-check-is-favored (limit)  
  "Return t if the text from the current point up to the limit has the property user-name setted to t."
  (identica-check-is-property limit 'favored))


					; ____________________
					; Font-lock 
;;
(defvar identica-mode-font-lock-keywords
  ;; font-lock-keywords  
  (list
    (cons identica-screen-name-regexp ''identica-username-face) 
    (cons identica-group-name-regexp ''identica-groupname-face)
    (cons identica-tag-name-regexp ''identica-tagname-face)
    (cons identica-url-regexp ''identica-uri-face)     
    (cons identica-heart-regexp ''identica-heart-face)
    (cons identica-redent-regexp ''identica-redent-face)
    (cons 'identica-check-is-user-name ''identica-username-dent-face)
    (cons 'identica-check-is-source ''identica-source-dent-face)
    (cons 'identica-check-is-location ''identica-location-dent-face)
    (cons 'identica-check-is-in-reply ''identica-in-reply-dent-face)
    (cons 'identica-check-is-protected ''identica-protected-dent-face)
    (cons 'identica-check-is-created-at ''identica-created-at-dent-face)
    (cons 'identica-check-is-seconds-ago ''identica-seconds-ago-dent-face)
    (cons 'identica-check-is-truncated ''identica-truncated-dent-face)
    ;;(cons 'identica-check-is-favored ''identica-favored-dent-face)
    )
  ;;
  "Font lock for `identica-mode'"
  )

;; Icons
;;; ACTIVE/INACTIVE
(defconst identica-active-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
	    "/* XPM */
static char * statusnet_xpm[] = {
\"16 16 14 1\",
\" 	c None\",
\".	c #8F0000\",
\"+	c #AB4040\",
\"@	c #D59F9F\",
\"#	c #E3BFBF\",
\"$	c #CE8F8F\",
\"%	c #C78080\",
\"&	c #FFFFFF\",
\"*	c #B96060\",
\"=	c #DCAFAF\",
\"-	c #C07070\",
\";	c #F1DFDF\",
\">	c #961010\",
\",	c #9D2020\",
\"    .......     \",
\"   .........    \",
\"  ...........   \",
\" ....+@#$+....  \",
\"....%&&&&&*.... \",
\"...+&&&&&&&+... \",
\"...=&&&&&&&$... \",
\"...#&&&&&&&#... \",
\"...=&&&&&&&@... \",
\"...*&&&&&&&-... \",
\"....@&&&&&&=... \",
\" ....-#&#$;&>.. \",
\" ..........,>.. \",
\"  ............. \",
\"    ............\",
\"       .      ..\"};")))


(defconst identica-inactive-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
	    "/* XPM */
static char * statusnet_off_xpm[] = {
\"16 16 13 1\",
\" 	g None\",
\".	g #5B5B5B\",
\"+	g #8D8D8D\",
\"@	g #D6D6D6\",
\"#	g #EFEFEF\",
\"$	g #C9C9C9\",
\"%	g #BEBEBE\",
\"&	g #FFFFFF\",
\"*	g #A5A5A5\",
\"=	g #E3E3E3\",
\"-	g #B2B2B2\",
\";	g #676767\",
\">	g #747474\",
\"    .......     \",
\"   .........    \",
\"  ...........   \",
\" ....+@#$+....  \",
\"....%&&&&&*.... \",
\"...+&&&&&&&+... \",
\"...=&&&&&&&$... \",
\"...#&&&&&&&#... \",
\"...=&&&&&&&@... \",
\"...*&&&&&&&-... \",
\"....@&&&&&&=... \",
\" ....-#&#$&&;.. \",
\" ..........>;.. \",
\"  ............. \",
\"    ............\",
\"       .      ..\"};")))

(defun identica-update-mode-line ()
  "Update mode line."
  (force-mode-line-update))

(let ((props
       (when (display-mouse-p)
	 `(local-map
	   ,(purecopy (make-mode-line-mouse-map
		       'mouse-2 #'identica-toggle-activate-buffer))
	   help-echo "mouse-2 toggles automatic updates"))))
  (defconst identica-modeline-active
    (if identica-active-indicator-image
	(apply 'propertize " "
	       `(display ,identica-active-indicator-image ,@props))
      " "))
  (defconst identica-modeline-inactive
    (if identica-inactive-indicator-image
	(apply 'propertize "INACTIVE"
	       `(display ,identica-inactive-indicator-image ,@props))
      "INACTIVE")))

					; ________________________________________
					; Moving and positioning functions

(defun identica-get-username-at-pos (pos)
  (let ((start-pos pos)
	(end-pos))
    (catch 'not-found
      (while (memq-face identica-username-face (get-text-property start-pos 'face))
	(setq start-pos (1- start-pos))
	(when (or (eq start-pos nil) (eq start-pos 0)) (throw 'not-found nil)))
      (setq start-pos (1+ start-pos))
      (setq end-pos (next-single-property-change pos 'face))
      (buffer-substring start-pos end-pos))))

(defun identica-goto-next-status ()
  "Go to next status."
  (interactive)
  (let ((pos))
    (setq pos (identica-get-next-username-face-pos (point)))
    (if pos
	(goto-char pos)
      (progn (goto-char (buffer-end 1)) (message "End of status.")))))

(defun identica-goto-previous-status ()
  "Go to previous status."
  (interactive)
  (let ((pos))
    (setq pos (identica-get-previous-username-face-pos (point)))
    (if pos
	(goto-char pos)
      (message "Start of status."))))

(defun identica-get-next-username-face-pos (pos &optional object)
  "Returns the position of the next username after POS, or nil when end of string or buffer is reached."
  (interactive "P")
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (memq-face identica-username-face prop)))
	(setq pos (next-single-property-change pos 'face object))
	(when (eq pos nil) (throw 'not-found nil))
	(setq prop (get-text-property pos 'face object)))
      pos)))

(defun identica-get-previous-username-face-pos (pos &optional object)
  "Returns the position of the previous username before POS, or nil when start of string or buffer is reached."
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (memq-face identica-username-face prop)))
	(setq pos (previous-single-property-change pos 'face object))
	(when (eq pos nil) (throw 'not-found nil))
	(setq prop (get-text-property pos 'face object)))
      pos)))

(defun identica-goto-next-status-of-user ()
  "Go to next status of user."
  (interactive)
  (let ((user-name (identica-get-username-at-pos (point)))
	(pos (identica-get-next-username-face-pos (point))))
    (while (and (not (eq pos nil))
		(not (equal (identica-get-username-at-pos pos) user-name)))
      (setq pos (identica-get-next-username-face-pos pos)))
    (if pos
	(goto-char pos)
      (if user-name
	  (message "End of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun identica-goto-previous-status-of-user ()
  "Go to previous status of user."
  (interactive)
  (let ((user-name (identica-get-username-at-pos (point)))
	(pos (identica-get-previous-username-face-pos (point))))
    (while (and (not (eq pos nil))
		(not (equal (identica-get-username-at-pos pos) user-name)))
      (setq pos (identica-get-previous-username-face-pos pos)))
    (if pos
	(goto-char pos)
      (if user-name
	  (message "Start of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun identica-next-link nil
  (interactive)
  (goto-char (next-single-property-change (point) 'uri))
  (when (not (get-text-property (point) 'uri))
    (goto-char (next-single-property-change (point) 'uri))))

(defun identica-prev-link nil
  (interactive)
  (goto-char (previous-single-property-change (point) 'uri))
  (when (not (get-text-property (point) 'uri))
    (goto-char (previous-single-property-change (point) 'uri))))


(defun identica-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (when uri (browse-url uri))))

(defun identica-enter ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(id (get-text-property (point) 'id))
	(uri (get-text-property (point) 'uri))
        (group (get-text-property (point) 'group))
        (tag (get-text-property (point) 'tag)))
    (if group (identica-group-timeline group)
      (if tag (identica-tag-timeline tag)
        (if uri (browse-url uri)
          (if username
	      (identica-update-status identica-update-status-method
				      (concat "@" username " ") id)))))))


(defun identica-set-mode-string (loading identica-method server &optional buffer)
  "Change the `mode-name' so can display the current status of identica-mode.

It is needed the IDENTICA-METHOD and the SERVER, both are strings.
LOADING is a boolean that set the apropiate string that show to the user that identica-mode is working on something.
If BUFFER is not present or nil, set the `mode-name' in the `identica-buffer'."
  (let ((buff (or buffer (identica-buffer))))
    (with-current-buffer buff
      (let ((timeline-url
	     (concat server "/" identica-method)))
	(setq mode-name
	      (if loading (concat
			   (if (stringp loading) loading "loading")
			   " " timeline-url "...")
		timeline-url))
	;;	(debug-print mode-name)
	))))


(provide 'identica-major-mode)