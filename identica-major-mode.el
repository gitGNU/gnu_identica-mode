
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


(require 'identica-mode)

(defvar identica-mode-map (make-sparse-keymap "Identi.ca"))
(defvar menu-bar-identica-mode-menu nil)

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

(defun identica-mode ()
  "Major mode for Identica."
  (interactive)
  (identica-autoload-oauth)
  (switch-to-buffer (identica-buffer))
  (buffer-disable-undo (identica-buffer))
  (kill-all-local-variables)
  (identica-mode-init-variables)
  (use-local-map identica-mode-map)
  (setq major-mode 'identica-mode)
  (setq mode-name identica-mode-string)
  (setq mode-line-buffer-identification
	`(,(default-value 'mode-line-buffer-identification)
	  (:eval (identica-mode-line-buffer-identification))))
  (identica-update-mode-line)
  (set-syntax-table identica-mode-syntax-table)
  (font-lock-mode -1)
  (if identica-soft-wrap-status
      (if (fboundp 'visual-line-mode)
          (visual-line-mode t)
	(if (fboundp 'longlines-mode)
	    (longlines-mode t))))
  (identica-retrieve-configuration) 
  (add-hook 'kill-buffer-hook 'identica-kill-buffer-function)
  (run-mode-hooks 'identica-mode-hook))


(defun identica-mode-init-variables ()
  ;; (make-variable-buffer-local 'variable)
  ;; (setq variable nil)
  (make-variable-buffer-local 'identica-active-mode)
  (set-default 'identica-active-mode t)
  (font-lock-mode -1)
  (defface identica-username-face
    `((t nil)) "" :group 'faces)
  (set-face-attribute 'identica-username-face nil :underline t)

  (defface identica-reply-face
    `((t nil)) "" :group 'faces)
  (set-face-attribute 'identica-reply-face nil :background "DarkSlateGray")

  (defface identica-stripe-face
    `((t nil)) "" :group 'faces)
  (set-face-attribute 'identica-stripe-face nil :background "LightSlateGray")

  (defface identica-highlight-face
    `((t nil)) "" :group 'faces)
  (set-face-attribute 'identica-highlight-face nil :background "SlateGray")

  (defface identica-uri-face
    `((t nil)) "" :group 'faces)
  (set-face-attribute 'identica-uri-face nil :underline t)

  (defface identica-heart-face
    `((t nil)) "" :group 'faces)
  (set-face-attribute 'identica-heart-face nil :foreground "firebrick1" :height 2.0)  

  (add-to-list 'minor-mode-alist '(identica-icon-mode " id-icon"))
  (add-to-list 'minor-mode-alist '(identica-scroll-mode " id-scroll"))

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



