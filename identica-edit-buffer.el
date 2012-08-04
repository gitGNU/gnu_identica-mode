
    ;; identica-edit-buffer.el
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

    ;; Sábado 04 De Agosto Del 2012    


(require 'identica-edit-mode)

(defconst identica-edit-buffer-name "*identica-status-update-edit*"
  "Name of the edit buffer for identi.ca")

(defvar identica-edit-buffer-type nil
  "Type of the edit buffer, this variable can have one of these values:

- nil: Buffer is not being used or not setted.
- 'update-status : Buffer has to edit a status message.
- 'direct-message : Buffer has to edit a direct message.")

(defvar identica-edit-buffer-text-limit nil
  "Amount of characters to tell the user.

Ensure to set this before using the buffer. 

NOTE:
This variable is different from the identica-http.el module. This one is independent so you can use an edit buffer
for differents type of statusnet servers.

Remember that differents statusnet servers can have differents text-limits values.")

(defun identica-edit-buffer-startup (text-limit &optional init-str is-direct-message reply-to-id)
  "Initialize and show the edit-buffer.

TEXT-LIMIT is the amount of charactes the user can use. It will not restrict but is useful to alert the user that she/he has typed more that she/he should. See `identica-edit-buffer-text-limit' variable.

INIT-STR is the initial string, if nil use the empty string.

IS-DIRECT-MESSAGE if t then this status is a direct-message type.

REPLY-TO-ID tells to what status id the user is answering. Useful for conversations!"
  (or init-str (setq init-str ""))
  (setq identica-edit-buffer-text-limit text-limit)
  (let ((buf (get-buffer-create identica-edit-buffer-name)))    
    (pop-to-buffer buf)
    (with-current-buffer buf
      (fit-window-to-buffer (selected-window) 10 10)
      (identica-update-status-edit-mode)
      (when identica-soft-wrap-status
	(when (fboundp 'visual-line-mode)
	  (visual-line-mode t)))
      (identica-edit-buffer-set-mode-line)
      (insert init-str)
      (message "Type C-c C-c to post (C-c C-k to cancel)."))))
    
(defun identica-edit-buffer-set-mode-line ()
  "Acording to `identica-edit-buffer-type' set the line format properly"
  (setq mode-line-format 
	(cons 
	 (format "%s (%%i/%s) " 
		 (cond ((eq identica-edit-buffer-type 'update-status)		
			"identica update status")
		       ((eq identica-edit-buffer-type 'direct-message)
			"identica direct message")
		       )
		 identica-edit-buffer-text-limit)
	 mode-line-format)))

(provide 'identica-edit-buffer)