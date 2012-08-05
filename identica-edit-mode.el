
    ;; identica-edit-mode.el
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


(require 'identica-common-things)

(defgroup identica-edit-mode-faces nil
  "Identica edit-buffer mode Faces"
  :tag "Identica Edit Mode Faces"
  :group 'identica-mode
  :group 'faces)

(defvar identica-update-status-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'identica-update-status-from-edit-buffer-send)
    (define-key map (kbd "C-c C-k") 'identica-update-status-from-edit-buffer-cancel)
    map))


(defface identica-edit-username-face
  '(
    (t :underline t
       :foreground "OliveDrab3")    
    )
  "Face for usernames in edit-buffer"
  :group 'identica-edit-mode-faces)

(defface identica-edit-groupname-face
  '(
    (t :underline t
       :foreground "aquamarine")    
    )
  "Face for groupnames in edit-buffer"
  :group 'identica-edit-mode-faces)

(defface identica-edit-tagname-face
  '(
    (t :underline t
       :foreground "light coral")    
    )
  "Face for tagnames in edit-buffer"
  :group 'identica-edit-mode-faces)

(defface identica-edit-uri-face
  '(
    (t :inherit 'link)    
    )
  "Face for URIs links in edit-buffer"
  :group 'identica-edit-mode-faces)

(defface identica-edit-heart-face
  '(
    (t :foreground "firebrick1"
       :height 2.0)  
    )
  "Face for heart symbols in edit-buffer"
  :group 'identica-edit-mode-faces)

(defface identica-edit-redent-face
    '(
    (t :foreground "firebrick1" 
       :height 2.0)  
    )
    "Face for redents symbols in edit-buffer"
  :group 'identica-edit-mode-faces)

;;
(defvar identica-edit-mode-font-lock-keywords
  ;; font-lock-keywords  
  (list
   (cons identica-screen-name-regexp ''identica-edit-username-face) 
   (cons identica-group-name-regexp ''identica-edit-groupname-face)
   (cons identica-tag-name-regexp ''identica-edit-tagname-face)
   (cons identica-url-regexp ''identica-edit-uri-face)     
   (cons identica-heart-regexp ''identica-edit-heart-face)
   (cons identica-redent-regexp ''identica-edit-redent-face)
   )
  ;;
  "Font lock for `identica-edit-mode'"
  )

(define-derived-mode identica-edit-mode nil "Identica Status Edit"
  (use-local-map identica-update-status-edit-map)
  (set (make-local-variable 'font-lock-defaults)
       '(identica-edit-mode-font-lock-keywords)))

(defvar identica-update-status-edit-method-class)
(defvar identica-update-status-edit-method)
(defvar identica-update-status-edit-parameters)
(defvar identica-update-status-edit-reply-to-id)

(provide 'identica-edit-mode)