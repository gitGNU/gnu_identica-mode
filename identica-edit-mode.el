
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



(defvar identica-update-status-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'identica-update-status-from-edit-buffer-send)
    (define-key map (kbd "C-c C-k") 'identica-update-status-from-edit-buffer-cancel)
    map))

(define-derived-mode identica-update-status-edit-mode text-mode "Identica Status Edit"
  (use-local-map identica-update-status-edit-map))

(defvar identica-update-status-edit-method-class)
(defvar identica-update-status-edit-method)
(defvar identica-update-status-edit-parameters)
(defvar identica-update-status-edit-reply-to-id)

(provide 'identica-edit-mode)