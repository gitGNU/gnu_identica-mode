
    ;; identica-users.el
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

    ;; Miércoles 08 De Agosto Del 2012    


;; PURPOSE:
;; ____________________
;;
;; Give support to identica-mode for getting and displaying user information.
;; 
;; ____________________

(require 'identica-translator)

(defvar identica-user-data nil
  "This is a cache for identica-user.el module.")

(defun identica-user-process-http-buffer ()
  "Process the HTTP buffer for getting user information. 

This functions leave the identica-timeline cache intact using another temporal variable `identica-user-data'."
  (identica-process-http-buffer identica-user-data) )

(defun identica-user-insert ()
  "Insert user information in the `current-buffer'."
  )
