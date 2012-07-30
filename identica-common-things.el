
    ;; identica-common-things.el
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
;; Miscelaneous functions that doesn't fit anywhere! :P
;;
;; Common functions and variables that may be needed by any other module.
;; ____________________
;;

(defconst identica-mode-version "1.2.1")

(defun assoc-workaround (tag array)
  "Workaround odd semi-associative array returned by url-http."
  (or (assoc tag array)
      (and (equal tag (car array))
	   (cadr array))))


(defun identica-compare-statuses (a b)
  "Compare a pair of statuses.
For use as a predicate for sort."
  (< (assoc-default 'id b) (assoc-default 'id a)))

(provide 'identica-common-things)