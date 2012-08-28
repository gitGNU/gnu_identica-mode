
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



					; ____________________
					; Regexps for identifing texts

(defconst identica-screen-name-regexp "@\\([_[:word:]0-9]+\\)"
  "Regexp for user-names.")

(defconst identica-group-name-regexp "!\\([_[:word:]0-9\-]+\\)"
  "Regexp for group-names.")

(defconst identica-tag-name-regexp "#\\([_[:word:]0-9\-]+\\)"
  "Regexp for tag-names.")

(defconst identica-url-regexp "\\(ur1\.ca/[a-z0-9]+/?\\|https?://[-_.!~*'()[:word:]0-9\;/?:@&=+$,%#]+\\)"
  "Regexp for http URLs and ur1 shorter.")

(defconst identica-heart-regexp "❤"
  "Regexp for the favored heart char.")

(defconst identica-redent-regexp "[♺♻]"
  "Regexp for the redent recycle char.")

(provide 'identica-common-things)
