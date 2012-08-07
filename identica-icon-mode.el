
    ;; identica-icon-mode.el
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

    ;; Lunes 06 De Agosto Del 2012    


;; PURPOSE:
;; ____________________
;;
;; Give icon support for identica-mode so you can se faces!
;;
;; You need the wget program so you can download from Internet.
;;
;; ____________________

(require 'image)

(defvar identica-icon-tmp-images-dir
  (expand-file-name (concat "identicamode-images-" (user-login-name))
		    temporary-file-directory)
  "Where the images are stored temporary?")

(defvar identica-icon-image-stack nil

  "Stack of images URL to download.")

(defun identica-icon-image-type (file-name)
  "Return the type symbol depending on the extension."
  (cond
   ((string-match "\\.jpe?g" file-name) 'jpeg)
   ((string-match "\\.png" file-name) 'png)
   ((string-match "\\.gif" file-name) 'gif)
   (t nil)))

(defun identica-icon-get-icons ()
  "Retrieve icons if icon-mode is active.

The icons to retrieve must be stored in `identica-icon-image-stack'."
  (if (and identica-icon-image-stack window-system)
      (let ((proc
	     (apply
	      #'start-process
	      "wget-images"
	      nil
	      "wget"
	      (format "--directory-prefix=%s" identica-icon-tmp-images-dir)
	      "--no-clobber"
	      "--no-directories"
	      "--quiet"
	      identica-icon-image-stack)))
	(set-process-sentinel
	 proc
	 (lambda (proc stat)
	   (clear-image-cache)
	   )))))

(defun identica-icon-insert-image (profile-image-url)
  "Insert the image given by the URL PROFILE-IMAGE-URL (if exists in the temporal dir) in the `current-buffer'.

The image is supposed to be downloaded in the `identica-icon-tmp-images-dir'.

If the image has been downloaded, then the function will return nil. Otherwise, return t."
  (let ((filename (identica-icon-get-tmp-filename profile-image-url)))
    (when filename
      (let ((avatar (create-image (concat identica-icon-tmp-images-dir filename))))
	;; Make sure the avatar is 48 pixels (which it should already be!, but hey...)
	;; For offenders, the top left slice of 48 by 48 pixels is displayed
	;; TODO: perhaps make this configurable?
	(insert-image avatar nil nil `(0 0 48 48))))))

(defun identica-icon-get-tmp-filename (profile-image-url)
  "Return only the filename if it exists in the temporary directory given by variable `identica-icon-tmp-images-dir'.

If the file doesn't exists, the return nil."
  (string-match "/\\([^/?]+\\)\\(?:\\?\\|$\\)" profile-image-url) ;; match the filename from the URL
  (let ((filename (match-string-no-properties 1 profile-image-url))
	(xfilename (match-string-no-properties 0 profile-image-url)))
    (cond
     ((file-exists-p (concat identica-icon-tmp-images-dir filename))
      filename)
     ((file-exists-p (concat identica-icon-tmp-images-dir xfilename))
      xfilename)
     (t nil))))

(defun identica-icon-set-for-download (profile-image-url)
  "Mark the PROFILE-IMAGE-URL for downloading if the image hasn't been downloaded before."
  (unless (identica-icon-get-tmp-filename profile-image-url)
    (add-to-list 'identica-icon-image-stack profile-image-url)))

(provide 'identica-icon-mode)