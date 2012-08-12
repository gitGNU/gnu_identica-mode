
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

(defun identica-icon-get-icons (&optional overide)
  "Retrieve icons if icon-mode is active.

The icons to retrieve must be stored in `identica-icon-image-stack'.

If OVERIDE is t then download no matter if it was downloaded before."
  (if (and identica-icon-image-stack window-system)
      (let ((proc
	     (apply
	      #'start-process
	      "wget-images"
	      nil
	      "wget"
	      (format "--directory-prefix=%s" identica-icon-tmp-images-dir)
	      (if overide 
		  "" 
		"--no-clobber")
	      "--no-directories"
	      "--quiet"
	      identica-icon-image-stack)))
	(set-process-sentinel
	 proc
	 (lambda (proc stat)
	   (clear-image-cache)
	   (setq identica-icon-image-stack nil)
	   (when identica-icon-finish-sentinel	     
	     (apply identica-icon-finish-sentinel 
		    identica-icon-finish-sentinel-parameters))
	   )))))

(defvar identica-icon-finish-sentinel nil
  "If setted then use this function when `identica-icon-get-icons' function finish download every icon image.")
(defvar identica-icon-finish-sentinel-parameters nil
  "This is the parameters that the function `identica-icon-finish-sentinel' will be called.")

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

(defun identica-icon-get-image-string (profile-image-url)
  "Return a string with the image, this image has been inserted as a property."
  (let ((filename (identica-icon-get-tmp-filename profile-image-url t)))
    (propertize " " 'display (cons 'image 
				   (list :type (identica-icon-image-type filename)
					 :file filename)))))
    


(defun identica-icon-get-tmp-filename (profile-image-url &optional no-check)
  "Return only the filename if it exists in the temporary directory given by variable `identica-icon-tmp-images-dir'.

If the file doesn't exists, the return nil.

If NO-CHECK if t then, don't check if the temporary filename exists."
  (string-match "/\\([^/?]+\\)\\(?:\\?\\|$\\)" profile-image-url) ;; match the filename from the URL
  (let ((filename (match-string-no-properties 1 profile-image-url))
	(xfilename (match-string-no-properties 0 profile-image-url)))
    (cond
     ((file-exists-p (concat identica-icon-tmp-images-dir filename))
      (concat identica-icon-tmp-images-dir filename))
     ((file-exists-p (concat identica-icon-tmp-images-dir xfilename))
      (concat identica-icon-tmp-images-dir xfilename))
     (no-check
      (concat identica-icon-tmp-images-dir xfilename))
     (t nil))))

(defun identica-icon-set-for-download (profile-image-url &optional overide)
  "Mark the PROFILE-IMAGE-URL for downloading if the image hasn't been downloaded before.

If OVERIDE is t, then insert it in the image stack no matter if it was downloaded."
  (unless (and (not overide)
	       (identica-icon-get-tmp-filename profile-image-url))
    (add-to-list 'identica-icon-image-stack profile-image-url)))

(provide 'identica-icon-mode)