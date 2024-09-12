;;; nefle.el --- Search the nearest file from a given file-system location
;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Philippe IVALDI
;;
;; Author: Philippe IVALDI <emacs@MY-NAME.me>
;; Maintainer: Philippe IVALDI <emacs@MY-NAME.me>
;; Created: September 11, 2024
;; Modified: September 11, 2024
;; Version: 0.0.1
;; Keywords: files file matching find nearest directory
;; Homepage: https://github.com/pi/nefle
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Emacs package to search the nearest file from a given file-system location
;;  This work is based on the zigler's code http://www.emacswiki.org/cgi-bin/wiki/UsingMakefileFromParentDirectory
;;
;;; Code:

;;;###autoload
(defun nefle-get-up-dir-containing-file (filename &optional start-dir plafond)
  "Search for the directory containing the given
filemane traversing up the directory tree.
Start from STAR-DIR if no nil otherwise start from the buffer file name
directory if exists otherwise start from `default-directory'.
If PLAFOND is not nil, don't search up the directory included."
  (interactive "sFilename to match : ")
  (when filename
    (setq filename (file-name-nondirectory filename))
    (when plafond (setq plafond (file-name-as-directory (expand-file-name plafond))))
    (let* ((start-dir (when start-dir
                        (if (file-directory-p start-dir)
                            start-dir (file-name-directory start-dir))))
           (current-dir (file-name-as-directory
                         (expand-file-name
                          (or start-dir
                              (if (buffer-file-name) (file-name-directory (buffer-file-name)))
                              default-directory))))
           (parent-dir (file-name-directory (directory-file-name current-dir)))
           nearest-dir file-path)
      ;; (insert current-dir "\n")
      (while (and
              (not (string= current-dir parent-dir))
              (or (null plafond) (not (string= current-dir plafond)))
              (not nearest-dir))
        (setq file-path (concat current-dir filename))
        (when (and (file-readable-p file-path) (not (file-directory-p file-path)))
          (setq nearest-dir current-dir))
        (setq current-dir parent-dir
              parent-dir (file-name-directory (directory-file-name parent-dir))))
      (if (called-interactively-p 'any)
          (if nearest-dir
              (find-file nearest-dir)
            (message "File not found…"))
        nearest-dir))))

(provide 'nefle)
;;; nefle.el ends here
