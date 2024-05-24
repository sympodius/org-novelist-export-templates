;;; org-org-export-to-org.el --- Org Novelist export template to Org -*- lexical-binding: t; -*-

;; Example export template for Org Novelist.
;; Copyright (c) 2024 John Urquhart Ferguson
;;
;; Author: John Urquhart Ferguson <mail@johnurquhartferguson.info>
;; Maintainer: John Urquhart Ferguson <mail@johnurquhartferguson.info>
;; URL: https://johnurquhartferguson.info
;; Keywords: fiction, writing, outlines
;; Prefix: org-novelist
;; Package-Requires: ((emacs "28.1") (org "9.5.5"))

;; Version 0.0.3

;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see https://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Org Novelist is a methodology for writing novel-length fiction using
;; Org mode within Emacs. It involves creating and laying out Org mode
;; files such that notes and plans can be easily created and quickly
;; accessed while writing the main text of a story. Org Novelist's
;; secondary function is the ability to use this known structure to
;; easily export and publish stories to other formats. This package
;; supplies an example export to the standard Org mode clean Org
;; format (removing export directives for other systems, plus other
;; light processing).
;;
;; Creating, linking, and laying out files in the Org Novelist
;; methodology can be done without the use of Emacs or the Org Novelist
;; package, but using the package within Emacs will provide helper
;; functions that make the methodology much easier to use; allowing the
;; following of links, programmatic updating of cross-references, and
;; ability to programmatically export to other formats.
;;
;; Installation, Activation, and Documentation
;; -------------------------------------------
;; See the corresponding section of the website at
;;
;;   https://johnurquhartferguson.info
;;
;;; Code:

;;;; Require other packages

(require 'org)  ; Org Novelist is built upon the incredible work of Org mode


(defun ooeto--string-to-file (str filename)
  "Create/Overwrite FILENAME with the contents of STR."
  (catch 'FILE-NOT-WRITABLE
    (if (get-file-buffer filename)
        ;; Filename already open in a buffer. Update buffer and save.
        (with-current-buffer (get-file-buffer filename)
          (erase-buffer)
          (insert str)
          (save-buffer))  ; Calling `save-buffer' with an argument of 0 would stop back-up files being created, but it's probably best to respect the user's Emacs setup in this regard
      ;; Filename not open in a buffer. Just deal with file.
      (with-temp-buffer
        (insert str)
        ;; If directory doesn't exist, create it.
        (unless (file-exists-p (file-name-directory filename))
          (make-directory (file-name-directory filename) t))
        (if (file-writable-p filename)
            (write-region (point-min) (point-max) filename)
          (progn
            (error (concat filename " is not writable"))
            (throw 'FILE-NOT-WRITABLE (concat filename " is not writable"))))))))


;;;; Required Entry Point Function for Org Novelist Export

(defun org-novelist--export-template (org-input-file output-file)
  "Given an ORG-INPUT-FILE from Org Novelist, export to OUTPUT-FILE."
  (let ((org-export-with-toc-orig nil)
        (org-export-with-title-orig nil)
        (org-export-with-author-orig nil)
        (org-export-with-email-orig nil)
        (org-export-with-date-orig nil)
        (org-export-with-latex-orig nil)
        (org-export-backends-orig nil))
    (when (boundp 'org-export-with-toc)
      (setq org-export-with-toc-orig org-export-with-toc))
    (when (boundp 'org-export-with-title)
      (setq org-export-with-title-orig org-export-with-title))
    (when (boundp 'org-export-with-author)
      (setq org-export-with-author-orig org-export-with-author))
    (when (boundp 'org-export-with-email)
      (setq org-export-with-email-orig org-export-with-email))
    (when (boundp 'org-export-with-date)
      (setq org-export-with-date-orig org-export-with-date))
    (when (boundp 'org-export-with-latex)
      (setq org-export-with-latex-orig org-export-with-latex))
    (when (boundp 'org-export-backends)
      (setq org-export-backends-orig org-export-backends))
    (setq org-export-with-toc t)
    (setq org-export-with-title t)
    (setq org-export-with-author t)
    (setq org-export-with-email t)
    (setq org-export-with-date t)
    (setq org-export-with-latex t)
    (progn
      (setq org-export-registered-backends
            (cl-remove-if-not
             (lambda (backend)
               (let ((name (org-export-backend-name backend)))
                 (or (memq name (quote (ascii html icalendar latex odt md org)))
                     (catch 'parentp
                       (dolist (b (quote (ascii html icalendar latex odt md org)))
                         (and (org-export-derived-backend-p b name)
                              (throw 'parentp t)))))))
             org-export-registered-backends))
      (let ((new-list (mapcar #'org-export-backend-name
                              org-export-registered-backends)))
        (dolist (backend (quote (ascii html icalendar latex odt md org)))
          (cond
           ((not (load (format "ox-%s" backend) t t))
            (message "Problems while trying to load export back-end `%s'"
                     backend))
           ((not (memq backend new-list)) (push backend new-list))))
        (set-default 'org-export-backends new-list)))
    (find-file org-input-file)
    (ooeto--string-to-file (org-export-as 'org) (concat (file-name-sans-extension org-input-file) "_temp.org"))
    (setq org-export-with-toc org-export-with-toc-orig)
    (setq org-export-with-title org-export-with-title-orig)
    (setq org-export-with-author org-export-with-author-orig)
    (setq org-export-with-email org-export-with-email-orig)
    (setq org-export-with-date org-export-with-date-orig)
    (setq org-export-with-latex org-export-with-latex-orig)
    (progn
      (setq org-export-registered-backends
            (cl-remove-if-not
             (lambda (backend)
               (let ((name (org-export-backend-name backend)))
                 (or (memq name org-export-backends-orig)
                     (catch 'parentp
                       (dolist (b org-export-backends-orig)
                         (and (org-export-derived-backend-p b name)
                              (throw 'parentp t)))))))
             org-export-registered-backends))
      (let ((new-list (mapcar #'org-export-backend-name
                              org-export-registered-backends)))
        (dolist (backend org-export-backends-orig)
          (cond
           ((not (load (format "ox-%s" backend) t t))
            (message "Problems while trying to load export back-end `%s'"
                     backend))
           ((not (memq backend new-list)) (push backend new-list))))
        (set-default 'org-export-backends new-list)))
    (make-directory (file-name-directory output-file) t)
    (rename-file (concat (file-name-sans-extension org-input-file) "_temp.org") output-file t)))

(provide 'org-org-export-to-org)
;;; org-org-export-to-org.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ooeto-" . "org-org-export-to-org-"))
;; End:
