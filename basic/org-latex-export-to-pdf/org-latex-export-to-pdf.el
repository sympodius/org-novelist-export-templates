;;; org-latex-export-to-pdf.el --- Org Novelist export template to PDF -*- lexical-binding: t; -*-

;; Example export template for Org Novelist.
;; Copyright (c) 2023 John Urquhart Ferguson
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
;; supplies an example export to the standard Org mode PDF format.
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


;;;; Required Entry Point Function for Org Novelist Export

(defun org-novelist--export-template (org-input-file output-file)
  "Given an ORG-INPUT-FILE from Org Novelist, export to OUTPUT-FILE."
  (let ((org-export-with-toc-orig nil)
        (org-export-with-title-orig nil)
        (org-export-with-author-orig nil)
        (org-export-with-email-orig nil)
        (org-export-with-date-orig nil)
	(org-export-with-latex-orig nil))
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
    (setq org-export-with-toc t)
    (setq org-export-with-title t)
    (setq org-export-with-author t)
    (setq org-export-with-email t)
    (setq org-export-with-date t)
    (setq org-export-with-latex t)
    (find-file org-input-file)
    (org-latex-export-to-pdf)
    (setq org-export-with-toc org-export-with-toc-orig)
    (setq org-export-with-title org-export-with-title-orig)
    (setq org-export-with-author org-export-with-author-orig)
    (setq org-export-with-email org-export-with-email-orig)
    (setq org-export-with-date org-export-with-date-orig)
    (setq org-export-with-latex org-export-with-latex-orig)
    (make-directory (file-name-directory output-file) t)
    (rename-file (concat (file-name-sans-extension org-input-file) ".pdf") output-file t)
    (rename-file (concat (file-name-sans-extension org-input-file) ".tex") (concat (file-name-sans-extension output-file) ".tex") t)
    (delete-file (concat (file-name-sans-extension org-input-file) ".ilg"))
    (delete-file (concat (file-name-sans-extension org-input-file) ".ind"))))

(provide 'org-latex-export-to-pdf)
;;; org-latex-export-to-pdf.el ends here
