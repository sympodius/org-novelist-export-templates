;;; org-latex-export-to-pdf-tradeback-cubes-en-us.el --- Org Novelist export template to PDF US Tradeback -*- lexical-binding: t; -*-

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


;;;; User Variables

(defvar oletptceu--typeface-size 0.795 "Typeface size (fraction of normal) for the document text.")
(defvar oletptceu--monofont-typeface-size-adjustment 1.000 "Typeface size (fraction of oletptceu--typeface-size) for the document text.")
(defvar oletptceu--typeface-size-chapter 20 "Typeface size (pt) for the chapter heading text.")
(defvar oletptceu--typeface-size-section 15 "Typeface size (pt) for the section heading text.")
(defvar oletptceu--typeface-size-subsection 11 "Typeface size (pt) for the subsection heading text.")
(defvar oletptceu--typeface-size-subsubsection 11 "Typeface size (pt) for the subsubsection heading text.")
(defvar oletptceu--typeface-size-paragraph 11 "Typeface size (pt) for the paragraph heading text.")
(defvar oletptceu--typeface-size-subparagraph 11 "Typeface size (pt) for the subparagraph heading text.")
(defvar oletptceu--mainfont "Libre Baskerville" "Main text font, must be installed on system already.")
(defvar oletptceu--sansfont "Josefin Sans" "Main text font, must be installed on system already.")
(defvar oletptceu--monofont "DejaVu Sans Mono" "Main text font, must be installed on system already.")
(defvar oletptceu--signaturefont "Alegreya SC" "Main text font, must be installed on system already.")
(defvar oletptceu--title-page-graphic "/home/sympodius/Git/sympodius/org-novelist-export-templates/org-latex-export-to-pdf-tradeback-cubes-en-us/cubes.png" "Location of image file to use in title page.")
(defvar oletptceu--isbn "" "ISBN number of book, if there is one.")
(defvar oletptceu--edition "Early Draft Edition" "Text describing this edition.")
(defvar oletptceu--rights "Creative Commons Attribution-Non-Commercial-ShareAlike 4.0 International License" "Copyright statement or license.")
(defvar oletptceu--sigil-graphic "/home/sympodius/Git/sympodius/org-novelist-export-templates/org-latex-export-to-pdf-tradeback-cubes-en-us/juf-sigil.pdf" "Location of image file to use as sigil in legal page.")
(defvar oletptceu--make-booklet t "Also output an imposed booklet version of PDF for bookbinding.")
(defvar oletptceu--signature-size 6 "The number of pieces of paper you wish to use in your booklet signatures for bookbinding.")
(defvar oletptceu--booklet-buffer-pages 2 "The number of blank pages to add to the start and end of your booklet for bookbinding.")



;;;; Global Variables
(defvar oletptceu--fm-found nil "Temporary variable to show at least one front matter chapter found.")
(defvar oletptceu--mm-found nil "Temporary variable to show at least one main matter chapter found.")
(defvar oletptceu--bm-found nil "Temporary variable to show at least one back matter chapter found.")


;;;; Helper Functions

(defun oletptceu--fold-show-all ()
  "Run the deprecated `org-show-all' when Org version is less than 9.6.
Otherwise, run `org-fold-show-all'."
  (if (or (> (string-to-number (nth 0 (split-string (org-version) "\\."))) 9)
	  (and (= (string-to-number (nth 0 (split-string (org-version) "\\."))) 9)
               (>= (string-to-number (nth 1 (split-string (org-version) "\\."))) 6)))
      (org-fold-show-all)
    (org-show-all)))

(defun oletptceu--format-time-string (format-string &optional time-zone)
  "Run the deprecated `org-format-time-string' when Org version is less than 9.6.
Otherwise, run `format-time-string'.
FORMAT-STRING is the output format.
TIME-ZONE is the given time. If omitted or nil, use local time."
  (if (or (> (string-to-number (nth 0 (split-string (org-version) "\\."))) 9)
	  (and (= (string-to-number (nth 0 (split-string (org-version) "\\."))) 9)
               (>= (string-to-number (nth 1 (split-string (org-version) "\\."))) 6)))
      (format-time-string format-string time-zone)
    (org-format-time-string format-string time-zone)))

(defun oletptceu--set-file-property-value (property value &optional file no-overwrite)
  "Given a FILE and VALUE, change PROPERTY value of that file.
If property not found, add it.
If no file given, attmept to use current buffer.
If NO-OVERWRITE is t, don't replace existing property, just add new one."
  (when file
    (when (file-exists-p file)
      (when (file-readable-p file)
        (find-file file))))
  (let* ((regexp (format "^[ \t]*#\\+%s:" (regexp-quote property)))
         (case-fold-search t)
         (property-found-p nil))
    (goto-char (point-min))
    (while (and (re-search-forward regexp nil t) (not no-overwrite))
      (setq property-found-p t)
      (insert " ")
      (delete-region (point) (line-end-position))
      (insert value))
    (unless property-found-p
      (goto-char (point-min))
      (end-of-line)
      (insert (format "\n\#\+%s\: %s" property value)))))

(defun oletptceu--string-to-file (str filename)
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

(defun oletptceu--delete-current-file (&optional no-prompt)
  "Delete the file associated with the current buffer.
Kill the current buffer too. If no file is associated, just kill buffer without
prompt for save. If NO-PROMPT is non-nil, don't ask user for confirmation."
  (let ((current-file (buffer-file-name)))
    (if no-prompt
        (progn
          (set-buffer-modified-p nil)
          (kill-buffer (current-buffer))
          (when current-file
            (delete-file current-file)))
      (when (yes-or-no-p (concat "Delete file? " current-file " "))
        (kill-buffer (current-buffer))
        (when current-file
          (delete-file current-file))))))

(defun oletptceu--delete-line ()
  "If Emacs version is less than 29, delete line the old fashioned way."
  (let ((inhibit-field-text-motion t))
    (if (>= (string-to-number (nth 0 (split-string (string-trim-left (emacs-version) "GNU Emacs ") "\\."))) 29)
        (delete-line)
      (delete-region (line-beginning-position) (line-beginning-position 2)))))

(defun oletptceu--get-file-property-value (file property)
  "Given an Org FILE, return the value of PROPERTY."
  (let ((value "")
        (regexp (format "^[ \t]*#\\+%s:" (regexp-quote property)))
        (case-fold-search t)
        beg)
    (with-temp-buffer
      (when (file-exists-p file)
        (when (file-readable-p file)
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward regexp nil t)
            (when (looking-at-p " ")
              (forward-char))
            (setq beg (point))
            (end-of-line)
            (setq value (org-trim (buffer-substring beg (point))))))))
    value))

(defun oletptceu--get-file-properties-and-values (file)
  "Given a FILE, return the properties and values."
  (let ((property-list '())
        (regexp-start "^[ \t]*#\\+")
        (regexp-end ": ")
        (case-fold-search t)
        beg
        beg-line-num)
    (with-temp-buffer
      (when (file-exists-p file)
        (when (file-readable-p file)
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward regexp-start nil t)
            (setq beg (point))
            (setq beg-line-num (line-number-at-pos))
            (when (re-search-forward regexp-end nil t)
              (when (= beg-line-num (line-number-at-pos))
                (forward-char -2)
                (push `(,(org-trim (buffer-substring beg (point))) . ,(org-trim (buffer-substring (+ (point) 2) (line-end-position)))) property-list)))))))
    property-list))


;;;; Required Entry Point Function for Org Novelist Export

(defun org-novelist--export-template (org-input-file output-file)
  "Given an ORG-INPUT-FILE from Org Novelist, export to OUTPUT-FILE."
  (let* ((temp-org (concat (file-name-sans-extension output-file) ".org"))
         (org-export-with-toc-orig nil)
         (org-export-with-date-orig nil)
         (org-export-with-tags-orig nil)
         (org-export-with-email-orig nil)
         (org-export-with-latex-orig nil)
         (org-export-with-tasks-orig nil)
         (org-export-with-title-orig nil)
         (org-export-with-author-orig nil)
         (org-export-with-clocks-orig nil)
         (org-export-with-tables-orig nil)
         (org-export-with-creator-orig nil)
         (org-export-with-drawers-orig nil)
         (org-export-with-entities-orig nil)
         (org-export-with-planning-orig nil)
         (org-export-with-priority-orig nil)
         (org-export-with-emphasize-orig nil)
         (org-export-with-footnotes-orig nil)
         (org-export-with-properties-orig nil)
         (org-export-with-timestamps-orig nil)
         (org-export-with-fixed-width-orig nil)
         (org-export-with-inlinetasks-orig nil)
         (org-export-with-broken-links-orig nil)
         (org-export-with-smart-quotes-orig nil)
         (org-export-with-todo-keywords-orig nil)
         (org-export-with-archived-trees-orig nil)
         (org-export-with-section-numbers-orig nil)
         (org-export-with-special-strings-orig nil)
         (org-export-with-sub-superscripts-orig nil)
         (org-use-sub-superscripts-orig nil)
         (org-export-with-statistics-cookies-orig nil)
         (undo-tree-auto-save-history-orig nil)
         (chap-format (concat "\\titleformat{\\chapter}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-chapter) "}{" (number-to-string oletptceu--typeface-size-chapter) "}\\selectfont\\chaptername\\,\\thechapter}{0pt}{~\\linebreak\\fontsize{" (number-to-string oletptceu--typeface-size-chapter) "}{" (number-to-string oletptceu--typeface-size-chapter) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]"))
         (sec-format (concat "\\titleformat{\\section}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-section) "}{" (number-to-string oletptceu--typeface-size-section) "}\\selectfont\\thesection}{0pt}{\\,\\,\\,~\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-section) "}{" (number-to-string oletptceu--typeface-size-section) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]"))
         (subsec-format (concat "\\titleformat{\\subsection}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subsection) "}{" (number-to-string oletptceu--typeface-size-subsection) "}\\selectfont\\thesubsection}{0pt}{\\,\\,\\,~\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-subsection) "}{" (number-to-string oletptceu--typeface-size-subsection) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]"))
         (subsubsec-format (concat "\\titleformat{\\subsubsection}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subsubsection) "}{" (number-to-string oletptceu--typeface-size-subsubsection) "}\\selectfont\\thesubsubsection}{0pt}{\\,\\,\\,~\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-subsubsection) "}{" (number-to-string oletptceu--typeface-size-subsubsection) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]"))
	 (paragraph-format (concat "\\titleformat{\\paragraph}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-paragraph) "}{" (number-to-string oletptceu--typeface-size-paragraph) "}\\selectfont\\theparagraph}{0pt}{\\,\\,\\,~\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-paragraph) "}{" (number-to-string oletptceu--typeface-size-paragraph) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]"))
	 (subparagraph-format (concat "\\titleformat{\\subparagraph}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subparagraph) "}{" (number-to-string oletptceu--typeface-size-subparagraph) "}\\selectfont\\thesubparagraph}{0pt}{\\,\\,\\,~\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-subparagraph) "}{" (number-to-string oletptceu--typeface-size-subparagraph) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]"))
         curr-heading
         curr-level
         curr-matter
         beg
         curr-properties-list
         curr-property
         (toc-head-string "")
         (no-header nil)
         (no-header-name nil)
         (no-header-preamble nil)
         (no-pagestyle nil)
         (no-toc-entry nil)
         (pagestyle nil))
    ;;  Store original user-set Org export settings.
    (when (boundp 'org-export-with-toc)
      (setq org-export-with-toc-orig org-export-with-toc))
    (when (boundp 'org-export-with-date)
      (setq org-export-with-date-orig org-export-with-date))
    (when (boundp 'org-export-with-tags)
      (setq org-export-with-tags-orig org-export-with-tags))
    (when (boundp 'org-export-with-email)
      (setq org-export-with-email-orig org-export-with-email))
    (when (boundp 'org-export-with-latex)
      (setq org-export-with-latex-orig org-export-with-latex))
    (when (boundp 'org-export-with-tasks)
      (setq org-export-with-tasks-orig org-export-with-tasks))
    (when (boundp 'org-export-with-title)
      (setq org-export-with-title-orig org-export-with-title))
    (when (boundp 'org-export-with-author)
      (setq org-export-with-author-orig org-export-with-author))
    (when (boundp 'org-export-with-clocks)
      (setq org-export-with-clocks-orig org-export-with-clocks))
    (when (boundp 'org-export-with-tables)
      (setq org-export-with-tables-orig org-export-with-tables))
    (when (boundp 'org-export-with-creator)
      (setq org-export-with-creator-orig org-export-with-creator))
    (when (boundp 'org-export-with-drawers)
      (setq org-export-with-drawers-orig org-export-with-drawers))
    (when (boundp 'org-export-with-entities)
      (setq org-export-with-entities-orig org-export-with-entities))
    (when (boundp 'org-export-with-planning)
      (setq org-export-with-planning-orig org-export-with-planning))
    (when (boundp 'org-export-with-priority)
      (setq org-export-with-priority-orig org-export-with-priority))
    (when (boundp 'org-export-with-emphasize)
      (setq org-export-with-emphasize-orig org-export-with-emphasize))
    (when (boundp 'org-export-with-footnotes)
      (setq org-export-with-footnotes-orig org-export-with-footnotes))
    (when (boundp 'org-export-with-properties)
      (setq org-export-with-properties-orig org-export-with-properties))
    (when (boundp 'org-export-with-timestamps)
      (setq org-export-with-timestamps-orig org-export-with-timestamps))
    (when (boundp 'org-export-with-fixed-width)
      (setq org-export-with-fixed-width-orig org-export-with-fixed-width))
    (when (boundp 'org-export-with-inlinetasks)
      (setq org-export-with-inlinetasks-orig org-export-with-inlinetasks))
    (when (boundp 'org-export-with-broken-links)
      (setq org-export-with-broken-links-orig org-export-with-broken-links))
    (when (boundp 'org-export-with-smart-quotes)
      (setq org-export-with-smart-quotes-orig org-export-with-smart-quotes))
    (when (boundp 'org-export-with-todo-keywords)
      (setq org-export-with-todo-keywords-orig org-export-with-todo-keywords))
    (when (boundp 'org-export-with-archived-trees)
      (setq org-export-with-archived-trees-orig org-export-with-archived-trees))
    (when (boundp 'org-export-with-section-numbers)
      (setq org-export-with-section-numbers-orig org-export-with-section-numbers))
    (when (boundp 'org-export-with-special-strings)
      (setq org-export-with-special-strings-orig org-export-with-special-strings))
    (when (boundp 'org-export-with-sub-superscripts)
      (setq org-export-with-sub-superscripts-orig org-export-with-sub-superscripts))
    (when (boundp 'org-use-sub-superscripts)
      (setq org-use-sub-superscripts-orig org-use-sub-superscripts))
    (when (boundp 'org-export-with-statistics-cookies)
      (setq org-export-with-statistics-cookies-orig org-export-with-statistics-cookies))
    (when (boundp 'undo-tree-auto-save-history)
      (setq undo-tree-auto-save-history-orig undo-tree-auto-save-history))
    (setq org-export-with-toc nil)
    (setq org-export-with-date nil)
    (setq org-export-with-tags t)
    (setq org-export-with-email nil)
    (setq org-export-with-latex t)
    (setq org-export-with-tasks t)
    (setq org-export-with-title nil)
    (setq org-export-with-author nil)
    (setq org-export-with-clocks nil)
    (setq org-export-with-tables t)
    (setq org-export-with-creator nil)
    (setq org-export-with-drawers t)
    (setq org-export-with-entities t)
    (setq org-export-with-planning t)
    (setq org-export-with-priority t)
    (setq org-export-with-emphasize t)
    (setq org-export-with-footnotes t)
    (setq org-export-with-properties t)
    (setq org-export-with-timestamps t)
    (setq org-export-with-fixed-width t)
    (setq org-export-with-inlinetasks t)
    (setq org-export-with-broken-links 'mark)
    (setq org-export-with-smart-quotes t)
    (setq org-export-with-todo-keywords t)
    (setq org-export-with-archived-trees nil)
    (setq org-export-with-section-numbers t)
    (setq org-export-with-special-strings t)
    (setq org-export-with-sub-superscripts t)
    (setq org-use-sub-superscripts '{})
    (setq org-export-with-statistics-cookies t)
    (when (file-exists-p org-input-file)
      (when (file-readable-p org-input-file)
        (with-temp-buffer
          (insert-file-contents org-input-file)
          (org-mode)
          (oletptceu--fold-show-all)
	  ;; Add index flags to the story. Doing this here, before any other processing, ensures we won't include things like title pages and copyright pages in the index.
	  (goto-char (point-min))
	  (insert "\n")
	  (goto-char (point-min))
          (setq curr-properties-list (oletptceu--get-file-properties-and-values org-input-file))
          (while curr-properties-list
            (setq curr-property (pop curr-properties-list))
            (when (string= (car curr-property) "ORG_NOVELIST_INDEX_ENTRY")
              (let ((case-fold-search t)
                    (curr-term (cdr curr-property))
                    (pos (point-min))
		    (search-bound-pos (point-max))
		    (curr-term-insert-str ""))
                (when (> (length (split-string curr-term "!" t " ")) 1)
                  (setq curr-term (car (last (split-string curr-term "!" t " ")))))
		(while (not (org-next-visible-heading 1))
		  ;; Don't include glossary entries in the index (keep in mind that this template is intended for en-US language).
		  (unless (string= (downcase (nth 4 (org-heading-components))) "glossary")
		    ;; Don't include heading appearances of term in the index.
		    (forward-line)
		    (beginning-of-line)
		    (setq pos (point))
		    (if (not (org-next-visible-heading 1))
			(progn
			  ;; Don't include heading appearances of term in the index.
			  (beginning-of-line)
			  (forward-char -1)
			  (setq search-bound-pos (point)))
		      (setq search-bound-pos (point-max)))
		    (goto-char pos)
                    (while (re-search-forward (format "[[:space:][:punct:]]+?%s\\('s\\)?[[:punct:][:space:]]+?" (regexp-quote curr-term)) search-bound-pos t)
                      ;; Check insert not already done in previous loop.
                      (setq pos (point))
                      (unless (or (looking-at-p "@@latex:\\\\index{") (looking-at-p "}?@@"))
			(goto-char pos)
                        ;; Don't match Document or Section properties.
                        (unless (or (looking-at-p "^[ \t]*#\\+") (looking-at-p "^[ \t]*:+?[^\s]+?:+?"))
                          (goto-char pos)
                          (setq curr-term-insert-str (concat "@@latex:\\index{" (cdr curr-property) "}@@"))
                          (insert curr-term-insert-str)
                          ;; Increase search-bound-pos by the number of characters we've added.
                          (setq search-bound-pos (+ search-bound-pos (length curr-term-insert-str))))))))))
            (goto-char (point-min)))
          (goto-char (point-min))
          (oletptceu--delete-line)
          ;; Only print an Index at the end of the story if user requested it.
          (when (member "index" (split-string (oletptceu--get-file-property-value org-input-file "GENERATE") "[,\f\t\n\r\v]+" t " "))
            (goto-char (point-max))
            (insert "\n#+LATEX: \\printindex\n"))
          ;; Set fallback fonts if user selections not found.
          (cond
           ((find-font (font-spec :name oletptceu--mainfont))
            (setq oletptceu--mainfont oletptceu--mainfont))
           (t
            (setq oletptceu--mainfont "cmr")))
          (cond
           ((find-font (font-spec :name oletptceu--sansfont))
            (setq oletptceu--sansfont oletptceu--sansfont))
           (t
            (setq oletptceu--sansfont "cmss")))
          (cond
           ((find-font (font-spec :name oletptceu--monofont))
            (setq oletptceu--monofont oletptceu--monofont))
           (t
            (setq oletptceu--monofont "cmtt")))
          (cond
           ((find-font (font-spec :name oletptceu--signaturefont))
            (setq oletptceu--signaturefont oletptceu--signaturefont))
           (t
            (setq oletptceu--signaturefont "cmss")))
          ;; Setup LaTeX document options.
          (oletptceu--set-file-property-value "LATEX_COMPILER" "xelatex")
          (oletptceu--set-file-property-value "LATEX_CLASS" "book")
          (oletptceu--set-file-property-value "LATEX_CLASS_OPTIONS" "[11pt,twoside,a5paper,titlepage,openright]")
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\usepackage{makeidx}" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\makeindex" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\docParindent" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\docParskip" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" subsubsec-format nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" subsec-format nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" sec-format nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" chap-format nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\usepackage{fix-cm}" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\RequirePackage[calcwidth]{titlesec}" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\newcommand{\\legalParindent}{\\parindent 0cm}" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\newcommand{\\legalParskip}{\\parskip 0.25cm}" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\newcommand{\\docParindent}{\\parindent 0.75cm}" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\newcommand{\\docParskip}{\\parskip 0cm}" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\newcommand{\\tocParindent}{\\parindent 0cm}" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\newcommand{\\tocParskip}{\\parskip 0cm}" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\setcounter{secnumdepth}{4}" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\renewcommand{\\baselinestretch}{1.0}" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\oddsidemargin 0cm" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\evensidemargin -0.6cm" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\topmargin -1.20cm" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\footskip 0.7cm" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\textwidth 10.69cm" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\textheight 18.0cm" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\paperwidth 15.24cm" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\paperheight 22.86cm" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\renewcommand{\\thefootnote}{$^[$\\arabic{footnote}$^]$}" nil t)
          (oletptceu--set-file-property-value "ATTR_LATEX" ":width \\linewidth :thickness 0.25mm")
          (when (find-font (font-spec :name oletptceu--signaturefont))
            (oletptceu--set-file-property-value "LATEX_HEADER" (concat "\\newfontfamily\\signaturefont[Scale=" (number-to-string oletptceu--typeface-size) "]{" oletptceu--signaturefont "}") nil t))
          (when (find-font (font-spec :name oletptceu--monofont))
            (oletptceu--set-file-property-value "LATEX_HEADER" (concat "\\setmonofont[Scale=MatchLowercase,ScaleAgain=" (number-to-string oletptceu--monofont-typeface-size-adjustment)"]{" oletptceu--monofont "}") nil t))
          (when (find-font (font-spec :name oletptceu--sansfont))
            (oletptceu--set-file-property-value "LATEX_HEADER" (concat "\\setsansfont[Scale=MatchLowercase]{" oletptceu--sansfont "}") nil t))
          (when (find-font (font-spec :name oletptceu--mainfont))
            (oletptceu--set-file-property-value "LATEX_HEADER" (concat "\\setmainfont[Scale=" (number-to-string oletptceu--typeface-size) "]{" oletptceu--mainfont "}") nil t))
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\usepackage{fontspec}" nil t)
          (goto-char (point-min))
          (when (org-goto-first-child)
            (beginning-of-line)
            (insert "\n")
            (forward-line -1)
            ;; Title Page
            (insert "#+BEGIN_EXPORT latex\n"
                    "\\frontmatter{}\n"
                    "\\begin{titlepage}\n"
                    "\\begin{center}\n")
            (if (file-readable-p oletptceu--title-page-graphic)
                (insert "\\includegraphics[scale=0.175]{" oletptceu--title-page-graphic "}\\\\[3cm]\n")
              (insert "~\\\\[8.5cm]\n"
                      "\n"))
            (insert "\\noindent\\rule{\\textwidth}{0.5pt} \\\\[0.5cm]\n"
                    "\\textsf{ \\huge \\bfseries " (oletptceu--get-file-property-value org-input-file "TITLE") "}\\\\[0.2cm]\n")
	    (if (> (length (oletptceu--get-file-property-value org-input-file "SUBTITLE")) 0)
		(insert "\\noindent\\rule{\\textwidth}{0.5pt} \\\\[0.5cm]\n"
			"\\textsf{ \\large \\itshape " (oletptceu--get-file-property-value org-input-file "SUBTITLE") "}\\\\[3.5cm]\n")
	      (insert "\\noindent\\rule{\\textwidth}{0.5pt} \\\\[4.0cm]\n"))
            (if (find-font (font-spec :name oletptceu--signaturefont))
                (insert "{\\signaturefont {\\Large " (oletptceu--get-file-property-value org-input-file "AUTHOR") "}}\\\\[0.25cm]\n")
              (insert "\\textsc {\\Large " (oletptceu--get-file-property-value org-input-file "AUTHOR") "}\\\\[0.25cm]\n"))
            (insert "\\vfill\n"
                    "\\end{center}\n"
                    "\\end{titlepage}\n"
                    "#+END_EXPORT\n")
            ;; Legal Page
            (insert "#+BEGIN_EXPORT latex\n"
                    "\\thispagestyle{empty}\n"
                    "\\legalParindent\n"
                    "\\legalParskip\n"
                    (oletptceu--get-file-property-value org-input-file "TITLE"))
	    (unless (string= (oletptceu--get-file-property-value org-input-file "SUBTITLE") "")
		(insert " --- " (oletptceu--get-file-property-value org-input-file "SUBTITLE")))
            (insert "\n"
		    "\n")
            (if (find-font (font-spec :name oletptceu--signaturefont))
                (insert "Author: {\\signaturefont " (oletptceu--get-file-property-value org-input-file "AUTHOR") "}\n"
                        "\n")
              (insert "Author: \\textsc{" (oletptceu--get-file-property-value org-input-file "AUTHOR") "}\n"
                      "\n"))
            (insert "Cover: \\textit{Cube Family} by Martin Anderson (2012--?)\\\\Made with Blender 3D --- \\url{https://www.blender.org}\n"
                    "\n"
                    "\\vspace{1cm}\n"
                    "\n"
                    "This book, including the cover art, is copyright \\copyright~"
                    (oletptceu--format-time-string
                     "%Y"
                     (org-time-from-absolute
                      (org-time-string-to-absolute
                       (oletptceu--get-file-property-value org-input-file "DATE"))))  " "
                    (oletptceu--get-file-property-value org-input-file "AUTHOR") ".\n"
                    "\n")
            (if (or (string= oletptceu--rights "Creative Commons Attribution-Non-Commercial-ShareAlike 4.0 International License") (string= oletptceu--rights "by-nc-sa"))
                (insert "The electronic forms of this book, including the cover art, are licensed under the Creative Commons Attribution-Non-Commerc\\-ial-Share\\\\ Alike 4.0 International License. To view a copy of this license, visit:\n"
                        "\n"
                        "\\url{https://creativecommons.org/licenses/by-nc-sa/4.0/}\n"
                        "\n"
                        "Or, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.\n")
              (insert oletptceu--rights "\n"))
            (insert "\n"
                    "The author assumes no liability for errors or omissions in this book, or for damages or loss of revenue resulting from the use of the information contained herein. The characters and incidents portrayed in this book are fictional. Any similarities to real persons, living, dead or yet to exist, is entirely coincidental.\n"
                    "\n")
            (unless (string= (oletptceu--get-file-property-value org-input-file "EMAIL") "")
              (insert "You can contact the author via e-mail:\n\n"
                      "\\href{mailto:" (oletptceu--get-file-property-value org-input-file "EMAIL")
                      "}{" (oletptceu--get-file-property-value org-input-file "EMAIL") "}\n"
                      "\n"
                      "\\vspace{1cm}\n"
                      "\n"))
            (unless (string= oletptceu--isbn "")
              (insert "ISBN " oletptceu--isbn "\n"
                      "\n"))
            (insert oletptceu--edition ": "
                    (oletptceu--format-time-string
                     "%B %Y"
                     (org-time-from-absolute
                      (org-time-string-to-absolute
                       (oletptceu--get-file-property-value org-input-file "DATE")))) "\n"
                    "\n")
            (when (file-readable-p oletptceu--sigil-graphic)
              (insert "\\vspace{1cm}\n"
                      "\n"
                      "\\begin{center}\n"
                      "\\includegraphics[scale=0.5]{" oletptceu--sigil-graphic "}\n"
                      "\n"
                      "\\end{center}\n"
                      "\n"))
            (insert "\\docParindent\n"
                    "\\docParskip\n"
                    "#+END_EXPORT\n"))
          (goto-char (point-min))
          (insert "\n")
          (goto-char (point-min))
          (while (not (org-next-visible-heading 1))
            ;; If tags "no_header" or "no-pagestyle" were used in Chapter Index headings, then act appropriately with formatting.
            (when (nth 5 (org-heading-components))
              (when (member "no_header" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
                (setq no-header t))
              (when (member "no_header_name" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
                (setq no-header-name t))
              (when (member "no_header_preamble" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
                (setq no-header-preamble t))
              (when (member "no_toc_entry" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
                (setq no-toc-entry t))
              (when (member "no_pagestyle" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
                (setq no-pagestyle t))
              (when (member "plain_pagestyle" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
                (setq pagestyle "plain")))
            ;; Check matter type and replace appropriately, convert heading level to same output level. If no matter type, assume front matter.
            (cond ((string= (org-entry-get (point) "ORG-NOVELIST-MATTER-TYPE") "FRONT MATTER")
                   (setq curr-matter "FRONT MATTER")
                   (setq curr-heading (nth 4 (org-heading-components)))
                   (setq curr-level (org-current-level))
                   (setq curr-heading (replace-regexp-in-string (regexp-quote "&") "&amp;" curr-heading nil t))
                   (setq curr-heading (replace-regexp-in-string "\\\\thinsp" "" curr-heading nil t))
                   (beginning-of-line)
                   (oletptceu--delete-line)
                   (setq beg (point))
                   (re-search-forward ":END:" nil t)
                   (delete-region beg (point))
                   (oletptceu--delete-line)
                   (insert "#+BEGIN_EXPORT latex\n")
                   (unless oletptceu--fm-found
                     (insert "\\frontmatter{}\n")
                     (setq oletptceu--fm-found t))
                   (when (string= curr-heading "")
                     (setq no-header-name t))
                   (when (and no-header-name no-header-preamble)
                     (setq no-header t))
                   (if (or no-header no-header-name)
                       (cond ((= 1 curr-level)
                              (insert "\\titleformat{\\chapter}[runin]{}{}{0pt}{}\n")
                              (insert "\\chapter*{}\n"))
                             ((= 2 curr-level)
                              (insert "\\titleformat{\\section}[runin]{}{}{0pt}{}\n")
                              (insert "\\section*{}\n"))
                             ((= 3 curr-level)
                              (insert "\\titleformat{\\subsection}[runin]{}{}{0pt}{}\n")
                              (insert "\\subsection*{}\n"))
                             ((= 4 curr-level)
                              (insert "\\titleformat{\\subsubsection}[runin]{}{}{0pt}{}\n")
                              (insert "\\subsubsection*{}\n"))
			     ((= 5 curr-level)
			      (insert "\\titleformat{\\paragraph}[runin]{}{}{0pt}{}\n")
			      (insert "\\paragraph*{}\n"))
			     ((= 6 curr-level)
			      (insert "\\titleformat{\\subparagraph}[runin]{}{}{0pt}{}\n")
			      (insert "\\subparagraph*{}\n"))
                             (t
                              (insert "\\subparagraph*{}\n")))
                     (progn
                       (cond ((= 1 curr-level)
                              (insert chap-format "\n")
                              (insert "\\chapter*{" curr-heading "}\n")
                              (unless no-toc-entry
                                (insert "\\addcontentsline{toc}{chapter}{" curr-heading "}\n")))
                             ((= 2 curr-level)
                              (insert sec-format "\n")
                              (insert "\\section*{" curr-heading "}\n")
                              (unless no-toc-entry
                                (insert "\\addcontentsline{toc}{section}{" curr-heading "}\n")))
                             ((= 3 curr-level)
                              (insert subsec-format "\n")
                              (insert "\\subsection*{" curr-heading "}\n")
                              (unless no-toc-entry
                                (insert "\\addcontentsline{toc}{subsection}{" curr-heading "}\n")))
                             ((= 4 curr-level)
                              (insert subsubsec-format "\n")
                              (insert "\\subsubsection*{" curr-heading "}\n")
                              (unless no-toc-entry
                                (insert "\\addcontentsline{toc}{subsubsection}{" curr-heading "}\n")))
                             ((= 5 curr-level)
                              (insert paragraph-format "\n")
                              (insert "\\paragraph*{" curr-heading "}\n")
                              (unless no-toc-entry
                                (insert "\\addcontentsline{toc}{paragraph}{" curr-heading "}\n")))
                             ((= 6 curr-level)
                              (insert subparagraph-format "\n")
                              (insert "\\subparagraph*{" curr-heading "}\n")
                              (unless no-toc-entry
                                (insert "\\addcontentsline{toc}{subparagraph}{" curr-heading "}\n")))
                             (t
                              (insert "\\subparagraph*{" curr-heading "}\n")))
                       (insert "\\label{" curr-heading "}\n")))
                   (if no-pagestyle
                       (insert "\\thispagestyle{empty}\n"
                               "\\pagestyle{empty}\n")
                     (cond ((string= "plain" pagestyle)
                            (insert "\\pagestyle{plain}\n"
                                    "\\legalParindent\n"
                                    "\\legalParskip\n"))
                           (t
                            (insert "\\thispagestyle{plain}\n"
                                    "\\pagestyle{plain}\n"
                                    "\\legalParindent\n"
                                    "\\legalParskip\n"))))
                   (insert "#+END_EXPORT\n")
                   (forward-char -1))
                  ((string= (org-entry-get (point) "ORG-NOVELIST-MATTER-TYPE") "MAIN MATTER")
                   (setq curr-matter "MAIN MATTER")
                   (setq curr-heading (nth 4 (org-heading-components)))
                   (setq curr-level (org-current-level))
                   (setq curr-heading (replace-regexp-in-string (regexp-quote "&") "&amp;" curr-heading nil t))
                   (setq curr-heading (replace-regexp-in-string "\\\\thinsp" "" curr-heading nil t))
                   (beginning-of-line)
                   (oletptceu--delete-line)
                   (setq beg (point))
                   (re-search-forward ":END:" nil t)
                   (delete-region beg (point))
                   (oletptceu--delete-line)
                   (insert "#+BEGIN_EXPORT latex\n")
                   (unless oletptceu--mm-found
                     (insert "\\tocParindent\n"
                             "\\tocParskip\n"
                             "\\setcounter{tocdepth}{4}\n"
                             "\\tableofcontents\n"
                             "\\docParindent\n"
                             "\\docParskip\n"
                             "\\newpage\n"
                             "\\thispagestyle{empty}\n"
                             "\\mainmatter{}\n")
                     (setq oletptceu--mm-found t))
                   ;; blank title => equivalent to no-header
                   ;; no-header => Chapter X NOT used, other title NOT used, entry still appears (blank) in toc
                   ;; no-header-name => Chapter X used, other title NOT used, entry still apears (blank) in toc
                   ;; no-header-preamble => Chapter X NOT used, , other title used, entry still appear (blank) in toc
                   ;; no-header-name AND no-header-preamble => equivalent to no-header
                   ;; no-toc-entry => remove entry from toc
                   (when (string= curr-heading "")
                     (setq no-header-name t))
                   (when (and no-header-name no-header-preamble)
                     (setq no-header t))
                   (when no-toc-entry
                     (setq toc-head-string "*"))
                   (cond (no-header
                          (cond ((= 1 curr-level)
                                 (insert "\\titleformat{\\chapter}[runin]{}{}{0pt}{}\n")
                                 (insert "\\chapter" toc-head-string "{}\n"))
                                ((= 2 curr-level)
                                 (insert "\\titleformat{\\section}[runin]{}{}{0pt}{}\n")
                                 (insert "\\section" toc-head-string "{}\n"))
                                ((= 3 curr-level)
                                 (insert "\\titleformat{\\subsection}[runin]{}{}{0pt}{}\n")
                                 (insert "\\subsection" toc-head-string "{}\n"))
                                ((= 4 curr-level)
                                 (insert "\\titleformat{\\subsubsection}[runin]{}{}{0pt}{}\n")
                                 (insert "\\subsubsection" toc-head-string "{}\n"))
                                ((= 5 curr-level)
                                 (insert "\\titleformat{\\paragraph}[runin]{}{}{0pt}{}\n")
                                 (insert "\\paragraph" toc-head-string "{}\n"))
                                ((= 6 curr-level)
                                 (insert "\\titleformat{\\subparagraph}[runin]{}{}{0pt}{}\n")
                                 (insert "\\subparagraph" toc-head-string "{}\n"))
                                (t
                                 (insert "\\subparagraph" toc-head-string "{}\n"))))
                         (no-header-preamble
                          (cond ((= 1 curr-level)
                                 (insert "\\titleformat{\\chapter}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-chapter) "}{" (number-to-string oletptceu--typeface-size-chapter) "}\\selectfont}{0pt}{~\\linebreak\\fontsize{" (number-to-string oletptceu--typeface-size-chapter) "}{" (number-to-string oletptceu--typeface-size-chapter) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]")
                                 (insert "\\chapter" toc-head-string "{" curr-heading "}\n"))
                                ((= 2 curr-level)
                                 (insert "\\titleformat{\\section}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-section) "}{" (number-to-string oletptceu--typeface-size-section) "}\\selectfont}{0pt}{\\,\\,\\,--\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-section) "}{" (number-to-string oletptceu--typeface-size-section) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]")
                                 (insert "\\section" toc-head-string "{" curr-heading "}\n"))
                                ((= 3 curr-level)
                                 (insert "\\titleformat{\\subsection}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subsection) "}{" (number-to-string oletptceu--typeface-size-subsection) "}\\selectfont}{0pt}{\\,\\,\\,--\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-subsection) "}{" (number-to-string oletptceu--typeface-size-subsection) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]")
                                 (insert "\\subsection" toc-head-string "{" curr-heading "}\n"))
                                ((= 4 curr-level)
                                 (insert "\\titleformat{\\subsubsection}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subsubsection) "}{" (number-to-string oletptceu--typeface-size-subsubsection) "}\\selectfont}{0pt}{\\,\\,\\,--\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-subsubsection) "}{" (number-to-string oletptceu--typeface-size-subsubsection) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]")
                                 (insert "\\subsubsection" toc-head-string "{" curr-heading "}\n"))
                                ((= 5 curr-level)
                                 (insert "\\titleformat{\\paragraph}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-paragraph) "}{" (number-to-string oletptceu--typeface-size-paragraph) "}\\selectfont}{0pt}{\\,\\,\\,--\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-paragraph) "}{" (number-to-string oletptceu--typeface-size-paragraph) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]")
                                 (insert "\\paragraph" toc-head-string "{" curr-heading "}\n"))
                                ((= 6 curr-level)
                                 (insert "\\titleformat{\\subparagraph}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subparagraph) "}{" (number-to-string oletptceu--typeface-size-subparagraph) "}\\selectfont}{0pt}{\\,\\,\\,--\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-subparagraph) "}{" (number-to-string oletptceu--typeface-size-subparagraph) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]")
                                 (insert "\\subparagraph" toc-head-string "{" curr-heading "}\n"))
                                (t
                                 (insert "\\subparagraph" toc-head-string "{" curr-heading "}\n")))
                          (insert "\\label{" curr-heading "}\n"))
                         (no-header-name
                          (cond ((= 1 curr-level)
                                 (insert "\\titleformat{\\chapter}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-chapter) "}{" (number-to-string oletptceu--typeface-size-chapter) "}\\selectfont\\chaptername\\,\\thechapter}{0pt}{\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]\n")
                                 (insert "\\chapter" toc-head-string "{}\n"))
                                ((= 2 curr-level)
                                 (insert "\\titleformat{\\section}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-section) "}{" (number-to-string oletptceu--typeface-size-section) "}\\selectfont\\thesection}{0pt}{\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]\n")
                                 (insert "\\section" toc-head-string "{}\n"))
                                ((= 3 curr-level)
                                 (insert "\\titleformat{\\subsection}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subsection) "}{" (number-to-string oletptceu--typeface-size-subsection) "}\\selectfont\\thesubsection}{0pt}{\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]\n")
                                 (insert "\\subsection" toc-head-string "{}\n"))
                                ((= 4 curr-level)
                                 (insert "\\titleformat{\\subsubsection}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subsubsection) "}{" (number-to-string oletptceu--typeface-size-subsubsection) "}\\selectfont\\thesubsubsection}{0pt}{\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]\n")
                                 (insert "\\subsubsection" toc-head-string "{}\n"))
                                ((= 5 curr-level)
                                 (insert "\\titleformat{\\paragraph}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-paragraph) "}{" (number-to-string oletptceu--typeface-size-paragraph) "}\\selectfont\\theparagraph}{0pt}{\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]\n")
                                 (insert "\\paragraph" toc-head-string "{}\n"))
                                ((= 6 curr-level)
                                 (insert "\\titleformat{\\subparagraph}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subparagraph) "}{" (number-to-string oletptceu--typeface-size-subparagraph) "}\\selectfont\\thesubparagraph}{0pt}{\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]\n")
                                 (insert "\\subparagraph" toc-head-string "{}\n"))
                                (t
                                 (insert "\\subparagraph" toc-head-string "{}\n"))))
                         (t
                          (cond ((= 1 curr-level)
                                 (insert chap-format "\n")
                                 (insert "\\chapter" toc-head-string "{" curr-heading "}\n"))
                                ((= 2 curr-level)
                                 (insert sec-format "\n")
                                 (insert "\\section" toc-head-string "{" curr-heading "}\n"))
                                ((= 3 curr-level)
                                 (insert subsec-format "\n")
                                 (insert "\\subsection" toc-head-string "{" curr-heading "}\n"))
                                ((= 4 curr-level)
                                 (insert subsubsec-format "\n")
                                 (insert "\\subsubsection" toc-head-string "{" curr-heading "}\n"))
                                ((= 5 curr-level)
                                 (insert paragraph-format "\n")
                                 (insert "\\paragraph" toc-head-string "{" curr-heading "}\n"))
                                ((= 6 curr-level)
                                 (insert subparagraph-format "\n")
                                 (insert "\\subparagraph" toc-head-string "{" curr-heading "}\n"))
                                (t
                                 (insert "\\subparagraph" toc-head-string "{" curr-heading "}\n")))
                          (insert "\\label{" curr-heading "}\n")))
                   (if no-pagestyle
                       (insert "\\thispagestyle{empty}\n"
                               "\\pagestyle{empty}\n")
                     (cond ((string= "plain" pagestyle)
                            (insert "\\pagestyle{plain}\n"
                                    "\\legalParindent\n"
                                    "\\legalParskip\n"))
                           (t
                            (insert "\\pagestyle{headings}\n"
                                    "\\docParindent\n"
                                    "\\docParskip\n"))))
                   (insert "#+END_EXPORT\n")
                   (forward-char -1))
                  ((string= (org-entry-get (point) "ORG-NOVELIST-MATTER-TYPE") "BACK MATTER")
                   (setq curr-matter "BACK MATTER")
                   (setq curr-heading (nth 4 (org-heading-components)))
                   (setq curr-level (org-current-level))
                   (setq curr-heading (replace-regexp-in-string (regexp-quote "&") "&amp;" curr-heading nil t))
                   (setq curr-heading (replace-regexp-in-string "\\\\thinsp" "" curr-heading nil t))
                   (beginning-of-line)
                   (oletptceu--delete-line)
                   (setq beg (point))
                   (re-search-forward ":END:" nil t)
                   (delete-region beg (point))
                   (oletptceu--delete-line)
                   (insert "#+BEGIN_EXPORT latex\n")
                   (unless oletptceu--bm-found
                     (insert "\\newpage\n"
                             "\\thispagestyle{empty}\n"
                             "\\backmatter{}\n")
                     (setq oletptceu--bm-found t))
                   (when (string= curr-heading "")
                     (setq no-header-name t))
                   (when (and no-header-name no-header-preamble)
                     (setq no-header t))
                   (if (or no-header no-header-name)
                       (cond ((= 1 curr-level)
                              (insert "\\titleformat{\\chapter}[runin]{}{}{0pt}{}\n")
                              (insert "\\chapter*{}\n"))
                             ((= 2 curr-level)
                              (insert "\\titleformat{\\section}[runin]{}{}{0pt}{}\n")
                              (insert "\\section*{}\n"))
                             ((= 3 curr-level)
                              (insert "\\titleformat{\\subsection}[runin]{}{}{0pt}{}\n")
                              (insert "\\subsection*{}\n"))
                             ((= 4 curr-level)
                              (insert "\\titleformat{\\subsubsection}[runin]{}{}{0pt}{}\n")
                              (insert "\\subsubsection*{}\n"))
                             ((= 5 curr-level)
                              (insert "\\titleformat{\\paragraph}[runin]{}{}{0pt}{}\n")
                              (insert "\\paragraph*{}\n"))
                             ((= 6 curr-level)
                              (insert "\\titleformat{\\subparagraph}[runin]{}{}{0pt}{}\n")
                              (insert "\\subparagraph*{}\n"))
                             (t
                              (insert "\\subparagraph*{}\n")))
                     (progn
                       (cond ((= 1 curr-level)
                              (insert chap-format "\n")
                              (insert "\\chapter*{" curr-heading "}\n")
                              (unless no-toc-entry
                                (insert "\\addcontentsline{toc}{chapter}{" curr-heading "}\n")))
                             ((= 2 curr-level)
                              (insert sec-format "\n")
                              (insert "\\section*{" curr-heading "}\n")
                              (unless no-toc-entry
                                (insert "\\addcontentsline{toc}{section}{" curr-heading "}\n")))
                             ((= 3 curr-level)
                              (insert subsec-format "\n")
                              (insert "\\subsection*{" curr-heading "}\n")
                              (unless no-toc-entry
                                (insert "\\addcontentsline{toc}{subsection}{" curr-heading "}\n")))
                             ((= 4 curr-level)
                              (insert subsubsec-format "\n")
                              (insert "\\subsubsection*{" curr-heading "}\n")
                              (unless no-toc-entry
                                (insert "\\addcontentsline{toc}{subsubsection}{" curr-heading "}\n")))
                             ((= 5 curr-level)
                              (insert paragraph-format "\n")
                              (insert "\\paragraph*{" curr-heading "}\n")
                              (unless no-toc-entry
                                (insert "\\addcontentsline{toc}{paragraph}{" curr-heading "}\n")))
                             ((= 6 curr-level)
                              (insert subparagraph-format "\n")
                              (insert "\\subparagraph*{" curr-heading "}\n")
                              (unless no-toc-entry
                                (insert "\\addcontentsline{toc}{subparagraph}{" curr-heading "}\n")))
                             (t
                              (insert "\\subparagraph*{" curr-heading "}\n")))
                       (insert "\\label{" curr-heading "}\n")))
                   (if no-pagestyle
                       (insert "\\thispagestyle{empty}\n"
                               "\\pagestyle{empty}\n")
                     (cond ((string= "plain" pagestyle)
                            (insert "\\pagestyle{plain}\n"
                                    "\\legalParindent\n"
                                    "\\legalParskip\n"))
                           (t
                            (insert "\\pagestyle{plain}\n"
                                    "\\legalParindent\n"
                                    "\\legalParskip\n"))))
                   (insert "#+END_EXPORT\n")
                   (forward-char -1))
                  (t
                   (setq curr-heading (nth 4 (org-heading-components)))
                   (setq curr-level (org-current-level))
                   (setq curr-heading (replace-regexp-in-string (regexp-quote "&") "&amp;" curr-heading nil t))
                   (setq curr-heading (replace-regexp-in-string "\\\\thinsp" "" curr-heading nil t))
                   (beginning-of-line)
                   (oletptceu--delete-line)
                   (insert "#+BEGIN_EXPORT latex\n")
                   (cond ((string= curr-matter "FRONT MATTER")
                          (unless oletptceu--fm-found
                            (insert "\\frontmatter{}\n")
                            (setq oletptceu--fm-found t))
                          (setq no-toc-entry t))
                         ((string= curr-matter "MAIN MATTER")
                          (unless oletptceu--mm-found
                            (insert "\\tocParindent\n"
                                    "\\tocParskip\n"
                                    "\\setcounter{tocdepth}{4}\n"
                                    "\\tableofcontents\n"
                                    "\\docParindent\n"
                                    "\\docParskip\n"
                                    "\\newpage\n"
                                    "\\thispagestyle{empty}\n"
                                    "\\mainmatter{}\n")
                            (setq oletptceu--mm-found t)))
                         ((string= curr-matter "BACK MATTER")
                          (unless oletptceu--bm-found
                            (insert "\\newpage\n"
                                    "\\thispagestyle{empty}\n"
                                    "\\backmatter{}\n")
                            (setq oletptceu--bm-found t))
                          (setq no-toc-entry t)))
                   ;; blank title => equivalent to no-header
                   ;; no-header => Chapter X NOT used, other title NOT used, entry still appears (blank) in toc
                   ;; no-header-name => Chapter X used, other title NOT used, entry still apears (blank) in toc
                   ;; no-header-preamble => Chapter X NOT used, , other title used, entry still appear (blank) in toc
                   ;; no-header-name AND no-header-preamble => equivalent to no-header
                   ;; no-toc-entry => remove entry from toc
                   (when (string= curr-heading "")
                     (setq no-header-name t))
                   (when (and no-header-name no-header-preamble)
                     (setq no-header t))
                   (when no-toc-entry
                     (setq toc-head-string "*"))
                   (cond (no-header
                          (cond ((= 1 curr-level)
                                 (insert "\\titleformat{\\chapter}[runin]{}{}{0pt}{}\n")
                                 (insert "\\chapter" toc-head-string "{}\n"))
                                ((= 2 curr-level)
                                 (insert "\\titleformat{\\section}[runin]{}{}{0pt}{}\n")
                                 (insert "\\section" toc-head-string "{}\n"))
                                ((= 3 curr-level)
                                 (insert "\\titleformat{\\subsection}[runin]{}{}{0pt}{}\n")
                                 (insert "\\subsection" toc-head-string "{}\n"))
                                ((= 4 curr-level)
                                 (insert "\\titleformat{\\subsubsection}[runin]{}{}{0pt}{}\n")
                                 (insert "\\subsubsection" toc-head-string "{}\n"))
                                ((= 5 curr-level)
                                 (insert "\\titleformat{\\paragraph}[runin]{}{}{0pt}{}\n")
                                 (insert "\\paragraph" toc-head-string "{}\n"))
                                ((= 6 curr-level)
                                 (insert "\\titleformat{\\subparagraph}[runin]{}{}{0pt}{}\n")
                                 (insert "\\subparagraph" toc-head-string "{}\n"))
                                (t
                                 (insert "\\subparagraph" toc-head-string "{}\n"))))
                         (no-header-preamble
                          (cond ((= 1 curr-level)
                                 (insert "\\titleformat{\\chapter}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-chapter) "}{" (number-to-string oletptceu--typeface-size-chapter) "}\\selectfont}{0pt}{~\\linebreak\\fontsize{" (number-to-string oletptceu--typeface-size-chapter) "}{" (number-to-string oletptceu--typeface-size-chapter) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]")
                                 (insert "\\chapter" toc-head-string "{" curr-heading "}\n"))
                                ((= 2 curr-level)
                                 (insert "\\titleformat{\\section}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-section) "}{" (number-to-string oletptceu--typeface-size-section) "}\\selectfont}{0pt}{\\,\\,\\,~\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-section) "}{" (number-to-string oletptceu--typeface-size-section) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]")
                                 (insert "\\section" toc-head-string "{" curr-heading "}\n"))
                                ((= 3 curr-level)
                                 (insert "\\titleformat{\\subsection}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subsection) "}{" (number-to-string oletptceu--typeface-size-subsection) "}\\selectfont}{0pt}{\\,\\,\\,~\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-subsection) "}{" (number-to-string oletptceu--typeface-size-subsection) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]")
                                 (insert "\\subsection" toc-head-string "{" curr-heading "}\n"))
                                ((= 4 curr-level)
                                 (insert "\\titleformat{\\subsubsection}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subsubsection) "}{" (number-to-string oletptceu--typeface-size-subsubsection) "}\\selectfont}{0pt}{\\,\\,\\,~\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-subsubsection) "}{" (number-to-string oletptceu--typeface-size-subsubsection) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]")
                                 (insert "\\subsubsection" toc-head-string "{" curr-heading "}\n"))
                                ((= 5 curr-level)
                                 (insert "\\titleformat{\\paragraph}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-paragraph) "}{" (number-to-string oletptceu--typeface-size-paragraph) "}\\selectfont}{0pt}{\\,\\,\\,~\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-paragraph) "}{" (number-to-string oletptceu--typeface-size-paragraph) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]")
                                 (insert "\\paragraph" toc-head-string "{" curr-heading "}\n"))
                                ((= 6 curr-level)
                                 (insert "\\titleformat{\\subparagraph}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subparagraph) "}{" (number-to-string oletptceu--typeface-size-subparagraph) "}\\selectfont}{0pt}{\\,\\,\\,~\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-subparagraph) "}{" (number-to-string oletptceu--typeface-size-subparagraph) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]")
                                 (insert "\\subparagraph" toc-head-string "{" curr-heading "}\n"))
                                (t
                                 (insert "\\subparagraph" toc-head-string "{" curr-heading "}\n")))
                          (insert "\\label{" curr-heading "}\n"))
                         (no-header-name
                          (cond ((= 1 curr-level)
                                 (insert "\\titleformat{\\chapter}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-chapter) "}{" (number-to-string oletptceu--typeface-size-chapter) "}\\selectfont\\chaptername\\,\\thechapter}{0pt}{\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]\n")
                                 (insert "\\chapter" toc-head-string "{}\n"))
                                ((= 2 curr-level)
                                 (insert "\\titleformat{\\section}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-section) "}{" (number-to-string oletptceu--typeface-size-section) "}\\selectfont\\thesection}{0pt}{\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]\n")
                                 (insert "\\section" toc-head-string "{}\n"))
                                ((= 3 curr-level)
                                 (insert "\\titleformat{\\subsection}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subsection) "}{" (number-to-string oletptceu--typeface-size-subsection) "}\\selectfont\\thesubsection}{0pt}{\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]\n")
                                 (insert "\\subsection" toc-head-string "{}\n"))
                                ((= 4 curr-level)
                                 (insert "\\titleformat{\\subsubsection}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subsubsection) "}{" (number-to-string oletptceu--typeface-size-subsubsection) "}\\selectfont\\thesubsubsection}{0pt}{\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]\n")
                                 (insert "\\subsubsection" toc-head-string "{}\n"))
                                ((= 5 curr-level)
                                 (insert "\\titleformat{\\paragraph}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-paragraph) "}{" (number-to-string oletptceu--typeface-size-paragraph) "}\\selectfont\\theparagraph}{0pt}{\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]\n")
                                 (insert "\\paragraph" toc-head-string "{}\n"))
                                ((= 6 curr-level)
                                 (insert "\\titleformat{\\subparagraph}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subparagraph) "}{" (number-to-string oletptceu--typeface-size-subparagraph) "}\\selectfont\\thesubparagraph}{0pt}{\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]\n")
                                 (insert "\\subparagraph" toc-head-string "{}\n"))
                                (t
                                 (insert "\\subparagraph" toc-head-string "{}\n"))))
                         (t
                          (cond ((= 1 curr-level)
                                 (insert chap-format "\n")
                                 (insert "\\chapter" toc-head-string "{" curr-heading "}\n"))
                                ((= 2 curr-level)
                                 (insert sec-format "\n")
                                 (insert "\\section" toc-head-string "{" curr-heading "}\n"))
                                ((= 3 curr-level)
                                 (insert subsec-format "\n")
                                 (insert "\\subsection" toc-head-string "{" curr-heading "}\n"))
                                ((= 4 curr-level)
                                 (insert subsubsec-format "\n")
                                 (insert "\\subsubsection" toc-head-string "{" curr-heading "}\n"))
                                ((= 5 curr-level)
                                 (insert paragraph-format "\n")
                                 (insert "\\paragraph" toc-head-string "{" curr-heading "}\n"))
                                ((= 6 curr-level)
                                 (insert subparagraph-format "\n")
                                 (insert "\\subparagraph" toc-head-string "{" curr-heading "}\n"))
                                (t
                                 (insert "\\subparagraph" toc-head-string "{" curr-heading "}\n")))
                          (insert "\\label{" curr-heading "}\n")))
                   (if no-pagestyle
                       (insert "\\thispagestyle{empty}\n"
                               "\\pagestyle{empty}\n")
                     (cond ((string= "plain" pagestyle)
                            (insert "\\pagestyle{plain}\n"
                                    "\\legalParindent\n"
                                    "\\legalParskip\n"))
                           (t
                            (insert "\\pagestyle{headings}\n"
                                    "\\docParindent\n"
                                    "\\docParskip\n"))))
                   (insert "#+END_EXPORT\n")
                   (forward-char -1)))
            (setq no-header nil)
            (setq no-header-name nil)
            (setq no-header-preamble nil)
            (setq no-pagestyle nil)
            (setq no-toc-entry nil)
            (setq toc-head-string ""))
	  ;; Remap image embeds to correct directory.
          (goto-char (point-min))
          (let ((case-fold-search t))
            (while (re-search-forward (format "^[ \t]*%s" (regexp-quote "[[file:../Images/")) nil t)
              (delete-char -7)
	      (insert "../Images/")))
          (goto-char (point-min))
          (oletptceu--delete-line)
          (oletptceu--string-to-file (buffer-string) temp-org))))  ; Write new Org file to be fed to exporter
    (setq undo-tree-auto-save-history nil)  ; Try to prevent undo-tree making back-ups for autogenerated files
    (find-file temp-org)
    (org-latex-export-to-pdf)  ; Use Org mode's built-in LaTeX -> PDF exporter to generate PDF from the new Org file
    (oletptceu--delete-current-file t)
    (setq org-export-with-toc org-export-with-toc-orig)
    (setq org-export-with-date org-export-with-date-orig)
    (setq org-export-with-tags org-export-with-tags-orig)
    (setq org-export-with-email org-export-with-email-orig)
    (setq org-export-with-latex org-export-with-latex-orig)
    (setq org-export-with-tasks org-export-with-tasks-orig)
    (setq org-export-with-title org-export-with-title-orig)
    (setq org-export-with-author org-export-with-author-orig)
    (setq org-export-with-clocks org-export-with-clocks-orig)
    (setq org-export-with-tables org-export-with-tables-orig)
    (setq org-export-with-creator org-export-with-creator-orig)
    (setq org-export-with-drawers org-export-with-drawers-orig)
    (setq org-export-with-entities org-export-with-entities-orig)
    (setq org-export-with-planning org-export-with-planning-orig)
    (setq org-export-with-priority org-export-with-priority-orig)
    (setq org-export-with-emphasize org-export-with-emphasize-orig)
    (setq org-export-with-footnotes org-export-with-footnotes-orig)
    (setq org-export-with-properties org-export-with-properties-orig)
    (setq org-export-with-timestamps org-export-with-timestamps-orig)
    (setq org-export-with-fixed-width org-export-with-fixed-width-orig)
    (setq org-export-with-inlinetasks org-export-with-inlinetasks-orig)
    (setq org-export-with-broken-links org-export-with-broken-links-orig)
    (setq org-export-with-smart-quotes org-export-with-smart-quotes-orig)
    (setq org-export-with-todo-keywords org-export-with-todo-keywords-orig)
    (setq org-export-with-archived-trees org-export-with-archived-trees-orig)
    (setq org-export-with-section-numbers org-export-with-section-numbers-orig)
    (setq org-export-with-special-strings org-export-with-special-strings-orig)
    (setq org-export-with-sub-superscripts org-export-with-sub-superscripts-orig)
    (setq org-use-sub-superscripts org-use-sub-superscripts-orig)
    (setq org-export-with-statistics-cookies org-export-with-statistics-cookies-orig)
    (make-directory (file-name-directory output-file) t)
    (rename-file (concat (file-name-sans-extension temp-org) ".pdf") output-file t)
    (rename-file (concat (file-name-sans-extension temp-org) ".tex") (concat (file-name-sans-extension output-file) ".tex") t)
    (delete-file (concat (file-name-sans-extension temp-org) ".ilg"))
    (delete-file (concat (file-name-sans-extension temp-org) ".ind"))
    ;; The next block creates an imposed booklet for bookbinding if required.
    (when (and oletptceu--make-booklet (executable-find "xelatex"))
      (with-temp-buffer
	(insert "\\documentclass[a4paper]{article}\n"
		"\\usepackage[xetex]{color,graphicx,epsfig}\n"
		"\\usepackage[final]{pdfpages}\n"
		"\\begin{document}\n"
		"\\includepdf[pages=")
	(when (> oletptceu--booklet-buffer-pages 0)
	  (insert "{"))
	(let ((i oletptceu--booklet-buffer-pages))
	  (while (> i 0)
	    (insert "{},")
	    (setq i (- i 1))))
	(insert "-")
	(let ((i oletptceu--booklet-buffer-pages))
	  (while (> i 0)
	    (insert ",{}")
	    (setq i (- i 1))))
	(when (> oletptceu--booklet-buffer-pages 0)
	  (insert "}"))
	(insert ",nup=1x2,landscape,signature=" (number-to-string (* 4 oletptceu--signature-size)) "]{./" (file-name-base output-file) ".pdf}\n"
		"\\end{document}")
	(oletptceu--string-to-file (buffer-string) (concat (file-name-sans-extension output-file) "Booklet.tex")))
      (when (file-readable-p (concat (file-name-sans-extension output-file) "Booklet.tex"))
	(let ((default-directory (file-name-directory output-file)))
	  (shell-command-to-string (concat "xelatex \"" (file-name-sans-extension output-file) "Booklet.tex\""))))
      (delete-file (concat (file-name-sans-extension output-file) "Booklet.aux"))
      (delete-file (concat (file-name-sans-extension output-file) "Booklet.log")))
    (setq oletptceu--fm-found nil)
    (setq oletptceu--mm-found nil)
    (setq oletptceu--bm-found nil)
    (setq undo-tree-auto-save-history undo-tree-auto-save-history-orig)))

(provide 'org-latex-export-to-pdf-tradeback-cubes-en-us)
;;; org-latex-export-to-pdf-tradeback-cubes-en-us.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("oletptceu-" . "org-latex-export-to-pdf-tradeback-cubes-en-us-"))
;; End:
