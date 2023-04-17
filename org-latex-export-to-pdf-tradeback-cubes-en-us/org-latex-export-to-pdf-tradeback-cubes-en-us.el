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
;; following of links, programmatic updating of crossreferences, and
;; ability to programatically export to other formats.
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
(defvar oletptceu--typeface-size-chapter 20 "Typeface size (pt) for the chapter heading text.")
(defvar oletptceu--typeface-size-section 15 "Typeface size (pt) for the section heading text.")
(defvar oletptceu--typeface-size-subsection 11 "Typeface size (pt) for the subsection heading text.")
(defvar oletptceu--typeface-size-subsubsection 11 "Typeface size (pt) for the subsubsection heading text.")
(defvar oletptceu--mainfont "Libre Baskerville" "Main text font, must be installed on system already.")
(defvar oletptceu--sansfont "Josefin Sans" "Main text font, must be installed on system already.")
(defvar oletptceu--monofont "DejaVu Sans Mono" "Main text font, must be installed on system already.")
(defvar oletptceu--signaturefont "Alegreya SC" "Main text font, must be installed on system already.")
(defvar oletptceu--title-page-graphic "/home/sympodius/Git/sympodius/org-novelist-export-templates/org-latex-export-to-pdf-tradeback-cubes-en-us/cubes.png" "Location of image file to use in title page.")
(defvar oletptceu--isbn "" "ISBN number of book, if there is one.")
(defvar oletptceu--edition "Early Draft Edition" "Text describing this edition.")
(defvar oletptceu--rights "Creative Commons Attribution-Non-Commercial-ShareAlike 4.0 International License" "Copyright statement or license.")
(defvar oletptceu--sigil-graphic "/home/sympodius/Git/sympodius/org-novelist-export-templates/org-latex-export-to-pdf-tradeback-cubes-en-us/juf-sigil.pdf" "Location of image file to use as sigil in legal page.")



;;;; Global Variables
(defvar oletptceu--fm-found nil "Temporary variable to show at least one front matter chapter found.")
(defvar oletptceu--mm-found nil "Temporary variable to show at least one main matter chapter found.")
(defvar oletptceu--bm-found nil "Temporary variable to show at least one back matter chapter found.")


;;;; Helper Functions

(defun oletptceu--fold-show-all ()
  "Run the deprecated `org-show-all' when Org version is less than 9.6.
Otherwise, run `org-fold-show-all'."
  (if (and (>= (string-to-number (nth 0 (split-string (org-version) "\\."))) 9)
           (>= (string-to-number (nth 1 (split-string (org-version) "\\."))) 6))
      (org-fold-show-all)
    (org-show-all)))

(defun oletptceu--format-time-string (format-string &optional time-zone)
  "Run the deprecated `org-format-time-string' when Org version is less than 9.6.
Otherwise, run `format-time-string'.
FORMAT-STRING is the output format.
TIME-ZONE is the given time. If omitted or nil, use local time."
  (if (and (>= (string-to-number (nth 0 (split-string (org-version) "\\."))) 9)
           (>= (string-to-number (nth 1 (split-string (org-version) "\\."))) 6))
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


;;;; Required Entry Point Function for Org Novelist Export

(defun org-novelist--export-template (org-input-file output-file)
  "Given an ORG-INPUT-FILE from Org Novelist, export to OUTPUT-FILE."
  (let ((temp-org (concat (file-name-sans-extension output-file) ".org"))
        (org-export-with-toc-orig nil)
        (org-export-with-title-orig nil)
        (org-export-with-author-orig nil)
        (org-export-with-email-orig nil)
        (org-export-with-date-orig nil)
        (undo-tree-auto-save-history-orig nil)
        curr-heading
        curr-level
        beg)
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
    (when (boundp 'undo-tree-auto-save-history)
      (setq undo-tree-auto-save-history-orig undo-tree-auto-save-history))
    (setq org-export-with-toc nil)
    (setq org-export-with-title nil)
    (setq org-export-with-author nil)
    (setq org-export-with-email nil)
    (setq org-export-with-date nil)
    (when (file-exists-p org-input-file)
      (when (file-readable-p org-input-file)
        (with-temp-buffer
          (insert-file-contents org-input-file)
          (org-mode)
          (oletptceu--fold-show-all)
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
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\docParindent" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\docParskip" nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" (concat "\\titleformat{\\subsubsection}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subsubsection) "}{" (number-to-string oletptceu--typeface-size-subsubsection) "}\\selectfont\\thesubsubsection}{0pt}{\\,\\,\\,--\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-subsubsection) "}{" (number-to-string oletptceu--typeface-size-subsubsection) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]") nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" (concat "\\titleformat{\\subsection}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subsection) "}{" (number-to-string oletptceu--typeface-size-subsection) "}\\selectfont\\thesubsection}{0pt}{\\,\\,\\,--\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-subsection) "}{" (number-to-string oletptceu--typeface-size-subsection) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]") nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" (concat "\\titleformat{\\section}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-section) "}{" (number-to-string oletptceu--typeface-size-section) "}\\selectfont\\thesection}{0pt}{\\,\\,\\,--\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-section) "}{" (number-to-string oletptceu--typeface-size-section) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]") nil t)
          (oletptceu--set-file-property-value "LATEX_HEADER" (concat "\\titleformat{\\chapter}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-chapter) "}{" (number-to-string oletptceu--typeface-size-chapter) "}\\selectfont\\chaptername\\,\\thechapter}{0pt}{~\\linebreak\\fontsize{" (number-to-string oletptceu--typeface-size-chapter) "}{" (number-to-string oletptceu--typeface-size-chapter) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]") nil t)
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
            (oletptceu--set-file-property-value "LATEX_HEADER" (concat "\\setmonofont[Scale=MatchLowercase]{" oletptceu--monofont "}") nil t))
          (when (find-font (font-spec :name oletptceu--sansfont))
            (oletptceu--set-file-property-value "LATEX_HEADER" (concat "\\setsansfont[Scale=MatchLowercase]{" oletptceu--sansfont "}") nil t))
          (when (find-font (font-spec :name oletptceu--mainfont))
            (oletptceu--set-file-property-value "LATEX_HEADER" (concat "\\setmainfont[Scale=" (number-to-string oletptceu--typeface-size) "]{" oletptceu--mainfont "}") nil t))
          (oletptceu--set-file-property-value "LATEX_HEADER" "\\usepackage{fontspec} " nil t)
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
                    "\\textsf{ \\huge \\bfseries " (oletptceu--get-file-property-value org-input-file "TITLE") "}\\\\[0.2cm]\n"
                    "\\noindent\\rule{\\textwidth}{0.5pt} \\\\[4.0cm]\n")
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
                    (oletptceu--get-file-property-value org-input-file "TITLE") "\n"
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
            (if (or (string= oletptceu--rights "Creative Commons Attribution-Non-Commercial-ShareAlike 4.0 International License") (string= opeteceu--rights "by-nc-sa"))
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
            ;; Check matter type and replace appropriately, convert heading level to same output level. If no matter type, assume front matter.
            (cond ((string= (org-entry-get (point) "ORG-NOVELIST-MATTER-TYPE") "FRONT MATTER")
                   (setq curr-heading (nth 4 (org-heading-components)))
                   (setq curr-level (org-current-level))
                   (setq curr-heading (replace-regexp-in-string (regexp-quote "&") "&amp;" curr-heading nil t))
                   (beginning-of-line)
                   (oletptceu--delete-line)
                   (setq beg (point))
                   (re-search-forward ":END:" nil t)
                   (delete-region beg (point))
                   (oletptceu--delete-line)
                   (insert "#+BEGIN_EXPORT latex\n")
                   (cond ((= 1 curr-level)
                          (insert "\\chapter*{" curr-heading "}\n"
                                  "\\addcontentsline{toc}{chapter}{" curr-heading "}\n"))
                         ((= 2 curr-level)
                          (insert "\\section*{" curr-heading "}\n"
                                  "\\addcontentsline{toc}{section}{" curr-heading "}\n"))
                         ((= 3 curr-level)
                          (insert "\\subsection*{" curr-heading "}\n"
                                  "\\addcontentsline{toc}{subsection}{" curr-heading "}\n"))
                         ((= 4 curr-level)
                          (insert "\\subsubsection*{" curr-heading "}\n"
                                  "\\addcontentsline{toc}{subsubsection}{" curr-heading "}\n"))
                         (t
                          (insert "\\subsubsubsection*{" curr-heading "}\n")))
                   (insert "\\label{" curr-heading "}\n"
                           "\\pagestyle{plain}\n"
                           "\\legalParindent\n"
                           "\\legalParskip\n"
                           "#+END_EXPORT\n"))
                  ((string= (org-entry-get (point) "ORG-NOVELIST-MATTER-TYPE") "MAIN MATTER")
                   (setq curr-heading (nth 4 (org-heading-components)))
                   (setq curr-level (org-current-level))
                   (setq curr-heading (replace-regexp-in-string (regexp-quote "&") "&amp;" curr-heading nil t))
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
                   (cond ((= 1 curr-level)
                          (insert "\\chapter{" curr-heading "}\n"))
                         ((= 2 curr-level)
                          (insert "\\section{" curr-heading "}\n"))
                         ((= 3 curr-level)
                          (insert "\\subsection{" curr-heading "}\n"))
                         ((= 4 curr-level)
                          (insert "\\subsubsection{" curr-heading "}\n"))
                         (t
                          (insert "\\subsubsubsection{" curr-heading "}\n")))
                   (insert "\\label{" curr-heading "}\n"
                           "\\pagestyle{headings}\n"
                           "\\docParindent\n"
                           "\\docParskip\n"
                           "#+END_EXPORT\n"))
                  ((string= (org-entry-get (point) "ORG-NOVELIST-MATTER-TYPE") "BACK MATTER")
                   (setq curr-heading (nth 4 (org-heading-components)))
                   (setq curr-level (org-current-level))
                   (setq curr-heading (replace-regexp-in-string (regexp-quote "&") "&amp;" curr-heading nil t))
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
                   (cond ((= 1 curr-level)
                          (insert "\\chapter*{" curr-heading "}\n"
                                  "\\addcontentsline{toc}{chapter}{" curr-heading "}\n"))
                         ((= 2 curr-level)
                          (insert "\\section*{" curr-heading "}\n"
                                  "\\addcontentsline{toc}{section}{" curr-heading "}\n"))
                         ((= 3 curr-level)
                          (insert "\\subsection*{" curr-heading "}\n"
                                  "\\addcontentsline{toc}{subsection}{" curr-heading "}\n"))
                         ((= 4 curr-level)
                          (insert "\\subsubsection*{" curr-heading "}\n"
                                  "\\addcontentsline{toc}{subsubsection}{" curr-heading "}\n"))
                         (t
                          (insert "\\subsubsubsection*{" curr-heading "}\n")))
                   (insert "\\label{" curr-heading "}\n"
                           "\\pagestyle{plain}\n"
                           "\\legalParindent\n"
                           "\\legalParskip\n"
                           "#+END_EXPORT\n"))
                  (t
                   (setq curr-heading (nth 4 (org-heading-components)))
                   (setq curr-level (org-current-level))
                   (setq curr-heading (replace-regexp-in-string (regexp-quote "&") "&amp;" curr-heading nil t))
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
                   (cond ((= 1 curr-level)
                          (insert "\\chapter*{" curr-heading "}\n"
                                  "\\addcontentsline{toc}{chapter}{" curr-heading "}\n"))
                         ((= 2 curr-level)
                          (insert "\\section*{" curr-heading "}\n"
                                  "\\addcontentsline{toc}{section}{" curr-heading "}\n"))
                         ((= 3 curr-level)
                          (insert "\\subsection*{" curr-heading "}\n"
                                  "\\addcontentsline{toc}{subsection}{" curr-heading "}\n"))
                         ((= 4 curr-level)
                          (insert "\\subsubsection*{" curr-heading "}\n"
                                  "\\addcontentsline{toc}{subsubsection}{" curr-heading "}\n"))
                         (t
                          (insert "\\subsubsubsection*{" curr-heading "}\n")))
                   (insert "\\label{" curr-heading "}\n"
                           "\\pagestyle{plain}\n"
                           "\\legalParindent\n"
                           "\\legalParskip\n"
                           "#+END_EXPORT\n"))))
          (goto-char (point-min))
          (oletptceu--delete-line)
          (oletptceu--string-to-file (buffer-string) temp-org))))  ; Write new Org file to be fed to exporter
    (setq undo-tree-auto-save-history nil)  ; Try to prevent undo-tree making back-ups for autogenerated files
    (find-file temp-org)
    (org-latex-export-to-pdf)  ; Use Org mode's built-in LaTeX -> PDF exporter to generate PDF from the new Org file
    (oletptceu--delete-current-file t)
    (setq org-export-with-toc org-export-with-toc-orig)
    (setq org-export-with-title org-export-with-title-orig)
    (setq org-export-with-author org-export-with-author-orig)
    (setq org-export-with-email org-export-with-email-orig)
    (setq org-export-with-date org-export-with-date-orig)
    (make-directory (file-name-directory output-file) t)
    (rename-file (concat (file-name-sans-extension temp-org) ".pdf") output-file t)
    (rename-file (concat (file-name-sans-extension temp-org) ".tex") (concat (file-name-sans-extension output-file) ".tex") t)
    (setq oletptceu--fm-found nil)
    (setq oletptceu--mm-found nil)
    (setq oletptceu--bm-found nil)
    (setq undo-tree-auto-save-history undo-tree-auto-save-history-orig)))

(provide 'org-latex-export-to-pdf-tradeback-cubes-en-us)
;;; org-latex-export-to-pdf-tradeback-cubes-en-us.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("oletptceu-" . "org-latex-export-to-pdf-tradeback-cubes-en-us-"))
;; End:
