;;; org-odt-export-to-manuscript-en-us.el --- Org Novelist export template to ODT Manuscript -*- lexical-binding: t; -*-

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
;; easily export and publish stories to other formats. The Org Novelist
;; package supplies a collection of support functions which make it
;; easier to use this methodology.
;;
;; Creating, linking, and laying out files in the Org Novelist
;; methodology can be done without the use of Emacs or the Org Novelist
;; package, but using the package within Emacs will provide helper
;; functions that make the methodology much easier to use; allowing the
;; following of links, programmatic updating of cross-references, and
;; the ability to programmatically export to other formats.
;;
;; This package supplies an example export template to manuscript
;; format, suitable for submission to publishers and publications.
;;
;; It can be used with the standard Org file that is exported from the
;; Org Novelist package, or be called though Org Novelist's export
;; functions.
;;
;; Installation, Activation, and Documentation
;; -------------------------------------------
;; See the corresponding section of the website at
;;
;;   https://johnurquhartferguson.info
;;
;;
;; You should also have the following typeface installed and
;; accessible on your system:
;;
;; Courier Prime
;; (https://fonts.google.com/specimen/Courier+Prime)
;;
;;
;; Front matter chapters will not have "Chapter X" in the title, but
;; will have the chapter name (unless told not). Front matter chapters
;; will appear before the main matter chapters.
;;
;; Main matter chapters will have "Chapter X" and the chapter name in
;; the title (unless told not). Main matter chapters will appear after
;; the front matter chapters.
;;
;; Back matter chapters will not have "Chapter X" in the title, but
;; will have the chapter name (unless told not). Back matter chapters
;; will appear after the main matter chapters.
;;
;;
;; The following chapter index tags are supported and can be applied to
;; chapter headings in the Org Novelist chapter index:
;;
;; :no_header:
;; Do not include any sort of header at the start of the chapter.
;;
;; :no_header_name:
;; Do not include the chapter name at the start of the chapter.
;; The text "Chapter X" will still be shown if in the main matter.
;;
;; :no_header_preamble:
;; Do not include the text "Chapter X" at the start of the chapter.
;; The chapter name will still be shown. Chapter will still be used to
;; calculate the numbering of other main matter chapters.
;;
;; :no_toc_entry:
;; Chapter will have no "Chapter X" text, and will not be used to
;; calculate the numbering of main matter chapters. However, if user
;; actively generates a table of contents in post, then this chapter
;; will likely still be included.
;;
;; :part:
;; Treat heading as a "Part" of the story (the level above chapter).
;;
;;
;; The following optional configuration overrides are supported and can
;; be applied using the org-novelist-config.org file of the story:
;;
;; #+TITLE:
;; The title of the book.
;; eg: Book Title
;;
;; #+SUBTITLE:
;; The secondary title of the book.
;; eg: Book Subtitle
;;
;; #+AUTHOR:
;; The author of the book.
;; eg: Book Author
;;
;; #+EMAIL:
;; The email address of the book author.
;; eg: mail@author-email.org
;;
;; #+DATE:
;; The publication date of the book as an inactive Org timestamp.
;; eg: [2013-02-08 Fri 09:29]

;;; Code:

;;;; Require other packages

(require 'org)  ; Org Novelist is built upon the incredible work of Org mode
(require 'ox)  ; Necessary to call Org's built-in export functions.


;;;; Helper Functions

(defun ooetmeu--fold-show-all ()
  "Run the deprecated `org-show-all' when Org version is less than 9.6.
Otherwise, run `org-fold-show-all'."
  (if (string-version-lessp (org-version) "9.6")
      (org-show-all)
    (org-fold-show-all)))

(defun ooetmeu--delete-line ()
  "If Emacs version is less than 29, delete line the old fashioned way."
  (let ((inhibit-field-text-motion t))
    (if (>= (string-to-number (nth 0 (split-string (string-trim-left (emacs-version) "GNU Emacs ") "\\."))) 29)
        (delete-line)
      (delete-region (line-beginning-position) (line-beginning-position 2)))))

(defun ooetmeu--delete-current-file (&optional no-prompt)
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

(defun ooetmeu--string-to-file (str filename)
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

(defun ooetmeu--set-file-property-value (property value &optional file no-overwrite)
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

(defun ooetmeu--get-file-property-value (property &optional file)
  "Given an Org FILE, return the value of PROPERTY.
If FILE not provided, work on current buffer."
  (let ((value "")
        (regexp (format "^[ \t]*#\\+%s:" (regexp-quote property)))
        (case-fold-search t)
        beg
        (curr-buff-str (buffer-string)))
    (with-temp-buffer
      (if file
          (when (file-exists-p file)
            (when (file-readable-p file)
              (insert-file-contents file)))
        (insert curr-buff-str))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (when (looking-at-p " ")
          (forward-char))
        (setq beg (point))
        (end-of-line)
        (setq value (org-trim (buffer-substring beg (point))))))
    value))

(defun ooetmeu--delete-file-property-value (property &optional file)
  "Given a FILE, delete the entry for PROPERTY.
If FILE not provided, work on current buffer."
  (when file
    (when (file-exists-p file)
      (when (file-readable-p file)
        (find-file file))))
  (let ((regexp (format "^[ \t]*#\\+%s:" (regexp-quote property)))
        (case-fold-search t))
    (while (re-search-forward regexp nil t)
      (beginning-of-line)
      (ooetmeu--delete-line))))

(defun ooetmeu--generate-odt-style-string (org-input-file &optional typeface)
  "Generate the ODT XML stylesheet using ORG-INPUT-FILE.
Override the default TYPEFACE (Courier Prime) if required."
  (unless typeface
    (setq typeface "Courier Prime"))
  (unless (find-font (font-spec :name typeface))
    (setq typeface "Courier Prime"))
  (unless (find-font (font-spec :name typeface))
    (setq typeface "Courier New"))
  (let ((out-xml ""))
    (setq out-xml (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<office:document-styles xmlns:css3t=\"http://www.w3.org/TR/css3-text/\" xmlns:grddl=\"http://www.w3.org/2003/g/data-view#\" xmlns:xhtml=\"http://www.w3.org/1999/xhtml\" xmlns:dom=\"http://www.w3.org/2001/xml-events\" xmlns:script=\"urn:oasis:names:tc:opendocument:xmlns:script:1.0\" xmlns:form=\"urn:oasis:names:tc:opendocument:xmlns:form:1.0\" xmlns:math=\"http://www.w3.org/1998/Math/MathML\" xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\" xmlns:ooo=\"http://openoffice.org/2004/office\" xmlns:fo=\"urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0\" xmlns:ooow=\"http://openoffice.org/2004/writer\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:drawooo=\"http://openoffice.org/2010/draw\" xmlns:oooc=\"http://openoffice.org/2004/calc\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:calcext=\"urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0\" xmlns:style=\"urn:oasis:names:tc:opendocument:xmlns:style:1.0\" xmlns:text=\"urn:oasis:names:tc:opendocument:xmlns:text:1.0\" xmlns:of=\"urn:oasis:names:tc:opendocument:xmlns:of:1.2\" xmlns:tableooo=\"http://openoffice.org/2009/table\" xmlns:draw=\"urn:oasis:names:tc:opendocument:xmlns:drawing:1.0\" xmlns:dr3d=\"urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0\" xmlns:rpt=\"http://openoffice.org/2005/report\" xmlns:svg=\"urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0\" xmlns:chart=\"urn:oasis:names:tc:opendocument:xmlns:chart:1.0\" xmlns:officeooo=\"http://openoffice.org/2009/office\" xmlns:table=\"urn:oasis:names:tc:opendocument:xmlns:table:1.0\" xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\" xmlns:loext=\"urn:org:documentfoundation:names:experimental:office:xmlns:loext:1.0\" xmlns:number=\"urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0\" xmlns:field=\"urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0\" office:version=\"1.3\"><office:font-face-decls><style:font-face style:name=\"" typeface "\" svg:font-family=\"&apos;" typeface "&apos;\" style:font-family-generic=\"modern\" style:font-pitch=\"fixed\"/><style:font-face style:name=\"" typeface "\" svg:font-family=\"&apos;" typeface "&apos;\" style:font-adornments=\"Regular\" style:font-pitch=\"fixed\"/><style:font-face style:name=\"NSimSun\" svg:font-family=\"NSimSun\" style:font-family-generic=\"modern\" style:font-pitch=\"fixed\"/><style:font-face style:name=\"SimSun\" svg:font-family=\"SimSun\" style:font-family-generic=\"system\" style:font-pitch=\"variable\"/><style:font-face style:name=\"Tahoma\" svg:font-family=\"Tahoma\"/><style:font-face style:name=\"Tahoma1\" svg:font-family=\"Tahoma\" style:font-family-generic=\"system\" style:font-pitch=\"variable\"/><style:font-face style:name=\"Times New Roman\" svg:font-family=\"&apos;Times New Roman&apos;\" style:font-family-generic=\"roman\" style:font-pitch=\"variable\"/></office:font-face-decls><office:styles><style:default-style style:family=\"graphic\"><style:graphic-properties svg:stroke-color=\"#3465a4\" draw:fill-color=\"#729fcf\" fo:wrap-option=\"wrap\" draw:shadow-offset-x=\"0.3cm\" draw:shadow-offset-y=\"0.3cm\" draw:start-line-spacing-horizontal=\"0.283cm\" draw:start-line-spacing-vertical=\"0.283cm\" draw:end-line-spacing-horizontal=\"0.283cm\" draw:end-line-spacing-vertical=\"0.283cm\" style:writing-mode=\"lr-tb\" style:flow-with-text=\"false\"/><style:paragraph-properties style:text-autospace=\"ideograph-alpha\" style:line-break=\"strict\" style:writing-mode=\"lr-tb\" style:font-independent-line-spacing=\"false\"><style:tab-stops/></style:paragraph-properties><style:text-properties style:use-window-font-color=\"true\" loext:opacity=\"0%\" loext:color-lum-mod=\"100%\" loext:color-lum-off=\"0%\" style:font-name=\"Times New Roman\" fo:font-size=\"12pt\" fo:language=\"en\" fo:country=\"GB\" style:letter-kerning=\"true\" style:font-name-asian=\"SimSun\" style:font-size-asian=\"12pt\" style:language-asian=\"zh\" style:country-asian=\"CN\" style:font-name-complex=\"Tahoma1\" style:font-size-complex=\"12pt\" style:language-complex=\"hi\" style:country-complex=\"IN\"/></style:default-style><style:default-style style:family=\"paragraph\"><style:paragraph-properties fo:hyphenation-ladder-count=\"no-limit\" style:text-autospace=\"ideograph-alpha\" style:punctuation-wrap=\"hanging\" style:line-break=\"strict\" style:tab-stop-distance=\"1.251cm\" style:writing-mode=\"page\"/><style:text-properties style:use-window-font-color=\"true\" loext:opacity=\"0%\" style:font-name=\"Times New Roman\" fo:font-size=\"12pt\" fo:language=\"en\" fo:country=\"GB\" style:letter-kerning=\"true\" style:font-name-asian=\"SimSun\" style:font-size-asian=\"12pt\" style:language-asian=\"zh\" style:country-asian=\"CN\" style:font-name-complex=\"Tahoma1\" style:font-size-complex=\"12pt\" style:language-complex=\"hi\" style:country-complex=\"IN\" fo:hyphenate=\"false\" fo:hyphenation-remain-char-count=\"2\" fo:hyphenation-push-char-count=\"2\" loext:hyphenation-no-caps=\"false\" loext:hyphenation-no-last-word=\"false\" loext:hyphenation-word-char-count=\"no-limit\" loext:hyphenation-zone=\"no-limit\"/></style:default-style><style:default-style style:family=\"table\"><style:table-properties table:border-model=\"collapsing\"/></style:default-style><style:default-style style:family=\"table-row\"><style:table-row-properties fo:keep-together=\"auto\"/></style:default-style><style:style style:name=\"Standard\" style:family=\"paragraph\" style:class=\"text\"><style:paragraph-properties fo:line-height=\"200%\"/><style:text-properties style:font-name=\"" typeface "\" fo:font-family=\"&apos;" typeface "&apos;\" style:font-style-name=\"Regular\" style:font-pitch=\"fixed\"/></style:style><style:style style:name=\"Heading\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:next-style-name=\"Text_20_body\" style:class=\"text\"><style:paragraph-properties fo:margin-top=\"0.423cm\" fo:margin-bottom=\"0.212cm\" style:contextual-spacing=\"false\" fo:keep-with-next=\"always\"><style:tab-stops><style:tab-stop style:position=\"17cm\" style:type=\"right\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"Text_20_body\" style:display-name=\"Text body\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"text\" style:master-page-name=\"\"><style:paragraph-properties fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:margin-top=\"0cm\" fo:margin-bottom=\"0cm\" style:contextual-spacing=\"false\" fo:orphans=\"0\" fo:widows=\"0\" fo:text-indent=\"1.27cm\" style:auto-text-indent=\"false\" style:page-number=\"auto\"/></style:style><style:style style:name=\"List\" style:family=\"paragraph\" style:parent-style-name=\"Text_20_body\" style:class=\"list\"><style:text-properties style:font-name-complex=\"Tahoma\" style:font-family-complex=\"Tahoma\"/></style:style><style:style style:name=\"Caption\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"extra\"><style:paragraph-properties fo:margin-top=\"0.212cm\" fo:margin-bottom=\"0.212cm\" style:contextual-spacing=\"false\" text:number-lines=\"false\" text:line-number=\"0\"/></style:style><style:style style:name=\"Index\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"index\"><style:paragraph-properties text:number-lines=\"false\" text:line-number=\"0\"/><style:text-properties style:font-name-complex=\"Tahoma\" style:font-family-complex=\"Tahoma\"/></style:style><style:style style:name=\"RightAlign\" style:family=\"paragraph\" style:parent-style-name=\"Text_20_body\"><style:paragraph-properties fo:text-align=\"right\" style:justify-single-word=\"false\"/></style:style><style:style style:name=\"Heading_20_1\" style:display-name=\"Heading 1\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:default-outline-level=\"1\" style:class=\"text\" style:master-page-name=\"\"><style:paragraph-properties fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:margin-top=\"9.31cm\" fo:margin-bottom=\"0.42cm\" style:contextual-spacing=\"false\" fo:text-align=\"center\" style:justify-single-word=\"false\" fo:orphans=\"2\" fo:widows=\"2\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\" style:page-number=\"auto\" fo:break-before=\"page\"/><style:text-properties fo:font-size=\"12pt\" fo:font-weight=\"normal\" style:font-size-asian=\"115%\" style:font-weight-asian=\"bold\" style:font-size-complex=\"115%\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"Heading_5f_20_5f_1_5f_unnumbered\" style:display-name=\"Heading_20_1_unnumbered\" style:family=\"paragraph\" style:parent-style-name=\"Heading_20_1\" style:default-outline-level=\"\" style:list-style-name=\"\" style:master-page-name=\"\"><style:paragraph-properties fo:margin-top=\"9.31cm\" fo:margin-bottom=\"0.42cm\" style:contextual-spacing=\"false\" fo:orphans=\"2\" fo:widows=\"2\" style:page-number=\"auto\" fo:break-before=\"page\"><style:tab-stops/></style:paragraph-properties><style:text-properties fo:font-size=\"13.8000001907349pt\" fo:font-weight=\"normal\"/></style:style><style:style style:name=\"Heading_20_2\" style:display-name=\"Heading 2\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:default-outline-level=\"2\" style:class=\"text\"><style:text-properties fo:font-size=\"14pt\" fo:font-style=\"italic\" fo:font-weight=\"bold\" style:font-size-asian=\"14pt\" style:font-style-asian=\"italic\" style:font-weight-asian=\"bold\" style:font-size-complex=\"14pt\" style:font-style-complex=\"italic\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"Heading_5f_20_5f_2_5f_unnumbered\" style:display-name=\"Heading_20_2_unnumbered\" style:family=\"paragraph\" style:parent-style-name=\"Heading_20_2\" style:default-outline-level=\"\" style:list-style-name=\"\"/><style:style style:name=\"Heading_20_3\" style:display-name=\"Heading 3\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:default-outline-level=\"3\" style:class=\"text\"><style:text-properties fo:font-size=\"14pt\" fo:font-weight=\"bold\" style:font-size-asian=\"14pt\" style:font-weight-asian=\"bold\" style:font-size-complex=\"14pt\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"Heading_5f_20_5f_3_5f_unnumbered\" style:display-name=\"Heading_20_3_unnumbered\" style:family=\"paragraph\" style:parent-style-name=\"Heading_20_3\" style:default-outline-level=\"\" style:list-style-name=\"\"/><style:style style:name=\"Heading_20_4\" style:display-name=\"Heading 4\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:default-outline-level=\"4\" style:class=\"text\"><style:text-properties fo:font-size=\"85%\" fo:font-style=\"italic\" fo:font-weight=\"bold\" style:font-size-asian=\"85%\" style:font-style-asian=\"italic\" style:font-weight-asian=\"bold\" style:font-size-complex=\"85%\" style:font-style-complex=\"italic\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"Heading_5f_20_5f_4_5f_unnumbered\" style:display-name=\"Heading_20_4_unnumbered\" style:family=\"paragraph\" style:parent-style-name=\"Heading_20_4\" style:default-outline-level=\"\" style:list-style-name=\"\"/><style:style style:name=\"Heading_20_5\" style:display-name=\"Heading 5\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:default-outline-level=\"5\" style:class=\"text\"><style:text-properties fo:font-size=\"85%\" fo:font-weight=\"bold\" style:font-size-asian=\"85%\" style:font-weight-asian=\"bold\" style:font-size-complex=\"85%\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"Heading_5f_20_5f_5_5f_unnumbered\" style:display-name=\"Heading_20_5_unnumbered\" style:family=\"paragraph\" style:parent-style-name=\"Heading_20_5\" style:default-outline-level=\"\" style:list-style-name=\"\"/><style:style style:name=\"Heading_20_6\" style:display-name=\"Heading 6\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:default-outline-level=\"6\" style:class=\"text\"><style:text-properties fo:font-size=\"75%\" fo:font-weight=\"bold\" style:font-size-asian=\"75%\" style:font-weight-asian=\"bold\" style:font-size-complex=\"75%\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"Heading_5f_20_5f_6_5f_unnumbered\" style:display-name=\"Heading_20_6_unnumbered\" style:family=\"paragraph\" style:parent-style-name=\"Heading_20_6\" style:default-outline-level=\"\" style:list-style-name=\"\"/><style:style style:name=\"Heading_20_7\" style:display-name=\"Heading 7\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:default-outline-level=\"7\" style:class=\"text\"><style:text-properties fo:font-size=\"75%\" fo:font-weight=\"bold\" style:font-size-asian=\"75%\" style:font-weight-asian=\"bold\" style:font-size-complex=\"75%\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"Heading_5f_20_5f_7_5f_unnumbered\" style:display-name=\"Heading_20_7_unnumbered\" style:family=\"paragraph\" style:parent-style-name=\"Heading_20_7\" style:default-outline-level=\"\" style:list-style-name=\"\"/><style:style style:name=\"Heading_20_8\" style:display-name=\"Heading 8\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:default-outline-level=\"8\" style:class=\"text\"><style:text-properties fo:font-size=\"75%\" fo:font-weight=\"bold\" style:font-size-asian=\"75%\" style:font-weight-asian=\"bold\" style:font-size-complex=\"75%\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"Heading_5f_20_5f_8_5f_unnumbered\" style:display-name=\"Heading_20_8_unnumbered\" style:family=\"paragraph\" style:parent-style-name=\"Heading_20_8\" style:default-outline-level=\"\" style:list-style-name=\"\"/><style:style style:name=\"Heading_20_9\" style:display-name=\"Heading 9\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:default-outline-level=\"9\" style:class=\"text\"><style:text-properties fo:font-size=\"75%\" fo:font-weight=\"bold\" style:font-size-asian=\"75%\" style:font-weight-asian=\"bold\" style:font-size-complex=\"75%\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"Heading_5f_20_5f_9_5f_unnumbered\" style:display-name=\"Heading_20_9_unnumbered\" style:family=\"paragraph\" style:parent-style-name=\"Heading_20_9\" style:default-outline-level=\"\" style:list-style-name=\"\"/><style:style style:name=\"Heading_20_10\" style:display-name=\"Heading 10\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:default-outline-level=\"10\" style:class=\"text\"><style:text-properties fo:font-size=\"75%\" fo:font-weight=\"bold\" style:font-size-asian=\"75%\" style:font-weight-asian=\"bold\" style:font-size-complex=\"75%\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"Heading_5f_20_5f_10_5f_unnumbered\" style:display-name=\"Heading_20_10_unnumbered\" style:family=\"paragraph\" style:parent-style-name=\"Heading_20_10\" style:default-outline-level=\"\" style:list-style-name=\"\"/><style:style style:name=\"Heading_20_1.title\" style:display-name=\"Heading 1.title\" style:family=\"paragraph\" style:parent-style-name=\"Heading_20_1\" style:default-outline-level=\"\" style:list-style-name=\"\"><style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/></style:style><style:style style:name=\"Title\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Subtitle\" style:class=\"chapter\"><style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/><style:text-properties fo:font-size=\"18pt\" fo:font-weight=\"bold\" style:font-size-asian=\"18pt\" style:font-weight-asian=\"bold\" style:font-size-complex=\"18pt\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"OrgTitle\" style:family=\"paragraph\" style:parent-style-name=\"Title\" style:master-page-name=\"\"><style:paragraph-properties fo:margin-top=\"7.001cm\" fo:margin-bottom=\"0cm\" style:contextual-spacing=\"false\" fo:orphans=\"0\" fo:widows=\"0\" style:page-number=\"auto\" fo:keep-with-next=\"auto\"/><style:text-properties fo:font-size=\"12pt\" fo:font-weight=\"normal\"/></style:style><style:style style:name=\"Subtitle\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:next-style-name=\"Text_20_body\" style:class=\"chapter\"><style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/><style:text-properties fo:font-size=\"14pt\" fo:font-style=\"italic\" style:font-size-asian=\"14pt\" style:font-style-asian=\"italic\" style:font-size-complex=\"14pt\" style:font-style-complex=\"italic\"/></style:style><style:style style:name=\"OrgSubtitle\" style:family=\"paragraph\" style:parent-style-name=\"Subtitle\" style:master-page-name=\"\"><style:paragraph-properties fo:margin-top=\"0cm\" fo:margin-bottom=\"1cm\" style:contextual-spacing=\"false\" fo:orphans=\"0\" fo:widows=\"0\" style:page-number=\"auto\" fo:keep-with-next=\"auto\"/><style:text-properties style:font-name=\"" typeface "\" fo:font-family=\"&apos;" typeface "&apos;\" style:font-style-name=\"Regular\" style:font-pitch=\"fixed\" fo:font-size=\"12pt\" fo:font-style=\"normal\"/></style:style><style:style style:name=\"Text_20_body_20_indent\" style:display-name=\"Text body indent\" style:family=\"paragraph\" style:parent-style-name=\"Text_20_body\" style:class=\"text\"><style:paragraph-properties fo:margin-left=\"0.499cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\"/></style:style><style:style style:name=\"List_20_Indent\" style:display-name=\"List Indent\" style:family=\"paragraph\" style:parent-style-name=\"Text_20_body\" style:class=\"text\"><style:paragraph-properties fo:margin-left=\"5.001cm\" fo:margin-right=\"0cm\" fo:text-indent=\"-4.5cm\" style:auto-text-indent=\"false\"><style:tab-stops><style:tab-stop style:position=\"0cm\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"First_20_line_20_indent\" style:display-name=\"First line indent\" style:family=\"paragraph\" style:parent-style-name=\"Text_20_body\" style:class=\"text\"><style:paragraph-properties fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0.499cm\" style:auto-text-indent=\"false\"/></style:style><style:style style:name=\"Hanging_20_indent\" style:display-name=\"Hanging indent\" style:family=\"paragraph\" style:parent-style-name=\"Text_20_body\" style:class=\"text\"><style:paragraph-properties fo:margin-left=\"1cm\" fo:margin-right=\"0cm\" fo:text-indent=\"-0.499cm\" style:auto-text-indent=\"false\"><style:tab-stops><style:tab-stop style:position=\"0cm\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"Salutation\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"text\"><style:paragraph-properties text:number-lines=\"false\" text:line-number=\"0\"/></style:style><style:style style:name=\"Index_20_Heading\" style:display-name=\"Index Heading\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:class=\"index\"><style:paragraph-properties fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\" text:number-lines=\"false\" text:line-number=\"0\"/><style:text-properties fo:font-size=\"16pt\" fo:font-weight=\"bold\" style:font-size-asian=\"16pt\" style:font-weight-asian=\"bold\" style:font-size-complex=\"16pt\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"Contents_20_Heading\" style:display-name=\"Contents Heading\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:class=\"index\"><style:paragraph-properties fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\" text:number-lines=\"false\" text:line-number=\"0\"/><style:text-properties fo:font-size=\"16pt\" fo:font-weight=\"bold\" style:font-size-asian=\"16pt\" style:font-weight-asian=\"bold\" style:font-size-complex=\"16pt\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"Contents_20_1\" style:display-name=\"Contents 1\" style:family=\"paragraph\" style:parent-style-name=\"Index\" style:class=\"index\"><style:paragraph-properties fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\"><style:tab-stops><style:tab-stop style:position=\"17cm\" style:type=\"right\" style:leader-style=\"dotted\" style:leader-text=\".\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"Contents_20_2\" style:display-name=\"Contents 2\" style:family=\"paragraph\" style:parent-style-name=\"Index\" style:class=\"index\"><style:paragraph-properties fo:margin-left=\"0.499cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\"><style:tab-stops><style:tab-stop style:position=\"16.501cm\" style:type=\"right\" style:leader-style=\"dotted\" style:leader-text=\".\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"Contents_20_3\" style:display-name=\"Contents 3\" style:family=\"paragraph\" style:parent-style-name=\"Index\" style:class=\"index\"><style:paragraph-properties fo:margin-left=\"0.998cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\"><style:tab-stops><style:tab-stop style:position=\"16.002cm\" style:type=\"right\" style:leader-style=\"dotted\" style:leader-text=\".\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"Contents_20_4\" style:display-name=\"Contents 4\" style:family=\"paragraph\" style:parent-style-name=\"Index\" style:class=\"index\"><style:paragraph-properties fo:margin-left=\"1.498cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\"><style:tab-stops><style:tab-stop style:position=\"15.503cm\" style:type=\"right\" style:leader-style=\"dotted\" style:leader-text=\".\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"Contents_20_5\" style:display-name=\"Contents 5\" style:family=\"paragraph\" style:parent-style-name=\"Index\" style:class=\"index\"><style:paragraph-properties fo:margin-left=\"1.997cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\"><style:tab-stops><style:tab-stop style:position=\"15.004cm\" style:type=\"right\" style:leader-style=\"dotted\" style:leader-text=\".\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"Contents_20_6\" style:display-name=\"Contents 6\" style:family=\"paragraph\" style:parent-style-name=\"Index\" style:class=\"index\"><style:paragraph-properties fo:margin-left=\"2.496cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\"><style:tab-stops><style:tab-stop style:position=\"14.504cm\" style:type=\"right\" style:leader-style=\"dotted\" style:leader-text=\".\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"Contents_20_7\" style:display-name=\"Contents 7\" style:family=\"paragraph\" style:parent-style-name=\"Index\" style:class=\"index\"><style:paragraph-properties fo:margin-left=\"2.995cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\"><style:tab-stops><style:tab-stop style:position=\"14.005cm\" style:type=\"right\" style:leader-style=\"dotted\" style:leader-text=\".\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"Contents_20_8\" style:display-name=\"Contents 8\" style:family=\"paragraph\" style:parent-style-name=\"Index\" style:class=\"index\"><style:paragraph-properties fo:margin-left=\"3.494cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\"><style:tab-stops><style:tab-stop style:position=\"13.506cm\" style:type=\"right\" style:leader-style=\"dotted\" style:leader-text=\".\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"Contents_20_9\" style:display-name=\"Contents 9\" style:family=\"paragraph\" style:parent-style-name=\"Index\" style:class=\"index\"><style:paragraph-properties fo:margin-left=\"3.993cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\"><style:tab-stops><style:tab-stop style:position=\"13.007cm\" style:type=\"right\" style:leader-style=\"dotted\" style:leader-text=\".\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"Contents_20_10\" style:display-name=\"Contents 10\" style:family=\"paragraph\" style:parent-style-name=\"Index\" style:class=\"index\"><style:paragraph-properties fo:margin-left=\"4.493cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\"><style:tab-stops><style:tab-stop style:position=\"12.508cm\" style:type=\"right\" style:leader-style=\"dotted\" style:leader-text=\".\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"Quotations\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"html\" style:master-page-name=\"\"><style:paragraph-properties fo:margin-left=\"3cm\" fo:margin-right=\"3cm\" fo:margin-top=\"0.25cm\" fo:margin-bottom=\"0.25cm\" style:contextual-spacing=\"false\" fo:line-height=\"200%\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\" style:page-number=\"auto\"/></style:style><style:style style:name=\"OrgFootnoteQuotations\" style:family=\"paragraph\" style:parent-style-name=\"Footnote\" style:class=\"html\"><style:paragraph-properties fo:margin-left=\"1cm\" fo:margin-right=\"1cm\" fo:margin-top=\"0cm\" fo:margin-bottom=\"0.499cm\" style:contextual-spacing=\"false\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\"/></style:style><style:style style:name=\"Preformatted_20_Text\" style:display-name=\"Preformatted Text\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"html\"><style:paragraph-properties fo:margin-top=\"0cm\" fo:margin-bottom=\"0cm\" style:contextual-spacing=\"false\"/><style:text-properties style:font-name=\"" typeface "\" fo:font-family=\"&apos;" typeface "&apos;\" style:font-family-generic=\"modern\" style:font-pitch=\"fixed\" fo:font-size=\"10pt\" style:font-name-asian=\"NSimSun\" style:font-family-asian=\"NSimSun\" style:font-family-generic-asian=\"modern\" style:font-pitch-asian=\"fixed\" style:font-size-asian=\"10pt\" style:font-name-complex=\"" typeface "\" style:font-family-complex=\"&apos;" typeface "&apos;\" style:font-family-generic-complex=\"modern\" style:font-pitch-complex=\"fixed\" style:font-size-complex=\"10pt\"/></style:style><style:style style:name=\"OrgVerse\" style:family=\"paragraph\" style:parent-style-name=\"Preformatted_20_Text\"><loext:graphic-properties draw:fill=\"none\" draw:fill-color=\"#729fcf\"/><style:paragraph-properties fo:background-color=\"transparent\" fo:padding=\"0cm\" fo:border=\"none\" style:shadow=\"none\"/><style:text-properties style:font-name=\"" typeface "1\" fo:font-family=\"&apos;" typeface "&apos;\" style:font-style-name=\"Regular\" style:font-pitch=\"fixed\" fo:font-size=\"12pt\"/></style:style><style:style style:name=\"OrgClock\" style:family=\"paragraph\" style:parent-style-name=\"Text_20_body\"><style:paragraph-properties fo:margin-top=\"0cm\" fo:margin-bottom=\"0cm\" style:contextual-spacing=\"false\"/></style:style><style:style style:name=\"OrgClockLastLine\" style:family=\"paragraph\" style:parent-style-name=\"OrgClock\"/><style:style style:name=\"OrgPlanning\" style:family=\"paragraph\" style:parent-style-name=\"Text_20_body\"/><style:style style:name=\"OrgFixedWidthBlock\" style:family=\"paragraph\" style:parent-style-name=\"Preformatted_20_Text\"><loext:graphic-properties draw:fill=\"solid\" draw:fill-color=\"#c0c0c0\" draw:opacity=\"100%\"/><style:paragraph-properties fo:background-color=\"#c0c0c0\" fo:padding=\"0.049cm\" fo:border=\"0.06pt solid #000000\" style:shadow=\"none\"/></style:style><style:style style:name=\"OrgFixedWidthBlockLastLine\" style:family=\"paragraph\" style:parent-style-name=\"OrgFixedWidthBlock\"><style:paragraph-properties fo:margin-top=\"0cm\" fo:margin-bottom=\"0.21cm\" style:contextual-spacing=\"false\"/></style:style><style:style style:name=\"OrgFormula\" style:family=\"paragraph\" style:parent-style-name=\"OrgPlanning\"><style:paragraph-properties><style:tab-stops><style:tab-stop style:position=\"17cm\" style:type=\"right\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"OrgSrcBlockLastLine\" style:family=\"paragraph\" style:parent-style-name=\"OrgSrcBlock\"><style:paragraph-properties fo:margin-top=\"0cm\" fo:margin-bottom=\"0.21cm\" style:contextual-spacing=\"false\"/></style:style><style:style style:name=\"OrgCenter\" style:family=\"paragraph\" style:parent-style-name=\"Text_20_body\"><style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/></style:style><style:style style:name=\"OrgFootnoteCenter\" style:family=\"paragraph\" style:parent-style-name=\"Footnote\"><style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/></style:style><style:style style:name=\"OrgTableContents\" style:family=\"paragraph\" style:parent-style-name=\"Text_20_body\"/><style:style style:name=\"OrgTableHeading\" style:family=\"paragraph\" style:parent-style-name=\"OrgTableContents\" style:class=\"extra\"><style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\" text:number-lines=\"false\" text:line-number=\"0\"/><style:text-properties fo:font-weight=\"normal\" style:font-weight-asian=\"bold\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"OrgTableHeadingLeft\" style:family=\"paragraph\" style:parent-style-name=\"OrgTableHeading\"><style:paragraph-properties fo:text-align=\"start\" style:justify-single-word=\"false\"/></style:style><style:style style:name=\"OrgTableHeadingRight\" style:family=\"paragraph\" style:parent-style-name=\"OrgTableHeading\"><style:paragraph-properties fo:text-align=\"end\" style:justify-single-word=\"false\"/></style:style><style:style style:name=\"OrgTableHeadingCenter\" style:family=\"paragraph\" style:parent-style-name=\"OrgTableHeading\"><style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/></style:style><style:style style:name=\"OrgTableContentsLeft\" style:family=\"paragraph\" style:parent-style-name=\"OrgTableContents\"><style:paragraph-properties fo:text-align=\"start\" style:justify-single-word=\"false\"/></style:style><style:style style:name=\"OrgTableContentsRight\" style:family=\"paragraph\" style:parent-style-name=\"OrgTableContents\"><style:paragraph-properties fo:text-align=\"end\" style:justify-single-word=\"false\"/></style:style><style:style style:name=\"OrgTableContentsCenter\" style:family=\"paragraph\" style:parent-style-name=\"OrgTableContents\"><style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/></style:style><style:style style:name=\"Text_20_body_20_bold\" style:display-name=\"Text body bold\" style:family=\"paragraph\" style:parent-style-name=\"Text_20_body\" style:next-style-name=\"Text_20_body\"><style:text-properties fo:font-weight=\"bold\"/></style:style><style:style style:name=\"Footnote\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"extra\"><style:paragraph-properties fo:margin-left=\"0.499cm\" fo:margin-right=\"0cm\" fo:text-indent=\"-0.499cm\" style:auto-text-indent=\"false\" text:number-lines=\"false\" text:line-number=\"0\"/><style:text-properties fo:font-size=\"10pt\" style:font-size-asian=\"10pt\" style:font-size-complex=\"10pt\"/></style:style><style:style style:name=\"Figure\" style:family=\"paragraph\" style:parent-style-name=\"Caption\" style:class=\"extra\"/><style:style style:name=\"Illustration_20_Index_20_Heading\" style:display-name=\"Illustration Index Heading\" style:family=\"paragraph\" style:parent-style-name=\"Heading\" style:class=\"index\"><style:paragraph-properties fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\" text:number-lines=\"false\" text:line-number=\"0\"/><style:text-properties fo:font-size=\"16pt\" fo:font-weight=\"bold\" style:font-size-asian=\"16pt\" style:font-weight-asian=\"bold\" style:font-size-complex=\"16pt\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"Table\" style:family=\"paragraph\" style:parent-style-name=\"Caption\" style:class=\"extra\"><style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/></style:style><style:style style:name=\"Listing\" style:family=\"paragraph\" style:parent-style-name=\"Caption\" style:class=\"extra\"><style:paragraph-properties fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\" fo:keep-with-next=\"always\"><style:tab-stops/></style:paragraph-properties></style:style><style:style style:name=\"Horizontal_20_Line\" style:display-name=\"Horizontal Line\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:next-style-name=\"Text_20_body\" style:class=\"html\" style:master-page-name=\"\"><style:paragraph-properties fo:margin-top=\"0cm\" fo:margin-bottom=\"0.21cm\" style:contextual-spacing=\"false\" style:page-number=\"auto\" fo:padding=\"0cm\" fo:border-left=\"none\" fo:border-right=\"none\" fo:border-top=\"none\" fo:border-bottom=\"0.06pt solid #000000\" style:shadow=\"none\" text:number-lines=\"false\" text:line-number=\"0\" style:join-border=\"false\"/><style:text-properties fo:font-size=\"6pt\" style:font-size-asian=\"6pt\" style:font-size-complex=\"6pt\"/></style:style><style:style style:name=\"OrgInlineTaskHeading\" style:family=\"paragraph\" style:parent-style-name=\"Caption\" style:next-style-name=\"Text_20_body\"><style:text-properties fo:font-style=\"normal\" fo:font-weight=\"bold\"/></style:style><style:style style:name=\"Header_20_and_20_Footer\" style:display-name=\"Header and Footer\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"extra\"><style:paragraph-properties text:number-lines=\"false\" text:line-number=\"0\"><style:tab-stops><style:tab-stop style:position=\"8.795cm\" style:type=\"center\"/><style:tab-stop style:position=\"17.59cm\" style:type=\"right\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"Footer\" style:family=\"paragraph\" style:parent-style-name=\"Header_20_and_20_Footer\" style:class=\"extra\"><style:paragraph-properties text:number-lines=\"false\" text:line-number=\"0\"><style:tab-stops><style:tab-stop style:position=\"8.795cm\" style:type=\"center\"/><style:tab-stop style:position=\"17.59cm\" style:type=\"right\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"Addressee\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"extra\"><style:paragraph-properties fo:margin-top=\"0cm\" fo:margin-bottom=\"0.106cm\" style:contextual-spacing=\"false\" text:number-lines=\"false\" text:line-number=\"0\"/></style:style><style:style style:name=\"Drawing\" style:family=\"paragraph\" style:parent-style-name=\"Caption\" style:class=\"extra\"/><style:style style:name=\"Illustration\" style:family=\"paragraph\" style:parent-style-name=\"Caption\" style:class=\"extra\"/><style:style style:name=\"Text\" style:family=\"paragraph\" style:parent-style-name=\"Caption\" style:class=\"extra\"/><style:style style:name=\"Object_20_index_20_heading\" style:display-name=\"Object index heading\" style:family=\"paragraph\" style:parent-style-name=\"Index_20_Heading\" style:class=\"index\"><style:paragraph-properties fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\" text:number-lines=\"false\" text:line-number=\"0\"/><style:text-properties fo:font-size=\"12pt\" fo:font-weight=\"normal\" style:font-size-asian=\"16pt\" style:font-weight-asian=\"bold\" style:font-size-complex=\"16pt\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"Header\" style:family=\"paragraph\" style:parent-style-name=\"Header_20_and_20_Footer\" style:class=\"extra\" style:master-page-name=\"\"><style:paragraph-properties fo:margin-top=\"0cm\" fo:margin-bottom=\"0cm\" style:contextual-spacing=\"false\" fo:line-height=\"100%\" fo:text-align=\"end\" style:justify-single-word=\"false\" fo:orphans=\"2\" fo:widows=\"2\" style:page-number=\"auto\" text:number-lines=\"false\" text:line-number=\"0\"><style:tab-stops><style:tab-stop style:position=\"8.26cm\" style:type=\"center\"/><style:tab-stop style:position=\"16.51cm\" style:type=\"right\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"Header_20_left\" style:display-name=\"Header left\" style:family=\"paragraph\" style:parent-style-name=\"Header\" style:class=\"extra\"><style:paragraph-properties text:number-lines=\"false\" text:line-number=\"0\"><style:tab-stops><style:tab-stop style:position=\"8.795cm\" style:type=\"center\"/><style:tab-stop style:position=\"17.59cm\" style:type=\"right\"/></style:tab-stops></style:paragraph-properties></style:style><style:style style:name=\"Table_20_Contents\" style:display-name=\"Table Contents\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"extra\"><style:paragraph-properties fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:line-height=\"100%\" fo:text-align=\"start\" style:justify-single-word=\"false\" fo:text-indent=\"0cm\" style:auto-text-indent=\"false\" text:number-lines=\"false\" text:line-number=\"0\" style:snap-to-layout-grid=\"true\" style:writing-mode=\"lr-tb\"/></style:style><style:style style:name=\"Table_20_Heading\" style:display-name=\"Table Heading\" style:family=\"paragraph\" style:parent-style-name=\"Table_20_Contents\" style:class=\"extra\"><style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\" text:number-lines=\"false\" text:line-number=\"0\"/><style:text-properties fo:font-weight=\"bold\" style:font-weight-asian=\"bold\" style:font-weight-complex=\"bold\"/></style:style><style:style style:name=\"Heading_20_1_20_First\" style:display-name=\"Heading 1 First\" style:family=\"paragraph\" style:parent-style-name=\"Heading_20_1\" style:auto-update=\"true\" style:default-outline-level=\"\" style:list-style-name=\"\" style:master-page-name=\"\"><style:paragraph-properties fo:margin-top=\"0.84cm\" fo:margin-bottom=\"0.42cm\" style:contextual-spacing=\"false\" fo:orphans=\"0\" fo:widows=\"0\" style:page-number=\"auto\" fo:break-before=\"auto\" fo:break-after=\"auto\" fo:keep-with-next=\"auto\"/></style:style><style:style style:name=\"Table_20_Contents_20_Right_20_Align\" style:display-name=\"Table Contents Right Align\" style:family=\"paragraph\" style:parent-style-name=\"Table_20_Contents\"><style:paragraph-properties fo:text-align=\"end\" style:justify-single-word=\"false\"/></style:style><style:style style:name=\"Emphasis\" style:family=\"text\"><style:text-properties fo:font-style=\"italic\" style:font-style-asian=\"italic\" style:font-style-complex=\"italic\"/></style:style><style:style style:name=\"Underline\" style:family=\"text\"><style:text-properties style:text-underline-style=\"solid\" style:text-underline-width=\"auto\" style:text-underline-color=\"font-color\" fo:background-color=\"transparent\"/></style:style><style:style style:name=\"Strikethrough\" style:family=\"text\"><style:text-properties style:text-line-through-style=\"solid\" style:text-line-through-type=\"single\"/></style:style><style:style style:name=\"Source_20_Text\" style:display-name=\"Source Text\" style:family=\"text\"><style:text-properties style:font-name=\"" typeface "\" fo:font-family=\"&apos;" typeface "&apos;\" style:font-family-generic=\"modern\" style:font-pitch=\"fixed\" fo:background-color=\"transparent\" style:font-name-asian=\"NSimSun\" style:font-family-asian=\"NSimSun\" style:font-family-generic-asian=\"modern\" style:font-pitch-asian=\"fixed\" style:font-name-complex=\"" typeface "\" style:font-family-complex=\"&apos;" typeface "&apos;\" style:font-family-generic-complex=\"modern\" style:font-pitch-complex=\"fixed\"/></style:style><style:style style:name=\"Citation\" style:family=\"text\"><style:text-properties fo:font-style=\"italic\" style:font-style-asian=\"italic\" style:font-style-complex=\"italic\"/></style:style><style:style style:name=\"Example\" style:family=\"text\"><style:text-properties style:font-name=\"" typeface "\" fo:font-family=\"&apos;" typeface "&apos;\" style:font-family-generic=\"modern\" style:font-pitch=\"fixed\" fo:background-color=\"transparent\" style:font-name-asian=\"NSimSun\" style:font-family-asian=\"NSimSun\" style:font-family-generic-asian=\"modern\" style:font-pitch-asian=\"fixed\" style:font-name-complex=\"" typeface "\" style:font-family-complex=\"&apos;" typeface "&apos;\" style:font-family-generic-complex=\"modern\" style:font-pitch-complex=\"fixed\"/></style:style><style:style style:name=\"OrgCode\" style:family=\"text\" style:parent-style-name=\"Source_20_Text\"/><style:style style:name=\"OrgTodo\" style:family=\"text\"/><style:style style:name=\"OrgDone\" style:family=\"text\"/><style:style style:name=\"OrgTag\" style:family=\"text\"><style:text-properties fo:font-variant=\"small-caps\" fo:background-color=\"transparent\"/></style:style><style:style style:name=\"OrgTags\" style:family=\"text\"/><style:style style:name=\"OrgPriority\" style:family=\"text\"/><style:style style:name=\"OrgPriority-A\" style:family=\"text\" style:parent-style-name=\"OrgPriority\"/><style:style style:name=\"OrgPriority-B\" style:family=\"text\" style:parent-style-name=\"OrgPriority\"/><style:style style:name=\"OrgPriority-C\" style:family=\"text\" style:parent-style-name=\"OrgPriority\"/><style:style style:name=\"OrgTimestamp\" style:family=\"text\"><style:text-properties style:font-name=\"" typeface "\" fo:font-family=\"&apos;" typeface "&apos;\" style:font-family-generic=\"modern\" style:font-pitch=\"fixed\" fo:background-color=\"transparent\" style:font-name-asian=\"NSimSun\" style:font-family-asian=\"NSimSun\" style:font-family-generic-asian=\"modern\" style:font-pitch-asian=\"fixed\" style:font-name-complex=\"" typeface "\" style:font-family-complex=\"&apos;" typeface "&apos;\" style:font-family-generic-complex=\"modern\" style:font-pitch-complex=\"fixed\"/></style:style><style:style style:name=\"OrgActiveTimestamp\" style:family=\"text\" style:parent-style-name=\"OrgTimestamp\"/><style:style style:name=\"OrgInactiveTimestamp\" style:family=\"text\" style:parent-style-name=\"OrgTimestamp\"/><style:style style:name=\"OrgTimestampKeyword\" style:family=\"text\"><style:text-properties style:use-window-font-color=\"true\" loext:opacity=\"0%\" fo:font-weight=\"bold\"/></style:style><style:style style:name=\"OrgScheduledKeyword\" style:family=\"text\" style:parent-style-name=\"OrgTimestampKeyword\"/><style:style style:name=\"OrgDeadlineKeyword\" style:family=\"text\" style:parent-style-name=\"OrgTimestampKeyword\"/><style:style style:name=\"OrgClockKeyword\" style:family=\"text\" style:parent-style-name=\"OrgTimestampKeyword\"/><style:style style:name=\"OrgClosedKeyword\" style:family=\"text\" style:parent-style-name=\"OrgTimestampKeyword\"/><style:style style:name=\"OrgTimestampWrapper\" style:family=\"text\"/><style:style style:name=\"OrgTarget\" style:family=\"text\"/><style:style style:name=\"Bold\" style:family=\"text\"><style:text-properties fo:font-weight=\"bold\"/></style:style><style:style style:name=\"Numbering_20_Symbols\" style:display-name=\"Numbering Symbols\" style:family=\"text\"/><style:style style:name=\"Footnote_20_Symbol\" style:display-name=\"Footnote Symbol\" style:family=\"text\"/><style:style style:name=\"Footnote_20_anchor\" style:display-name=\"Footnote anchor\" style:family=\"text\"><style:text-properties style:text-position=\"super 58%\"/></style:style><style:style style:name=\"OrgSuperscript\" style:family=\"text\"><style:text-properties style:text-position=\"super 58%\"/></style:style><style:style style:name=\"OrgSubscript\" style:family=\"text\"><style:text-properties style:text-position=\"sub 58%\"/></style:style><style:style style:name=\"Internet_20_link\" style:display-name=\"Internet link\" style:family=\"text\"><style:text-properties fo:color=\"#000080\" loext:opacity=\"100%\" fo:language=\"zxx\" fo:country=\"none\" style:text-underline-style=\"solid\" style:text-underline-width=\"auto\" style:text-underline-color=\"font-color\" style:language-asian=\"zxx\" style:country-asian=\"none\" style:language-complex=\"zxx\" style:country-complex=\"none\"/></style:style><style:style style:name=\"Bullet_5f_20_5f_Symbols\" style:display-name=\"Bullet_20_Symbols\" style:family=\"text\"/><style:style style:name=\"Graphics\" style:family=\"graphic\"><style:graphic-properties text:anchor-type=\"paragraph\" svg:x=\"0cm\" svg:y=\"0cm\" style:wrap=\"none\" style:vertical-pos=\"top\" style:vertical-rel=\"paragraph\" style:horizontal-pos=\"center\" style:horizontal-rel=\"paragraph\"/></style:style><style:style style:name=\"Frame\" style:family=\"graphic\"><style:graphic-properties text:anchor-type=\"paragraph\" svg:x=\"0cm\" svg:y=\"0cm\" fo:margin-left=\"0.201cm\" fo:margin-right=\"0.201cm\" fo:margin-top=\"0.201cm\" fo:margin-bottom=\"0.201cm\" style:wrap=\"parallel\" style:number-wrapped-paragraphs=\"no-limit\" style:wrap-contour=\"false\" style:vertical-pos=\"top\" style:vertical-rel=\"paragraph-content\" style:horizontal-pos=\"center\" style:horizontal-rel=\"paragraph-content\" fo:padding=\"0.15cm\" fo:border=\"0.06pt solid #000000\"/></style:style><style:style style:name=\"OrgDisplayImage\" style:family=\"graphic\" style:parent-style-name=\"Graphics\"><style:graphic-properties text:anchor-type=\"paragraph\" svg:x=\"0cm\" svg:y=\"0cm\" style:wrap=\"none\" style:vertical-pos=\"top\" style:vertical-rel=\"paragraph\" style:horizontal-pos=\"center\" style:horizontal-rel=\"paragraph\"/></style:style><style:style style:name=\"OrgPageImage\" style:family=\"graphic\" style:parent-style-name=\"Graphics\"><style:graphic-properties text:anchor-type=\"page\" svg:x=\"0cm\" svg:y=\"0cm\" fo:margin-top=\"0.21cm\" fo:margin-bottom=\"0.21cm\" style:vertical-pos=\"middle\" style:vertical-rel=\"page\" style:horizontal-pos=\"center\" style:horizontal-rel=\"page\" fo:background-color=\"transparent\" draw:fill=\"none\" draw:fill-color=\"#729fcf\" style:shadow=\"none\" draw:shadow-opacity=\"100%\"/></style:style><style:style style:name=\"OrgCaptionedImage\" style:family=\"graphic\" style:parent-style-name=\"Graphics\"><style:graphic-properties svg:width=\"0.041cm\" style:rel-width=\"100%\" fo:min-height=\"0.041cm\" text:anchor-type=\"paragraph\" svg:x=\"0cm\" svg:y=\"0cm\" fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:margin-top=\"0cm\" fo:margin-bottom=\"0cm\" style:run-through=\"foreground\" style:wrap=\"none\" style:vertical-pos=\"from-top\" style:vertical-rel=\"paragraph-content\" style:horizontal-pos=\"from-left\" style:horizontal-rel=\"paragraph-content\" fo:padding=\"0cm\" fo:border=\"none\" style:shadow=\"none\" draw:shadow-opacity=\"100%\" loext:rel-width-rel=\"paragraph\"/></style:style><style:style style:name=\"OrgImageCaptionFrame\" style:family=\"graphic\" style:parent-style-name=\"Frame\"><style:graphic-properties text:anchor-type=\"paragraph\" svg:x=\"0cm\" svg:y=\"0cm\" fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:margin-top=\"0cm\" fo:margin-bottom=\"0cm\" style:wrap=\"none\" style:vertical-pos=\"top\" style:vertical-rel=\"paragraph\" style:horizontal-pos=\"center\" style:horizontal-rel=\"paragraph\" fo:padding=\"0cm\" fo:border=\"none\"/></style:style><style:style style:name=\"OrgPageImageCaptionFrame\" style:family=\"graphic\" style:parent-style-name=\"Frame\"><style:graphic-properties text:anchor-type=\"paragraph\" svg:x=\"0cm\" svg:y=\"0cm\" fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:margin-top=\"0.21cm\" fo:margin-bottom=\"0.21cm\" style:wrap=\"none\" style:vertical-pos=\"middle\" style:vertical-rel=\"page\" style:horizontal-pos=\"center\" style:horizontal-rel=\"page\" fo:background-color=\"transparent\" draw:fill=\"none\" draw:fill-color=\"#729fcf\" fo:padding=\"0cm\" fo:border=\"none\" style:shadow=\"none\" draw:shadow-opacity=\"100%\"/></style:style><style:style style:name=\"OrgInlineImage\" style:family=\"graphic\" style:parent-style-name=\"Graphics\"><style:graphic-properties text:anchor-type=\"as-char\" svg:x=\"0cm\" svg:y=\"0cm\" style:vertical-pos=\"top\" style:vertical-rel=\"baseline\" style:horizontal-pos=\"center\" style:horizontal-rel=\"paragraph\"/></style:style><style:style style:name=\"OrgFormula\" style:family=\"graphic\"><style:graphic-properties text:anchor-type=\"as-char\" svg:y=\"0cm\" fo:margin-left=\"0.201cm\" fo:margin-right=\"0.201cm\" style:vertical-pos=\"middle\" style:vertical-rel=\"text\" style:shadow=\"none\" draw:shadow-opacity=\"100%\"/></style:style><style:style style:name=\"OrgInlineFormula\" style:family=\"graphic\" style:parent-style-name=\"Formula\"><style:graphic-properties svg:y=\"0cm\" style:vertical-pos=\"middle\" style:vertical-rel=\"text\"/></style:style><style:style style:name=\"OrgDisplayFormula\" style:family=\"graphic\" style:parent-style-name=\"OrgFormula\"><style:graphic-properties svg:x=\"0cm\" svg:y=\"0cm\" style:vertical-pos=\"middle\" style:vertical-rel=\"text\" style:horizontal-pos=\"from-left\" style:horizontal-rel=\"paragraph-content\"/></style:style><style:style style:name=\"OrgFormulaCaptionFrame\" style:family=\"graphic\" style:parent-style-name=\"Frame\"><style:graphic-properties text:anchor-type=\"paragraph\" svg:x=\"0cm\" svg:y=\"0cm\" fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:margin-top=\"0cm\" fo:margin-bottom=\"0cm\" style:wrap=\"right\" style:number-wrapped-paragraphs=\"1\" style:wrap-contour=\"false\" style:vertical-pos=\"top\" style:vertical-rel=\"paragraph\" style:horizontal-pos=\"center\" style:horizontal-rel=\"paragraph\" fo:padding=\"0cm\" fo:border=\"none\"/></style:style><style:style style:name=\"OrgCaptionedFormula\" style:family=\"graphic\" style:parent-style-name=\"OrgFormula\"><style:graphic-properties svg:x=\"0cm\" svg:y=\"0cm\" fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:margin-top=\"0cm\" fo:margin-bottom=\"0cm\" style:run-through=\"foreground\" style:wrap=\"none\" style:vertical-pos=\"from-top\" style:horizontal-pos=\"center\" style:horizontal-rel=\"paragraph-content\" fo:padding=\"0cm\" fo:border=\"none\" style:shadow=\"none\" draw:shadow-opacity=\"100%\"/></style:style><style:style style:name=\"OrgInlineTaskFrame\" style:family=\"graphic\" style:parent-style-name=\"Frame\"><style:graphic-properties svg:x=\"0cm\" svg:y=\"0cm\" style:wrap=\"none\" style:vertical-pos=\"top\" style:vertical-rel=\"paragraph-content\" style:horizontal-pos=\"center\" style:horizontal-rel=\"paragraph-content\" fo:background-color=\"#ffffcc\" style:background-transparency=\"0%\" draw:fill=\"solid\" draw:fill-color=\"#ffffcc\" draw:opacity=\"100%\" fo:padding=\"0.15cm\" fo:border=\"0.26pt solid #000000\" style:shadow=\"none\" draw:shadow-opacity=\"100%\"/></style:style><style:style style:name=\"Formula\" style:family=\"graphic\"><style:graphic-properties text:anchor-type=\"as-char\" svg:y=\"0cm\" fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" style:vertical-pos=\"middle\" style:vertical-rel=\"text\"/></style:style><text:outline-style style:name=\"Outline\"><text:outline-level-style text:level=\"1\" loext:num-list-format=\"%1%\" style:num-format=\"\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"nothing\" fo:text-indent=\"-0.762cm\" fo:margin-left=\"0.762cm\"/></style:list-level-properties></text:outline-level-style><text:outline-level-style text:level=\"2\" loext:num-list-format=\"%2%\" style:num-format=\"\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"nothing\" fo:text-indent=\"-1.016cm\" fo:margin-left=\"1.016cm\"/></style:list-level-properties></text:outline-level-style><text:outline-level-style text:level=\"3\" loext:num-list-format=\"%3%\" style:num-format=\"\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"nothing\" fo:text-indent=\"-1.27cm\" fo:margin-left=\"1.27cm\"/></style:list-level-properties></text:outline-level-style><text:outline-level-style text:level=\"4\" loext:num-list-format=\"%4%\" style:num-format=\"\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"nothing\" fo:text-indent=\"-1.524cm\" fo:margin-left=\"1.524cm\"/></style:list-level-properties></text:outline-level-style><text:outline-level-style text:level=\"5\" loext:num-list-format=\"%5%\" style:num-format=\"\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"nothing\" fo:text-indent=\"-1.778cm\" fo:margin-left=\"1.778cm\"/></style:list-level-properties></text:outline-level-style><text:outline-level-style text:level=\"6\" loext:num-list-format=\"%6%\" style:num-format=\"\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"nothing\" fo:text-indent=\"-2.032cm\" fo:margin-left=\"2.032cm\"/></style:list-level-properties></text:outline-level-style><text:outline-level-style text:level=\"7\" loext:num-list-format=\"%7%\" style:num-format=\"\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"nothing\" fo:text-indent=\"-2.286cm\" fo:margin-left=\"2.286cm\"/></style:list-level-properties></text:outline-level-style><text:outline-level-style text:level=\"8\" loext:num-list-format=\"%8%\" style:num-format=\"\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"nothing\" fo:text-indent=\"-2.54cm\" fo:margin-left=\"2.54cm\"/></style:list-level-properties></text:outline-level-style><text:outline-level-style text:level=\"9\" loext:num-list-format=\"%9%\" style:num-format=\"\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"nothing\" fo:text-indent=\"-2.794cm\" fo:margin-left=\"2.794cm\"/></style:list-level-properties></text:outline-level-style><text:outline-level-style text:level=\"10\" loext:num-list-format=\"%10%\" style:num-format=\"\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"nothing\" fo:text-indent=\"-3.048cm\" fo:margin-left=\"3.048cm\"/></style:list-level-properties></text:outline-level-style></text:outline-style><text:list-style style:name=\"List_20_1\" style:display-name=\"List 1\"><text:list-level-style-bullet text:level=\"1\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%1%\" text:bullet-char=\"•\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"0.4cm\" fo:text-indent=\"-0.4cm\" fo:margin-left=\"0.4cm\"/></style:list-level-properties><style:text-properties fo:font-family=\"OpenSymbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"2\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%2%\" text:bullet-char=\"•\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"0.801cm\" fo:text-indent=\"-0.4cm\" fo:margin-left=\"0.801cm\"/></style:list-level-properties><style:text-properties fo:font-family=\"OpenSymbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"3\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%3%\" text:bullet-char=\"•\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"1.199cm\" fo:text-indent=\"-0.4cm\" fo:margin-left=\"1.199cm\"/></style:list-level-properties><style:text-properties fo:font-family=\"OpenSymbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"4\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%4%\" text:bullet-char=\"•\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"1.6cm\" fo:text-indent=\"-0.4cm\" fo:margin-left=\"1.6cm\"/></style:list-level-properties><style:text-properties fo:font-family=\"OpenSymbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"5\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%5%\" text:bullet-char=\"•\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"2cm\" fo:text-indent=\"-0.4cm\" fo:margin-left=\"2cm\"/></style:list-level-properties><style:text-properties fo:font-family=\"OpenSymbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"6\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%6%\" text:bullet-char=\"•\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"2.401cm\" fo:text-indent=\"-0.4cm\" fo:margin-left=\"2.401cm\"/></style:list-level-properties><style:text-properties fo:font-family=\"OpenSymbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"7\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%7%\" text:bullet-char=\"•\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"2.799cm\" fo:text-indent=\"-0.4cm\" fo:margin-left=\"2.799cm\"/></style:list-level-properties><style:text-properties fo:font-family=\"OpenSymbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"8\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%8%\" text:bullet-char=\"•\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"3.2cm\" fo:text-indent=\"-0.4cm\" fo:margin-left=\"3.2cm\"/></style:list-level-properties><style:text-properties fo:font-family=\"OpenSymbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"9\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%9%\" text:bullet-char=\"•\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"3.6cm\" fo:text-indent=\"-0.4cm\" fo:margin-left=\"3.6cm\"/></style:list-level-properties><style:text-properties fo:font-family=\"OpenSymbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"10\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%10%\" text:bullet-char=\"•\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"4.001cm\" fo:text-indent=\"-0.4cm\" fo:margin-left=\"4.001cm\"/></style:list-level-properties><style:text-properties fo:font-family=\"OpenSymbol\"/></text:list-level-style-bullet></text:list-style><text:list-style style:name=\"Numbering_20_1\" style:display-name=\"Numbering 1\"><text:list-level-style-number text:level=\"1\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%1%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"0.499cm\" fo:text-indent=\"-0.499cm\" fo:margin-left=\"0.499cm\"/></style:list-level-properties></text:list-level-style-number><text:list-level-style-number text:level=\"2\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%2%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"1cm\" fo:text-indent=\"-0.499cm\" fo:margin-left=\"1cm\"/></style:list-level-properties></text:list-level-style-number><text:list-level-style-number text:level=\"3\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%3%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"1.499cm\" fo:text-indent=\"-0.499cm\" fo:margin-left=\"1.499cm\"/></style:list-level-properties></text:list-level-style-number><text:list-level-style-number text:level=\"4\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%4%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"2cm\" fo:text-indent=\"-0.499cm\" fo:margin-left=\"2cm\"/></style:list-level-properties></text:list-level-style-number><text:list-level-style-number text:level=\"5\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%5%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"2.499cm\" fo:text-indent=\"-0.499cm\" fo:margin-left=\"2.499cm\"/></style:list-level-properties></text:list-level-style-number><text:list-level-style-number text:level=\"6\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%6%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"3cm\" fo:text-indent=\"-0.499cm\" fo:margin-left=\"3cm\"/></style:list-level-properties></text:list-level-style-number><text:list-level-style-number text:level=\"7\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%7%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"3.5cm\" fo:text-indent=\"-0.499cm\" fo:margin-left=\"3.5cm\"/></style:list-level-properties></text:list-level-style-number><text:list-level-style-number text:level=\"8\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%8%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"4.001cm\" fo:text-indent=\"-0.499cm\" fo:margin-left=\"4.001cm\"/></style:list-level-properties></text:list-level-style-number><text:list-level-style-number text:level=\"9\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%9%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"4.5cm\" fo:text-indent=\"-0.499cm\" fo:margin-left=\"4.5cm\"/></style:list-level-properties></text:list-level-style-number><text:list-level-style-number text:level=\"10\" text:style-name=\"Numbering_20_Symbols\" loext:num-list-format=\"%10%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:list-level-position-and-space-mode=\"label-alignment\"><style:list-level-label-alignment text:label-followed-by=\"listtab\" text:list-tab-stop-position=\"5.001cm\" fo:text-indent=\"-0.499cm\" fo:margin-left=\"5.001cm\"/></style:list-level-properties></text:list-level-style-number></text:list-style><text:list-style style:name=\"OrgNumberedList\"><text:list-level-style-number text:level=\"1\" loext:num-list-format=\"%1%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"0.635cm\" text:min-label-width=\"0.635cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"2\" loext:num-list-format=\"%2%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"1.27cm\" text:min-label-width=\"0.635cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"3\" loext:num-list-format=\"%3%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"1.905cm\" text:min-label-width=\"0.635cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"4\" loext:num-list-format=\"%4%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"2.54cm\" text:min-label-width=\"0.635cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"5\" loext:num-list-format=\"%5%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"3.175cm\" text:min-label-width=\"0.635cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"6\" loext:num-list-format=\"%6%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"3.81cm\" text:min-label-width=\"0.635cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"7\" loext:num-list-format=\"%7%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"4.445cm\" text:min-label-width=\"0.635cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"8\" loext:num-list-format=\"%8%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"5.08cm\" text:min-label-width=\"0.635cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"9\" loext:num-list-format=\"%9%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"5.715cm\" text:min-label-width=\"0.635cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"10\" loext:num-list-format=\"%10%.\" style:num-suffix=\".\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"6.35cm\" text:min-label-width=\"0.635cm\"/></text:list-level-style-number></text:list-style><text:list-style style:name=\"OrgBulletedList\"><text:list-level-style-bullet text:level=\"1\" text:style-name=\"Bullet_5f_20_5f_Symbols\" loext:num-list-format=\"%1%.\" style:num-suffix=\".\" text:bullet-char=\"•\"><style:list-level-properties text:space-before=\"0.635cm\" text:min-label-width=\"0.635cm\"/><style:text-properties fo:font-family=\"StarSymbol\" style:font-charset=\"x-symbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"2\" text:style-name=\"Bullet_5f_20_5f_Symbols\" loext:num-list-format=\"%2%.\" style:num-suffix=\".\" text:bullet-char=\"•\"><style:list-level-properties text:space-before=\"1.27cm\" text:min-label-width=\"0.635cm\"/><style:text-properties fo:font-family=\"StarSymbol\" style:font-charset=\"x-symbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"3\" text:style-name=\"Bullet_5f_20_5f_Symbols\" loext:num-list-format=\"%3%.\" style:num-suffix=\".\" text:bullet-char=\"•\"><style:list-level-properties text:space-before=\"1.905cm\" text:min-label-width=\"0.635cm\"/><style:text-properties fo:font-family=\"StarSymbol\" style:font-charset=\"x-symbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"4\" text:style-name=\"Bullet_5f_20_5f_Symbols\" loext:num-list-format=\"%4%.\" style:num-suffix=\".\" text:bullet-char=\"•\"><style:list-level-properties text:space-before=\"2.54cm\" text:min-label-width=\"0.635cm\"/><style:text-properties fo:font-family=\"StarSymbol\" style:font-charset=\"x-symbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"5\" text:style-name=\"Bullet_5f_20_5f_Symbols\" loext:num-list-format=\"%5%.\" style:num-suffix=\".\" text:bullet-char=\"•\"><style:list-level-properties text:space-before=\"3.175cm\" text:min-label-width=\"0.635cm\"/><style:text-properties fo:font-family=\"StarSymbol\" style:font-charset=\"x-symbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"6\" text:style-name=\"Bullet_5f_20_5f_Symbols\" loext:num-list-format=\"%6%.\" style:num-suffix=\".\" text:bullet-char=\"•\"><style:list-level-properties text:space-before=\"3.81cm\" text:min-label-width=\"0.635cm\"/><style:text-properties fo:font-family=\"StarSymbol\" style:font-charset=\"x-symbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"7\" text:style-name=\"Bullet_5f_20_5f_Symbols\" loext:num-list-format=\"%7%.\" style:num-suffix=\".\" text:bullet-char=\"•\"><style:list-level-properties text:space-before=\"4.445cm\" text:min-label-width=\"0.635cm\"/><style:text-properties fo:font-family=\"StarSymbol\" style:font-charset=\"x-symbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"8\" text:style-name=\"Bullet_5f_20_5f_Symbols\" loext:num-list-format=\"%8%.\" style:num-suffix=\".\" text:bullet-char=\"•\"><style:list-level-properties text:space-before=\"5.08cm\" text:min-label-width=\"0.635cm\"/><style:text-properties fo:font-family=\"StarSymbol\" style:font-charset=\"x-symbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"9\" text:style-name=\"Bullet_5f_20_5f_Symbols\" loext:num-list-format=\"%9%.\" style:num-suffix=\".\" text:bullet-char=\"•\"><style:list-level-properties text:space-before=\"5.715cm\" text:min-label-width=\"0.635cm\"/><style:text-properties fo:font-family=\"StarSymbol\" style:font-charset=\"x-symbol\"/></text:list-level-style-bullet><text:list-level-style-bullet text:level=\"10\" text:style-name=\"Bullet_5f_20_5f_Symbols\" loext:num-list-format=\"%10%.\" style:num-suffix=\".\" text:bullet-char=\"•\"><style:list-level-properties text:space-before=\"6.35cm\" text:min-label-width=\"0.635cm\"/><style:text-properties fo:font-family=\"StarSymbol\" style:font-charset=\"x-symbol\"/></text:list-level-style-bullet></text:list-style><text:list-style style:name=\"OrgDescriptionList\"><text:list-level-style-number text:level=\"1\" loext:num-list-format=\"%1%\" style:num-format=\"\"><style:list-level-properties text:space-before=\"0.635cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"2\" loext:num-list-format=\"%2%\" style:num-format=\"\"><style:list-level-properties text:space-before=\"1.27cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"3\" loext:num-list-format=\"%3%\" style:num-format=\"\"><style:list-level-properties text:space-before=\"1.905cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"4\" loext:num-list-format=\"%4%\" style:num-format=\"\"><style:list-level-properties text:space-before=\"2.54cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"5\" loext:num-list-format=\"%5%\" style:num-format=\"\"><style:list-level-properties text:space-before=\"3.175cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"6\" loext:num-list-format=\"%6%\" style:num-format=\"\"><style:list-level-properties text:space-before=\"3.81cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"7\" loext:num-list-format=\"%7%\" style:num-format=\"\"><style:list-level-properties text:space-before=\"4.445cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"8\" loext:num-list-format=\"%8%\" style:num-format=\"\"><style:list-level-properties text:space-before=\"5.08cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"9\" loext:num-list-format=\"%9%\" style:num-format=\"\"><style:list-level-properties text:space-before=\"5.715cm\"/></text:list-level-style-number><text:list-level-style-number text:level=\"10\" loext:num-list-format=\"%10%\" style:num-format=\"\"><style:list-level-properties text:space-before=\"6.35cm\"/></text:list-level-style-number></text:list-style><text:list-style style:name=\"OrgSrcBlockNumberedLine\"><text:list-level-style-number text:level=\"1\" loext:num-list-format=\"%1%\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"0.635cm\" text:min-label-width=\"0.635cm\" text:min-label-distance=\"0.101cm\" fo:text-align=\"end\"/></text:list-level-style-number><text:list-level-style-number text:level=\"2\" loext:num-list-format=\"%2%\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"1.27cm\" text:min-label-width=\"0.635cm\" text:min-label-distance=\"0.101cm\" fo:text-align=\"end\"/></text:list-level-style-number><text:list-level-style-number text:level=\"3\" loext:num-list-format=\"%3%\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"1.905cm\" text:min-label-width=\"0.635cm\" text:min-label-distance=\"0.101cm\" fo:text-align=\"end\"/></text:list-level-style-number><text:list-level-style-number text:level=\"4\" loext:num-list-format=\"%4%\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"2.54cm\" text:min-label-width=\"0.635cm\" text:min-label-distance=\"0.101cm\" fo:text-align=\"end\"/></text:list-level-style-number><text:list-level-style-number text:level=\"5\" loext:num-list-format=\"%5%\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"3.175cm\" text:min-label-width=\"0.635cm\" text:min-label-distance=\"0.101cm\" fo:text-align=\"end\"/></text:list-level-style-number><text:list-level-style-number text:level=\"6\" loext:num-list-format=\"%6%\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"3.81cm\" text:min-label-width=\"0.635cm\" text:min-label-distance=\"0.101cm\" fo:text-align=\"end\"/></text:list-level-style-number><text:list-level-style-number text:level=\"7\" loext:num-list-format=\"%7%\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"4.445cm\" text:min-label-width=\"0.635cm\" text:min-label-distance=\"0.101cm\" fo:text-align=\"end\"/></text:list-level-style-number><text:list-level-style-number text:level=\"8\" loext:num-list-format=\"%8%\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"5.08cm\" text:min-label-width=\"0.635cm\" text:min-label-distance=\"0.101cm\" fo:text-align=\"end\"/></text:list-level-style-number><text:list-level-style-number text:level=\"9\" loext:num-list-format=\"%9%\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"5.715cm\" text:min-label-width=\"0.635cm\" text:min-label-distance=\"0.101cm\" fo:text-align=\"end\"/></text:list-level-style-number><text:list-level-style-number text:level=\"10\" loext:num-list-format=\"%10%\" style:num-format=\"1\"><style:list-level-properties text:space-before=\"6.35cm\" text:min-label-width=\"0.635cm\" text:min-label-distance=\"0.101cm\" fo:text-align=\"end\"/></text:list-level-style-number></text:list-style><text:notes-configuration text:note-class=\"footnote\" text:citation-style-name=\"Footnote_20_Symbol\" text:citation-body-style-name=\"Footnote_20_anchor\" style:num-format=\"1\" text:start-value=\"0\" text:footnotes-position=\"page\" text:start-numbering-at=\"document\"/><text:notes-configuration text:note-class=\"endnote\" style:num-format=\"i\" text:start-value=\"0\"/><text:linenumbering-configuration text:number-lines=\"false\" text:offset=\"0.499cm\" style:num-format=\"1\" text:number-position=\"left\" text:increment=\"5\"/></office:styles><office:automatic-styles><style:style style:name=\"MP1\" style:family=\"paragraph\" style:parent-style-name=\"Header\"><style:text-properties officeooo:rsid=\"0016ebb7\" officeooo:paragraph-rsid=\"0016ebb7\"/></style:style><style:style style:name=\"MP2\" style:family=\"paragraph\" style:parent-style-name=\"Footer\"><style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/></style:style><style:page-layout style:name=\"Mpm1\"><style:page-layout-properties fo:page-width=\"21.59cm\" fo:page-height=\"27.94cm\" style:num-format=\"1\" style:print-orientation=\"portrait\" fo:margin-top=\"3.099cm\" fo:margin-bottom=\"2.54cm\" fo:margin-left=\"2.54cm\" fo:margin-right=\"2.54cm\" style:writing-mode=\"lr-tb\" style:layout-grid-color=\"#c0c0c0\" style:layout-grid-lines=\"20\" style:layout-grid-base-height=\"0.706cm\" style:layout-grid-ruby-height=\"0.353cm\" style:layout-grid-mode=\"none\" style:layout-grid-ruby-below=\"false\" style:layout-grid-print=\"false\" style:layout-grid-display=\"false\" style:footnote-max-height=\"0cm\" loext:margin-gutter=\"0cm\"><style:columns fo:column-count=\"1\" fo:column-gap=\"0cm\"/><style:footnote-sep style:width=\"0.018cm\" style:distance-before-sep=\"0.101cm\" style:distance-after-sep=\"0.101cm\" style:line-style=\"none\" style:adjustment=\"left\" style:rel-width=\"25%\" style:color=\"#000000\"/></style:page-layout-properties><style:header-style><style:header-footer-properties fo:min-height=\"0.6cm\" fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:margin-bottom=\"0.499cm\" fo:background-color=\"transparent\" style:dynamic-spacing=\"false\" draw:fill=\"none\" draw:fill-color=\"#729fcf\"/></style:header-style><style:footer-style/></style:page-layout><style:page-layout style:name=\"Mpm2\"><style:page-layout-properties fo:page-width=\"21.001cm\" fo:page-height=\"29.7cm\" style:num-format=\"1\" style:print-orientation=\"portrait\" fo:margin-top=\"2cm\" fo:margin-bottom=\"2cm\" fo:margin-left=\"2cm\" fo:margin-right=\"2cm\" style:writing-mode=\"lr-tb\" style:layout-grid-color=\"#c0c0c0\" style:layout-grid-lines=\"20\" style:layout-grid-base-height=\"0.706cm\" style:layout-grid-ruby-height=\"0.353cm\" style:layout-grid-mode=\"none\" style:layout-grid-ruby-below=\"false\" style:layout-grid-print=\"false\" style:layout-grid-display=\"false\" style:footnote-max-height=\"0cm\" loext:margin-gutter=\"0cm\"><style:footnote-sep style:width=\"0.018cm\" style:distance-before-sep=\"0.101cm\" style:distance-after-sep=\"0.101cm\" style:line-style=\"solid\" style:adjustment=\"left\" style:rel-width=\"25%\" style:color=\"#000000\"/></style:page-layout-properties><style:header-style/><style:footer-style/></style:page-layout><style:page-layout style:name=\"Mpm3\" style:page-usage=\"mirrored\"><style:page-layout-properties fo:page-width=\"21.001cm\" fo:page-height=\"29.7cm\" style:num-format=\"i\" style:print-orientation=\"portrait\" fo:margin-top=\"2cm\" fo:margin-bottom=\"2cm\" fo:margin-left=\"2cm\" fo:margin-right=\"2cm\" style:writing-mode=\"lr-tb\" style:layout-grid-color=\"#c0c0c0\" style:layout-grid-lines=\"20\" style:layout-grid-base-height=\"0.706cm\" style:layout-grid-ruby-height=\"0.353cm\" style:layout-grid-mode=\"none\" style:layout-grid-ruby-below=\"false\" style:layout-grid-print=\"false\" style:layout-grid-display=\"false\" style:footnote-max-height=\"0cm\" loext:margin-gutter=\"0cm\"><style:footnote-sep style:width=\"0.018cm\" style:distance-before-sep=\"0.101cm\" style:distance-after-sep=\"0.101cm\" style:line-style=\"solid\" style:adjustment=\"left\" style:rel-width=\"25%\" style:color=\"#000000\"/></style:page-layout-properties><style:header-style/><style:footer-style><style:header-footer-properties fo:min-height=\"0cm\" fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:margin-top=\"0.499cm\"/></style:footer-style></style:page-layout><style:page-layout style:name=\"Mpm4\" style:page-usage=\"right\"><style:page-layout-properties fo:page-width=\"21.001cm\" fo:page-height=\"29.7cm\" style:num-format=\"1\" style:print-orientation=\"portrait\" fo:margin-top=\"2cm\" fo:margin-bottom=\"2cm\" fo:margin-left=\"2cm\" fo:margin-right=\"2cm\" fo:background-color=\"transparent\" style:writing-mode=\"lr-tb\" style:layout-grid-color=\"#c0c0c0\" style:layout-grid-lines=\"20\" style:layout-grid-base-height=\"0.706cm\" style:layout-grid-ruby-height=\"0.353cm\" style:layout-grid-mode=\"none\" style:layout-grid-ruby-below=\"false\" style:layout-grid-print=\"false\" style:layout-grid-display=\"false\" draw:fill=\"none\" draw:fill-color=\"#729fcf\" style:footnote-max-height=\"0cm\" loext:margin-gutter=\"0cm\"><style:footnote-sep style:width=\"0.018cm\" style:distance-before-sep=\"0.101cm\" style:distance-after-sep=\"0.101cm\" style:line-style=\"solid\" style:adjustment=\"left\" style:rel-width=\"25%\" style:color=\"#000000\"/></style:page-layout-properties><style:header-style/><style:footer-style><style:header-footer-properties fo:min-height=\"0.6cm\" fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:margin-top=\"0.499cm\" style:dynamic-spacing=\"false\"/></style:footer-style></style:page-layout><style:page-layout style:name=\"Mpm5\" style:page-usage=\"mirrored\"><style:page-layout-properties fo:page-width=\"21.001cm\" fo:page-height=\"29.7cm\" style:num-format=\"1\" style:print-orientation=\"portrait\" fo:margin-top=\"2cm\" fo:margin-bottom=\"2cm\" fo:margin-left=\"2cm\" fo:margin-right=\"2cm\" style:writing-mode=\"lr-tb\" style:layout-grid-color=\"#c0c0c0\" style:layout-grid-lines=\"20\" style:layout-grid-base-height=\"0.706cm\" style:layout-grid-ruby-height=\"0.353cm\" style:layout-grid-mode=\"none\" style:layout-grid-ruby-below=\"false\" style:layout-grid-print=\"false\" style:layout-grid-display=\"false\" style:footnote-max-height=\"0cm\" loext:margin-gutter=\"0cm\"><style:footnote-sep style:width=\"0.018cm\" style:distance-before-sep=\"0.101cm\" style:distance-after-sep=\"0.101cm\" style:line-style=\"solid\" style:adjustment=\"left\" style:rel-width=\"25%\" style:color=\"#000000\"/></style:page-layout-properties><style:header-style/><style:footer-style><style:header-footer-properties fo:min-height=\"0.6cm\" fo:margin-left=\"0cm\" fo:margin-right=\"0cm\" fo:margin-top=\"0.499cm\" style:dynamic-spacing=\"false\"/></style:footer-style></style:page-layout><style:page-layout style:name=\"Mpm6\"><style:page-layout-properties fo:page-width=\"21.59cm\" fo:page-height=\"27.94cm\" style:num-format=\"1\" style:print-orientation=\"portrait\" fo:margin-top=\"3.999cm\" fo:margin-bottom=\"2.54cm\" fo:margin-left=\"2.54cm\" fo:margin-right=\"2.54cm\" style:writing-mode=\"lr-tb\" style:layout-grid-color=\"#c0c0c0\" style:layout-grid-lines=\"20\" style:layout-grid-base-height=\"0.706cm\" style:layout-grid-ruby-height=\"0.353cm\" style:layout-grid-mode=\"none\" style:layout-grid-ruby-below=\"false\" style:layout-grid-print=\"true\" style:layout-grid-display=\"true\" style:footnote-max-height=\"0cm\" loext:margin-gutter=\"0cm\"><style:footnote-sep style:width=\"0.018cm\" style:distance-before-sep=\"0.101cm\" style:distance-after-sep=\"0.101cm\" style:line-style=\"solid\" style:adjustment=\"left\" style:rel-width=\"25%\" style:color=\"#000000\"/></style:page-layout-properties><style:header-style/><style:footer-style/></style:page-layout><style:style style:name=\"Mdp1\" style:family=\"drawing-page\"><style:drawing-page-properties draw:background-size=\"full\"/></style:style><style:style style:name=\"Mdp2\" style:family=\"drawing-page\"><style:drawing-page-properties draw:fill=\"none\" draw:background-size=\"full\" draw:fill-color=\"#729fcf\"/></style:style></office:automatic-styles><office:master-styles><style:master-page style:name=\"Standard\" style:page-layout-name=\"Mpm1\" draw:style-name=\"Mdp1\"><style:header><text:p text:style-name=\"MP1\">" (car (last (split-string (ooetmeu--get-file-property-value "AUTHOR" org-input-file)))) "/" (ooetmeu--get-file-property-value "TITLE" org-input-file) "/<text:page-number text:select-page=\"current\">10</text:page-number></text:p><text:p text:style-name=\"MP1\"/></style:header><style:header-first><text:p text:style-name=\"MP1\"/></style:header-first></style:master-page><style:master-page style:name=\"OrgTitlePage\" style:page-layout-name=\"Mpm2\" draw:style-name=\"Mdp1\" style:next-style-name=\"OrgFrontMatterPage\"/><style:master-page style:name=\"OrgFrontMatterPage\" style:page-layout-name=\"Mpm3\" draw:style-name=\"Mdp1\"><style:footer><text:p text:style-name=\"MP2\"><text:page-number text:select-page=\"current\">0</text:page-number></text:p></style:footer></style:master-page><style:master-page style:name=\"OrgFirstPage\" style:page-layout-name=\"Mpm4\" draw:style-name=\"Mdp2\" style:next-style-name=\"OrgPage\"><style:footer><text:p text:style-name=\"MP2\"><text:page-number text:select-page=\"current\">0</text:page-number></text:p></style:footer></style:master-page><style:master-page style:name=\"OrgPage\" style:page-layout-name=\"Mpm5\" draw:style-name=\"Mdp1\"><style:footer><text:p text:style-name=\"MP2\"><text:page-number text:select-page=\"current\">0</text:page-number></text:p></style:footer></style:master-page><style:master-page style:name=\"First_20_Page\" style:display-name=\"First Page\" style:page-layout-name=\"Mpm6\" draw:style-name=\"Mdp1\" style:next-style-name=\"Standard\"/></office:master-styles></office:document-styles>
"))
    out-xml))

(defun ooetmeu--restyle-headings (file-contents)
  "Given a string FILE-CONTENTS, add Org mode ODT exports to restyle headings.
Return string of new file contents."
  (let ((out-str "")
        curr-heading
        (curr-heading-construct "")
        curr-level
        (chap-num 0)
        (part-num 0)
        curr-cust-id
        (no-header nil)
        (no-header-name nil)
        (no-header-preamble nil)
        (no-toc-entry nil)
        (part nil)
        beg)
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (ooetmeu--fold-show-all)
      (unless (string= (ooetmeu--get-file-property-value "SUBTITLE") "")
        (ooetmeu--set-file-property-value "TITLE" (concat (ooetmeu--get-file-property-value "TITLE")" — " (ooetmeu--get-file-property-value "SUBTITLE")))
        (ooetmeu--delete-file-property-value "SUBTITLE"))
      (goto-char (point-min))
      (when (org-goto-first-child)
        (beginning-of-line)
        (insert "\n")
        (forward-line -1)
        (insert "#+BEGIN_EXPORT odt\n"
                "<table:table table:name=\"Table1\" table:style-name=\"Table1\">\n"
                "  <table:table-column table:style-name=\"Table1.A\"/>\n"
                "  <table:table-column table:style-name=\"Table1.B\"/>\n"
                "    <table:table-row>\n"
                "      <table:table-cell office:value-type=\"string\"><text:p text:style-name=\"Table_20_Contents\">"
                (ooetmeu--get-file-property-value "AUTHOR")
                "</text:p></table:table-cell>\n"
                "      <table:table-cell office:value-type=\"string\"><text:p text:style-name=\"Table Contents Right Align\">"
                "<text:word-count>11650</text:word-count><text:s/>words</text:p></table:table-cell>\n"
                "    </table:table-row>\n"
                "    <table:table-row>\n"
                "<table:table-cell office:value-type=\"string\"><text:p text:style-name=\"Table_20_Contents\">"
                "<text:a xlink:type=\"simple\" xlink:href=\"mailto:"
                (ooetmeu--get-file-property-value "EMAIL")
                "\" text:style-name=\"Internet_20_link\" text:visited-style-name=\"Visited_20_Internet_20_Link\">"
                (ooetmeu--get-file-property-value "EMAIL")
                "</text:a>"
                "</text:p></table:table-cell>\n"
                "    </table:table-row>\n"
                "</table:table>\n"
                "<text:p text:style-name=\"OrgTitle\"><text:title>"
                (ooetmeu--get-file-property-value "TITLE"))
        (unless (string= (ooetmeu--get-file-property-value "SUBTITLE") "")
          (insert " — " (ooetmeu--get-file-property-value "SUBTITLE")))
        (insert "</text:title></text:p>\n"
                "\n"
                "<text:p text:style-name=\"OrgSubtitle\"><text:span>by "
                (ooetmeu--get-file-property-value "AUTHOR")
                "</text:span></text:p>\n"
                "#+END_EXPORT\n"))
      (goto-char (point-min))
      (insert "Temp Line\n")
      (goto-char (point-min))
      (while (not (org-next-visible-heading 1))
        ;; If tags "no_header" was used in Chapter Index headings, then act appropriately with formatting.
        (when (nth 5 (org-heading-components))
          (when (member "no_header" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
            (setq no-header t))
          (when (member "no_header_name" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
            (setq no-header-name t))
          (when (member "no_header_preamble" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
            (setq no-header-preamble t))
          (when (member "no_toc_entry" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
            (setq no-toc-entry t))
          (when (member "part" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
            (setq part t)))
        (setq curr-cust-id (org-entry-get (point) "CUSTOM_ID"))
        (unless curr-cust-id
          (when (string= "" curr-cust-id)
            (setq curr-cust-id nil)))
        ;; Check matter type and replace appropriately, convert heading level to same output level. If no matter type, assume front matter.
        (cond ((string= (org-entry-get (point) "ORG-NOVELIST-MATTER-TYPE") "FRONT MATTER")
               (setq curr-heading (nth 4 (org-heading-components)))
               (setq curr-level (number-to-string (org-current-level)))
               (if part
                   (setq curr-level "1")
                 (setq curr-level (number-to-string (+ (string-to-number curr-level) 1))))
               (setq curr-heading (replace-regexp-in-string (regexp-quote "&") "&amp;" curr-heading nil t))
               (setq curr-heading (replace-regexp-in-string "\\\\thinsp" "" curr-heading nil t))
               (when (or no-header no-header-name)
                 (setq curr-heading ""))
               (beginning-of-line)
               (ooetmeu--delete-line)
               (setq beg (point))
               (re-search-forward ":END:" nil t)
               (delete-region beg (point))
               (ooetmeu--delete-line)
               (when no-toc-entry
                 (setq no-header-preamble t))
               (insert "\n#+BEGIN_EXPORT odt\n"
                       "<text:h text:style-name=\"Heading_20_1_20_First\" text:outline-level=\""
                       curr-level "\" text:is-list-header=\"false\">\n")
               (when curr-cust-id
                 (insert "<text:bookmark-start text:name=\"OrgXref." curr-cust-id "\"/>\n"
                         "<text:bookmark text:name=\"" curr-cust-id "\"/>\n"
                         "<text:bookmark-end text:name=\"OrgXref." curr-cust-id "\"/>"))
               (unless (or (string= curr-heading "") (string= curr-heading "Glossary") (string= curr-heading "Index"))
                 (insert "<text:bookmark-start text:name=\"OrgXref." curr-heading "\"/>\n"
                         "<text:bookmark text:name=\"" curr-heading "\"/>"))
               (insert curr-heading "\n")
               (unless (or (string= curr-heading "") (string= curr-heading "Glossary") (string= curr-heading "Index"))
                 (insert "<text:bookmark-end text:name=\"OrgXref." curr-heading "\"/>"))
               (insert "</text:h>\n"
                       "#+END_EXPORT\n"))
              ((string= (org-entry-get (point) "ORG-NOVELIST-MATTER-TYPE") "MAIN MATTER")
               (setq curr-heading (nth 4 (org-heading-components)))
               (setq curr-level (number-to-string (org-current-level)))
               (if part
                   (setq curr-level "1")
                 (setq curr-level (number-to-string (+ (string-to-number curr-level) 1))))
               (setq curr-heading (replace-regexp-in-string (regexp-quote "&") "&amp;" curr-heading nil t))
               (setq curr-heading (replace-regexp-in-string "\\\\thinsp" "" curr-heading nil t))
               (beginning-of-line)
               (ooetmeu--delete-line)
               (setq beg (point))
               (re-search-forward ":END:" nil t)
               (delete-region beg (point))
               (ooetmeu--delete-line)
               (when no-toc-entry
                 (setq no-header-preamble t))
               (when (and part (not no-header) (not no-toc-entry))
                 (setq part-num (+ part-num 1)))
               (when (and (string= curr-level "2") (not no-header) (not no-toc-entry))
                 (setq chap-num (+ chap-num 1)))
               (insert "\n#+BEGIN_EXPORT odt\n"
                       "<text:h text:style-name=\"Heading_20_1\" text:outline-level=\""
                       curr-level "\" text:is-list-header=\"false\">\n")
               (when curr-cust-id
                 (insert "<text:bookmark-start text:name=\"OrgXref." curr-cust-id "\"/>\n"
                         "<text:bookmark text:name=\"" curr-cust-id "\"/>\n"
                         "<text:bookmark-end text:name=\"OrgXref." curr-cust-id "\"/>"))
               (unless (or (string= curr-heading "") (string= curr-heading "Glossary") (string= curr-heading "Index"))
                 (insert "<text:bookmark-start text:name=\"OrgXref." curr-heading "\"/>\n"
                         "<text:bookmark text:name=\"" curr-heading "\"/>"))
               (unless no-header-preamble
                 (if part
                     (setq curr-heading-construct (concat "Part " (number-to-string part-num)))
                   (setq curr-heading-construct (concat "Chapter " (number-to-string chap-num)))))
               (unless (or no-header-preamble no-header-name)
                 (setq curr-heading-construct (concat curr-heading-construct " - ")))
               (unless no-header-name
                 (setq curr-heading-construct (concat curr-heading-construct curr-heading)))
               (when no-header
                 (setq curr-heading-construct ""))
               (insert curr-heading-construct "\n")
               (unless (or (string= curr-heading "") (string= curr-heading "Glossary") (string= curr-heading "Index"))
                 (insert "<text:bookmark-end text:name=\"OrgXref." curr-heading "\"/>"))
               (insert "</text:h>\n"
                       "#+END_EXPORT\n"))
              ((string= (org-entry-get (point) "ORG-NOVELIST-MATTER-TYPE") "BACK MATTER")
               (setq curr-heading (nth 4 (org-heading-components)))
               (setq curr-level (number-to-string (org-current-level)))
               (if part
                   (setq curr-level "1")
                 (setq curr-level (number-to-string (+ (string-to-number curr-level) 1))))
               (setq curr-heading (replace-regexp-in-string (regexp-quote "&") "&amp;" curr-heading nil t))
               (setq curr-heading (replace-regexp-in-string "\\\\thinsp" "" curr-heading nil t))
               (when (or no-header no-header-name)
                 (setq curr-heading ""))
               (beginning-of-line)
               (ooetmeu--delete-line)
               (setq beg (point))
               (re-search-forward ":END:" nil t)
               (delete-region beg (point))
               (ooetmeu--delete-line)
               (when no-toc-entry
                 (setq no-header-preamble t))
               (insert "\n#+BEGIN_EXPORT odt\n"
                       "<text:h text:style-name=\"Heading_20_1\" text:outline-level=\""
                       curr-level "\" text:is-list-header=\"false\">\n")
               (when curr-cust-id
                 (insert "<text:bookmark-start text:name=\"OrgXref." curr-cust-id "\"/>\n"
                         "<text:bookmark text:name=\"" curr-cust-id "\"/>\n"
                         "<text:bookmark-end text:name=\"OrgXref." curr-cust-id "\"/>"))
               (unless (string= curr-heading "")
                 (insert "<text:bookmark-start text:name=\"OrgXref." curr-heading "\"/>\n"
                         "<text:bookmark text:name=\"" curr-heading "\"/>" curr-heading "\n"
                         "<text:bookmark-end text:name=\"OrgXref." curr-heading "\"/>"))
               (insert "</text:h>\n"
                       "#+END_EXPORT\n"))
              (t
               (setq curr-heading (nth 4 (org-heading-components)))
               (setq curr-level (number-to-string (org-current-level)))
               (if part
                   (setq curr-level "1")
                 (setq curr-level (number-to-string (+ (string-to-number curr-level) 1))))
               (setq curr-heading (replace-regexp-in-string (regexp-quote "&") "&amp;" curr-heading nil t))
               (setq curr-heading (replace-regexp-in-string "\\\\thinsp" "" curr-heading nil t))
               (beginning-of-line)
               (ooetmeu--delete-line)
               (when no-toc-entry
                 (setq no-header-preamble t))
               (when (and part (not no-header) (not no-toc-entry))
                 (setq part-num (+ part-num 1)))
               (when (and (string= curr-level "2") (not no-header) (not no-toc-entry))
                 (setq chap-num (+ chap-num 1)))
               (insert "\n#+BEGIN_EXPORT odt\n"
                       "<text:h text:style-name=\"Heading_20_1\" text:outline-level=\""
                       curr-level "\" text:is-list-header=\"false\">\n")
               (when curr-cust-id
                 (insert "<text:bookmark-start text:name=\"OrgXref." curr-cust-id "\"/>\n"
                         "<text:bookmark text:name=\"" curr-cust-id "\"/>\n"
                         "<text:bookmark-end text:name=\"OrgXref." curr-cust-id "\"/>"))
               (unless (or (string= curr-heading "") (string= curr-heading "Glossary") (string= curr-heading "Index"))
                 (insert "<text:bookmark-start text:name=\"OrgXref." curr-heading "\"/>\n"
                         "<text:bookmark text:name=\"" curr-heading "\"/>"))
               (unless no-header-preamble
                 (if part
                     (setq curr-heading-construct (concat "Part " (number-to-string part-num)))
                   (when (string= curr-level "2")
                     (setq curr-heading-construct (concat "Chapter " (number-to-string chap-num))))))
               (unless (or no-header-preamble no-header-name)
                 (when (string= curr-level "2")
                   (setq curr-heading-construct (concat curr-heading-construct " - "))))
               (unless no-header-name
                 (setq curr-heading-construct (concat curr-heading-construct curr-heading)))
               (when no-header
                 (setq curr-heading-construct ""))
               (insert curr-heading-construct "\n")
               (unless (or (string= curr-heading "") (string= curr-heading "Glossary") (string= curr-heading "Index"))
                 (insert "<text:bookmark-end text:name=\"OrgXref." curr-heading "\"/>"))
               (insert "</text:h>\n"
                       "#+END_EXPORT\n")))
        (setq curr-heading-construct "")
        (setq no-header nil)
        (setq no-header-name nil)
        (setq no-header-preamble nil)
        (setq no-toc-entry nil)
        (setq part nil))
      (goto-char (point-min))
      (ooetmeu--delete-line)
      (setq out-str (buffer-string)))
    out-str))

(defun ooetmeu--remap-image-links (file-contents)
  "Given a string FILE-CONTENTS, remap the Org mode image links correctly.
Return string of new file contents."
  (let ((out-str ""))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (ooetmeu--fold-show-all)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward (format "^[ \t]*%s" (regexp-quote "[[file:../Images/")) nil t)
          (delete-char -7)
          (insert "../Images/")))
      (setq out-str (buffer-string)))
    out-str))

(defun ooetmeu--restyle-verse-blocks (file-contents)
  "Given a string FILE-CONTENTS, restyle the Org mode verse blocks correctly.
Return string of new file contents."
  (let ((out-str ""))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (ooetmeu--fold-show-all)
      (goto-char (point-min))
      (let ((case-fold-search t)
            (verse-contents "")
            beg)
        (while (re-search-forward "^[ \t]*#\\+begin_verse" nil t)
          (ooetmeu--delete-line)
          (setq beg (point))
          (re-search-forward "^[ \t]*#\\+end_verse" nil t)
          (ooetmeu--delete-line)
          (setq verse-contents (buffer-substring beg (point)))
          (delete-region beg (point))
          (setq verse-contents (replace-regexp-in-string "[\r\n]+?" "<text:line-break/>" verse-contents))
          (setq verse-contents (replace-regexp-in-string "[\t]+?" "<text:tab/>" verse-contents))
          (setq verse-contents (replace-regexp-in-string "[ ]+?" "<text:s/>" verse-contents))
          (insert "#+BEGIN_EXPORT odt\n<text:p text:style-name=\"OrgVerse\">\n<text:line-break/>")
          (insert verse-contents)
          (insert "</text:p>\n#+END_EXPORT\n")))
      (setq out-str (buffer-string)))
    out-str))

(defun ooetmeu--remap-internal-links (file-contents)
  "Given a string FILE-CONTENTS, remap the Org mode internal links correctly.
This is necessary when original Org headings have been replaced by ODT code.
Return string of new file contents."
  (let ((out-str file-contents)
        (case-fold-search t)
        (beg nil)
        link-val
        link-text
        (blind-link-num 0))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (ooetmeu--fold-show-all)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[[^:/\.\n\r]+?]]" nil t)
        (unless beg
          (setq out-str ""))
        (setq link-text nil)
        (setq beg (point))
        (when (re-search-backward "\\[\\[" nil t)
          (setq link-val (buffer-substring (point) beg))
          (delete-region (point) beg)
          (with-temp-buffer
            (insert link-val)
            (goto-char (point-max))
            (delete-char -2)
            (goto-char (point-min))
            (delete-char 2)
            (when (re-search-forward "#" nil t)
              (replace-match ""))
            (goto-char (point-min))
            (when (re-search-forward "*" nil t)
              (replace-match ""))
            (goto-char (point-min))
            (when (re-search-forward "]\\[" nil t)
              (delete-char -2)
              (setq link-text (buffer-substring (point) (point-max)))
              (delete-region (point) (point-max)))
            (setq link-val (buffer-string)))
          (if link-text
              (insert "@@odt:<text:a xlink:type=\"simple\" xlink:href=\"#" link-val "\">" link-text "</text:a>@@")
            (progn
              (setq blind-link-num (+ 1 blind-link-num))
              (insert "@@odt:<text:bookmark-ref text:reference-format=\"chapter\" text:ref-name=\"OrgXref." link-val "\">" (number-to-string blind-link-num) "</text:bookmark-ref>@@"))))
        (setq out-str (buffer-string))))
    out-str))

(defun ooetmeu--remap-pagebreaks (file-contents)
  "Given a string FILE-CONTENTS, remap the Org mode pagebreaks correctly.
Return string of new file contents."
  ;; This still doesn't really do the right thing, but it's better than leaving the command visible in the final document.
  (let ((out-str ""))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (ooetmeu--fold-show-all)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward "\\\\pagebreak[ \t]*" nil t)
          (ooetmeu--delete-line)
          (insert "#+BEGIN_EXPORT odt\n<text:line-break/>\n")
          (insert "#+END_EXPORT\n")))
      (setq out-str (buffer-string)))
    out-str))

(defun ooetmeu--remap-shy-inclusions (file-contents)
  "Given a string FILE-CONTENTS, remap the Org mode shy inclusions correctly.
Return string of new file contents."
  ;; This removes shy inclusions completely as ODT doesn't need them.
  (let ((out-str ""))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (ooetmeu--fold-show-all)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward "\\\\-" nil t)
          (delete-char -2)))
      (setq out-str (buffer-string)))
    out-str))

(defun ooetmeu--remove-index-properties (file-contents)
  "Given a string FILE-CONTENTS, remove the #+INDEX: properties.
Return string of new file contents."
  ;; Remove #+INDEX: properties as ODT won't deal with them, and it will cause unwanted line breaks in final ODT.
  (let ((out-str ""))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (ooetmeu--fold-show-all)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward "^[ \t]*#\\+INDEX:" nil t)
          (ooetmeu--delete-line)))
      (setq out-str (buffer-string)))
    out-str))


;;;; Required Entry Point Function for Org Novelist Export

(defun org-novelist--export-template (org-input-file output-file)
  "Given an ORG-INPUT-FILE from Org Novelist, export to OUTPUT-FILE."
  (let ((temp-org (concat (file-name-sans-extension output-file) ".org"))
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
        (org-odt-styles-file-orig nil)
        (undo-tree-auto-save-history-orig nil)
        (org-export-backends-orig nil)
        (file-contents "")
        (odt-manuscript-styles-xml (ooetmeu--generate-odt-style-string org-input-file)))
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
    (when (boundp 'org-odt-styles-file)
      (setq org-odt-styles-file-orig org-odt-styles-file))
    (when (boundp 'undo-tree-auto-save-history)
      (setq undo-tree-auto-save-history-orig undo-tree-auto-save-history))
    (when (boundp 'org-export-backends)
      (setq org-export-backends-orig org-export-backends))
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
    ;; Construct new file to pass to Org export dispatcher, based on input file.
    (when (file-exists-p org-input-file)
      (when (file-readable-p org-input-file)
        (make-directory (file-name-directory output-file) t)
        ;; The next two lines setup the XML style file for the document.
        (ooetmeu--string-to-file odt-manuscript-styles-xml (concat (file-name-directory output-file) "styles.xml"))
        (setq org-odt-styles-file (concat (file-name-directory output-file) "styles.xml"))
        (setq file-contents (org-file-contents org-input-file))
        ;; Add ODT export codes to restyle the headings.
        (setq file-contents (ooetmeu--restyle-headings file-contents))
        ;; Add ODT export codes to restyle the verse blocks.
        (setq file-contents (ooetmeu--restyle-verse-blocks file-contents))
        ;; Remap pagebreaks correctly.
        (setq file-contents (ooetmeu--remap-pagebreaks file-contents))
        ;; Remap shy inclusions correctly.
        (setq file-contents (ooetmeu--remap-shy-inclusions file-contents))
        ;; Remove index properties from file as this export template won't use them.
        (setq file-contents (ooetmeu--remove-index-properties file-contents))
        ;; Remap the image links to the correct folder.
        (setq file-contents (ooetmeu--remap-image-links file-contents))
        ;; Remap the internal document links to point to replacement heading labels.
        (setq file-contents (ooetmeu--remap-internal-links file-contents))
        (with-temp-buffer
          (insert file-contents)
          (org-mode)
          (ooetmeu--fold-show-all)
          (ooetmeu--string-to-file (buffer-string) temp-org))
        (setq org-export-with-broken-links t)
        (when (boundp 'undo-tree-auto-save-history)
          (setq undo-tree-auto-save-history nil))
        (find-file temp-org)
        (org-odt-export-to-odt)
        (ooetmeu--delete-current-file t)
        (make-directory (file-name-directory output-file) t)
        (setq org-odt-styles-file org-odt-styles-file-orig)
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
        (rename-file (concat (file-name-sans-extension temp-org) ".odt") output-file t)
        (find-file (concat (file-name-directory output-file) "styles.xml"))
        (ooetmeu--delete-current-file t)
        (when (boundp 'undo-tree-auto-save-history)
          (setq undo-tree-auto-save-history undo-tree-auto-save-history-orig))))))


(provide 'org-odt-export-to-manuscript-en-us)
;;; org-odt-export-to-manuscript-en-us.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ooetmeu-" . "org-odt-export-to-manuscript-en-us-"))
;; End:
