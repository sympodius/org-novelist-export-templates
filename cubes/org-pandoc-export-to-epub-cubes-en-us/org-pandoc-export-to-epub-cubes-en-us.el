;;; org-pandoc-export-to-epub-cubes-en-us.el --- Org Novelist export template to ePub -*- lexical-binding: t; -*-

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
;; This package supplies an example export template to ePub format,
;; suitable for most eReaders.
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
;; This package requires Pandoc and Imagemagick to be installed and
;; accessible on the system. If Calibre's ebook-convert is available,
;; an Amazon Kindle azw3 file can also be generated.
;;
;;
;; You should also have the following typefaces installed and
;; accessible on your system if using the default settings for
;; generating the book cover:
;;
;; Josefin Sans
;; (https://fonts.google.com/specimen/Josefin+Sans)
;;
;; Alegreya SC
;; (https://fonts.google.com/specimen/Alegreya+SC)
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
;; Do not include chapter in the table of contents. Chapter will have
;; no "Chapter X" text, and will not be used to calculate the numbering
;; of main matter chapters.
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
;;
;; #+HEADFONT:
;; The typeface to use as the headings font.
;; Must be installed on the system.
;; eg: Josefin Sans
;;
;; #+SIGNATUREFONT:
;; The typeface to use as the signature font.
;; Must be installed on the system.
;; eg: Alegreya SC
;;
;; #+TYPEFACE_SIZE_HEADFONT_OPETECEU:
;; The size (in pt) of the title text in the generated cover.
;; eg: 126
;;
;; #+TYPEFACE_SIZE_SIGNATUREFONT_OPETECEU:
;; The size (in pt) of the author text in the generated cover.
;; eg: 72
;;
;; #+TITLE_PAGE_GRAPHIC:
;; Location of image file to include when generating the title page.
;; eg: ../Images/cubes.png
;;
;; #+TITLE_PAGE_GRAPHIC_SCALE:
;; The scaled display size (fraction of normal) of TITLE_PAGE_GRAPHIC.
;; eg: 0.175
;;
;; #+TITLE_PAGE_GRAPHIC_COPYRIGHT:
;; Copyright credit for TITLE_PAGE_GRAPHIC.
;; eg: `\textit{Cube Family}' is copyright \copyright~2012 Mr Anderson.
;;
;; #+TITLE_PAGE_GRAPHIC_LICENSE:
;; License statement for TITLE_PAGE_GRAPHIC.
;; Any text is viable (eg: All rights reserved.), but the following
;; Creative Commons codes will be handled automatically if used
;; (see https://creativecommons.org for more information):
;; CC0-1.0
;; CC-BY-4.0
;; CC-BY-SA-4.0
;; CC-BY-ND-4.0
;; CC-BY-NC-4.0
;; CC-BY-NC-SA-4.0
;; CC-BY-NC-ND-4.0
;;
;; #+TITLE_PAGE_REPLACEMENT_GRAPHIC_OPETECEU:
;; Location of image file to replace the generated title page, if
;; needed.
;; eg: ../Images/ReplacementTitlePage.png
;;
;; #+TITLE_PAGE_REPLACEMENT_GRAPHIC_OPETECEU_SCALE:
;; The scaled display size (fraction of normal) of
;; TITLE_PAGE_REPLACEMENT_GRAPHIC_OPETECEU.
;; eg: 0.750
;;
;; #+PUBLISHER:
;; The name of the story's publisher, if there is one.
;; eg: Good and Evil Publishing
;;
;; #+ISBN:
;; ISBN number of the book, if there is one.
;; eg: 1234567890123
;;
;; #+EDITION:
;; Text describing this edition.
;; eg: Early Draft Edition (not for publication)
;;
;; #+LICENSE:
;; License statement for story.
;; Any text is viable (eg: All rights reserved.), but the following
;; Creative Commons codes will be handled automatically if used
;; (see https://creativecommons.org for more information):
;; CC0-1.0
;; CC-BY-4.0
;; CC-BY-SA-4.0
;; CC-BY-ND-4.0
;; CC-BY-NC-4.0
;; CC-BY-NC-SA-4.0
;; CC-BY-NC-ND-4.0
;;
;; #+SIGIL_GRAPHIC:
;; Location of author's sigil image file, if needed.
;; eg: ../Images/juf-sigil.pdf
;;
;; #+SIGIL_GRAPHIC_SCALE:
;; The scaled display size (fraction of normal) of SIGIL_GRAPHIC.
;; eg: 0.500

;;; Code:

;;;; Require other packages

(require 'org)  ; Org Novelist is built upon the incredible work of Org mode
(require 'ox)  ; Necessary to call Org's built-in export functions.


;;;; User Variables

(defvar opeteceu--headfont-default "Josefin Sans" "Heading text font, must be installed on system already.")
(defvar opeteceu--pointsize-headfont-default 126 "Typeface size (pt) for the Title text on the generated cover.")
(defvar opeteceu--signaturefont-default "Alegreya SC" "Author signature text font, must be installed on system already.")
(defvar opeteceu--pointsize-signaturefont-default 72 "Typeface size (pt) for the Author text on the generated cover.")
(defvar opeteceu--title-page-graphic-default "cubes.png" "Location of image file to use in title page.")
(defvar opeteceu--title-page-graphic-scale-default 0.175 "Display scale of the title page image.")
(defvar opeteceu--title-page-graphic-copyright-default "*Cube Family* is copyright &copy; 2012 Martin Anderson (2012--?)\\\nMade with Blender 3D --- <code><https://www.blender.org></code>" "Copyright credit for title page image.")
(defvar opeteceu--title-page-graphic-license-default "Licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. To view a copy of this license, visit: <div class=\"center-legal\"><code><https://creativecommons.org/licenses/by-nc-sa/4.0/></code></div> Or, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "Title page image license statement.")
(defvar opeteceu--title-page-replacement-graphic-default nil "Location of image file to use as a replacement for the generated title page.")
(defvar opeteceu--title-page-replacement-graphic-scale-default 1.000 "Display scale of the title page replacement image.")
(defvar opeteceu--publisher-default "" "The publisher of the story.")
(defvar opeteceu--isbn-default "" "ISBN number of book, if there is one.")
(defvar opeteceu--edition-default "Early Draft Edition (not for publication)" "Text describing this edition.")
(defvar opeteceu--license-default "All rights reserved." "License statement.")
(defvar opeteceu--sigil-graphic-default "" "Location of image file to use as sigil in legal page.")
(defvar opeteceu--sigil-graphic-scale-default 1.000 "Display scale of the sigil image.")


;;;; Global Variables

(defvar opeteceu--index-entries-exist-in-text-p nil "Temporary variable to show at least one index entry found in text.")
(defvar opeteceu--headfont nil "Heading text font, must be installed on system already.")
(defvar opeteceu--pointsize-headfont nil "Typeface size (pt) for the Title text on the generated cover.")
(defvar opeteceu--signaturefont nil "Author signature text font, must be installed on system already.")
(defvar opeteceu--pointsize-signaturefont nil "Typeface size (pt) for the Author text on the generated cover.")
(defvar opeteceu--title-page-graphic nil "Location of image file to use in title page.")
(defvar opeteceu--title-page-graphic-scale nil "Display scale of the title page image.")
(defvar opeteceu--title-page-graphic-copyright nil "Copyright credit for title page image.")
(defvar opeteceu--title-page-graphic-license nil "Title page image license statement.")
(defvar opeteceu--title-page-replacement-graphic nil "Location of image file to use as a replacement for the generated title page.")
(defvar opeteceu--title-page-replacement-graphic-scale nil "Display scale of the title page replacement image.")
(defvar opeteceu--title-page nil "Final title page image for story.")
(defvar opeteceu--publisher nil "The publisher of the story.")
(defvar opeteceu--isbn nil "ISBN number of book, if there is one.")
(defvar opeteceu--edition nil "Text describing this edition.")
(defvar opeteceu--license nil "License statement.")
(defvar opeteceu--sigil-graphic nil "Location of image file to use as sigil in legal page.")
(defvar opeteceu--sigil-graphic-scale nil "Display scale of the sigil image.")
(defvar opeteceu--license-cc0-1.0 "This book, excluding the cover art, is licensed under the Creative Commons CC0 1.0 Universal License. To view a copy of this license, visit: <div class=\"center-legal\"><code><https://creativecommons.org/publicdomain/zero/1.0/></code></div> Or, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC0.")
(defvar opeteceu--license-cc-by-4.0 "This book, excluding the cover art, is licensed under the Creative Commons Attribution 4.0 International License. To view a copy of this license, visit: <div class=\"center-legal\"><code><https://creativecommons.org/licenses/by/4.0/></code></div> Or, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution.")
(defvar opeteceu--license-cc-by-sa-4.0 "This book, excluding the cover art, is licensed under the Creative Commons Attribution-ShareAlike 4.0 International License. To view a copy of this license, visit: <div class=\"center-legal\"><code><https://creativecommons.org/licenses/by-sa/4.0/></code></div> Or, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-ShareAlike.")
(defvar opeteceu--license-cc-by-nd-4.0 "This book, excluding the cover art, is licensed under the Creative Commons Attribution-NoDerivs 4.0 International License. To view a copy of this license, visit: <div class=\"center-legal\"><code><https://creativecommons.org/licenses/by-nd/4.0/></code></div> Or, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-NoDerivs.")
(defvar opeteceu--license-cc-by-nc-4.0 "This book, excluding the cover art, is licensed under the Creative Commons Attribution-NonCommercial 4.0 International License. To view a copy of this license, visit: <div class=\"center-legal\"><code><https://creativecommons.org/licenses/by-nc/4.0/></code></div> Or, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-NonCommercial.")
(defvar opeteceu--license-cc-by-nc-sa-4.0 "This book, excluding the cover art, is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. To view a copy of this license, visit: <div class=\"center-legal\"><code><https://creativecommons.org/licenses/by-nc-sa/4.0/></code></div> Or, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-NonCommercial-ShareAlike.")
(defvar opeteceu--license-cc-by-nc-nd-4.0 "This book, excluding the cover art, is licensed under the Creative Commons Attribution-NonCommercial-NoDerivs 4.0 International License. To view a copy of this license, visit: <div class=\"center-legal\"><code><https://creativecommons.org/licenses/by-nc-nd/4.0/></code></div> Or, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-NonCommercial-NoDerivs.")
(defvar opeteceu--title-page-graphic-license-cc0-1.0 "Licensed under the Creative Commons CC0 1.0 Universal License. To view a copy of this license, visit: <div class=\"center-legal\"><code><https://creativecommons.org/publicdomain/zero/1.0/></code></div> Or, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC0.")
(defvar opeteceu--title-page-graphic-license-cc-by-4.0 "Licensed under the Creative Commons Attribution 4.0 International License. To view a copy of this license, visit: <div class=\"center-legal\"><code><https://creativecommons.org/licenses/by/4.0/></code></div> Or, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution.")
(defvar opeteceu--title-page-graphic-license-cc-by-sa-4.0 "Licensed under the Creative Commons Attribution-ShareAlike 4.0 International License. To view a copy of this license, visit: <div class=\"center-legal\"><code><https://creativecommons.org/licenses/by-sa/4.0/></code></div> Or, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-ShareAlike.")
(defvar opeteceu--title-page-graphic-license-cc-by-nd-4.0 "Licensed under the Creative Commons Attribution-NoDerivs 4.0 International License. To view a copy of this license, visit: <div class=\"center-legal\"><code><https://creativecommons.org/licenses/by-nd/4.0/></code></div> Or, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-NoDerivs.")
(defvar opeteceu--title-page-graphic-license-cc-by-nc-4.0 "Licensed under the Creative Commons Attribution-NonCommercial 4.0 International License. To view a copy of this license, visit: <div class=\"center-legal\"><code><https://creativecommons.org/licenses/by-nc/4.0/></code></div> Or, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-NonCommercial.")
(defvar opeteceu--title-page-graphic-license-cc-by-nc-sa-4.0 "Licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. To view a copy of this license, visit: <div class=\"center-legal\"><code><https://creativecommons.org/licenses/by-nc-sa/4.0/></code></div> Or, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-NonCommercial-ShareAlike.")
(defvar opeteceu--title-page-graphic-license-cc-by-nc-nd-4.0 "Licensed under the Creative Commons Attribution-NonCommercial-NoDerivs 4.0 International License. To view a copy of this license, visit: <div class=\"center-legal\"><code><https://creativecommons.org/licenses/by-nc-nd/4.0/></code></div> Or, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-NonCommercial-NoDerivs.")


;;;; Global Constants

(defconst opeteceu--language "en-US" "Language for the book.")
(defconst opeteceu--glossary-heading "Glossary" "The expected string to represent the heading of a Glossary.")


;;;; Helper Functions

(defun opeteceu--fold-show-all ()
  "Run the deprecated `org-show-all' when Org version is less than 9.6.
Otherwise, run `org-fold-show-all'."
  (if (string-version-lessp (org-version) "9.6")
      (org-show-all)
    (org-fold-show-all)))

(defun opeteceu--format-time-string (format-string &optional time-zone)
  "Run the deprecated `org-format-time-string' when Org version is less than 9.6.
Otherwise, run `format-time-string'.
FORMAT-STRING is the output format.
TIME-ZONE is the given time. If omitted or nil, use local time."
  (if (string-version-lessp (org-version) "9.6")
      (org-format-time-string format-string time-zone)
    (format-time-string format-string time-zone)))

(defun opeteceu--delete-line ()
  "If Emacs version is less than 29, delete line the old fashioned way."
  (let ((inhibit-field-text-motion t))
    (if (>= (string-to-number (nth 0 (split-string (string-trim-left (emacs-version) "GNU Emacs ") "\\."))) 29)
        (delete-line)
      (delete-region (line-beginning-position) (line-beginning-position 2)))))

(defun opeteceu--delete-current-file (&optional no-prompt)
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

(defun opeteceu--string-to-file (str filename)
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

(defun opeteceu--get-file-property-value (property &optional file)
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

(defun opeteceu--get-file-properties-and-values (&optional file)
  "Given a FILE, return the properties and values.
If FILE not provided, work on current buffer."
  (let ((property-list '())
        (regexp-start "^[ \t]*#\\+")
        (regexp-end ": ")
        (case-fold-search t)
        beg
        beg-line-num
        (curr-buff-str (buffer-string)))
    (with-temp-buffer
      (if file
          (when (file-exists-p file)
            (when (file-readable-p file)
              (insert-file-contents file)))
        (insert curr-buff-str))
      (goto-char (point-min))
      (while (re-search-forward regexp-start nil t)
        (setq beg (point))
        (setq beg-line-num (line-number-at-pos))
        (when (re-search-forward regexp-end nil t)
          (when (= beg-line-num (line-number-at-pos))
            (forward-char -2)
            (push `(,(org-trim (buffer-substring beg (point))) . ,(org-trim (buffer-substring (+ (point) 2) (line-end-position)))) property-list)))))
    property-list))

(defun opeteceu--set-book-configuration-overrides (org-input-file output-file)
  "Override default template values using configuration values from ORG-INPUT-FILE.
Any relative file names will be relative to OUTPUT-FILE."
  (let (prop-val)
    (setq prop-val (opeteceu--get-file-property-value "HEADFONT" org-input-file))
    (if (string= "" prop-val)
        (if (find-font (font-spec :name opeteceu--headfont-default))
            (setq opeteceu--headfont opeteceu--headfont-default)
          (setq opeteceu--headfont "Arial"))
      (if (find-font (font-spec :name prop-val))
          (setq opeteceu--headfont prop-val)
        (if (find-font (font-spec :name opeteceu--headfont-default))
            (setq opeteceu--headfont opeteceu--headfont-default)
          (setq opeteceu--headfont "Arial"))))
    (setq prop-val (opeteceu--get-file-property-value "TYPEFACE_SIZE_HEADFONT_OPETECEU" org-input-file))
    (if (string= "" prop-val)
        (setq opeteceu--pointsize-headfont opeteceu--pointsize-headfont-default)
      (setq opeteceu--pointsize-headfont (string-to-number prop-val)))
    (setq prop-val (opeteceu--get-file-property-value "SIGNATUREFONT" org-input-file))
    (if (string= "" prop-val)
        (if (find-font (font-spec :name opeteceu--signaturefont-default))
            (setq opeteceu--signaturefont opeteceu--signaturefont-default)
          (setq opeteceu--signaturefont "Garamond"))
      (if (find-font (font-spec :name prop-val))
          (setq opeteceu--signaturefont prop-val)
        (if (find-font (font-spec :name opeteceu--signaturefont-default))
            (setq opeteceu--signaturefont opeteceu--signaturefont-default)
          (setq opeteceu--signaturefont "Garamond"))))
    (setq prop-val (opeteceu--get-file-property-value "TYPEFACE_SIZE_SIGNATUREFONT_OPETECEU" org-input-file))
    (if (string= "" prop-val)
        (setq opeteceu--pointsize-signaturefont opeteceu--pointsize-signaturefont-default)
      (setq opeteceu--pointsize-signaturefont (string-to-number prop-val)))
    (setq prop-val (opeteceu--get-file-property-value "TITLE_PAGE_GRAPHIC" org-input-file))
    (if (string= "" prop-val)
        (setq opeteceu--title-page-graphic (expand-file-name
                                            opeteceu--title-page-graphic-default
                                            (expand-file-name
                                             (file-name-directory
                                              (symbol-file 'org-pandoc-export-to-epub-cubes-en-us--fold-show-all)))))
      (setq opeteceu--title-page-graphic (expand-file-name prop-val (expand-file-name (file-name-directory org-input-file)))))
    (setq prop-val (opeteceu--get-file-property-value "TITLE_PAGE_GRAPHIC_SCALE" org-input-file))
    (if (string= "" prop-val)
        (setq opeteceu--title-page-graphic-scale opeteceu--title-page-graphic-scale-default)
      (setq opeteceu--title-page-graphic-scale (string-to-number prop-val)))
    (setq prop-val (opeteceu--get-file-property-value "TITLE_PAGE_GRAPHIC_COPYRIGHT" org-input-file))
    (if (string= "" prop-val)
        (setq opeteceu--title-page-graphic-copyright opeteceu--title-page-graphic-copyright-default)
      (let ((new-str ""))
        (setq new-str (string-trim-right
                       (string-trim-left
                        (replace-regexp-in-string "`" "\\\\\`" (org-export-string-as prop-val 'html t) t) "<p>\n") "</p>\n"))
        (setq opeteceu--title-page-graphic-copyright new-str)))
    (setq prop-val (opeteceu--get-file-property-value "TITLE_PAGE_GRAPHIC_LICENSE" org-input-file))
    (if (string= "" prop-val)
        (setq opeteceu--title-page-graphic-license opeteceu--title-page-graphic-license-default)
      (cond
       ((string= "CC0-1.0" prop-val)
        (setq opeteceu--title-page-graphic-license opeteceu--title-page-graphic-license-cc0-1.0))
       ((string= "CC-BY-4.0" prop-val)
        (setq opeteceu--title-page-graphic-license opeteceu--title-page-graphic-license-cc-by-4.0))
       ((string= "CC-BY-SA-4.0" prop-val)
        (setq opeteceu--title-page-graphic-license opeteceu--title-page-graphic-license-cc-by-sa-4.0))
       ((string= "CC-BY-ND-4.0" prop-val)
        (setq opeteceu--title-page-graphic-license opeteceu--title-page-graphic-license-cc-by-nd-4.0))
       ((string= "CC-BY-NC-4.0" prop-val)
        (setq opeteceu--title-page-graphic-license opeteceu--title-page-graphic-license-cc-by-nc-4.0))
       ((string= "CC-BY-NC-SA-4.0" prop-val)
        (setq opeteceu--title-page-graphic-license opeteceu--title-page-graphic-license-cc-by-nc-sa-4.0))
       ((string= "CC-BY-NC-ND-4.0" prop-val)
        (setq opeteceu--title-page-graphic-license opeteceu--title-page-graphic-license-cc-by-nc-nd-4.0))
       (t
        (let ((new-str ""))
          (setq new-str (string-trim-right
                         (string-trim-left
                          (replace-regexp-in-string "`" "\\\\\`" (org-export-string-as prop-val 'html t) t) "<p>\n") "</p>\n"))
          (setq opeteceu--title-page-graphic-license new-str)))))
    (setq prop-val (opeteceu--get-file-property-value "TITLE_PAGE_REPLACEMENT_GRAPHIC_OPETECEU" org-input-file))
    (if (string= "" prop-val)
        (setq opeteceu--title-page-replacement-graphic opeteceu--title-page-replacement-graphic-default)
      (setq opeteceu--title-page-replacement-graphic (expand-file-name prop-val (expand-file-name (file-name-directory org-input-file)))))
    (setq prop-val (opeteceu--get-file-property-value "TITLE_PAGE_REPLACEMENT_GRAPHIC_OPETECEU_SCALE" org-input-file))
    (if (string= "" prop-val)
        (setq opeteceu--title-page-replacement-graphic-scale opeteceu--title-page-replacement-graphic-scale-default)
      (setq opeteceu--title-page-replacement-graphic-scale (string-to-number prop-val)))
    (setq prop-val (opeteceu--get-file-property-value "PUBLISHER" org-input-file))
    (if (string= "" prop-val)
        (setq opeteceu--publisher opeteceu--publisher-default)
      (let ((new-str ""))
        (setq new-str (string-trim-right
                       (string-trim-left
                        (replace-regexp-in-string "`" "\\\\\`" (org-export-string-as prop-val 'html t) t) "<p>\n") "</p>\n"))
        (setq opeteceu--publisher new-str)))
    (setq prop-val (opeteceu--get-file-property-value "ISBN" org-input-file))
    (if (string= "" prop-val)
        (setq opeteceu--isbn opeteceu--isbn-default)
      (setq opeteceu--isbn prop-val))
    (setq prop-val (opeteceu--get-file-property-value "EDITION" org-input-file))
    (if (string= "" prop-val)
        (setq opeteceu--edition opeteceu--edition-default)
      (let ((new-str ""))
        (setq new-str (string-trim-right
                       (string-trim-left
                        (replace-regexp-in-string "`" "\\\\\`" (org-export-string-as prop-val 'html t) t) "<p>\n") "</p>\n"))
        (setq opeteceu--edition new-str)))
    (setq prop-val (opeteceu--get-file-property-value "LICENSE" org-input-file))
    (if (string= "" prop-val)
        (setq opeteceu--license opeteceu--license-default)
      (cond
       ((string= "CC0-1.0" prop-val)
        (setq opeteceu--license opeteceu--license-cc0-1.0))
       ((string= "CC-BY-4.0" prop-val)
        (setq opeteceu--license opeteceu--license-cc-by-4.0))
       ((string= "CC-BY-SA-4.0" prop-val)
        (setq opeteceu--license opeteceu--license-cc-by-sa-4.0))
       ((string= "CC-BY-ND-4.0" prop-val)
        (setq opeteceu--license opeteceu--license-cc-by-nd-4.0))
       ((string= "CC-BY-NC-4.0" prop-val)
        (setq opeteceu--license opeteceu--license-cc-by-nc-4.0))
       ((string= "CC-BY-NC-SA-4.0" prop-val)
        (setq opeteceu--license opeteceu--license-cc-by-nc-sa-4.0))
       ((string= "CC-BY-NC-ND-4.0" prop-val)
        (setq opeteceu--license opeteceu--license-cc-by-nc-nd-4.0))
       (t
        (let ((new-str ""))
          (setq new-str (string-trim-right
                         (string-trim-left
                          (replace-regexp-in-string "`" "\\\\\`" (org-export-string-as prop-val 'html t) t) "<p>\n") "</p>\n"))
          (setq opeteceu--license new-str)))))
    (setq prop-val (opeteceu--get-file-property-value "SIGIL_GRAPHIC_SCALE" org-input-file))
    (if (string= "" prop-val)
        (setq opeteceu--sigil-graphic-scale opeteceu--sigil-graphic-scale-default)
      (setq opeteceu--sigil-graphic-scale (string-to-number prop-val)))
    (setq prop-val (opeteceu--get-file-property-value "SIGIL_GRAPHIC" org-input-file))
    (if (string= "" prop-val)
        (setq opeteceu--sigil-graphic opeteceu--sigil-graphic-default)
      (if (string= (file-name-extension (expand-file-name prop-val (expand-file-name (file-name-directory org-input-file)))) "png")
          (setq opeteceu--sigil-graphic (expand-file-name prop-val (expand-file-name (file-name-directory org-input-file))))
        (when (and (executable-find "convert") (file-readable-p prop-val))
          (let ((resize-args ""))
            (setq resize-args " -density 600 -quality 10 ")
            (shell-command (concat "convert " resize-args "\"" (expand-file-name prop-val (expand-file-name (file-name-directory org-input-file))) "\" \"" (expand-file-name (file-name-directory org-input-file)) (file-name-base (expand-file-name prop-val (expand-file-name (file-name-directory org-input-file)))) ".png\""))
            (setq opeteceu--sigil-graphic (expand-file-name (concat (expand-file-name (file-name-directory org-input-file)) (file-name-base (expand-file-name prop-val (expand-file-name (file-name-directory org-input-file)))) ".png")))))))))

(defun opeteceu--generate-css-style-string ()
  "Generate the stylesheet using ORG-INPUT-FILE."
  (let ((out-css ""))
    (setq out-css (concat
                   "body {\n"
                   "  text-rendering: optimizeLegibility !important;\n"
                   "  margin: 5%;\n"
                   "  text-align: justify;\n"
                   "  font-size: medium;\n"
                   "  font-family: serif;\n"
                   "  text-transform: none;\n"
                   "  text-decoration: none;\n"
                   "  font-style: normal;\n"
                   "  font-weight: normal;\n"
                   "}\n"
                   "code { font-family: monospace; }\n"
                   "h1, h2, h3, h4, h5, h6 {\n"
                   "  font-family: sans-serif;\n"
                   "  text-align: right;\n"
                   "  text-transform: none;\n"
                                        ;"  margin: 0 auto;\n"
                   "  margin: 4em auto 0;\n"
                   "  font-weight: 100;\n"
                   "}\n"
                   "h1 {\n"
                   "  margin-top: 3em;\n"
                   "  line-height: 1.2em;\n"
                   "  font-size: 1.8em;\n"
                   "}\n"
                   "h1.title { }\n"
                   "h2.author { }\n"
                   "h3.date { }\n"
                   "ol.toc {\n"
                   "  padding: 0;\n"
                   "  margin-left: 1em;\n"
                   "}\n"
                   "ol.toc li {\n"
                   "  list-style-type: none;\n"
                   "  margin: 0;\n"
                   "  padding: 0;\n"
                   "}\n"
                   "a.footnoteRef { vertical-align: super; }\n"
                   "em, em em em, em em em em em { font-style: italic; }\n"
                   "em em, em em em em { font-style: normal; }\n"
                   "blockquote { font-style: italic; }\n"
                   ".center-legal {\n"
                   "  text-align: center;\n"
                   "  margin: 0 0 -1em 0;\n"
                   "  padding: 0;\n"
                   "  border: 0;\n"
                   "}\n"
                   ".center {\n"
                   "  text-align: center;\n"
                   "  margin: 0 auto;\n"
                   "}\n"
                   ".org-center {\n"
                   "  text-align: center;\n"
                   "  margin: 0 auto;\n"
                   "}\n"
                   "hr {\n"
                   "  text-align: center;\n"
                   "  margin-bottom: 4em;\n"
                   "}\n"
                   "hr::after {\n"
                   "  content: '';\n"
                   "}\n"))
    out-css))

(defun opeteceu--add-index-anchors (file-contents index-entries-hash-table)
  "Add index anchors to FILE-CONTENTS from INDEX-ENTRIES-HASH-TABLE.
Doing this early, before other processing, ensures title pages, copyright pages,
etc aren't included in the index."
  (let ((out-str "")
        curr-properties-list
        curr-property)
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (opeteceu--fold-show-all)
      (setq curr-properties-list (opeteceu--get-file-properties-and-values))
      (goto-char (point-min))
      (insert "Temp Line\n")
      (goto-char (point-min))
      (while curr-properties-list
        (setq curr-property (pop curr-properties-list))
        (when (string= (car curr-property) "ORG_NOVELIST_INDEX_ENTRY")
          (let ((case-fold-search t)
                (curr-term (cdr curr-property))
                (pos (point-min))
                (search-bound-pos (point-max))
                (curr-term-insert-str "")
                (buffer-str "")
                (word-count 0))
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
                      (insert "\n")  ; Make sure bounds don't include any of the content text by adding a newline.
                      (setq search-bound-pos (point)))
                  (setq search-bound-pos (point-max)))
                (goto-char pos)
                (while (re-search-forward (format "[[:space:][:punct:]]+?%s\\('s\\)?[[:punct:][:space:]]+?" (regexp-quote curr-term)) search-bound-pos t)
                  ;; Check insert not already done in previous loop.
                  (setq pos (point))
                  (unless (or (looking-at-p "@@html:<a ") (looking-at-p "></a>"))
                    (goto-char pos)
                    ;; Don't match Document or Section properties.
                    (unless (or (looking-at-p "^[ \t]*#\\+") (looking-at-p "^[ \t]*:+?[^\s]+?:+?"))
                      (goto-char pos)
                      ;; Get accurate(ish) word count at position.
                      (insert "<<<<<org-novelist-word-count-tag>>>>>")
                      (setq buffer-str (buffer-string))
                      (when (re-search-backward "<<<<<org-novelist-word-count-tag>>>>>" nil t)
                        (replace-match "" nil nil))
                      (with-temp-buffer
                        (insert buffer-str)
                        (fundamental-mode)
                        (goto-char (point-min))
                        (while (re-search-forward "@@html:<a.+?></a>@@" nil t)
                          (replace-match "" nil nil))
                        (goto-char (point-max))
                        (when (re-search-backward "<<<<<org-novelist-word-count-tag>>>>>" nil t)
                          (replace-match "" nil nil)
                          (setq word-count (how-many "\\w+" (point-min) (point)))))
                      ;; We should now have a resonably accurate word count.
                      (setq curr-term-insert-str (concat "@@html:<a id=\"" (string-replace " " "" (string-replace "!" "" (cdr curr-property))) "-" (number-to-string word-count) "\"></a>@@"))
                      ;; Make lists of the known page positions of terms in the story, based on word count.
                      (puthash (cdr curr-property) (cons word-count (gethash (cdr curr-property) index-entries-hash-table)) index-entries-hash-table)
                      (setq word-count 0)
                      (setq opeteceu--index-entries-exist-in-text-p t)
                      (insert curr-term-insert-str)
                      ;; Increase search-bound-pos by the number of characters we've added.
                      (setq search-bound-pos (+ search-bound-pos (length curr-term-insert-str))))))))))
        (goto-char (point-min)))
      (goto-char (point-min))
      (opeteceu--delete-line)
      (setq out-str (buffer-string)))
    out-str))

(defun opeteceu--make-index-string (index-entries-hash-table &optional words-per-page)
  "Create an index without header.
INDEX-ENTRIES-HASH-TABLE is a hash table of lists. Each list is a series of
word positions in a story for the given key.
WORDS-PER-PAGE is the average number of words in a page of the story in order
to generate the virtual page numbers in the index."
  (unless words-per-page
    (setq words-per-page 248))  ; Just an average that takes blank pages, glossaries, indices, half used pages, and title pages into account
  (let ((keys (sort (hash-table-keys index-entries-hash-table) 'string<))
        key
        (pages-hash (make-hash-table :test 'eql))
        (curr-pages-list '())
        curr-word
        curr-page
        (terms '())
        curr-term
        curr-parent
        pages-keys
        pages-key
        (low-page -1)
        (index-str ""))
    (while keys
      (setq key (pop keys))
      (when (> (length (split-string key "!" t " ")) 1)
        (setq terms (cons (car (split-string key "!" t " ")) terms)))
      (setq terms (cons key terms)))
    (setq terms (delete-dups terms))
    (setq terms (sort terms 'string<))
    (while terms
      (setq key (pop terms))
      (setq curr-term key)
      (setq curr-parent nil)
      (when (> (length (split-string key "!" t " ")) 1)
        (setq curr-parent (car (split-string key "!" t " ")))
        (setq curr-term (car (last (split-string key "!" t " ")))))
      (if curr-parent
          (setq index-str (concat index-str " \\nbsp\\nbsp\\nbsp\\nbsp\\nbsp\\nbsp\\nbsp " curr-term))
        (setq index-str (concat index-str curr-term)))
      (when (gethash key index-entries-hash-table)
        (setq index-str (concat index-str ", "))
        ;; Process into pages here.
        ;; Pages will be in reverse order. Create hash map where page is the key, and anchor is the value. Adding new ones will continue to update the anchor until it hits the correct (earliest) value. Reset the hash table for each iteration of the loop.
        (setq curr-pages-list (sort (gethash key index-entries-hash-table) '>))
        (while curr-pages-list
          (setq curr-word (pop curr-pages-list))
          (setq curr-page (+ 1 (/ curr-word words-per-page)))
          (puthash curr-page (concat "@@html:[" (number-to-string curr-page) "](#" (string-replace " " "" (string-replace "!" "" key)) "-" (number-to-string curr-word) ")@@") pages-hash))
        ;; We have the correct page numbers, and the correct link for each. The only thing left to do is concatenate consecutive pages into one link from a range.
        (setq pages-keys (sort (hash-table-keys pages-hash) '<))
        (while pages-keys
          (setq pages-key (pop pages-keys))
          (when (= low-page -1)
            (setq low-page pages-key))
          (if (not (car pages-keys))
              ;; Last one
              (progn
                (if (not (eq low-page pages-key))
                    (setq index-str (concat index-str (format "%s&ndash;%s" (gethash low-page pages-hash) (gethash pages-key pages-hash))))
                  (setq index-str (concat index-str (format "%s" (gethash pages-key pages-hash)))))
                (setq low-page -1))
            (unless (eq pages-key (- (car pages-keys) 1))
              (if (not (eq low-page pages-key))
                  (setq index-str (concat index-str (format "%s&ndash;%s, " (gethash low-page pages-hash) (gethash pages-key pages-hash))))
                (setq index-str (concat index-str (format "%s, " (gethash pages-key pages-hash)))))
              (setq low-page -1))))
        (clrhash pages-hash))
      (if curr-parent
          (setq index-str (concat index-str "\n\n"))
        (setq index-str (concat index-str "@@html:\\@@\n"))))
    (string-trim index-str)))

(defun opeteceu--add-indices (file-contents index-string)
  "Add INDEX-STRING to FILE-CONTENTS wherever necessary."
  (let ((out-str ""))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (opeteceu--fold-show-all)
      (when opeteceu--index-entries-exist-in-text-p
        ;; After the Index string has been constructed, we can place it anywhere in the code it is required.
        (when (member "index" (split-string (opeteceu--get-file-property-value "GENERATE") "[,\f\t\n\r\v]+" t " "))
          ;; Add index to end of story.
          (goto-char (point-max))
          (insert (concat "* Index                  :no_header_preamble:no_toc_entry:plain_pagestyle:\n"))  ; Don't add as backmatter as this will add the index to TOC. LaTeX doesn't add it to TOC, so I'm just making it match.
          (insert index-string))
        ;; Replace "#+latex: \printindex" with an actual index﻿. A little broad as a solution, but works for me most of the time.
        (goto-char (point-min))
        (let ((case-fold-search t))
          (while (re-search-forward "^[ \t]*#\\+latex: \\\\printindex" nil t)
            (replace-match (concat "* Index                  :no_header_preamble:no_toc_entry:plain_pagestyle:\n" index-string) nil t))))
      (setq out-str (buffer-string)))
    out-str))

(defun opeteceu--magick-text-dimension (dimension text family style pointsize)
  "Find the given dimension of an image of text generated by Imagemagick.
DIMENSION should be either \"width\" or \"height\".
TEXT is the string to be tested.
FAMILY in the name of the font family being used.
STYLE is the style of the font family being used.
POINTSIZE is the font size being tested."
  (catch 'MAGICK_NOT_FOUND
    (if (executable-find "magick")
        (let ((width ""))
          (with-temp-buffer
            ;; (insert-buffer-substring shell-command-buffer-name)
            (insert (shell-command-to-string (concat "magick -debug annotate xc: -family \"" family "\" -style \"" style "\" -pointsize " (number-to-string pointsize) " -annotate 0 \"" (replace-regexp-in-string (regexp-quote "\"") (regexp-quote "\\\"") text) "\" null: 2>&1")))
            (goto-char (point-min))
            (let ((case-fold-search t)
                  beg)
              (while (re-search-forward (concat dimension ": ") nil t)
                (setq beg (point))
                (re-search-forward ";" nil t)
                (forward-char -1)
                (setq width (buffer-substring beg (point))))))
          (string-to-number width))
      (progn
        (user-error "Magick executable was not found, but is required")
        (throw 'MAGICK_NOT_FOUND "Magick executable was not found, but is required")))))

(defun opeteceu--magick-text-width (text family style pointsize)
  "Find the width of an image of text generated by Imagemagick.
TEXT is the string to be tested.
FAMILY in the name of the font family being used.
STYLE is the style of the font family being used.
POINTSIZE is the font size being tested."
  (opeteceu--magick-text-dimension "width" text family style pointsize))

(defun opeteceu--magick-text-height (text family style pointsize)
  "Find the height of an image of text generated by Imagemagick.
TEXT is the string to be tested.
FAMILY in the name of the font family being used.
STYLE is the style of the font family being used.
POINTSIZE is the font size being tested."
  (opeteceu--magick-text-dimension "height" text family style pointsize))

(defun opeteceu--generate-cover (org-input-file cover-output-file)
  "Generate a cover for the story, COVER-OUTPUT-FILE, from ORG-INPUT-FILE.
The final cover image location will also be stored in
`org-pandoc-export-to-epub-cubes-en-us--title-page'."
  (let ((valid-replacement-graphic-p nil))
    (when opeteceu--title-page-replacement-graphic
      (when (file-readable-p opeteceu--title-page-replacement-graphic)
        (setq valid-replacement-graphic-p t)))
    (if valid-replacement-graphic-p
        ;; Resize image if required.
        (if (and (executable-find "magick") opeteceu--title-page-replacement-graphic-scale)
            (progn
              (shell-command (concat "magick \"" opeteceu--title-page-replacement-graphic "\" -resize " (number-to-string (* 100 opeteceu--title-page-replacement-graphic-scale)) "% \"" cover-output-file "\""))
              (setq opeteceu--title-page cover-output-file))
          (progn
            (copy-file opeteceu--title-page-replacement-graphic cover-output-file t)
            (setq opeteceu--title-page cover-output-file)))
      (let* (cover-graphic-arg
             (text-pixel-width-max 1195)
             (pointsize-subtitle (floor (* opeteceu--pointsize-headfont 0.5)))
             title-words
             curr-word
             (curr-line "")
             (title-lines '())
             (title-graphic-arg "")
             (curr-title-pos 0)
             (last-title-pos curr-title-pos)
             (title-line-space-scale 1.1)
             subtitle-words
             (subtitle-lines '())
             (subtitle-graphic-arg "")
             (curr-subtitle-pos 0)
             (last-subtitle-pos curr-subtitle-pos)
             (last-subtitle-pos-store last-subtitle-pos)
             (subtitle-line-space-scale 1.2)
             author-words
             (author-lines '())
             (author-graphic-arg "")
             (curr-author-pos 0)
             (author-line-space-scale 1.1))
        ;; If graphic available, it will be used in generating cover. Start with argument that will need to be passed to Imagemagick.
        (when (and (file-readable-p opeteceu--title-page-graphic) (not (string= "" opeteceu--title-page-graphic)))
          (setq cover-graphic-arg (concat " -draw 'image over 0,-800 0,0 \"" opeteceu--title-page-graphic "\"'")))
        ;; If Title longer than x, split into lines. Else, return Title.
        (let ((curr-line-height (opeteceu--magick-text-height (opeteceu--get-file-property-value "TITLE" org-input-file) opeteceu--headfont "Normal" opeteceu--pointsize-headfont)))
          (if (> (opeteceu--magick-text-width (opeteceu--get-file-property-value "TITLE" org-input-file) opeteceu--headfont "Normal" opeteceu--pointsize-headfont) text-pixel-width-max)
              ;; Title too long, split it up.
              (progn
                (setq title-words (split-string (replace-regexp-in-string (regexp-quote "\"") (regexp-quote "\\\\\"") (replace-regexp-in-string (regexp-quote "'") (regexp-quote "'\\''") (opeteceu--get-file-property-value "TITLE" org-input-file))) " "))  ; What text we're actually splitting up, stored as words
                ;; Loop through list of words in title and arrange into lines.
                (while title-words
                  (setq curr-word (car title-words))  ; Get first word left in list
                  (setq title-words (cdr title-words))  ; Remove first word left in list from list
                  ;; If adding current word to existing line will be too long, create a new line and add old one to title-lines
                  (if (> (+ (opeteceu--magick-text-width curr-line opeteceu--headfont "Normal" opeteceu--pointsize-headfont)
                            1
                            (opeteceu--magick-text-width curr-word opeteceu--headfont "Normal" opeteceu--pointsize-headfont))
                         text-pixel-width-max)
                      (progn
                        (setq title-lines (cons curr-line title-lines))
                        (setq curr-line curr-word))
                    ;; Add word to existing line
                    (if (< (length curr-line) 1)
                        (setq curr-line curr-word)
                      (setq curr-line (concat curr-line " " curr-word))))
                  ;; If no words left, add current line to title-lines
                  (unless title-words
                    (setq title-lines (cons curr-line title-lines))))
                (setq curr-line "")
                ;; Title has now been made into lines. However, lines are in reverse order, so switch them back
                (setq title-lines (reverse title-lines))
                ;; Turn title lines into imagemagick arguments.
                (while title-lines
                  (setq title-graphic-arg (concat title-graphic-arg "-draw 'text 0," (number-to-string curr-title-pos) " \"" (car title-lines) "\"' "))
                  (setq curr-title-pos (+ curr-title-pos (* title-line-space-scale curr-line-height)))
                  (setq title-lines (cdr title-lines)))
                (setq last-title-pos (- curr-title-pos (* title-line-space-scale curr-line-height)))
                ;; Return new title.
                (setq title-graphic-arg (concat "-draw \"line 100," (number-to-string (- 1280 (* title-line-space-scale curr-line-height))) " 1500," (number-to-string (- 1280 (* title-line-space-scale curr-line-height)))
                                                "\" -draw \"line 100," (number-to-string (+ 1280 (* title-line-space-scale curr-line-height) last-title-pos)) " 1500," (number-to-string (+ 1280 (* title-line-space-scale curr-line-height) last-title-pos))
                                                "\" " title-graphic-arg )))
            ;; Title is fine, just return it.
            (setq title-graphic-arg (concat "-draw \"line 100," (number-to-string (- 1280 (* title-line-space-scale curr-line-height))) " 1500," (number-to-string (- 1280 (* title-line-space-scale curr-line-height)))
                                            "\" -draw \"line 100," (number-to-string (+ 1280 (* title-line-space-scale curr-line-height))) " 1500," (number-to-string (+ 1280 (* title-line-space-scale curr-line-height)))
                                            "\" -draw 'text 0," (number-to-string curr-title-pos) " \"" (replace-regexp-in-string (regexp-quote "\"") (regexp-quote "\\\\\"") (replace-regexp-in-string (regexp-quote "'") (regexp-quote "'\\''") (opeteceu--get-file-property-value "TITLE"  org-input-file))) "\"' ")))
          (setq curr-subtitle-pos (+ last-title-pos (* 2 (* title-line-space-scale curr-line-height))))
          (setq last-subtitle-pos curr-subtitle-pos)
          (setq last-subtitle-pos-store last-subtitle-pos))
        ;; If Subtitle longer than x, split into lines. Else, return Subtitle.
        (let ((curr-line-height (opeteceu--magick-text-height (opeteceu--get-file-property-value "SUBTITLE" org-input-file) opeteceu--headfont "Italic" pointsize-subtitle)))
          (if (> (opeteceu--magick-text-width (opeteceu--get-file-property-value "SUBTITLE" org-input-file) opeteceu--headfont "Italic" pointsize-subtitle) text-pixel-width-max)
              ;; Subtitle too long, split it up.
              (progn
                (setq subtitle-words (split-string (replace-regexp-in-string (regexp-quote "\"") (regexp-quote "\\\\\"") (replace-regexp-in-string (regexp-quote "'") (regexp-quote "'\\''") (opeteceu--get-file-property-value "SUBTITLE" org-input-file))) " "))  ; What text we're actually splitting up, stored as words
                ;; Loop through list of words in subtitle and arrange into lines.
                (while subtitle-words
                  (setq curr-word (car subtitle-words))  ; Get first word left in list
                  (setq subtitle-words (cdr subtitle-words))  ; Remove first word left in list from list
                  ;; If adding current word to existing line will be too long, create a new line and add old one to subtitle-lines
                  (if (> (+ (opeteceu--magick-text-width curr-line opeteceu--headfont "Italic" pointsize-subtitle)
                            1
                            (opeteceu--magick-text-width curr-word opeteceu--headfont "Italic" pointsize-subtitle))
                         text-pixel-width-max)
                      (progn
                        (setq subtitle-lines (cons curr-line subtitle-lines))
                        (setq curr-line curr-word))
                    ;; Add word to existing line
                    (if (< (length curr-line) 1)
                        (setq curr-line curr-word)
                      (setq curr-line (concat curr-line " " curr-word))))
                  ;; If no words left, add current line to subtitle-lines
                  (unless subtitle-words
                    (setq subtitle-lines (cons curr-line subtitle-lines))))
                (setq curr-line "")
                ;; Title has now been made into lines. However, lines are in reverse order, so switch them back
                (setq subtitle-lines (reverse subtitle-lines))
                ;; Turn subtitle lines into imagemagick arguments.
                (while subtitle-lines
                  (setq subtitle-graphic-arg (concat subtitle-graphic-arg "-draw 'text 0," (number-to-string curr-subtitle-pos) " \"" (car subtitle-lines) "\"' "))
                  (setq curr-subtitle-pos (+ curr-subtitle-pos (* subtitle-line-space-scale curr-line-height)))
                  (setq subtitle-lines (cdr subtitle-lines)))
                (setq last-subtitle-pos (- curr-subtitle-pos (* subtitle-line-space-scale curr-line-height))))
            ;; Subtitle is fine, just return it.
            (setq subtitle-graphic-arg (concat "-draw 'text 0," (number-to-string curr-subtitle-pos) " \"" (replace-regexp-in-string (regexp-quote "\"") (regexp-quote "\\\\\"") (replace-regexp-in-string (regexp-quote "'") (regexp-quote "'\\''") (opeteceu--get-file-property-value "SUBTITLE"  org-input-file))) "\"' ")))
          (if (= last-subtitle-pos last-subtitle-pos-store)
              (setq curr-author-pos (+ last-subtitle-pos (* 4 (* subtitle-line-space-scale (opeteceu--magick-text-height (opeteceu--get-file-property-value "TITLE" org-input-file) opeteceu--headfont "Normal" opeteceu--pointsize-headfont)))))
            (setq curr-author-pos (+ last-subtitle-pos (* 4 (* subtitle-line-space-scale curr-line-height))))))
        ;; If Author longer than x, split into lines. Else, return Author.
        (let ((curr-line-height (opeteceu--magick-text-height (opeteceu--get-file-property-value "AUTHOR" org-input-file) opeteceu--signaturefont "Normal" opeteceu--pointsize-signaturefont)))
          (if (> (opeteceu--magick-text-width (opeteceu--get-file-property-value "AUTHOR" org-input-file) opeteceu--signaturefont "Normal" opeteceu--pointsize-signaturefont) text-pixel-width-max)
              ;; Author too long, split it up.
              (progn
                (setq author-words (split-string (replace-regexp-in-string (regexp-quote "\"") (regexp-quote "\\\\\"") (replace-regexp-in-string (regexp-quote "'") (regexp-quote "'\\''") (opeteceu--get-file-property-value "AUTHOR" org-input-file))) " "))  ; What text we're actually splitting up, stored as words
                ;; Loop through list of words in author and arrange into lines.
                (while author-words
                  (setq curr-word (car author-words))  ; Get first word left in list
                  (setq author-words (cdr author-words))  ; Remove first word left in list from list
                  ;; If adding current word to existing line will be too long, create a new line and add old one to author-lines
                  (if (> (+ (opeteceu--magick-text-width curr-line opeteceu--signaturefont "Normal" opeteceu--pointsize-signaturefont)
                            1
                            (opeteceu--magick-text-width curr-word opeteceu--signaturefont "Normal" opeteceu--pointsize-signaturefont))
                         text-pixel-width-max)
                      (progn
                        (setq author-lines (cons curr-line author-lines))
                        (setq curr-line curr-word))
                    ;; Add word to existing line
                    (if (< (length curr-line) 1)
                        (setq curr-line curr-word)
                      (setq curr-line (concat curr-line " " curr-word))))
                  ;; If no words left, add current line to author-lines
                  (unless author-words
                    (setq author-lines (cons curr-line author-lines))))
                (setq curr-line "")
                ;; Author has now been made into lines. However, lines are in reverse order, so switch them back
                (setq author-lines (reverse author-lines))
                ;; Turn author lines into imagemagick arguments.
                (while author-lines
                  (setq author-graphic-arg (concat author-graphic-arg "-draw 'text 0," (number-to-string curr-author-pos) " \"" (car author-lines) "\"' "))
                  (setq curr-author-pos (+ curr-author-pos (* author-line-space-scale curr-line-height)))
                  (setq author-lines (cdr author-lines))))
            ;; Author is fine, just return it.
            (setq author-graphic-arg (concat "-draw 'text 0," (number-to-string curr-author-pos) " \"" (replace-regexp-in-string (regexp-quote "\"") (regexp-quote "\\\\\"") (replace-regexp-in-string (regexp-quote "'") (regexp-quote "'\\''") (opeteceu--get-file-property-value "AUTHOR"  org-input-file))) "\"' "))))
        ;; If Imagemagick is available, generate cover.
        (when (and (executable-find "magick") (file-readable-p opeteceu--title-page-graphic))
          (shell-command (concat "magick -size 1600x2560 xc:white -fill black -stroke black -family \"" opeteceu--headfont "\" -style \"Normal\" -pointsize " (number-to-string opeteceu--pointsize-headfont) " -gravity center " title-graphic-arg "-family \"" opeteceu--headfont "\" -style \"Italic\" -pointsize " (number-to-string pointsize-subtitle) " " subtitle-graphic-arg "-family \"" opeteceu--signaturefont "\" -style \"Normal\" -pointsize " (number-to-string opeteceu--pointsize-signaturefont) " " author-graphic-arg cover-graphic-arg " \"" cover-output-file "\""))
          (setq opeteceu--title-page cover-output-file))))))

(defun opeteceu--add-legal-page (file-contents)
  "Given a string FILE-CONTENTS, add Org mode md export code for a title page.
Return string of new file contents."
  (let ((out-str ""))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (opeteceu--fold-show-all)
      (goto-char (point-min))
      (insert "Temp Line\n")
      (goto-char (point-min))
      (when (org-goto-first-child)
        (beginning-of-line)
        (insert "\n")
        (forward-line -1)
        ;; Markdown meta data and legal info.
        (insert "#+BEGIN_EXPORT md\n"
                "---\n"
                "title:\n"
                "- type: main\n"
                "  text: " (opeteceu--get-file-property-value "TITLE"))
        (unless (string= (opeteceu--get-file-property-value "SUBTITLE") "")
          (insert " --- " (opeteceu--get-file-property-value "SUBTITLE")))
        (insert "\n"
                "creator:\n"
                "- role: author\n"
                "  text: <i>" (opeteceu--get-file-property-value "AUTHOR") "</i>\n"
                "publisher: " opeteceu--publisher "\n"
                "rights: "
                (replace-regexp-in-string "</div>" ""
                                          (replace-regexp-in-string "<div class=\"center-legal\">" ""
                                                                    (replace-regexp-in-string ":" ""
                                                                                              (replace-regexp-in-string "\\\\\n" " "  opeteceu--license t) t) t) t) "\n"
                "date: " (opeteceu--format-time-string "%Y" (org-time-from-absolute (org-time-string-to-absolute (opeteceu--get-file-property-value "DATE")))) "\n"
                "lang: " opeteceu--language "\n"
                "toc-title: Contents\n")
        (when (and (file-readable-p opeteceu--title-page) (not (string= "" opeteceu--title-page)))
          (insert "cover-image: " opeteceu--title-page "\n"))
        (insert "...\n"
                "\n"
                "#   {.unnumbered .unlisted}\n\n"
                "**" (opeteceu--get-file-property-value "TITLE") "**")
        (unless (string= (opeteceu--get-file-property-value "SUBTITLE") "")
          (insert " --- " (opeteceu--get-file-property-value "SUBTITLE")))
        (insert "\n"
                "\n"
                "Author: *" (opeteceu--get-file-property-value "AUTHOR") "*\n"
                "\n"
                "Cover: ")
        (insert opeteceu--title-page-graphic-copyright)
        (insert "\\\nCover: ")
        (insert opeteceu--title-page-graphic-license)
        (insert "\n"
                "\\\n"
                "\\\n"
                "This book, excluding the cover art, is copyright &copy; " (opeteceu--format-time-string "%Y" (org-time-from-absolute (org-time-string-to-absolute (opeteceu--get-file-property-value "DATE"))))
                " " (opeteceu--get-file-property-value "AUTHOR") ".\n"
                "\n")
        (insert opeteceu--license "\n")
        (insert "\n"
                "The author assumes no liability for errors or omissions in this book, or for damages or loss of revenue resulting from the use of the information contained herein. The characters and incidents portrayed in this book are fictional. Any similarities to real persons, living, dead or yet to exist, is entirely coincidental.\n"
                "\n")
        (unless (string= (opeteceu--get-file-property-value "EMAIL") "")
          (insert "You can contact the author via e-mail:\\\n"
                  "<code><" (opeteceu--get-file-property-value "EMAIL") "></code>\n"
                  "\n"))
        (unless (string= (opeteceu--get-file-property-value "PUBLISHER") "")
          (insert "Published by " opeteceu--publisher "\n"
                  "\n"))
        (unless (string= opeteceu--isbn "")
          (insert "ISBN " opeteceu--isbn "\n"
                  "\n"))
        (insert opeteceu--edition ": " (opeteceu--format-time-string "%B %Y" (org-time-from-absolute (org-time-string-to-absolute (opeteceu--get-file-property-value "DATE")))) "\n"
                "\\\n"
                "\\\n"
                "\\\n")
        (when (and (file-readable-p opeteceu--sigil-graphic) (not (string= "" opeteceu--sigil-graphic)))
          (insert "<div class=\"center\">\n"
                  "![Sigil](" opeteceu--sigil-graphic " \"Sigil\"){ width=")
          (if opeteceu--sigil-graphic-scale
              (insert (number-to-string (* 15 opeteceu--sigil-graphic-scale)))
            (insert "100"))
          (insert "% }\\\n"
                  "</div>\n"))
        (insert "<div id=\"endmeta\"></div>\n"
                "#+END_EXPORT\n")
        (goto-char (point-min))
        (opeteceu--delete-line)
        (setq out-str (buffer-string))))
    out-str))

(defun opeteceu--restyle-headings (file-contents)
  "Given a string FILE-CONTENTS, add Org mode md exports to restyle headings.
Return string of new file contents."
  (let ((out-str "")
        (no-header nil)
        (no-header-name nil)
        (no-header-preamble nil)
        (no-toc-entry nil)
        curr-cust-id
        curr-heading
        curr-level
        (toc-head-string "")
        (chap-num 0)
        (part-num 0)
        (part nil)
        beg)
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (opeteceu--fold-show-all)
      (goto-char (point-min))
      (insert "Temp Line\n")
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
          (when (member "part" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
            (setq part t)))
        (setq curr-cust-id (org-entry-get (point) "CUSTOM_ID"))
        (unless curr-cust-id
          (when (string= "" curr-cust-id)
            (setq curr-cust-id nil)))
        ;; Check matter type and replace appropriately, convert heading level to same output level. If no matter type, assume front matter.
        (cond ((string= (org-entry-get (point) "ORG-NOVELIST-MATTER-TYPE") "FRONT MATTER")
               (setq curr-heading (nth 4 (org-heading-components)))
               (if part
                   (setq curr-level 1)
                 (setq curr-level (org-current-level)))
               (setq curr-heading (replace-regexp-in-string (regexp-quote "&") "&amp;" curr-heading nil t))
               (setq curr-heading (replace-regexp-in-string "\\\\thinsp" "" curr-heading nil t))
               (beginning-of-line)
               (opeteceu--delete-line)
               (setq beg (point))
               (re-search-forward ":END:" nil t)
               (delete-region beg (point))
               (opeteceu--delete-line)
               (insert "#+BEGIN_EXPORT md\n")
               (while (> curr-level 0)
                 (setq curr-level (- curr-level 1))
                 (insert "#"))
               (when (string= curr-heading "")
                 (setq no-header-name t))
               (when (and no-header-name no-header-preamble)
                 (setq no-header t))
               (when no-toc-entry
                 (setq toc-head-string "{.unnumbered .unlisted}"))
               (if (or no-header no-header-name)
                   (insert "   {.unnumbered .unlisted}\n"
                           "\n")
                 (insert " " curr-heading " " toc-head-string "\n"
                         "\n"
                         "---\n"))
               (when curr-cust-id
                 (insert "\n<a id=\"" curr-cust-id "\"></a>\n"))
               (unless (or (string= curr-heading "") (string= curr-heading "Glossary") (string= curr-heading "Index"))
                 (insert "\n<a id=\"" curr-heading "\"></a>\n"))
               (insert "#+END_EXPORT\n")
               (forward-char -1))
              ((string= (org-entry-get (point) "ORG-NOVELIST-MATTER-TYPE") "MAIN MATTER")
               (setq curr-heading (nth 4 (org-heading-components)))
               (if part
                   (setq curr-level 1)
                 (setq curr-level (org-current-level)))
               (setq curr-heading (replace-regexp-in-string (regexp-quote "&") "&amp;" curr-heading nil t))
               (setq curr-heading (replace-regexp-in-string "\\\\thinsp" "" curr-heading nil t))
               (beginning-of-line)
               (opeteceu--delete-line)
               (setq beg (point))
               (re-search-forward ":END:" nil t)
               (delete-region beg (point))
               (opeteceu--delete-line)
               (insert "#+BEGIN_EXPORT md\n")
               (when (> curr-level 1)
                 (setq no-header-preamble t))
               (when (string= curr-heading "")
                 (setq no-header-name t))
               (when (and no-header-name no-header-preamble)
                 (setq no-header t))
               (when no-toc-entry
                 (setq no-header-preamble t)
                 (setq toc-head-string "{.unnumbered .unlisted}"))
               (when (and (= curr-level 1) (not no-header) (not no-toc-entry) (not part))
                 (setq chap-num (+ chap-num 1)))
               (when part
                 (setq part-num (+ part-num 1)))
               (while (> curr-level 0)
                 (setq curr-level (- curr-level 1))
                 (insert "#"))
               (cond (no-header
                      (insert "   {.unnumbered .unlisted}\n"
                              "\n"))
                     (no-header-preamble
                      (insert " " curr-heading " " toc-head-string "\n"
                              "\n"
                              "---\n"))
                     (no-header-name
                      (if part
                          (insert " Part " (number-to-string part-num))
                        (insert " Chapter " (number-to-string chap-num)))
                      (insert " " toc-head-string "\n"
                              "\n"
                              "---\n"))
                     (t
                      (if part
                          (insert " Part " (number-to-string part-num))
                        (insert " Chapter " (number-to-string chap-num)))
                      (insert " --- " curr-heading " " toc-head-string "\n"
                              "\n"
                              "---\n")))
               (when curr-cust-id
                 (insert "\n<a id=\"" curr-cust-id "\"></a>\n"))
               (unless (or (string= curr-heading "") (string= curr-heading "Glossary") (string= curr-heading "Index"))
                 (insert "\n<a id=\"" curr-heading "\"></a>\n"))
               (insert "#+END_EXPORT\n")
               (forward-char -1))
              ((string= (org-entry-get (point) "ORG-NOVELIST-MATTER-TYPE") "BACK MATTER")
               (setq curr-heading (nth 4 (org-heading-components)))
               (if part
                   (setq curr-level 1)
                 (setq curr-level (org-current-level)))
               (setq curr-heading (replace-regexp-in-string (regexp-quote "&") "&amp;" curr-heading nil t))
               (setq curr-heading (replace-regexp-in-string "\\\\thinsp" "" curr-heading nil t))
               (beginning-of-line)
               (opeteceu--delete-line)
               (setq beg (point))
               (re-search-forward ":END:" nil t)
               (delete-region beg (point))
               (opeteceu--delete-line)
               (insert "#+BEGIN_EXPORT md\n")
               (while (> curr-level 0)
                 (setq curr-level (- curr-level 1))
                 (insert "#"))
               (when (string= curr-heading "")
                 (setq no-header-name t))
               (when (and no-header-name no-header-preamble)
                 (setq no-header t))
               (when no-toc-entry
                 (setq toc-head-string "{.unnumbered .unlisted}"))
               (if (or no-header no-header-name)
                   (insert "   {.unnumbered .unlisted}\n"
                           "\n")
                 (insert " " curr-heading " " toc-head-string "\n"
                         "\n"
                         "---\n"))
               (when curr-cust-id
                 (insert "\n<a id=\"" curr-cust-id "\"></a>\n"))
               (unless (string= curr-heading "")
                 (insert "\n<a id=\"" curr-heading "\"></a>\n"))
               (insert "#+END_EXPORT\n")
               (forward-char -1))
              (t
               (setq curr-heading (nth 4 (org-heading-components)))
               (if part
                   (setq curr-level 1)
                 (setq curr-level (org-current-level)))
               (setq curr-heading (replace-regexp-in-string (regexp-quote "&") "&amp;" curr-heading nil t))
               (setq curr-heading (replace-regexp-in-string "\\\\thinsp" "" curr-heading nil t))
               (beginning-of-line)
               (opeteceu--delete-line)
               (insert "#+BEGIN_EXPORT md\n")
               (when (> curr-level 1)
                 (setq no-header-preamble t))
               (when (string= curr-heading "")
                 (setq no-header-name t))
               (when (and no-header-name no-header-preamble)
                 (setq no-header t))
               (when no-toc-entry
                 (setq no-header-preamble t)
                 (setq toc-head-string "{.unnumbered .unlisted}"))
               (when (and (= curr-level 1) (not no-header) (not no-toc-entry) (not part))
                 (setq chap-num (+ chap-num 1)))
               (while (> curr-level 0)
                 (setq curr-level (- curr-level 1))
                 (insert "#"))
               (cond (no-header
                      (insert "   {.unnumbered .unlisted}\n"
                              "\n"))
                     (no-header-preamble
                      (insert " " curr-heading " " toc-head-string "\n"
                              "\n"
                              "---\n"))
                     (no-header-name
                      (if part
                          (insert " Part " (number-to-string part-num))
                        (insert " Chapter " (number-to-string chap-num)))
                      (insert " " toc-head-string "\n"
                              "\n"
                              "---\n"))
                     (t
                      (if part
                          (insert " Part " (number-to-string part-num))
                        (insert " Chapter " (number-to-string chap-num)))
                      (insert " --- " curr-heading " " toc-head-string "\n"
                              "\n"
                              "---\n")))
               (when curr-cust-id
                 (insert "\n<a id=\"" curr-cust-id "\"></a>\n"))
               (unless (or (string= curr-heading "") (string= curr-heading "Glossary") (string= curr-heading "Index"))
                 (insert "\n<a id=\"" curr-heading "\"></a>\n"))
               (insert "#+END_EXPORT\n")
               (forward-char -1)))
        (setq no-header nil)
        (setq no-header-name nil)
        (setq no-header-preamble nil)
        (setq no-toc-entry nil)
        (setq part nil)
        (setq toc-head-string ""))
      (goto-char (point-min))
      (opeteceu--delete-line)
      (setq out-str (buffer-string)))
    out-str))

(defun opeteceu--remap-shy-inclusions (file-contents)
  "Given a string FILE-CONTENTS, remap the Org mode shy inclusions correctly.
This actually just removes shy inclusions.
HTML won't process them correctly, and clearly doesn't need them anyway.
Return string of new file contents."
  (let ((out-str ""))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (opeteceu--fold-show-all)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward "\\\\-" nil t)
          (delete-char -2)))
      (setq out-str (buffer-string)))
    out-str))

(defun opeteceu--remap-grave-accent-as-opening-quote (file-contents)
  "Given a string FILE-CONTENTS, remap the Org mode grave accents correctly.
Return string of new file contents."
  ;; This solution is a little broad, but works for me most of the time.
  (let ((out-str ""))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (opeteceu--fold-show-all)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward " `" nil t)
          (replace-match " \\lsquo﻿" nil t)))
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward ">\\\\`" nil t)
          (replace-match ">&lsquo;﻿﻿" nil t)))
      (setq out-str (buffer-string)))
    out-str))

(defun opeteceu--remap-latex-index-anchors (file-contents)
  "Given a string FILE-CONTENTS, remap the Org mode LaTeX index anchors correctly.
Return string of new file contents."
  ;; Remove #+INDEX: properties as ePub won't deal with them, and it will cause unwanted line breaks in final ePub.
  (let ((out-str ""))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (opeteceu--fold-show-all)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward "^[ \t]*#\\+INDEX:" nil t)
          (opeteceu--delete-line)))
      (setq out-str (buffer-string)))
    out-str))

(defun opeteceu--remap-internal-links (file-contents)
  "Given a string FILE-CONTENTS, remap the Org mode internal links correctly.
This is necessary when original Org headings have been replaced by LaTeX code.
Return string of new file contents."
  (let ((out-str "")
        (case-fold-search t)
        beg
        link-val
        link-text
        (blind-link-num 0))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (opeteceu--fold-show-all)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[[^:/\.\n\r]+?]]" nil t)
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
              (insert "@@html:[" link-text "](#" link-val ")@@")
            (progn
              (setq blind-link-num (+ 1 blind-link-num))
              (insert "@@html:[" (number-to-string blind-link-num) "](#" link-val ")@@")))))
      (setq out-str (buffer-string)))
    out-str))

(defun opeteceu--remap-no-name-md-img-tags (file)
  "Given a markdown FILE, correct no-name img tags.
When images have no given name, remove the img name tags."
  (find-file file)
  (goto-char (point-min))
  (let ((case-fold-search t))
    (while (re-search-forward (format "^[ \t]*%s" (regexp-quote "![img](../Images/")) nil t)
      (delete-char -15)
      (insert "](../Images/"))
    (save-buffer)
    (kill-buffer)))


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
        (undo-tree-auto-save-history-orig nil)
        (org-export-backends-orig nil)
        (file-contents "")
        (cubes-css (opeteceu--generate-css-style-string))
        (index-entries (make-hash-table :test 'equal))
        (index-str ""))
    (setq opeteceu--index-entries-exist-in-text-p nil)
    (setq opeteceu--headfont nil)
    (setq opeteceu--signaturefont nil)
    (setq opeteceu--title-page-graphic nil)
    (setq opeteceu--title-page-graphic-scale nil)
    (setq opeteceu--title-page-graphic-copyright nil)
    (setq opeteceu--title-page-graphic-license nil)
    (setq opeteceu--title-page-replacement-graphic nil)
    (setq opeteceu--title-page-replacement-graphic-scale nil)
    (setq opeteceu--publisher nil)
    (setq opeteceu--isbn nil)
    (setq opeteceu--edition nil)
    (setq opeteceu--license nil)
    (setq opeteceu--sigil-graphic nil)
    (setq opeteceu--sigil-graphic-scale nil)
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
    (when (boundp 'org-export-backends)
      (setq org-export-backends-orig org-export-backends))
    ;; Setup Org export settings for this template.
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
    (opeteceu--set-book-configuration-overrides org-input-file output-file)  ; Override default template values using configuration values from book
    ;; Construct new file to pass to Org export dispatcher, based on input file.
    (when (file-exists-p org-input-file)
      (when (file-readable-p org-input-file)
        (make-directory (file-name-directory output-file) t)
        ;; The next two lines setup the stylesheet file for the document.
        (opeteceu--string-to-file cubes-css (concat (file-name-directory output-file) "stylesheet.css"))
        (setq file-contents (org-file-contents org-input-file))
        ;; Add index entry anchors to story before other processing to ensure title pages, copyright pages etc aren't included in the index.
        (setq file-contents (opeteceu--add-index-anchors file-contents index-entries))  ; Not sure this will work. Is actual variable being passed, or just value?
        ;; At this point, we should have the story full tagged with anchors for the index, as well as have a hash map containing all the index terms, and a way to link to every appearance in the text.
        (setq index-str (opeteceu--make-index-string index-entries))
        ;; Add the constructed index string anywhere it is needed.
        (setq file-contents (opeteceu--add-indices file-contents index-str))
        ;; Make a cover image for the story.
        (opeteceu--generate-cover org-input-file (concat (file-name-directory temp-org) "cover.png"))
        ;; Add md legal page before first Org mode heading.
        (setq file-contents (opeteceu--add-legal-page file-contents))
        ;; Add md export codes to restyle the headings.
        (setq file-contents (opeteceu--restyle-headings file-contents))
        ;; Remap the shy inclusions.
        (setq file-contents (opeteceu--remap-shy-inclusions file-contents))
        ;; Remap the grave accents as opening single quote where appropriate.
        (setq file-contents (opeteceu--remap-grave-accent-as-opening-quote file-contents))
        ;; Remap any LaTeX index anchors.
        (setq file-contents (opeteceu--remap-latex-index-anchors file-contents))
        ;; Remap the internal document links to point to replacement heading labels.
        (setq file-contents (opeteceu--remap-internal-links file-contents))
        (with-temp-buffer
          (insert file-contents)
          (org-mode)
          (opeteceu--fold-show-all)
          (opeteceu--string-to-file (buffer-string) temp-org))))  ; Write new Org file to be fed to exporter
    (when (boundp 'undo-tree-auto-save-history)
      (setq undo-tree-auto-save-history nil))  ; Try to prevent undo-tree making back-ups for autogenerated files
    (find-file temp-org)
    (org-md-export-to-markdown)  ; Use Org mode's built-in Markdown exporter to generate the file to be fed to Pandoc
    (opeteceu--delete-current-file t)
    ;; Remove img name tags for images without a given name.
    (opeteceu--remap-no-name-md-img-tags (concat (file-name-sans-extension output-file) ".md"))
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
    (make-directory (file-name-directory output-file) t)
    ;; Use Pandoc to create the ePub file from the Markdown file.
    (when (executable-find "pandoc")
      (shell-command (concat "pandoc \'" (file-name-sans-extension temp-org) ".md\' -f markdown+smart -t epub --css \'" (file-name-directory output-file)  "stylesheet.css\' --toc -s -o \'" (file-name-sans-extension temp-org) ".epub\'"))
      (when (file-exists-p (concat (file-name-sans-extension temp-org) ".epub"))
        (rename-file (concat (file-name-sans-extension temp-org) ".epub") (concat (file-name-sans-extension output-file) ".epub") t)))
    ;; If available, use Calibre's ebook-convert to generate an azw3 file from the ePub file.
    (when (executable-find "ebook-convert")
      (when (file-readable-p (concat (file-name-sans-extension output-file) ".epub"))
        (shell-command-to-string (concat "ebook-convert \'" (file-name-sans-extension output-file) ".epub\' \'" (file-name-sans-extension output-file) ".azw3\' --no-inline-toc"))))
    (when (file-exists-p (concat (file-name-sans-extension temp-org) ".md"))
      (rename-file (concat (file-name-sans-extension temp-org) ".md") (concat (file-name-sans-extension output-file) ".md") t))
    (find-file (concat (file-name-directory output-file) "stylesheet.css"))
    (opeteceu--delete-current-file t)
    (setq opeteceu--index-entries-exist-in-text-p nil)
    (setq opeteceu--headfont nil)
    (setq opeteceu--signaturefont nil)
    (setq opeteceu--title-page-graphic nil)
    (setq opeteceu--title-page-graphic-scale nil)
    (setq opeteceu--title-page-graphic-copyright nil)
    (setq opeteceu--title-page-graphic-license nil)
    (setq opeteceu--title-page-replacement-graphic nil)
    (setq opeteceu--title-page-replacement-graphic-scale nil)
    (setq opeteceu--publisher nil)
    (setq opeteceu--isbn nil)
    (setq opeteceu--edition nil)
    (setq opeteceu--license nil)
    (setq opeteceu--sigil-graphic nil)
    (setq opeteceu--sigil-graphic-scale nil)
    (when (boundp 'undo-tree-auto-save-history)
      (setq undo-tree-auto-save-history undo-tree-auto-save-history-orig))))

(provide 'org-pandoc-export-to-epub-cubes-en-us)
;;; org-pandoc-export-to-epub-cubes-en-us.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("opeteceu-" . "org-pandoc-export-to-epub-cubes-en-us-"))
;; End:
