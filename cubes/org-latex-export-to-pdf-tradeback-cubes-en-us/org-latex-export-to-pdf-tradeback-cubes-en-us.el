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
;; This package supplies an example export template to PDF format,
;; suitable for US Trade style books.
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
;; This package requires LaTeX to be installed and accessible on the
;; the system (XeLaTeX, to be specific).
;;
;;
;; You should also have the following typefaces installed and
;; accessible on your system if using the default settings:
;;
;; Libre Baskerville
;; (https://fonts.google.com/specimen/Libre+Baskerville)
;;
;; Josefin Sans
;; (https://fonts.google.com/specimen/Josefin+Sans)
;;
;; DejaVu Sans Mono
;; (https://dejavu-fonts.github.io/)
;;
;; Alegreya SC
;; (https://fonts.google.com/specimen/Alegreya+SC)
;;
;;
;; Front matter chapters will not have "Chapter X" in the title, but
;; will have the chapter name (unless told not). They also will not use
;; the main page counts, but rather their own Roman numeral page count.
;; Front matter chapters will appear before the table of contents.
;; Front matter chapters will use the plain pagestyle.
;;
;; Main matter chapters will have "Chapter X" and the chapter name in
;; the title (unless told not). They will use the main page counts.
;; Main matter chapters will appear after the table of contents. Main
;; matter chapters will use the headings pagestyle.
;;
;; Back matter chapters will not have "Chapter X" in the title, but
;; will have the chapter name (unless told not). They will use the main
;; page counts. Back matter chapters will appear after the main matter
;; chapters. Back matter chapters will use the plain pagestyle.
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
;; :empty_pagestyle:
;; Do not display page footers or page headers. In practical terms,
;; this means no page numbers will be dispayed for this chapter, and
;; the chapter name will not be shown at the top of each page.
;;
;; :plain_pagestyle:
;; Display page numbers, centered in the page footer. Do not display
;; any page headers (the chapter name will not be shown at the top of
;; each page). Line gap between paragraphs, but no indent on first
;; line.
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
;; #+TYPEFACE_SIZE:
;; The size of the main text (fraction of normal) for the document.
;; eg: 0.795
;;
;; #+MONOFONT_TYPEFACE_SIZE_ADJUSTMENT:
;; The size of monospaced fonts as a fraction of TYPEFACE_SIZE.
;; eg: 1.000
;;
;; #+TYPEFACE_SIZE_PART:
;; The size (in pt) of the part heading text.
;; eg: 20
;;
;; #+TYPEFACE_SIZE_CHAPTER:
;; The size (in pt) of the chapter heading text.
;; eg: 20
;;
;; #+TYPEFACE_SIZE_SECTION:
;; The size (in pt) of the section heading text.
;; eg: 15
;;
;; #+TYPEFACE_SIZE_SUBSECTION:
;; The size (in pt) of the subsection heading text.
;; eg: 11
;;
;; #+TYPEFACE_SIZE_SUBSUBSECTION:
;; The size (in pt) of the subsubsection heading text.
;; eg: 11
;;
;; #+TYPEFACE_SIZE_PARAGRAPH:
;; The size (in pt) of the paragraph heading text.
;; eg: 11
;;
;; #+TYPEFACE_SIZE_SUBPARAGRAPH:
;; The size (in pt) of the paragraph heading text.
;; eg: 11
;;
;; #+MAINFONT:
;; The typeface to use as the main text font.
;; Must be installed on the system.
;; eg: Libre Baskerville
;;
;; #+HEADFONT:
;; The typeface to use as the headings font.
;; Must be installed on the system.
;; eg: Josefin Sans
;;
;; #+MONOFONT:
;; The typeface to use as the monospaced font.
;; Must be installed on the system.
;; eg: DejaVu Sans Mono
;;
;; #+SIGNATUREFONT:
;; The typeface to use as the signature font.
;; Must be installed on the system.
;; eg: Alegreya SC
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
;; #+TITLE_PAGE_REPLACEMENT_GRAPHIC_OLETPTCEU:
;; Location of image file to replace the generated title page, if
;; needed.
;; eg: ../Images/ReplacementTitlePage.png
;;
;; #+TITLE_PAGE_REPLACEMENT_GRAPHIC_OLETPTCEU_SCALE:
;; The scaled display size (fraction of normal) of
;; TITLE_PAGE_REPLACEMENT_GRAPHIC_OLETPTCEU.
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
;;
;; #+MAKE_BOOKLET:
;; Also output an imposed booklet version of PDF for bookbinding?
;; eg: t
;;
;; #+BOOKLET_SIGNATURE_SIZE:
;; If MAKE_BOOKLET is t, set the number of leaves in booklet signatures
;; for bookbinding.
;; eg: 6
;;
;; #+BOOKLET_BUFFER_PAGES:
;; If MAKE_BOOKLET is t, set the number of blank pages at start and end
;; of booklet for bookbinding.
;; eg: 2

;;; Code:

;;;; Require other packages

(require 'org)  ; Org Novelist is built upon the incredible work of Org mode
(require 'ox)  ; Necessary to call Org's built-in export functions.


;;;; User Variables

(defvar oletptceu--typeface-size-default 0.795 "Typeface size (fraction of normal) for the document text.")
(defvar oletptceu--monofont-typeface-size-adjustment-default 1.000 "Typeface size (fraction of oletptceu--typeface-size) for the document text.")
(defvar oletptceu--typeface-size-part-default 20 "Typeface size (pt) for the part heading text.")
(defvar oletptceu--typeface-size-chapter-default 20 "Typeface size (pt) for the chapter heading text.")
(defvar oletptceu--typeface-size-section-default 15 "Typeface size (pt) for the section heading text.")
(defvar oletptceu--typeface-size-subsection-default 11 "Typeface size (pt) for the subsection heading text.")
(defvar oletptceu--typeface-size-subsubsection-default 11 "Typeface size (pt) for the subsubsection heading text.")
(defvar oletptceu--typeface-size-paragraph-default 11 "Typeface size (pt) for the paragraph heading text.")
(defvar oletptceu--typeface-size-subparagraph-default 11 "Typeface size (pt) for the subparagraph heading text.")
(defvar oletptceu--mainfont-default "Libre Baskerville" "Main text font, must be installed on system already.")
(defvar oletptceu--headfont-default "Josefin Sans" "Heading text font, must be installed on system already.")
(defvar oletptceu--monofont-default "DejaVu Sans Mono" "Mono text font, must be installed on system already.")
(defvar oletptceu--signaturefont-default "Alegreya SC" "Author signature text font, must be installed on system already.")
(defvar oletptceu--title-page-graphic-default "cubes.png" "Location of image file to use in title page.")
(defvar oletptceu--title-page-graphic-scale-default 0.175 "Display scale of the title page image.")
(defvar oletptceu--title-page-graphic-copyright-default "`\\textit{Cube Family}' is copyright \\copyright~2012 Martin Anderson (2012--?)\\\\Made with Blender 3D --- \\url{https://www.blender.org}" "Copyright credit for title page image.")
(defvar oletptceu--title-page-graphic-license-default "Licensed under the Creative Commons Attribution-Non\\-\\\\Commercial-ShareAlike 4.0 International License. To view a copy of this license, visit:\\\\\n\\makebox[\\textwidth]{\\url{https://creativecommons.org/licenses/by-nc-sa/4.0/}}\\\\\nOr, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "Title page image license statement.")
(defvar oletptceu--title-page-replacement-graphic-default nil "Location of image file to use as a replacement for the generated title page.")
(defvar oletptceu--title-page-replacement-graphic-scale-default nil "Display scale of the title page replacement image.")
(defvar oletptceu--publisher-default "" "The publisher of the story.")
(defvar oletptceu--isbn-default "" "ISBN number of book, if there is one.")
(defvar oletptceu--edition-default "Early Draft Edition (not for publication)" "Text describing this edition.")
(defvar oletptceu--license-default "All rights reserved." "License statement.")
(defvar oletptceu--sigil-graphic-default "" "Location of image file to use as sigil in legal page.")
(defvar oletptceu--sigil-graphic-scale-default 0.5 "Display scale of the sigil image.")
(defvar oletptceu--make-booklet-default t "Also output an imposed booklet version of PDF for bookbinding.")
(defvar oletptceu--booklet-signature-size-default 6 "Number of leaves you wish to use in your booklet signatures for bookbinding.")
(defvar oletptceu--booklet-buffer-pages-default 2 "Number of blank pages at start and end of your booklet for bookbinding.")


;;;; Global Variables

(defvar oletptceu--fm-found-p nil "Temporary variable to show at least one front matter chapter found.")
(defvar oletptceu--mm-found-p nil "Temporary variable to show at least one main matter chapter found.")
(defvar oletptceu--bm-found-p nil "Temporary variable to show at least one back matter chapter found.")
(defvar oletptceu--typeface-size nil "Typeface size (fraction of normal) for the document text.")
(defvar oletptceu--monofont-typeface-size-adjustment nil "Typeface size (fraction of oletptceu--typeface-size) for the document text.")
(defvar oletptceu--typeface-size-part nil "Typeface size (pt) for the part heading text.")
(defvar oletptceu--typeface-size-chapter nil "Typeface size (pt) for the chapter heading text.")
(defvar oletptceu--typeface-size-section nil "Typeface size (pt) for the section heading text.")
(defvar oletptceu--typeface-size-subsection nil "Typeface size (pt) for the subsection heading text.")
(defvar oletptceu--typeface-size-subsubsection nil "Typeface size (pt) for the subsubsection heading text.")
(defvar oletptceu--typeface-size-paragraph nil "Typeface size (pt) for the paragraph heading text.")
(defvar oletptceu--typeface-size-subparagraph nil "Typeface size (pt) for the subparagraph heading text.")
(defvar oletptceu--mainfont nil "Main text font, must be installed on system already.")
(defvar oletptceu--headfont nil "Heading text font, must be installed on system already.")
(defvar oletptceu--monofont nil "Mono text font, must be installed on system already.")
(defvar oletptceu--signaturefont nil "Author signature text font, must be installed on system already.")
(defvar oletptceu--title-page-graphic nil "Location of image file to use in title page.")
(defvar oletptceu--title-page-graphic-scale nil "Display scale of the title page image.")
(defvar oletptceu--title-page-graphic-copyright nil "Copyright credit for title page image.")
(defvar oletptceu--title-page-graphic-license nil "Title page image license statement.")
(defvar oletptceu--title-page-replacement-graphic nil "Location of image file to use as a replacement for the generated title page.")
(defvar oletptceu--title-page-replacement-graphic-scale nil "Display scale of the title page replacement image.")
(defvar oletptceu--publisher nil "The publisher of the story.")
(defvar oletptceu--isbn nil "ISBN number of book, if there is one.")
(defvar oletptceu--edition nil "Text describing this edition.")
(defvar oletptceu--license nil "License statement.")
(defvar oletptceu--sigil-graphic nil "Location of image file to use as sigil in legal page.")
(defvar oletptceu--sigil-graphic-scale nil "Display scale of the sigil image.")
(defvar oletptceu--make-booklet nil "Also output an imposed booklet version of PDF for bookbinding.")
(defvar oletptceu--booklet-signature-size nil "Number of leaves you wish to use in your booklet signatures for bookbinding.")
(defvar oletptceu--booklet-buffer-pages nil "Number of blank pages at start and end of your booklet for bookbinding.")
(defvar oletptceu--license-cc0-1.0 "This book, excluding the cover art, is licensed under the Creative Commons CC0 1.0 Universal License. To view a copy of this license, visit:\\\\\n\\makebox[\\textwidth]{\\url{https://creativecommons.org/publicdomain/zero/1.0/}}\\\\\nOr, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC0.")
(defvar oletptceu--license-cc-by-4.0 "This book, excluding the cover art, is licensed under the Creative Commons Attribution 4.0 International License. To view a copy of this license, visit:\\\\\n\\makebox[\\textwidth]{\\url{https://creativecommons.org/licenses/by/4.0/}}\\\\\nOr, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution.")
(defvar oletptceu--license-cc-by-sa-4.0 "This book, excluding the cover art, is licensed under the Creative Commons Attribution-ShareAlike 4.0 International License. To view a copy of this license, visit:\\\\\n\\makebox[\\textwidth]{\\url{https://creativecommons.org/licenses/by-sa/4.0/}}\\\\\nOr, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-ShareAlike.")
(defvar oletptceu--license-cc-by-nd-4.0 "This book, excluding the cover art, is licensed under the Creative Commons Attribution-NoDerivs 4.0 International License. To view a copy of this license, visit:\\\\\n\\makebox[\\textwidth]{\\url{https://creativecommons.org/licenses/by-nd/4.0/}}\\\\\nOr, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-NoDerivs.")
(defvar oletptceu--license-cc-by-nc-4.0 "This book, excluding the cover art, is licensed under the Creative Commons Attribution-NonCommercial 4.0 International License. To view a copy of this license, visit:\\\\\n\\makebox[\\textwidth]{\\url{https://creativecommons.org/licenses/by-nc/4.0/}}\\\\\nOr, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-NonCommercial.")
(defvar oletptceu--license-cc-by-nc-sa-4.0 "This book, excluding the cover art, is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. To view a copy of this license, visit:\\\\\n\\makebox[\\textwidth]{\\url{https://creativecommons.org/licenses/by-nc-sa/4.0/}}\\\\\nOr, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-NonCommercial-ShareAlike.")
(defvar oletptceu--license-cc-by-nc-nd-4.0 "This book, excluding the cover art, is licensed under the Creative Commons Attribution-NonCommercial-NoDerivs 4.0 International License. To view a copy of this license, visit:\\\\\n\\makebox[\\textwidth]{\\url{https://creativecommons.org/licenses/by-nc-nd/4.0/}}\\\\\nOr, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-NonCommercial-NoDerivs.")
(defvar oletptceu--title-page-graphic-license-cc0-1.0 "Licensed under the Creative Commons CC0 1.0 Universal License. To view a copy of this license, visit:\\\\\n\\makebox[\\textwidth]{\\url{https://creativecommons.org/publicdomain/zero/1.0/}}\\\\\nOr, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC0.")
(defvar oletptceu--title-page-graphic-license-cc-by-4.0 "Licensed under the Creative Commons Attribution 4.0 International License. To view a copy of this license, visit:\\\\\n\\makebox[\\textwidth]{\\url{https://creativecommons.org/licenses/by/4.0/}}\\\\\nOr, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution.")
(defvar oletptceu--title-page-graphic-license-cc-by-sa-4.0 "Licensed under the Creative Commons Attribution-ShareAlike 4.0 International License. To view a copy of this license, visit:\\\\\n\\makebox[\\textwidth]{\\url{https://creativecommons.org/licenses/by-sa/4.0/}}\\\\\nOr, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-ShareAlike.")
(defvar oletptceu--title-page-graphic-license-cc-by-nd-4.0 "Licensed under the Creative Commons Attribution-NoDerivs 4.0 International License. To view a copy of this license, visit:\\\\\n\\makebox[\\textwidth]{\\url{https://creativecommons.org/licenses/by-nd/4.0/}}\\\\\nOr, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-NoDerivs.")
(defvar oletptceu--title-page-graphic-license-cc-by-nc-4.0 "Licensed under the Creative Commons Attribution-Non\\-\\\\Commercial 4.0 International License. To view a copy of this license, visit:\\\\\n\\makebox[\\textwidth]{\\url{https://creativecommons.org/licenses/by-nc/4.0/}}\\\\\nOr, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-NonCommercial.")
(defvar oletptceu--title-page-graphic-license-cc-by-nc-sa-4.0 "Licensed under the Creative Commons Attribution-Non\\-\\\\Commercial-ShareAlike 4.0 International License. To view a copy of this license, visit:\\\\\n\\makebox[\\textwidth]{\\url{https://creativecommons.org/licenses/by-nc-sa/4.0/}}\\\\\nOr, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-NonCommercial-ShareAlike.")
(defvar oletptceu--title-page-graphic-license-cc-by-nc-nd-4.0 "Licensed under the Creative Commons Attribution-Non\\-\\\\Commercial-NoDerivs 4.0 International License. To view a copy of this license, visit:\\\\\n\\makebox[\\textwidth]{\\url{https://creativecommons.org/licenses/by-nc-nd/4.0/}}\\\\\nOr, send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA." "The license instructions for CC Attribution-NonCommercial-NoDerivs.")
(defvar oletptceu--part-format nil "LaTeX format string for part headings.")
(defvar oletptceu--chap-format nil "LaTeX format string for chapter headings.")
(defvar oletptceu--sec-format nil "LaTeX format string for section headings.")
(defvar oletptceu--subsec-format nil "LaTeX format string for sub-section headings.")
(defvar oletptceu--subsubsec-format nil "LaTeX format string for sub-sub-section headings.")
(defvar oletptceu--paragraph-format nil "LaTeX format string for paragraph headings.")
(defvar oletptceu--subparagraph-format nil "LaTeX format string for sub-paragraph headings.")


;;;; Global Constants

(defconst oletptceu--glossary-heading "Glossary" "The expected string to represent the heading of a Glossary.")


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

(defun oletptceu--delete-line ()
  "If Emacs version is less than 29, delete line the old fashioned way."
  (let ((inhibit-field-text-motion t))
    (if (>= (string-to-number (nth 0 (split-string (string-trim-left (emacs-version) "GNU Emacs ") "\\."))) 29)
        (delete-line)
      (delete-region (line-beginning-position) (line-beginning-position 2)))))

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

(defun oletptceu--get-file-property-value (property &optional file)
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

(defun oletptceu--get-file-properties-and-values (&optional file)
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

(defun oletptceu--set-book-configuration-overrides (org-input-file output-file)
  "Override default template values using configuration values from ORG-INPUT-FILE.
Any relative file names will be relative to OUTPUT-FILE."
  (let (prop-val)
    (setq prop-val (oletptceu--get-file-property-value "TYPEFACE_SIZE" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--typeface-size oletptceu--typeface-size-default)
      (setq oletptceu--typeface-size (string-to-number prop-val)))
    (setq prop-val (oletptceu--get-file-property-value "MONOFONT_TYPEFACE_SIZE_ADJUSTMENT" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--monofont-typeface-size-adjustment oletptceu--monofont-typeface-size-adjustment-default)
      (setq oletptceu--monofont-typeface-size-adjustment (string-to-number prop-val)))
    (setq prop-val (oletptceu--get-file-property-value "TYPEFACE_SIZE_PART" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--typeface-size-part oletptceu--typeface-size-part-default)
      (setq oletptceu--typeface-size-part (string-to-number prop-val)))
    (setq prop-val (oletptceu--get-file-property-value "TYPEFACE_SIZE_CHAPTER" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--typeface-size-chapter oletptceu--typeface-size-chapter-default)
      (setq oletptceu--typeface-size-chapter (string-to-number prop-val)))
    (setq prop-val (oletptceu--get-file-property-value "TYPEFACE_SIZE_SECTION" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--typeface-size-section oletptceu--typeface-size-section-default)
      (setq oletptceu--typeface-size-section (string-to-number prop-val)))
    (setq prop-val (oletptceu--get-file-property-value "TYPEFACE_SIZE_SUBSECTION" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--typeface-size-subsection oletptceu--typeface-size-subsection-default)
      (setq oletptceu--typeface-size-subsection (string-to-number prop-val)))
    (setq prop-val (oletptceu--get-file-property-value "TYPEFACE_SIZE_SUBSUBSECTION" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--typeface-size-subsubsection oletptceu--typeface-size-subsubsection-default)
      (setq oletptceu--typeface-size-subsubsection (string-to-number prop-val)))
    (setq prop-val (oletptceu--get-file-property-value "TYPEFACE_SIZE_PARAGRAPH" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--typeface-size-paragraph oletptceu--typeface-size-paragraph-default)
      (setq oletptceu--typeface-size-paragraph (string-to-number prop-val)))
    (setq prop-val (oletptceu--get-file-property-value "TYPEFACE_SIZE_SUBPARAGRAPH" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--typeface-size-subparagraph oletptceu--typeface-size-subparagraph-default)
      (setq oletptceu--typeface-size-subparagraph (string-to-number prop-val)))
    (setq prop-val (oletptceu--get-file-property-value "MAINFONT" org-input-file))
    (if (string= "" prop-val)
        (if (find-font (font-spec :name oletptceu--mainfont-default))
            (setq oletptceu--mainfont oletptceu--mainfont-default)
          (setq oletptceu--mainfont "cmr"))
      (if (find-font (font-spec :name prop-val))
          (setq oletptceu--mainfont prop-val)
        (if (find-font (font-spec :name oletptceu--mainfont-default))
            (setq oletptceu--mainfont oletptceu--mainfont-default)
          (setq oletptceu--mainfont "cmr"))))
    (setq prop-val (oletptceu--get-file-property-value "HEADFONT" org-input-file))
    (if (string= "" prop-val)
        (if (find-font (font-spec :name oletptceu--headfont-default))
            (setq oletptceu--headfont oletptceu--headfont-default)
          (setq oletptceu--headfont "cmss"))
      (if (find-font (font-spec :name prop-val))
          (setq oletptceu--headfont prop-val)
        (if (find-font (font-spec :name oletptceu--headfont-default))
            (setq oletptceu--headfont oletptceu--headfont-default)
          (setq oletptceu--headfont "cmss"))))
    (setq prop-val (oletptceu--get-file-property-value "MONOFONT" org-input-file))
    (if (string= "" prop-val)
        (if (find-font (font-spec :name oletptceu--monofont-default))
            (setq oletptceu--monofont oletptceu--monofont-default)
          (setq oletptceu--monofont "cmtt"))
      (if (find-font (font-spec :name prop-val))
          (setq oletptceu--monofont prop-val)
        (if (find-font (font-spec :name oletptceu--monofont-default))
            (setq oletptceu--monofont oletptceu--monofont-default)
          (setq oletptceu--monofont "cmtt"))))
    (setq prop-val (oletptceu--get-file-property-value "SIGNATUREFONT" org-input-file))
    (if (string= "" prop-val)
        (if (find-font (font-spec :name oletptceu--signaturefont-default))
            (setq oletptceu--signaturefont oletptceu--signaturefont-default)
          (setq oletptceu--signaturefont "cmss"))
      (if (find-font (font-spec :name prop-val))
          (setq oletptceu--signaturefont prop-val)
        (if (find-font (font-spec :name oletptceu--signaturefont-default))
            (setq oletptceu--signaturefont oletptceu--signaturefont-default)
          (setq oletptceu--signaturefont "cmss"))))
    (setq prop-val (oletptceu--get-file-property-value "TITLE_PAGE_GRAPHIC" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--title-page-graphic (expand-file-name
                                             oletptceu--title-page-graphic-default
                                             (expand-file-name
                                              (file-name-directory
                                               (symbol-file 'org-latex-export-to-pdf-tradeback-cubes-en-us--fold-show-all)))))
      (setq oletptceu--title-page-graphic (expand-file-name prop-val (expand-file-name (file-name-directory output-file)))))
    (setq prop-val (oletptceu--get-file-property-value "TITLE_PAGE_GRAPHIC_SCALE" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--title-page-graphic-scale oletptceu--title-page-graphic-scale-default)
      (setq oletptceu--title-page-graphic-scale (string-to-number prop-val)))
    (setq prop-val (oletptceu--get-file-property-value "TITLE_PAGE_GRAPHIC_COPYRIGHT" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--title-page-graphic-copyright oletptceu--title-page-graphic-copyright-default)
      (setq oletptceu--title-page-graphic-copyright (org-export-string-as prop-val 'latex t)))
    (setq prop-val (oletptceu--get-file-property-value "TITLE_PAGE_GRAPHIC_LICENSE" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--title-page-graphic-license oletptceu--title-page-graphic-license-default)
      (cond
       ((string= "CC0-1.0" prop-val)
        (setq oletptceu--title-page-graphic-license oletptceu--title-page-graphic-license-cc0-1.0))
       ((string= "CC-BY-4.0" prop-val)
        (setq oletptceu--title-page-graphic-license oletptceu--title-page-graphic-license-cc-by-4.0))
       ((string= "CC-BY-SA-4.0" prop-val)
        (setq oletptceu--title-page-graphic-license oletptceu--title-page-graphic-license-cc-by-sa-4.0))
       ((string= "CC-BY-ND-4.0" prop-val)
        (setq oletptceu--title-page-graphic-license oletptceu--title-page-graphic-license-cc-by-nd-4.0))
       ((string= "CC-BY-NC-4.0" prop-val)
        (setq oletptceu--title-page-graphic-license oletptceu--title-page-graphic-license-cc-by-nc-4.0))
       ((string= "CC-BY-NC-SA-4.0" prop-val)
        (setq oletptceu--title-page-graphic-license oletptceu--title-page-graphic-license-cc-by-nc-sa-4.0))
       ((string= "CC-BY-NC-ND-4.0" prop-val)
        (setq oletptceu--title-page-graphic-license oletptceu--title-page-graphic-license-cc-by-nc-nd-4.0))
       (t
        (setq oletptceu--title-page-graphic-license (org-export-string-as prop-val 'latex t)))))
    (setq prop-val (oletptceu--get-file-property-value "TITLE_PAGE_REPLACEMENT_GRAPHIC_OLETPTCEU" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--title-page-replacement-graphic oletptceu--title-page-replacement-graphic-default)
      (setq oletptceu--title-page-replacement-graphic (expand-file-name prop-val (expand-file-name (file-name-directory output-file)))))
    (setq prop-val (oletptceu--get-file-property-value "TITLE_PAGE_REPLACEMENT_GRAPHIC_OLETPTCEU_SCALE" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--title-page-replacement-graphic-scale oletptceu--title-page-replacement-graphic-scale-default)
      (setq oletptceu--title-page-replacement-graphic-scale (string-to-number prop-val)))
    (setq prop-val (oletptceu--get-file-property-value "PUBLISHER" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--publisher oletptceu--publisher-default)
      (setq oletptceu--publisher (org-export-string-as prop-val 'latex t)))
    (setq prop-val (oletptceu--get-file-property-value "ISBN" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--isbn oletptceu--isbn-default)
      (setq oletptceu--isbn prop-val))
    (setq prop-val (oletptceu--get-file-property-value "EDITION" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--edition oletptceu--edition-default)
      (setq oletptceu--edition (org-export-string-as prop-val 'latex t)))
    (setq prop-val (oletptceu--get-file-property-value "LICENSE" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--license oletptceu--license-default)
      (cond
       ((string= "CC0-1.0" prop-val)
        (setq oletptceu--license oletptceu--license-cc0-1.0))
       ((string= "CC-BY-4.0" prop-val)
        (setq oletptceu--license oletptceu--license-cc-by-4.0))
       ((string= "CC-BY-SA-4.0" prop-val)
        (setq oletptceu--license oletptceu--license-cc-by-sa-4.0))
       ((string= "CC-BY-ND-4.0" prop-val)
        (setq oletptceu--license oletptceu--license-cc-by-nd-4.0))
       ((string= "CC-BY-NC-4.0" prop-val)
        (setq oletptceu--license oletptceu--license-cc-by-nc-4.0))
       ((string= "CC-BY-NC-SA-4.0" prop-val)
        (setq oletptceu--license oletptceu--license-cc-by-nc-sa-4.0))
       ((string= "CC-BY-NC-ND-4.0" prop-val)
        (setq oletptceu--license oletptceu--license-cc-by-nc-nd-4.0))
       (t
        (setq oletptceu--license (org-export-string-as prop-val 'latex t)))))
    (setq prop-val (oletptceu--get-file-property-value "SIGIL_GRAPHIC" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--sigil-graphic oletptceu--sigil-graphic-default)
      (setq oletptceu--sigil-graphic (expand-file-name prop-val (expand-file-name (file-name-directory output-file)))))
    (setq prop-val (oletptceu--get-file-property-value "SIGIL_GRAPHIC_SCALE" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--sigil-graphic-scale oletptceu--sigil-graphic-scale-default)
      (setq oletptceu--sigil-graphic-scale (string-to-number prop-val)))
    (setq prop-val (oletptceu--get-file-property-value "MAKE_BOOKLET" org-input-file))
    (cond ((string= "t" prop-val)
           (setq oletptceu--make-booklet t))
          ((string= "nil" prop-val)
           (setq oletptceu--make-booklet nil))
          (t
           (setq oletptceu--make-booklet oletptceu--make-booklet-default)))
    (setq prop-val (oletptceu--get-file-property-value "BOOKLET_SIGNATURE_SIZE" org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--booklet-signature-size oletptceu--booklet-signature-size-default)
      (setq oletptceu--booklet-signature-size (string-to-number prop-val)))
    (setq prop-val (oletptceu--get-file-property-value "BOOKLET_BUFFER_PAGE"org-input-file))
    (if (string= "" prop-val)
        (setq oletptceu--booklet-buffer-pages oletptceu--booklet-buffer-pages-default)
      (setq oletptceu--booklet-buffer-pages (string-to-number prop-val)))))

(defun oletptceu--add-indices (file-contents)
  "Given a string FILE-CONTENTS, add required indices and return new string.
Return string of new file contents."
  (let (curr-properties-list
        curr-property
        (out-str ""))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (oletptceu--fold-show-all)
      ;; Add index flags to the story. This should be done before any other processing to ensure we won't include things like title pages and copyright pages in the index.
      (goto-char (point-min))
      (insert "Temp Line\n")
      (goto-char (point-min))
      (setq curr-properties-list (oletptceu--get-file-properties-and-values))
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
              (unless (string= (downcase (nth 4 (org-heading-components))) (downcase oletptceu--glossary-heading))
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
                  (unless (or (looking-at-p "@@latex:\\\\index{") (looking-at-p "}?@@"))
                    (goto-char pos)
                    ;; Don't match Document or Section properties.
                    (unless (or (looking-at-p "^[ \t]*#\\+") (looking-at-p "^[ \t]*:+?[^\s]+?:+?"))
                      (goto-char pos)
                      ;; The next few lines deal with a minor issue with Org markup not liking an embeded LaTeX code right after certain markup symbols (causing Org to ignore those symbols).
                      (forward-char -1)
                      (if (or (looking-at-p "*") (looking-at-p"/") (looking-at-p "_") (looking-at-p "=") (looking-at-p "~") (looking-at-p "+"))
                          (forward-char 2)
                        (forward-char))
                      (setq curr-term-insert-str (concat "@@latex:\\index{" (cdr curr-property) "}@@"))
                      (insert curr-term-insert-str)
                      ;; Increase search-bound-pos by the number of characters we've added.
                      (setq search-bound-pos (+ search-bound-pos (length curr-term-insert-str))))))))))
        (goto-char (point-min)))
      ;; Only print an Index at the end of the story if user requested it.
      (when (member "index" (split-string (oletptceu--get-file-property-value "GENERATE") "[,\f\t\n\r\v]+" t " "))
        (goto-char (point-max))
        (insert "\n#+LATEX: \\printindex\n"))
      (goto-char (point-min))
      (oletptceu--delete-line)
      (setq out-str (buffer-string)))
    out-str))

(defun oletptceu--set-latex-configuration (file-contents)
  "Given a string FILE-CONTENTS, add required Org mode overrides for LaTeX export.
Return string of new file contents."
  (setq oletptceu--part-format (concat "\\titleformat{\\part}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-part) "}{" (number-to-string oletptceu--typeface-size-part) "}\\selectfont\\partname\\,\\thepart}{0pt}{~\\linebreak\\fontsize{" (number-to-string oletptceu--typeface-size-part) "}{" (number-to-string oletptceu--typeface-size-part) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]"))
  (setq oletptceu--chap-format (concat "\\titleformat{\\chapter}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-chapter) "}{" (number-to-string oletptceu--typeface-size-chapter) "}\\selectfont\\chaptername\\,\\thechapter}{0pt}{~\\linebreak\\fontsize{" (number-to-string oletptceu--typeface-size-chapter) "}{" (number-to-string oletptceu--typeface-size-chapter) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]"))
  (setq oletptceu--sec-format (concat "\\titleformat{\\section}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-section) "}{" (number-to-string oletptceu--typeface-size-section) "}\\selectfont\\thesection}{0pt}{\\,\\,\\,~\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-section) "}{" (number-to-string oletptceu--typeface-size-section) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]"))
  (setq oletptceu--subsec-format (concat "\\titleformat{\\subsection}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subsection) "}{" (number-to-string oletptceu--typeface-size-subsection) "}\\selectfont\\thesubsection}{0pt}{\\,\\,\\,~\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-subsection) "}{" (number-to-string oletptceu--typeface-size-subsection) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]"))
  (setq oletptceu--subsubsec-format (concat "\\titleformat{\\subsubsection}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subsubsection) "}{" (number-to-string oletptceu--typeface-size-subsubsection) "}\\selectfont\\thesubsubsection}{0pt}{\\,\\,\\,~\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-subsubsection) "}{" (number-to-string oletptceu--typeface-size-subsubsection) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]"))
  (setq oletptceu--paragraph-format (concat "\\titleformat{\\paragraph}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-paragraph) "}{" (number-to-string oletptceu--typeface-size-paragraph) "}\\selectfont\\theparagraph}{0pt}{\\,\\,\\,~\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-paragraph) "}{" (number-to-string oletptceu--typeface-size-paragraph) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]"))
  (setq oletptceu--subparagraph-format (concat "\\titleformat{\\subparagraph}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-subparagraph) "}{" (number-to-string oletptceu--typeface-size-subparagraph) "}\\selectfont\\thesubparagraph}{0pt}{\\,\\,\\,~\\,\\,\\,\\fontsize{" (number-to-string oletptceu--typeface-size-subparagraph) "}{" (number-to-string oletptceu--typeface-size-subparagraph) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]"))
  (let ((out-str ""))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (oletptceu--fold-show-all)
      (oletptceu--set-file-property-value "LATEX_COMPILER" "xelatex")
      (oletptceu--set-file-property-value "LATEX_CLASS" "book")
      (oletptceu--set-file-property-value "LATEX_CLASS_OPTIONS" "[11pt,twoside,a5paper,titlepage,openright]")
      (oletptceu--set-file-property-value "LATEX_HEADER" "\\usepackage{makeidx}" nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" "\\makeindex" nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" "\\usepackage[utf8]{inputenc}" nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" "\\usepackage{graphicx}" nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" "\\usepackage{longtable}" nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" "\\usepackage{wrapfig}" nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" "\\usepackage{rotating}" nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" "\\usepackage[normalem]{ulem}" nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" "\\usepackage{amsmath}" nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" "\\usepackage{amssymb}" nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" "\\usepackage{capt-of}" nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" "\\usepackage{hyperref}" nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" "\\docParindent" nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" "\\docParskip" nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" oletptceu--subparagraph-format nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" oletptceu--paragraph-format nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" oletptceu--subsubsec-format nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" oletptceu--subsec-format nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" oletptceu--sec-format nil t)
      (oletptceu--set-file-property-value "LATEX_HEADER" oletptceu--chap-format nil t)
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
      (when (find-font (font-spec :name oletptceu--headfont))
        (oletptceu--set-file-property-value "LATEX_HEADER" (concat "\\setsansfont[Scale=MatchLowercase]{" oletptceu--headfont "}") nil t))
      (when (find-font (font-spec :name oletptceu--mainfont))
        (oletptceu--set-file-property-value "LATEX_HEADER" (concat "\\setmainfont[Scale=" (number-to-string oletptceu--typeface-size) "]{" oletptceu--mainfont "}") nil t))
      (oletptceu--set-file-property-value "LATEX_HEADER" "\\usepackage{fontspec}" nil t)
      (setq out-str (buffer-string)))
    out-str))

(defun oletptceu--add-title-page (file-contents)
  "Given a string FILE-CONTENTS, add Org mode LaTeX export code for a title page.
Return string of new file contents."
  (let ((out-str ""))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (oletptceu--fold-show-all)
      (goto-char (point-min))
      (insert "Temp Line\n")
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
        ;; If replacement graphic was supplied by story config, put it here instead of generating one.
        (if (and oletptceu--title-page-replacement-graphic (file-readable-p oletptceu--title-page-replacement-graphic) (not (string= "" oletptceu--title-page-replacement-graphic)))
            (if oletptceu--title-page-replacement-graphic-scale
                (insert "\\includegraphics[scale=" (number-to-string oletptceu--title-page-replacement-graphic-scale) "]{" oletptceu--title-page-replacement-graphic  "}\n")
              (insert "\\includegraphics[width=0.999\\textwidth,height=0.999\\textheight,keepaspectratio]{" oletptceu--title-page-replacement-graphic  "}\n"))
          (progn
            (if (and (file-readable-p oletptceu--title-page-graphic) (not (string= "" oletptceu--title-page-graphic)))
                (insert "\\includegraphics[scale=" (number-to-string oletptceu--title-page-graphic-scale) "]{" oletptceu--title-page-graphic "}\\\\[3cm]\n")
              (insert "~\\\\[8.5cm]\n"
                      "\n"))
            (insert "\\noindent\\rule{\\textwidth}{0.5pt} \\\\[0.5cm]\n"
                    "\\textsf{ \\huge \\bfseries " (oletptceu--get-file-property-value "TITLE") "}\\\\[0.2cm]\n")
            (if (> (length (oletptceu--get-file-property-value "SUBTITLE")) 0)
                (insert "\\noindent\\rule{\\textwidth}{0.5pt} \\\\[0.5cm]\n"
                        "\\textsf{ \\large \\itshape " (oletptceu--get-file-property-value "SUBTITLE") "}\\\\[3.5cm]\n")
              (insert "\\noindent\\rule{\\textwidth}{0.5pt} \\\\[4.0cm]\n"))
            (if (find-font (font-spec :name oletptceu--signaturefont))
                (insert "{\\signaturefont {\\Large " (oletptceu--get-file-property-value "AUTHOR") "}}\\\\[0.25cm]\n")
              (insert "\\textsc {\\Large " (oletptceu--get-file-property-value "AUTHOR") "}\\\\[0.25cm]\n"))
            (insert "\\vfill\n")))
        (insert "\\end{center}\n"
                "\\end{titlepage}\n"
                "#+END_EXPORT\n"))
      (goto-char (point-min))
      (oletptceu--delete-line)
      (setq out-str (buffer-string)))
    out-str))

(defun oletptceu--add-legal-page (file-contents)
  "Given a string FILE-CONTENTS, add Org mode LaTeX export code for a title page.
Return string of new file contents."
  (let ((out-str ""))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (oletptceu--fold-show-all)
      (goto-char (point-min))
      (insert "Temp Line\n")
      (goto-char (point-min))
      (when (org-goto-first-child)
        (beginning-of-line)
        (insert "\n")
        (forward-line -1)
        (insert "#+BEGIN_EXPORT latex\n"
                "\\thispagestyle{empty}\n"
                "\\legalParindent\n"
                "\\legalParskip\n"
                "\\textbf{" (oletptceu--get-file-property-value "TITLE") "}")
        (unless (string= (oletptceu--get-file-property-value "SUBTITLE") "")
          (insert " --- " (oletptceu--get-file-property-value "SUBTITLE")))
        (insert "\n"
                "\n")
        (if (find-font (font-spec :name oletptceu--signaturefont))
            (insert "Author: {\\signaturefont " (oletptceu--get-file-property-value "AUTHOR") "}\n"
                    "\n")
          (insert "Author: \\textsc{" (oletptceu--get-file-property-value "AUTHOR") "}\n"
                  "\n"))
        (insert "Cover: ")
        (insert oletptceu--title-page-graphic-copyright)
        (insert "\\\\\nCover: ")
        (insert oletptceu--title-page-graphic-license)
        (insert "\n\n"
                "\n"
                "This book, excluding the cover art, is copyright \\copyright~"
                (oletptceu--format-time-string
                 "%Y"
                 (org-time-from-absolute
                  (org-time-string-to-absolute
                   (oletptceu--get-file-property-value "DATE")))) " "
                (oletptceu--get-file-property-value "AUTHOR") ".\n"
                "\n")
        (insert oletptceu--license "\n\n")
        (insert "\n"
                "The author assumes no liability for errors or omissions in this book, or for damages or loss of revenue resulting from the use of the information contained herein. The characters and incidents portrayed in this book are fictional. Any similarities to real persons, living, dead or yet to exist, is entirely coincidental.\n"
                "\n")
        (unless (string= (oletptceu--get-file-property-value "EMAIL") "")
          (insert "You can contact the author via e-mail:\\\\\n"
                  "\\texttt{\\href{mailto:" (oletptceu--get-file-property-value "EMAIL")
                  "}{" (oletptceu--get-file-property-value "EMAIL") "}}\n"
                  "\n"
                  "\n"))
        (unless (string= oletptceu--publisher "")
          (insert "Published by " oletptceu--publisher "\n"
                  "\n"))
        (unless (string= oletptceu--isbn "")
          (insert "ISBN " oletptceu--isbn "\n"
                  "\n"))
        (insert oletptceu--edition ": "
                (oletptceu--format-time-string
                 "%B %Y"
                 (org-time-from-absolute
                  (org-time-string-to-absolute
                   (oletptceu--get-file-property-value "DATE")))) "\n"
                "\n")
        (when (and (file-readable-p oletptceu--sigil-graphic) (not (string= "" oletptceu--sigil-graphic)))
          (insert "\\vspace{0.5cm}\n"
                  "\n"
                  "\\begin{center}\n"
                  "\\includegraphics[scale=" (number-to-string oletptceu--sigil-graphic-scale) "]{" oletptceu--sigil-graphic "}\n"
                  "\n"
                  "\\end{center}\n"
                  "\n"))
        (insert "\\docParindent\n"
                "\\docParskip\n"
                "#+END_EXPORT\n"))
      (goto-char (point-min))
      (oletptceu--delete-line)
      (setq out-str (buffer-string)))
    out-str))

(defun oletptceu--make-booklet (output-file)
  "Make an imposed booklet from OUTPUT-FILE for bookbinding, if required.
Booklet will be in the same directory as OUTPUT-FILE.
This function will not do anything unless
`org-latex-export-to-pdf-tradeback-cubes-en-us--make-booklet' is set to true.
This function will not do anything if xelatex is not in the system path."
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
      (insert ",nup=1x2,landscape,signature=" (number-to-string (* 4 oletptceu--booklet-signature-size)) "]{./" (file-name-base output-file) ".pdf}\n"
              "\\end{document}")
      (oletptceu--string-to-file (buffer-string) (concat (file-name-sans-extension output-file) "Booklet.tex")))
    (when (file-readable-p (concat (file-name-sans-extension output-file) "Booklet.tex"))
      (let ((default-directory (file-name-directory output-file)))
        (shell-command-to-string (concat "xelatex \"" (file-name-sans-extension output-file) "Booklet.tex\""))))
    (delete-file (concat (file-name-sans-extension output-file) "Booklet.aux"))
    (delete-file (concat (file-name-sans-extension output-file) "Booklet.log"))))

(defun oletptceu--restyle-headings (file-contents)
  "Given a string FILE-CONTENTS, add Org mode LaTeX exports to restyle headings.
Return string of new file contents."
  (let ((out-str "")
        curr-heading
        curr-level
        curr-matter
        beg
        curr-cust-id
        (toc-head-string "")
        (no-header nil)
        (no-header-name nil)
        (no-header-preamble nil)
        (empty-pagestyle nil)
        (no-toc-entry nil)
        (part nil)
        (pagestyle "headings"))
    (setq oletptceu--fm-found-p nil)
    (setq oletptceu--mm-found-p nil)
    (setq oletptceu--bm-found-p nil)
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (oletptceu--fold-show-all)
      (goto-char (point-min))
      (insert "Temp Line\n")
      (goto-char (point-min))
      (while (not (org-next-visible-heading 1))
        ;; If tags "no_header" or "empty_pagestyle" etc were used in Chapter Index headings, then act appropriately with formatting.
        (when (nth 5 (org-heading-components))
          (when (member "no_header" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
            (setq no-header t))
          (when (member "no_header_name" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
            (setq no-header-name t))
          (when (member "no_header_preamble" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
            (setq no-header-preamble t))
          (when (member "no_toc_entry" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
            (setq no-toc-entry t))
          (when (member "empty_pagestyle" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
            (setq empty-pagestyle t))
          (when (member "plain_pagestyle" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
            (setq pagestyle "plain"))
          (when (member "part" (split-string (downcase (nth 5 (org-heading-components))) ":" t ":"))
            (setq part t)))
        (setq curr-cust-id (org-entry-get (point) "CUSTOM_ID"))
        (unless curr-cust-id
          (when (string= "" curr-cust-id)
            (setq curr-cust-id nil)))
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
               (unless oletptceu--fm-found-p
                 (insert "\\frontmatter{}\n")
                 (setq oletptceu--fm-found-p t))
               (when (string= curr-heading "")
                 (setq no-header-name t))
               (when (and no-header-name no-header-preamble)
                 (setq no-header t))
               (if (or no-header no-header-name)
                   (cond (part
                          (insert "\\titleformat{\\part}[runin]{}{}{0pt}{}\n")
                          (insert "\\part*{}\n"))
                         ((= 1 curr-level)
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
                   (cond (part
                          (insert oletptceu--part-format "\n")
                          (insert "\\part*{" curr-heading "}\n")
                          (unless no-toc-entry
                            (insert "\\addcontentsline{toc}{part}{" curr-heading "}\n")))
                         ((= 1 curr-level)
                          (insert oletptceu--chap-format "\n")
                          (insert "\\chapter*{" curr-heading "}\n")
                          (unless no-toc-entry
                            (insert "\\addcontentsline{toc}{chapter}{" curr-heading "}\n")))
                         ((= 2 curr-level)
                          (insert oletptceu--sec-format "\n")
                          (insert "\\section*{" curr-heading "}\n")
                          (unless no-toc-entry
                            (insert "\\addcontentsline{toc}{section}{" curr-heading "}\n")))
                         ((= 3 curr-level)
                          (insert oletptceu--subsec-format "\n")
                          (insert "\\subsection*{" curr-heading "}\n")
                          (unless no-toc-entry
                            (insert "\\addcontentsline{toc}{subsection}{" curr-heading "}\n")))
                         ((= 4 curr-level)
                          (insert oletptceu--subsubsec-format "\n")
                          (insert "\\subsubsection*{" curr-heading "}\n")
                          (unless no-toc-entry
                            (insert "\\addcontentsline{toc}{subsubsection}{" curr-heading "}\n")))
                         ((= 5 curr-level)
                          (insert oletptceu--paragraph-format "\n")
                          (insert "\\paragraph*{" curr-heading "}\n")
                          (unless no-toc-entry
                            (insert "\\addcontentsline{toc}{paragraph}{" curr-heading "}\n")))
                         ((= 6 curr-level)
                          (insert oletptceu--subparagraph-format "\n")
                          (insert "\\subparagraph*{" curr-heading "}\n")
                          (unless no-toc-entry
                            (insert "\\addcontentsline{toc}{subparagraph}{" curr-heading "}\n")))
                         (t
                          (insert "\\subparagraph*{" curr-heading "}\n")))
                   (unless (or (string= curr-heading "") (string= curr-heading "Glossary") (string= curr-heading "Index"))
                     (insert "\\label{" curr-heading "}\n"))
                   (when curr-cust-id
                     (insert "\\label{" curr-cust-id "}\n"))))
               (if empty-pagestyle
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
               (unless oletptceu--mm-found-p
                 (insert "\\tocParindent\n"
                         "\\tocParskip\n"
                         "\\setcounter{tocdepth}{4}\n"
                         "\\tableofcontents\n"
                         "\\docParindent\n"
                         "\\docParskip\n"
                         "\\newpage\n"
                         "\\thispagestyle{empty}\n"
                         "\\mainmatter{}\n")
                 (setq oletptceu--mm-found-p t))
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
                      (cond (part
                             (insert "\\titleformat{\\part}[runin]{}{}{0pt}{}\n")
                             (insert "\\part" toc-head-string "{}\n"))
                            ((= 1 curr-level)
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
                      (cond (part
                             (insert "\\titleformat{\\part}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-part) "}{" (number-to-string oletptceu--typeface-size-part) "}\\selectfont}{0pt}{~\\linebreak\\fontsize{" (number-to-string oletptceu--typeface-size-part) "}{" (number-to-string oletptceu--typeface-size-part) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]")
                             (insert "\\part" toc-head-string "{" curr-heading "}\n"))
                            ((= 1 curr-level)
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
                      (unless (or (string= curr-heading "") (string= curr-heading "Glossary") (string= curr-heading "Index"))
                        (insert "\\label{" curr-heading "}\n"))
                      (when curr-cust-id
                        (insert "\\label{" curr-cust-id "}\n")))
                     (no-header-name
                      (cond (part
                             (insert "\\titleformat{\\part}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-part) "}{" (number-to-string oletptceu--typeface-size-part) "}\\selectfont\\partname\\,\\thepart}{0pt}{\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]\n")
                             (insert "\\part" toc-head-string "{}\n"))
                            ((= 1 curr-level)
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
                      (cond (part
                             (insert oletptceu--part-format "\n")
                             (insert "\\part" toc-head-string "{" curr-heading "}\n"))
                            ((= 1 curr-level)
                             (insert oletptceu--chap-format "\n")
                             (insert "\\chapter" toc-head-string "{" curr-heading "}\n"))
                            ((= 2 curr-level)
                             (insert oletptceu--sec-format "\n")
                             (insert "\\section" toc-head-string "{" curr-heading "}\n"))
                            ((= 3 curr-level)
                             (insert oletptceu--subsec-format "\n")
                             (insert "\\subsection" toc-head-string "{" curr-heading "}\n"))
                            ((= 4 curr-level)
                             (insert oletptceu--subsubsec-format "\n")
                             (insert "\\subsubsection" toc-head-string "{" curr-heading "}\n"))
                            ((= 5 curr-level)
                             (insert oletptceu--paragraph-format "\n")
                             (insert "\\paragraph" toc-head-string "{" curr-heading "}\n"))
                            ((= 6 curr-level)
                             (insert oletptceu--subparagraph-format "\n")
                             (insert "\\subparagraph" toc-head-string "{" curr-heading "}\n"))
                            (t
                             (insert "\\subparagraph" toc-head-string "{" curr-heading "}\n")))
                      (unless (or (string= curr-heading "") (string= curr-heading "Glossary") (string= curr-heading "Index"))
                        (insert "\\label{" curr-heading "}\n"))
                      (when curr-cust-id
                        (insert "\\label{" curr-cust-id "}\n"))))
               (if empty-pagestyle
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
               (unless oletptceu--bm-found-p
                 (insert "\\newpage\n"
                         "\\thispagestyle{empty}\n"
                         "\\backmatter{}\n")
                 (setq oletptceu--bm-found-p t))
               (when (string= curr-heading "")
                 (setq no-header-name t))
               (when (and no-header-name no-header-preamble)
                 (setq no-header t))
               (if (or no-header no-header-name)
                   (cond (part
                          (insert "\\titleformat{\\part}[runin]{}{}{0pt}{}\n")
                          (insert "\\part*{}\n"))
                         ((= 1 curr-level)
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
                   (cond (part
                          (insert oletptceu--part-format "\n")
                          (insert "\\part*{" curr-heading "}\n")
                          (unless no-toc-entry
                            (insert "\\addcontentsline{toc}{part}{" curr-heading "}\n")))
                         ((= 1 curr-level)
                          (insert oletptceu--chap-format "\n")
                          (insert "\\chapter*{" curr-heading "}\n")
                          (unless no-toc-entry
                            (insert "\\addcontentsline{toc}{chapter}{" curr-heading "}\n")))
                         ((= 2 curr-level)
                          (insert oletptceu--sec-format "\n")
                          (insert "\\section*{" curr-heading "}\n")
                          (unless no-toc-entry
                            (insert "\\addcontentsline{toc}{section}{" curr-heading "}\n")))
                         ((= 3 curr-level)
                          (insert oletptceu--subsec-format "\n")
                          (insert "\\subsection*{" curr-heading "}\n")
                          (unless no-toc-entry
                            (insert "\\addcontentsline{toc}{subsection}{" curr-heading "}\n")))
                         ((= 4 curr-level)
                          (insert oletptceu--subsubsec-format "\n")
                          (insert "\\subsubsection*{" curr-heading "}\n")
                          (unless no-toc-entry
                            (insert "\\addcontentsline{toc}{subsubsection}{" curr-heading "}\n")))
                         ((= 5 curr-level)
                          (insert oletptceu--paragraph-format "\n")
                          (insert "\\paragraph*{" curr-heading "}\n")
                          (unless no-toc-entry
                            (insert "\\addcontentsline{toc}{paragraph}{" curr-heading "}\n")))
                         ((= 6 curr-level)
                          (insert oletptceu--subparagraph-format "\n")
                          (insert "\\subparagraph*{" curr-heading "}\n")
                          (unless no-toc-entry
                            (insert "\\addcontentsline{toc}{subparagraph}{" curr-heading "}\n")))
                         (t
                          (insert "\\subparagraph*{" curr-heading "}\n")))
                   (unless (string= curr-heading "")
                     (insert "\\label{" curr-heading "}\n"))
                   (when curr-cust-id
                     (insert "\\label{" curr-cust-id "}\n"))))
               (if empty-pagestyle
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
                      (unless oletptceu--fm-found-p
                        (insert "\\frontmatter{}\n")
                        (setq oletptceu--fm-found-p t))
                      (setq no-toc-entry t))
                     ((string= curr-matter "MAIN MATTER")
                      (unless oletptceu--mm-found-p
                        (insert "\\tocParindent\n"
                                "\\tocParskip\n"
                                "\\setcounter{tocdepth}{4}\n"
                                "\\tableofcontents\n"
                                "\\docParindent\n"
                                "\\docParskip\n"
                                "\\newpage\n"
                                "\\thispagestyle{empty}\n"
                                "\\mainmatter{}\n")
                        (setq oletptceu--mm-found-p t)))
                     ((string= curr-matter "BACK MATTER")
                      (unless oletptceu--bm-found-p
                        (insert "\\newpage\n"
                                "\\thispagestyle{empty}\n"
                                "\\backmatter{}\n")
                        (setq oletptceu--bm-found-p t))
                      (setq no-toc-entry t)))
               ;; blank title => equivalent to no-header
               ;; no-header => Chapter X NOT used, other title NOT used, entry still appears (blank) in toc
               ;; no-header-name => Chapter X used, other title NOT used, entry still appears (blank) in toc
               ;; no-header-preamble => Chapter X NOT used, other title used, entry still appears (blank) in toc
               ;; no-header-name AND no-header-preamble => equivalent to no-header
               ;; no-toc-entry => remove entry from toc
               (when (string= curr-heading "")
                 (setq no-header-name t))
               (when (and no-header-name no-header-preamble)
                 (setq no-header t))
               (when no-toc-entry
                 (setq toc-head-string "*"))
               (cond (no-header
                      (cond (part
                             (insert "\\titleformat{\\part}[runin]{}{}{0pt}{}\n")
                             (insert "\\part" toc-head-string "{}\n"))
                            ((= 1 curr-level)
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
                      (cond (part
                             (insert "\\titleformat{\\part}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-part) "}{" (number-to-string oletptceu--typeface-size-part) "}\\selectfont}{0pt}{~\\linebreak\\fontsize{" (number-to-string oletptceu--typeface-size-part) "}{" (number-to-string oletptceu--typeface-size-part) "}\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]")
                             (insert "\\part" toc-head-string "{" curr-heading "}\n"))
                            ((= 1 curr-level)
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
                      (unless (or (string= curr-heading "") (string= curr-heading "Glossary") (string= curr-heading "Index"))
                        (insert "\\label{" curr-heading "}\n"))
                      (when curr-cust-id
                        (insert "\\label{" curr-cust-id "}\n")))
                     (no-header-name
                      (cond (part
                             (insert "\\titleformat{\\part}[hang]{\\sffamily\\bfseries}{\\fontsize{" (number-to-string oletptceu--typeface-size-part) "}{" (number-to-string oletptceu--typeface-size-part) "}\\selectfont\\partname\\,\\thepart}{0pt}{\\selectfont\\raggedleft}[{\\titlerule[0.5pt]}]\n")
                             (insert "\\part" toc-head-string "{}\n"))
                            ((= 1 curr-level)
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
                      (cond (part
                             (insert oletptceu--part-format "\n")
                             (insert "\\part" toc-head-string "{" curr-heading "}\n"))
                            ((= 1 curr-level)
                             (insert oletptceu--chap-format "\n")
                             (insert "\\chapter" toc-head-string "{" curr-heading "}\n"))
                            ((= 2 curr-level)
                             (insert oletptceu--sec-format "\n")
                             (insert "\\section" toc-head-string "{" curr-heading "}\n"))
                            ((= 3 curr-level)
                             (insert oletptceu--subsec-format "\n")
                             (insert "\\subsection" toc-head-string "{" curr-heading "}\n"))
                            ((= 4 curr-level)
                             (insert oletptceu--subsubsec-format "\n")
                             (insert "\\subsubsection" toc-head-string "{" curr-heading "}\n"))
                            ((= 5 curr-level)
                             (insert oletptceu--paragraph-format "\n")
                             (insert "\\paragraph" toc-head-string "{" curr-heading "}\n"))
                            ((= 6 curr-level)
                             (insert oletptceu--subparagraph-format "\n")
                             (insert "\\subparagraph" toc-head-string "{" curr-heading "}\n"))
                            (t
                             (insert "\\subparagraph" toc-head-string "{" curr-heading "}\n")))
                      (unless (or (string= curr-heading "") (string= curr-heading "Glossary") (string= curr-heading "Index"))
                        (insert "\\label{" curr-heading "}\n"))
                      (when curr-cust-id
                        (insert "\\label{" curr-cust-id "}\n"))))
               (if empty-pagestyle
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
        (setq empty-pagestyle nil)
        (setq pagestyle "headings")
        (setq part nil)
        (setq no-toc-entry nil)
        (setq toc-head-string ""))
      (goto-char (point-min))
      (oletptceu--delete-line)
      (setq out-str (buffer-string)))
    (setq oletptceu--fm-found-p nil)
    (setq oletptceu--mm-found-p nil)
    (setq oletptceu--bm-found-p nil)
    out-str))

(defun oletptceu--remap-image-links (file-contents)
  "Given a string FILE-CONTENTS, remap the Org mode image links correctly.
Return string of new file contents."
  (let ((out-str ""))
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (oletptceu--fold-show-all)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward (format "^[ \t]*%s" (regexp-quote "[[file:../Images/")) nil t)
          (delete-char -7)
          (insert "../Images/")))
      (setq out-str (buffer-string)))
    out-str))

(defun oletptceu--remap-internal-links (file-contents)
  "Given a string FILE-CONTENTS, remap the Org mode internal links correctly.
This is necessary when original Org headings have been replaced by LaTeX code.
Return string of new file contents."
  (let ((out-str "")
        (case-fold-search t)
        beg
        link-val
        link-text)
    (with-temp-buffer
      (insert file-contents)
      (org-mode)
      (oletptceu--fold-show-all)
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
              (insert "@@latex:\\hyperref[" link-val "]{" link-text "}@@")
            (insert "@@latex:\\ref{" link-val "}@@"))))
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
        (undo-tree-auto-save-history-orig nil)
        (org-export-backends-orig nil)
        (file-contents ""))
    (setq oletptceu--typeface-size nil)
    (setq oletptceu--monofont-typeface-size-adjustment nil)
    (setq oletptceu--typeface-size-part nil)
    (setq oletptceu--typeface-size-chapter nil)
    (setq oletptceu--typeface-size-section nil)
    (setq oletptceu--typeface-size-subsection nil)
    (setq oletptceu--typeface-size-subsubsection nil)
    (setq oletptceu--typeface-size-paragraph nil)
    (setq oletptceu--typeface-size-subparagraph nil)
    (setq oletptceu--mainfont nil)
    (setq oletptceu--headfont nil)
    (setq oletptceu--monofont nil)
    (setq oletptceu--signaturefont nil)
    (setq oletptceu--title-page-graphic nil)
    (setq oletptceu--title-page-graphic-scale nil)
    (setq oletptceu--title-page-graphic-copyright nil)
    (setq oletptceu--title-page-graphic-license nil)
    (setq oletptceu--title-page-replacement-graphic nil)
    (setq oletptceu--title-page-replacement-graphic-scale nil)
    (setq oletptceu--publisher nil)
    (setq oletptceu--isbn nil)
    (setq oletptceu--edition nil)
    (setq oletptceu--license nil)
    (setq oletptceu--sigil-graphic nil)
    (setq oletptceu--sigil-graphic-scale nil)
    (setq oletptceu--make-booklet nil)
    (setq oletptceu--booklet-signature-size nil)
    (setq oletptceu--booklet-buffer-pages nil)
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
    (oletptceu--set-book-configuration-overrides org-input-file output-file)  ; Override default template values using configuration values from book
    ;; Construct new file to pass to Org export dispatcher, based on input file.
    (when (file-exists-p org-input-file)
      (when (file-readable-p org-input-file)
        (setq file-contents (org-file-contents org-input-file))
        ;; Add index flags to the story. Doing this here, before any other processing, ensures we won't include things like title pages and copyright pages in the index.
        (setq file-contents (oletptceu--add-indices file-contents))
        ;; Setup LaTeX settings for Org mode export.
        (setq file-contents (oletptceu--set-latex-configuration file-contents))
        ;; Add LaTeX title page before first Org mode heading.
        (setq file-contents (oletptceu--add-title-page file-contents))
        ;; Add LaTeX legal page before first Org mode heading.
        (setq file-contents (oletptceu--add-legal-page file-contents))
        ;; Add LaTeX export codes to restyle the headings.
        (setq file-contents (oletptceu--restyle-headings file-contents))
        ;; Remap the image links to the correct folder.
        (setq file-contents (oletptceu--remap-image-links file-contents))
        ;; Remap the internal document links to point to replacement heading labels.
        (setq file-contents (oletptceu--remap-internal-links file-contents))
        (with-temp-buffer
          (insert file-contents)
          (org-mode)
          (oletptceu--fold-show-all)
          (oletptceu--string-to-file (buffer-string) temp-org))))  ; Write new Org file to be fed to exporter
    (when (boundp 'undo-tree-auto-save-history)
      (setq undo-tree-auto-save-history nil))  ; Try to prevent undo-tree making back-ups for autogenerated files
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
    (rename-file (concat (file-name-sans-extension temp-org) ".pdf") output-file t)
    (rename-file (concat (file-name-sans-extension temp-org) ".tex") (concat (file-name-sans-extension output-file) ".tex") t)
    (delete-file (concat (file-name-sans-extension temp-org) ".ilg"))
    (delete-file (concat (file-name-sans-extension temp-org) ".ind"))
    (oletptceu--make-booklet output-file)  ; Creates an imposed booklet for bookbinding if required
    (setq oletptceu--typeface-size nil)
    (setq oletptceu--monofont-typeface-size-adjustment nil)
    (setq oletptceu--typeface-size-part nil)
    (setq oletptceu--typeface-size-chapter nil)
    (setq oletptceu--typeface-size-section nil)
    (setq oletptceu--typeface-size-subsection nil)
    (setq oletptceu--typeface-size-subsubsection nil)
    (setq oletptceu--typeface-size-paragraph nil)
    (setq oletptceu--typeface-size-subparagraph nil)
    (setq oletptceu--mainfont nil)
    (setq oletptceu--headfont nil)
    (setq oletptceu--monofont nil)
    (setq oletptceu--signaturefont nil)
    (setq oletptceu--title-page-graphic nil)
    (setq oletptceu--title-page-graphic-scale nil)
    (setq oletptceu--title-page-graphic-copyright nil)
    (setq oletptceu--title-page-graphic-license nil)
    (setq oletptceu--title-page-replacement-graphic nil)
    (setq oletptceu--title-page-replacement-graphic-scale nil)
    (setq oletptceu--publisher nil)
    (setq oletptceu--isbn nil)
    (setq oletptceu--edition nil)
    (setq oletptceu--license nil)
    (setq oletptceu--sigil-graphic nil)
    (setq oletptceu--sigil-graphic-scale nil)
    (setq oletptceu--make-booklet nil)
    (setq oletptceu--booklet-signature-size nil)
    (setq oletptceu--booklet-buffer-pages nil)
    (when (boundp 'undo-tree-auto-save-history)
      (setq undo-tree-auto-save-history undo-tree-auto-save-history-orig))))

(provide 'org-latex-export-to-pdf-tradeback-cubes-en-us)
;;; org-latex-export-to-pdf-tradeback-cubes-en-us.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("oletptceu-" . "org-latex-export-to-pdf-tradeback-cubes-en-us-"))
;; End:
