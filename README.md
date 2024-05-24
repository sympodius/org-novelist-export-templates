<div align="center">

# Org Novelist Export Templates

![Release Version](https://img.shields.io/github/tag/sympodius/org-novelist-export-templates.svg?style=flat-square&label=release&color=58839b)![Latest Release](https://img.shields.io/github/release-date/sympodius/org-novelist-export-templates?style=flat-square&label=)
![Supports Org Novelist v0.0.3 and Higher](https://img.shields.io/badge/Supports-Org_Novelist_v0.0.3_and_Higher-blue.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)
![Latest Commit](https://img.shields.io/github/last-commit/sympodius/org-novelist-export-templates/development?style=flat-square)

</div>


### Table of Contents
- [Introduction](#introduction)
- [Template Descriptions](#template-descriptions)
  - [Basic](#basic)
  - [Manuscript](#manuscript)
  - [Cubes](#cubes)


# Introduction
This repository contains export templates for the [Org Novelist](https://github.com/sympodius/org-novelist/) writing system. The [general instructions for using Export Templates](https://github.com/sympodius/org-novelist#exporting) can be found at the Org Novelist page. This repository only contains additional instructions for the specific features of the templates found here.


# Template Descriptions
The top level folders group the templates by categories.

## Basic
The templates in this folder demonstrate quick wrappers for [Org mode's](https://orgmode.org/) built-in exporters.

+ **org-latex-export-to-pdf** - Export Org file to PDF via [LaTeX](https://www.latex-project.org/)
+ **org-odt-export-to-odt** - Export Org file to [ODT](https://en.wikipedia.org/wiki/OpenDocument)
+ **org-org-export-to-org** - Export Org file to a clean [Org](https://orgmode.org/) file (removing export directives for other systems, plus other light processing)

## Manuscript
Templates to create an industry '[standard manuscript format](https://en.wikipedia.org/wiki/Standard_manuscript_format)' file for submitting to editors, agents, and publishers.

+ **org-odt-export-to-manuscript-en-us** - A manuscript [ODT](https://en.wikipedia.org/wiki/OpenDocument)

## Cubes
These are the 'flagship' templates for Org Novelist, containing a high degree of customisation, and the most polished output for publishing your final novel. I call this look 'Cubes' because the templates automatically generate a cover featuring cubes. Both Cubes templates (PDF and ePub) will produce output files that match each other stylistically. These templates can also produce PDF files suitable for home bookbinders, as well as [Kindle](https://en.wikipedia.org/wiki/Amazon_Kindle) compatible files.

+ **org-latex-export-to-pdf-tradeback-cubes-en-us** - A *ready-for-printing* 'US tradeback' style [PDF](https://en.wikipedia.org/wiki/PDF) via [XeLaTeX](https://www.latex-project.org/) (with options to print for traditional hand-bookbinders)
+ **org-pandoc-export-to-epub-cubes-en-us** - A *ready-for-digital-publication* [ePub](https://en.wikipedia.org/wiki/EPUB) (including [Kindle](https://en.wikipedia.org/wiki/Amazon_Kindle) compatible [azw3](https://en.wikipedia.org/wiki/Kindle_File_Format)) via [Markdown](https://daringfireball.net/projects/markdown/), [Pandoc](https://pandoc.org/), [Imagemagick](https://imagemagick.org/), and [Calibre](https://calibre-ebook.com/)
