<div align="center">

# Org Novelist Export Templates

![Release Version](https://img.shields.io/github/tag/sympodius/org-novelist-export-templates.svg?style=flat-square&label=release&color=58839b)![Latest Release](https://img.shields.io/github/last-commit/sympodius/org-novelist-export-templates/main?style=flat-square&label=)
![Supports Emacs 28.1 and Higher](https://img.shields.io/badge/Supports-Emacs_28.1_and_Higher-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)
![Latest Commit](https://img.shields.io/github/last-commit/sympodius/org-novelist-export-templates/development?style=flat-square)

</div>


### Table of Contents
- [Introduction](#introduction)
- [Template Descriptions](#template-descriptions)


# Introduction
This repository contains export templates for the [Org Novelist](https://github.com/sympodius/org-novelist/) writing system.


# Template Descriptions
The top level folders are used to group the templates by categories.

## Basic
The templates in this folder demonstrate simple wrapper templates for Org Mode's built-in exporters.

+ **org-latex-export-to-pdf** - Export Org file to PDF via [LaTeX](https://www.latex-project.org/)
+ **org-odt-export-to-odt** - Export Org file to [ODT](https://en.wikipedia.org/wiki/OpenDocument)
+ **org-org-export-to-org** - Export Org file to [Org}(https://orgmode.org/) (removing export directives for other systems)

## Cubes
These are the 'flagship' templates for Org Novelist, containing a high degree of customisation, and the most polished output for publishing your final novel.

+ **org-latex-export-to-pdf-tradeback-cubes-en-us** - A ready for printing 'tradeback' style PDF via [LaTeX](https://www.latex-project.org/) (with options to print for traditional hand-bookbinders)
+ **org-pandoc-export-to-epub-cubes-en-us** - A ready for digital publication ePub (including Kindle compatible azw3) via [Markdown](https://daringfireball.net/projects/markdown/) and [Pandoc](https://pandoc.org/)

## Manuscript
Templates to create an industry '[standard manuscript format](https://en.wikipedia.org/wiki/Standard_manuscript_format)' for submitting to editors, agents, and publishers.

+ **org-odt-export-to-manuscript-en-us** - A manuscript [ODT](https://en.wikipedia.org/wiki/OpenDocument)
