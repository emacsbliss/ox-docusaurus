;;; ox-docusaurus.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 emacsbliss
;;
;; Author: emacsbliss <https://github.com/emacsbliss>
;; Maintainer: emacsbliss <emacsbliss@gmail.com>
;; Created: February 20, 2022
;; Modified: February 20, 2022
;; Version: 0.0.1
;; Keywords: markdown org-mode Docusaurus
;; Homepage: https://github.com/emacsbliss/ox-docusaurus
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(load "ox-blackfriday")

(require 'ox-blackfriday)

;;;###autoload
(defun jk-org-kwds ()
  "parse the buffer and return a cons list of (property . value)
from lines like:
#+PROPERTY: value"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
                   (lambda (keyword) (cons (org-element-property :key keyword)
                                           (org-element-property :value keyword)))))

;;;###autoload
(defun jk-org-kwd (KEYWORD)
  "get the value of a KEYWORD in the form of #+KEYWORD: value"
  (cdr (assoc KEYWORD (jk-org-kwds))))


;;;; Body Filter
(defun org-docusaurus-body-filter (body _backend info)
  "Add front-matter to the BODY of the document.

BODY is the result of the export.
INFO is a plist holding export options."
  (let* ((title (jk-org-kwd "TITLE")))
        (format "---\ntitle: %s\n---\n\n%s" title body)
))


;;;###autoload
(defun org-docusaurus-export-to-md (&optional async subtreep visible-only)
  "Export current buffer to a Docusaurus-compatible Markdown file."
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'docusaurus outfile async subtreep visible-only)))

;;;###autoload
(defun org-docusaurus-export-as-md (&optional async subtreep visible-only)
  "Export current buffer to a Docusaurus-compatible Markdown buffer."
  (org-export-to-buffer 'docusaurus "*Org Docusaurus Export*"
          async subtreep visible-only nil nil (lambda () (text-mode)))
)

(org-export-define-derived-backend 'docusaurus 'blackfriday
  :menu-entry
  '(?D "Export to Docusaurus-compatible Markdown"
       (
        (?d "File to Md file"
            (lambda (a s v _b)
              (org-docusaurus-export-to-md a s v)))
        (?t "File to a temporary Md buffer"
            (lambda (a s v _b)
              (org-docusaurus-export-as-md a s v)))))

  :filters-alist '((:filter-body . org-docusaurus-body-filter))
)

(provide 'ox-docusaurus)
;;; ox-docusaurus.el ends here
