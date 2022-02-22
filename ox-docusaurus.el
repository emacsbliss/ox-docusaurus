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
;; Package-Requires: ((emacs "26"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(load! "ox-blackfriday")

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

(defun org-docusaurus-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.

INFO is a plist used as a communication channel.
This function is adapted from `org-blackfriday-special-block'."
  (let* ((block-type (org-element-property :type special-block))
         (block-type-plist (org-element-property :type-plist special-block))
         (contents (or (org-trim
                          (if (plist-get block-type-plist :raw)
                              ;; https://lists.gnu.org/r/emacs-orgmode/2022-01/msg00132.html
                              (org-element-interpret-data (org-element-contents special-block))
                            contents))
                         "")))
    (message "block-type: %s" block-type)

      ;; Admonitions support
      ;; https://docusaurus.io/docs/markdown-features/admonitions
      (cond
       ((string= block-type "note")
          (format ":::note\n\n%s\n\n:::" contents))
       ((string= block-type "tip")
          (format ":::tip\n\n%s\n\n:::" contents))
       ((string= block-type "info")
          (format ":::info\n\n%s\n\n:::" contents))
       ((string= block-type "caution")
          (format ":::caution\n\n%s\n\n:::" contents))
       ((string= block-type "danger")
          (format ":::danger\n\n%s\n\n:::" contents))

       (t (org-blackfriday-special-block special-block contents info)))
))

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
  :translate-alist '((special-block . org-docusaurus-special-block))
)

(provide 'ox-docusaurus)
;;; ox-docusaurus.el ends here
