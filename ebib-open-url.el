;;; ebib-open-url.el --- OpenURL Support for Ebib    -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Samuel W. Flint

;; Author: Samuel W. Flint <swflint@flintfam.org>

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; This file is part of Ebib, a BibTex database manager for Emacs.  It
;; contains functions that integrate Ebib with the Z39.88-2004
;; [OpenURL] system frequently offered by academic libraries to
;; resolve and retrieve scholarly resources.  This implementation
;; specifically targets San Antonio Profile Level 1 (KEV serialization
;; with HTTP GET), and has been tested against ExLibris Primo NDE.
;;
;; [OpenURL]: https://openurl.org/
;;
;; To use this code, `require' it in your init file, and configure the
;; following variables.

;;  - `ebib-open-url-resolver'

;;  - `ebib-open-url-query-params'

;; You should then add a binding for the command
;; `ebib-open-url-browse' in `ebib-index-mode-map', such as:
;;
;; (define-key ebib-index-mode-map (kbd "M-R") #'ebib-open-url-browse)


;;; Code:

(require 'ebib-db)
(require 'url-util)


;;; Customization

(defcustom ebib-open-url nil
  "OpenURL Integration for Ebib."
  :group 'ebib)

(defcustom ebib-open-url-resolver nil
  "OpenURL Resolver base URL."
  :group 'ebib-open-url
  :type '(choice (const :tag "Unconfigured" nil)
                 string))

(defcustom ebib-open-url-query-params nil
  "Resolver-required query parameters."
  :group 'ebib-open-url
  :type '(repeat (cons string (repeat string))))


;;; Data Required

(defvar ebib-open-url-translations
  '()
  "Translations to converrt bib entries to Context Objects.")


;;; Utility Functions

(defun ebib-open-url--context-object (key db format genre transforms)
  "Construct a Context Object alist for KEY in DB.

This will be done using a particular Z39.88-2004 FORMAT (here, KEV for
SAP Level 1 compliance) using an entry-type-relevant GENRE based on
TRANSFORMS.

TODO: Describe structure of transforms."
  (delq nil
        (append
         (list '("ctx_ver" "Z39.88-2004")
               ;; TODO: rfr_id
               `("rft_val_fmt" ,format)
               `("rft.genre" ,genre)
               fixed
               (mapcar (lambda (transform)
                         (pcase-let ((`(,field ,param ,function) transform))
                           (when-let* ((data (ebib-get-field-value field key db t t t t t))
                                       (transformed (if function (funcall function data) data))
                                       (as-list (if (listp transformed) transformed (list transformed))))
                             (cons param as-list))))
                       transforms)))))



;;; Programmatic Interface

(defun ebib-open-url-generate (key db)
  "TODO KEY DB."
  (unless ebib-open-url-resolver
    (error "`ebib-open-url-resolver' must be configured"))
  (pcase-let ((`(,type ,format ,genre ,transforms) (assoc (downcase (ebib-get-field-value "=type=" key db))
                                                          ebib-open-url-translations)))
    (format "%s?%s"
            ebib-open-url-resolver
            (url-build-query-string (append
                                     ebib-open-url-query-params
                                     (ebib-open-url--context-object key db format genre transforms))))))


;;; User Interface

(defun ebib-open-url-browse (key db)
  "Browse the bib entry described by KEY and DB.

This is done using the configured OpenURL resolver,
`ebib-open-url-resolver'."
  (interactive (list (ebib--get-key-at-point) ebib--cur-db))
  (browse-url (ebib-open-url-generate key db)))

(provide 'ebib-open-url)
;;; ebib-open-url.el ends here
