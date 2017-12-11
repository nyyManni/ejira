;;; ejira-parser.el --- Parsing to and from JIRA markup.

;; Copyright (C) 2017 Henrik Nyman

;; Author: Henrik Nyman <henrikjohannesnyman@gmail.com>
;; URL: https://github.com/nyyManni/ejira
;; Keywords: calendar, data, org, jira
;; Version: 1.0
;; Package-Requires: ((org "8.3") (ox-jira) (language-detection) (s "1.0"))

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Two-directional parser for JIRA-markdown. For translating org-mode string to
;; JIRA-format, the ox-jira -module is used directly. Translation in the other
;; direction is done with regular expressions and is still in early beta state.

;;; Code:

(require 'ox-jira)
(require 'cl-lib)
(require 'language-detection)
(require 's)

(setq ejira-jira-to-org-patterns
      '(

        ;; Code block
        ("^{code\\(?::.*language=\\(?1:[a-z]+\\)\\)?.*}\\(?2:.*\\(?:
.*\\)*?\\)?
?{code}"
         . (lambda ()
             (let ((lang (match-string 1))
                   (body (match-string 2))
                   (md (match-data)))
               (when (equal lang "none")
                 (setq lang (symbol-name (language-detection-string body)))
                 (when (equal lang "awk")
                   ;; Language-detection seems to fallback to awk. In that case
                   ;; it most likely is not code at all. (Some coworkers like to
                   ;; use source blocks for other things than code...)
                   (setq lang ""))
                 )
               (prog1
                   (concat
                    "#+BEGIN_SRC " lang "\n"
                    (replace-regexp-in-string "^" "  " body)
                    "\n#+END_SRC")

                 ;; Auto-detecting language alters match data, restore it.
                 (set-match-data md)))))

        ;; Link
        ("\\[\\(?:\\(.*\\)|\\)?\\([^\\]*\\)\\]"
         . (lambda ()
             (let ((url (format "[%s]" (match-string 2)))
                   (placeholder (if (match-string 1)
                                    (format "[%s]" (match-string 1))
                                  "")))
               (format "[%s%s]" url placeholder))))

        ("\\\\{" . (lambda () "{"))

        ;; Table
        ("\\(^|| .*||\\)\\(\\(?:
| .*|\\)*$\\)"
         . (lambda ()
             (let ((header (match-string 1))
                   (body (match-string 2))
                   (md (match-data)))
               (with-temp-buffer
                 (insert
                  (concat
                   (replace-regexp-in-string "||" "|" header)
                   "\n|"
                   (replace-regexp-in-string
                    "" "-"
                    (make-string (- (s-count-matches "||" header) 1) ?+)
                    nil nil nil 1)
                   "-|"
                   body))
                 (org-table-align)

                 (prog1
                     ;; Get rid of final newline that may be injected by org-table-align
                     (replace-regexp-in-string "\n$" "" (buffer-string))

                   ;; org-table-align modifies match data, restore it.
                   (set-match-data md))))))

        ;; Bullet- or numbered list
        ("^\\([#*]+\\) "
         . (lambda ()
             (let* ((prefixes (match-string 1))
                    (level (- (length prefixes) 1))
                    (indent (make-string (max 0 (* 4 level)) ? )))
               ;; Save numbered lists with a placeholder, they will be calculated
               ;; later.
               (concat indent (if (s-ends-with? "#" prefixes) "########" "-") " "))))

        ;; Heading
        ("^h\\([1-6]\\)\\. "
         . (lambda ()
             (concat
              ;; NOTE: Requires dynamic binding to be active.
              (make-string (+ (if (boundp 'jira-to-org--convert-level)
                                  jira-to-org--convert-level
                                0)
                              (string-to-number (match-string 1))) ?*) " ")))

        ;; Verbatim text
        ("{{\\([^}]*\\)}}"
         . (lambda () (concat "=" (match-string 1) "=")))

        ;; Italic text
        ("_\\([^_]*\\)_"
         . (lambda () (concat "/" (match-string 1) "/")))

        ))


(defun ejira-org-to-jira (s)
  "Transform org-style string S into JIRA format."
  (org-export-string-as s 'jira t))

(defun random-alpha ()
  "Generate a random lowercase character."
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i))))


(defun ejira-jira-to-org (s &optional level)
  "Transform JIRA-style string S into org-style.
If LEVEL is given, shift all
headings to the right by that amount."
  (condition-case nil
      (with-temp-buffer
        (let ((replacements (make-hash-table :test 'equal))
              (jira-to-org--convert-level (or level 0)))
          (insert (decode-coding-string s 'utf-8))
          (cl-loop
           for (pattern . replacement) in ejira-jira-to-org-patterns do
           (goto-char (point-min))
           (while (re-search-forward pattern nil t)
             (let ((identifier (concat (random-alpha) (random-alpha)
                                       (random-alpha) (random-alpha)
                                       (random-alpha) (random-alpha)
                                       (random-alpha) (random-alpha)
                                       (random-alpha) (random-alpha)
                                       (random-alpha) (random-alpha)
                                       (random-alpha) (random-alpha)
                                       (random-alpha) (random-alpha)))
                   (rep (funcall replacement)))
               (replace-match identifier)
               (puthash identifier rep replacements))))
          (maphash (lambda (key val)
                     (goto-char (point-min))
                     (search-forward key)
                     (replace-match val))
                   replacements)
          ;; Calculate numbered list indices
          (goto-char (point-min))
          (let ((counters (make-list 6 1)))  ; JIRA Supports 6 levels of headings
            (dolist (n (split-string (buffer-string) "\n"))
              (cond ((search-forward-regexp "^\\([[:blank:]]*\\)########"
                                            (line-end-position) t)
                     (let ((level (/ (length (match-string 1)) 4)))
                       (replace-match (format "%s%i." (match-string 1) (nth level counters)))
                       (setcar (nthcdr level counters) (1+ (nth level counters)))))
                    ((search-forward-regexp "^\\([[:blank:]]*\\)- " (line-end-position) t)
                     nil)
                    (t (setq counters (make-list 6 1))))
              (forward-line 1)))
          (delete-trailing-whitespace))
        (setq my-contents (buffer-string))
        (buffer-string))
    (error s))) ;; If conversion fails, just return plain text.

(provide 'ejira-parser)
;;; ejira-parser.el ends here
