;;; helm-ejira.el --- Helm-completion for ejira.el

;; Copyright (C) 2017 Henrik Nyman

;; Author: Henrik Nyman <henrikjohannesnyman@gmail.com>
;; URL: https://github.com/nyyManni/ejira
;; Keywords: calendar, data, org, jira
;; Version: 1.0
;; Package-Requires: ((org "8.3") (ox-jira) (language-detection) (s "1.0") (helm-org))

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

;; Helm-integration to ejira.el.

;;; Code:

(require 'helm)
(require 'org)
(require 'ejira)
(require 'ejira-core)
(require 'org-agenda)
(require 'helm-org)

(defgroup helm-ejira nil
  "Helm support for ejira."
  :prefix "helm-ejira-"
  :group `ejira)

;;;###autoload
(defcustom helm-ejira-fuzzy-match t
  "Enable fuzzy matching for Helm Ejira.
The search will be matched against the title, issue key and tags."
  :group 'helm-ejira)

(defun ejira--get-headings-in-file (filename plist)
  "Get ejira headings from FILENAME with parameters PLIST.
Parameters:
  :type  match for the TYPE-property (defaults to task, project and epic)
  :tags  match for the tags (defaults to any)
Without type, match for all ejira types (task, epic, project)"
  (let ((type (plist-get plist :type))
        (tags (plist-get plist :tags)))
    (with-current-buffer (pcase filename
                           ((pred bufferp) filename)
                           ((pred stringp) (find-file-noselect filename t)))
      (org-with-wide-buffer
       (ejira--with-expand-all
         (goto-char (point-min))
         (cl-loop while (search-forward-regexp "^\\*\\{2,4\\} " nil t)
                  if (and (when-let ((type_ (org-entry-get (point) "TYPE")))
                            (if type (equal type_ type)
                              (and (s-starts-with-p "ejira-" type_)
                                   (not (equal "ejira-comment" type_)))))
                          (when-let ((tags_ (org-get-tags)))
                            (equal tags (-intersection tags tags_)))
                          )
                  collect `(,(org-entry-get (point) "ID")
                            ,(ejira--strip-properties (org-get-heading t t t t))
                            ,(org-get-tags))))))))

(defun ejira--get-headings-in-agenda-files (&rest plist)
  "Get ejira headings from org agenda files, with parameters PLIST."
  (-mapcat (-rpartial #'ejira--get-headings-in-file plist) (org-agenda-files)))

(defun ejira--get-headings-in-current-file (&rest plist)
  "Get ejira headings from the current file, with parameters PLIST."
  (ejira--get-headings-in-file plist (buffer-file-name)))

(defun helm-ejira--format-entry (item width)
  "Format item ITEM for displaying with `helm' buffer of size WIDTH."
  (let* ((key (nth 0 item))
         (heading (nth 1 item))
         (tags (nth 2 item))
         (left-side (format "%-15s %s" (propertize key 'face 'font-lock-comment-face) heading))
         (right-side (propertize (s-join "," tags) 'face 'font-lock-type-face)))

    (format "%s%s%s"
            left-side
            (make-string (max 0 (- width (length left-side) (length right-side))) ? )
            right-side)))

(defmacro helm-ejira--define (cmd doc &rest plist)
  "Define helm-completer CMD for ejira with docstring DOC.
PLIST can have following options:
  :headings-fn  expression to get candidates (required)
  :prompt       the helm prompt (defaults to \"Ejira item: \")
  :action       the action to perform for the selected item. (defaults to focus)"
  `(progn
     (defun ,(intern (concat (symbol-name cmd) "--candidates")) ()
       (mapcar
        (-rpartial #'helm-ejira--format-entry (window-width (helm-window)))
        (eval ,(plist-get plist :headings-fn))))
     (defvar ,(intern (concat (symbol-name cmd) "--source"))
       (helm-build-sync-source ,(symbol-name cmd)
         :candidates ',(intern (concat (symbol-name cmd) "--candidates"))
         :fuzzy-match helm-ejira-fuzzy-match
         :action ,(or (plist-get plist :action)
                      `(lambda (c)
                         (ejira-focus-on-issue (nth 0 (split-string c))))))
       ,(concat "Helm source for " (symbol-name cmd) "."))
     (defun ,cmd ()
       ,doc
       (interactive)
       (helm :sources ,(intern (concat (symbol-name cmd) "--source"))
             :buffer "*helm ejira*"
             :prompt ,(or (plist-get plist :prompt) "Ejira item: ")))))

(function-put #'helm-ejira--define 'lisp-indent-function 'defun)

;;;###autoload
(helm-ejira--define helm-ejira-epic
  "Select an epic."
  :prompt "Epic: "
  :headings-fn '(ejira--get-headings-in-agenda-files :type "ejira-epic"))

;;;###autoload
(helm-ejira--define helm-ejira-issue
  "Select an issue."
  :prompt "Issue: "
  :headings-fn '(ejira--get-headings-in-agenda-files))

;;;###autoload
(helm-ejira--define helm-ejira-issue-assigned
  "Select an issue that is assigned to me."
  :prompt "Issue: "
  :headings-fn '(ejira--get-headings-in-agenda-files :tags '("Assigned")))

(provide 'helm-ejira)
;;; helm-ejira.el ends here
