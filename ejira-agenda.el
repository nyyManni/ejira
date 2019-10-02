;;; ejira-agenda.el --- org-agenda integration for ejira

;; Copyright (C) 2017 - 2019 Henrik Nyman

;; Author: Henrik Nyman <h@nyymanni.com>
;; URL: https://github.com/nyyManni/ejira
;; Keywords: calendar, data, org, jira

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

;; TODO:
;;  Agenda for kanban

;; Provides org-agenda commands for ejira

;;; Code:
(require 'ejira-core)
(require 'ejira-agile)
(require 'org-agenda)
(require 'jiralib2)

(defvar ejira-narrow-to-issue-from-agenda t
  "When set, pressing <RET> in agenda opens the issue in an indirect buffer.")

(declare-function ejira-focus-item-under-point "ejira.el")

(defvar ejira-narrow-to-issue-from-agenda t)
(defun ejira--focus-advice ()
  "Narrow and expand the issue selected from `org-agenda'."
  (when ejira-narrow-to-issue-from-agenda
    (ejira-focus-item-under-point)))
(advice-add 'org-agenda-switch-to :after #'ejira--focus-advice)

(defun ejira-agenda--format-item (key)
  "Format the heading with ID KEY."
  (message "Fetching: %s" key)
  (let ((marker (or (ejira--find-heading key)
                    (progn
                      (ejira--update-task key)
                      (ejira--find-heading key)))))
    (when marker
      (org-with-point-at marker
        (let ((props (list 'face 'default
		           'done-face 'org-agenda-done
		           'undone-face 'default
		           'mouse-face 'highlight
		           'org-not-done-regexp org-not-done-regexp
		           'org-todo-regexp org-todo-regexp
		           'org-complex-heading-regexp org-complex-heading-regexp
		           'help-echo
		           (format "mouse-2 or RET jump to Org file %S"
			           (abbreviate-file-name
			            (or (buffer-file-name (buffer-base-buffer))
				        (buffer-name (buffer-base-buffer))))))))
          (let* ((level (org-reduced-level (org-outline-level)))
                 (tags (org-get-tags))
                 (category (org-get-category))
                 (heading (org-agenda-format-item
                           ""
			   (concat
			    (if (eq org-tags-match-list-sublevels 'indented)
			        (make-string (1- level) ?.) "")
			    (org-get-heading))
			   (make-string level ?\s)
                           category
                           tags))
                 (priority (org-get-priority heading))
                 (todo (org-get-todo-state))
                 (org-marker (org-agenda-new-marker)))
            (org-add-props heading props
              'org-marker org-marker
              'org-clock-hd-marker org-marker
              'org-category category
              'priority priority)
            heading))))))

(defun ejira-agenda-view (keys)
  "Generate agenda view from JIRA identifier list KEYS.
A function similar to `org-tags-view' but instead of a tag search it uses
a list of JIRA keys and `org-id' to perform the search."
  (catch 'exit
    (org-agenda-prepare "Ejira agenda")
    (org-compile-prefix-format 'tags)
    (org-set-sorting-strategy 'tags)
    (setq org-agenda-redo-command (list 'ejira-agenda-view keys))
    (let ((headings (remq nil (mapcar #'ejira-agenda--format-item keys))))
      (org-agenda--insert-overriding-header
        "Headlines from selected JIRA keys")

      (org-agenda-mark-header-line (point-min))
      (when headings
        (insert (org-agenda-finalize-entries headings 'tags) "\n"))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer)))
    (org-agenda-finalize)
    (setq buffer-read-only t)))

(defvar ejira-agenda--jql-cache nil
  "Cache for JQL searches made by ejira agenda.
Association list ((<jql> . (<key1> <key2> <key3> ...)) ...)")

;;;###autoload
(defun ejira-jql (jql)
  "`org-agenda' -type which filters the issues with JQL.
Prefix argument causes discarding the cached issue key list."
  (when (or current-prefix-arg (not (assoc jql ejira-agenda--jql-cache)))
    (map-put ejira-agenda--jql-cache jql (mapcar
                                          (-partial #'alist-get 'key)
                                          (jiralib2-jql-search jql "key"))))

  (ejira-agenda-view (cdr (assoc jql ejira-agenda--jql-cache))))

(provide 'ejira-agenda)
;;; ejira-agenda.el ends here




