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


(defcustom ejira-agenda-boards-alist nil
  "Association list of board ids and names to make available through ejira.
Board name does not need to match the real name of the board, the lookup is done
with the ID."
  :group 'ejira
  :type '(alist :key-type (number :tag "Board ID")
                :value-type (string :tag "Board name")))

(defun ejira--agenda-board (board &optional ignore-cache)
  "View board items in agenda view.
With IGNORE-CACHE fetch board items from server. BOARD should be a cons cell
 (id . name)."

  (let* ((refresh ignore-cache)
         (board-id (car board))
         (board-name (cdr board))
         (org-agenda-custom-commands
          `(("x" "Ejira agenda"
             ,(ejira-agenda-define-board-agenda board-id board-name refresh)))))
    (org-agenda nil "x")))

(defun ejira-agenda-define-board-agenda (board-id title &optional refresh)
  "Create a new agenda view from board BOARD-ID. Use TITLE as the agenda header.
With REFRESH, ignore cache and pull the most recent data from server."
  `((tags (ejira-agenda--key-list-to-agenda-filter
           (ejira-agenda--jql-board-issues
            ,board-id "resolution = unresolved and assignee = currentUser() order by updated" ,refresh))
          ((org-agenda-overriding-header ,(format "%s\n\nAssigned to me" title))))
    (tags (ejira-agenda--key-list-to-agenda-filter
           (ejira-agenda--jql-board-issues
            ,board-id "resolution = unresolved order by updated" ,refresh))
          ((org-agenda-overriding-header "All items")))))

(defun ejira-agenda-board (&optional ignore-cache)
  "Select a board and view it's agenda.
With IGNORE-CACHE fetch board items from the server."
  (interactive "P")
  (ejira--agenda-board
   (rassoc
    (completing-read "Select board: " (mapcar #'cdr ejira-agenda-boards-alist))
    ejira-agenda-boards-alist)
   (when ignore-cache t)))


(defun ejira-agenda--key-list-to-agenda-filter (issues)
  "Make agenda property filter out of keys of ISSUES."
  (s-join "|" (mapcar (-partial #'format "ID=\"%s\"") issues)))

(defvar ejira-agenda--board-cache nil
  "A nested alist of board-id and queries.")
(defun ejira-agenda--jql-board-issues (board jql &optional refresh)
  "Get keys of the issues in BOARD matching JQL. With REFRESH ignore cache."
  (if-let ((r (not refresh))
           (keys (alist-get `(,board . ,jql) ejira-agenda--board-cache nil nil #'equal)))
      keys
    (let ((keys
           (mapcar
            (-partial #'alist-get 'key)
            (jiralib2-board-issues board `((fields . ("key")) (jql . ,jql))))))
      (add-to-list 'ejira-agenda--board-cache `((,board . ,jql) . ,keys) nil #'equal)
      keys)))

(defvar ejira-agenda--jql-cache nil
  "Cache for JQL searches made by ejira agenda.
Association list ((<jql> . (<key1> <key2> <key3> ...)) ...)")

(defun ejira-jql (jql)
  "`org-agenda' -type which filters the issues with JQL.
Prefix argument causes discarding the cached issue key list."
  (when (or current-prefix-arg (not (assoc jql ejira-agenda--jql-cache)))
    (map-put ejira-agenda--jql-cache jql (mapcar
                                          (-partial #'alist-get 'key)
                                          (jiralib2-jql-search jql "key"))))

  (let* ((key-list (cdr (assoc jql ejira-agenda--jql-cache)))
         (tag-filter (ejira-agenda--key-list-to-agenda-filter key-list)))
    (org-tags-view nil tag-filter)))

(provide 'ejira-agenda)
;;; ejira-agenda.el ends here
