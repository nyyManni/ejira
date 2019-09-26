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

(defcustom ejira-agenda-sprint-key "s"
  "Character to bind to `ejira-sprint-agenda'."
  :group 'ejira
  :type 'string)

(defcustom ejira-agenda-kanban-key "k"
  "Character to bind to `ejira-sprint-agenda'."
  :group 'ejira
  :type 'string)

(defvar ejira-narrow-to-issue-from-agenda t)
(defun ejira--focus-advice ()
  "Narrow and expand the issue selected from `org-agenda'."
  (when ejira-narrow-to-issue-from-agenda
    (ejira-focus-item-under-point)))
(advice-add 'org-agenda-switch-to :after #'ejira--focus-advice)

(defvar ejira-agenda-sprint-overview
  '(agenda "" ((org-agenda-overriding-header "Sprint's Schedule:")
	       (org-agenda-span 'week)
	       (org-agenda-ndays 5)
	       (org-agenda-start-on-weekday 1)
	       (org-agenda-todo-ignore-deadlines nil))))

(defvar ejira-agenda-sprint-my-issues
  '(tags (concat ejira-assigned-tagname "+" (ejira-current-sprint-tag))
         ((org-agenda-overriding-header "Assigned to me")
          (org-agenda-skip-function 'ejira--skip-if-not-todo-item))))

(defvar ejira-agenda-my-issues
  '(tags ejira-assigned-tagname
         ((org-agenda-overriding-header "Assigned to me")
          (org-agenda-skip-function 'ejira--skip-if-not-todo-item))))

(defvar ejira-agenda-sprint-content
  `(tags (ejira-current-sprint-tag)
         ((org-agenda-overriding-header (ejira-current-sprint))
          (org-agenda-skip-function 'ejira--skip-if-not-todo-item))))

(defun ejira--skip-if-not-todo-item ()
  "Skip agenda items that do not have a todo state."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (nth 3 (org-heading-components))
        nil
      subtree-end)))

(defun ejira--skip-if-not-in-current-sprint ()
  "Skip agenda items that are not tagged into current sprint."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (re-search-forward (format ":%s:" (ejira-current-sprint)) subtree-end t)
        nil
      subtree-end)))

(defvar ejira-kanban-agenda
  `(,ejira-agenda-kanban-key "Kanban Board"
                             (,ejira-agenda-my-issues)))

(defvar ejira-sprint-agenda
  `(,ejira-agenda-sprint-key "Active Sprint"
                             (,ejira-agenda-sprint-overview
                              ,ejira-agenda-sprint-my-issues
                              ,ejira-agenda-sprint-content))

  "`org-agenda' custom command for current sprint schedule.")

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
             ((tags ,(ejira-agenda--key-list-to-agenda-filter
                      (ejira-agenda--jql-board-issues
                       board-id "resolution = unresolved and assignee = currentUser() order by updated" refresh))
                    ((org-agenda-overriding-header ,(format "%s\n\nAssigned to me" board-name))))
              (tags ,(ejira-agenda--key-list-to-agenda-filter
                      (ejira-agenda--jql-board-issues
                       board-id "resolution = unresolved order by updated" refresh))
                    ((org-agenda-overriding-header "All items"))))))))
    (org-agenda nil "x")))

(defun ejira-agenda-board (&optional ignore-cache)
  "Select a board and view it's agenda.
With IGNORE-CACHE fetch board items from the server."
  (interactive "P")
  (ejira--agenda-board
   (rassoc
    (completing-read "Select board: " (mapcar #'cdr ejira-agenda-boards-alist))
    ejira-agenda-boards-alist)
   ignore-cache))


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
            (alist-get 'issues (jiralib2-board-issues board `((fields . ("key"))
                                                              (jql . ,jql)))))))
      (add-to-list 'ejira-agenda--board-cache `((,board . ,jql) . ,keys) nil #'equal)
      keys)))

(provide 'ejira-agenda)
;;; ejira-agenda.el ends here
