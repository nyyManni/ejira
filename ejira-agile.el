;;; ejira-agile.el --- Scrum and Kanban functionality for ejira

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

;; Low-level functions of Ejira package

;;; Code:

(require 'ejira-core)

(defvar ejira-scrum-project nil
  "Project name used for getting the active sprint information.")

(defcustom ejira-boards nil
  "Board ids to sync with."
  :group 'ejira
  :type '(repeat number))

(defvar *current-sprint* nil)
(defun ejira-update-current-sprint ()
  "Update the cached current sprint."
  (interactive)
  (setq *current-sprint*
        (catch 'active-sprint
          (mapc (lambda (s)
                  (when (equal (ejira-sprint-state s) "ACTIVE")
                    (throw 'active-sprint (ejira-sprint-name s))))
                (mapcar #'ejira--parse-sprint
                        (ejira--alist-get
                         (nth 0 (jiralib2-do-jql-search
                                 (concat "project in ("
                                         ejira-scrum-project
                                         ") and sprint in openSprints()")))
                         'fields ejira-sprint-field))))))

(defun ejira-current-sprint ()
  "Get the active sprint in current project."
  (or *current-sprint*
      (ejira-update-current-sprint)))


(defun ejira-current-sprint-tag ()
  "Convert current sprint name to a valid tag identifier."
  (ejira--to-tagname (or (ejira-current-sprint)
                         (error "No active sprint"))))

(defun ejira-current-sprint-num ()
  "Extract just the sprint number of active sprint."
  (replace-regexp-in-string ".*?\\([0-9]+\\)" "\\1" (ejira-current-sprint)))

(provide 'ejira-agile)
;;; ejira-agile.el ends here
