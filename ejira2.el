;;; ejira.el --- Syncing between Jira and Org-mode.

;; Copyright (C) 2017 - 2018 Henrik Nyman

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

;; TODO:
;; - Creating issues
;; - Modifying issue description and title
;; - Modifying comments
;; - Preserve comment order when restoring lost comments
;; - Attachments
;; - Update issues in current sprint should update old open tickets, they are
;;   most likely closed, and deadlines are dnagling in agenda.
;; - Refile to an issue in current sprint

;;; Code:

(require 'org)
(require 'org-id)
(require 'jiralib2)



(defgroup ejira nil
  "JIRA synchronization for Emacs."
  :prefix "ejira-")

(defvar ejira-sprint-field 'customfield_10001)
(defvar ejira-epic-field 'customfield_10002)
(defvar ejira-epic-summary-field 'customfield_10004)
(defvar ejira-epic-type-name "Epic")
(defvar ejira-story-type-name "Story")
(defvar ejira-subtask-type-name "Sub-task")

(cl-defstruct ejira-task
  (key nil :read-only t)
  type
  (reporter nil :read-only t)
  assignee
  deadline
  status
  created
  updated
  epic
  project
  priority
  sprint
  estimate
  remaining-estimate
  summary
  parent
  description
  comments)

(cl-defstruct ejira-comment
  id
  author
  created
  updated
  body)

(cl-defstruct ejira-sprint
  (id nil :read-only t)
  name
  state
  start-date
  end-date
  complete-date)

(cl-defstruct ejira-project
  (key nil :read-only t)
  name
  description)

(defun ejira--parse-sprint (s)
  "Parse a sprint object S. Return it as jira-sprint structure."
  (let ((args (mapcar
               (lambda (p)
                 (apply 'cons (split-string p "=")))

               ;; Throw away items not containing =-character. They probably are
               ;; there due to an extra , in some of the fields. This results in
               ;; field values being truncated on first comma. I blame JIRA for
               ;; not escaping the commas properly.
               (remove-if-not
                (lambda (i)
                  (s-contains-p "=" i))
                (split-string
                 (replace-regexp-in-string "^.*@[0-9a-f]*\\[\\(.*\\)\\]$" "\\1" s) ",")))))

    (make-ejira-sprint
     :id (cdr (assoc "id" args))
     :name (cdr (assoc "name" args))
     :state (cdr (assoc "state" args))
     :start-date (condition-case  nil
                     (date-to-time (cdr (assoc "startDate" args)))
                   (error nil))
     :end-date (condition-case nil
                   (date-to-time (cdr (assoc "endDate" args)))
                 (error nil))
     :complete-date (condition-case nil
                        (date-to-time (cdr (assoc "completeDate" args)))
                      (error nil)))))

(defun ejira--parse-project (project)
  "Parse a project structure from REST object PROJECT."
  (make-ejira-project
   :key (ejira--alist-get project 'key)
   :name (ejira--alist-get project 'name)
   :description (ejira--alist-get project 'description)))

(defun ejira--parse-comment (comment)
  "Parse a comment structure from REST object COMMENT."
  (make-ejira-comment
   :id (ejira--alist-get comment 'id)
   :author (ejira--alist-get comment 'author 'displayName)
   :created (date-to-time (ejira--alist-get comment 'fields 'created))
   :updated (date-to-time (ejira--alist-get comment 'fields 'updated))
   :body (ejira--alist-get comment 'body)))

(defun ejira--parse-item (item)
  "Parse an issue or epic structure from REST object ITEM."
  (let ((type (ejira--alist-get item 'fields 'issuetype 'name)))
    (make-ejira-task
     :key (ejira--alist-get item 'key)
     :type type
     :status (ejira--alist-get item 'fields 'status 'name)
     :created (date-to-time (ejira--alist-get item 'fields 'created))
     :updated (date-to-time (ejira--alist-get item 'fields 'updated))
     :reporter (ejira--alist-get item 'fields 'reporter 'displayName)
     :assignee (ejira--alist-get item 'fields 'assignee 'displayName)
     :deadline (ejira--alist-get item 'fields 'duedate)
     :epic (unless (equal type ejira-epic-type-name)
             (ejira--alist-get item 'fields ejira-epic-field))
     :project (ejira--alist-get item 'fields 'project 'key)
     :estimate (ejira--alist-get item 'fields 'timetracking
                                    'originalEstimateSeconds)
     :remaining-estimate (ejira--alist-get item 'fields 'timetracking
                                              'remainingEstimateSeconds)
     :sprint (ejira-get-sprint-name (ejira--alist-get item 'fields
                                                         ejira-sprint-field))
     :parent (when (equal type ejira-subtask-type-name)
               (ejira--alist-get item 'fields 'parent 'key))
     :priority (ejira--alist-get item 'fields 'priority 'name)
     :summary (ejira--parse-body (ejira--alist-get item 'fields 'summary))
     :description (ejira--alist-get item 'fields 'description)
     :comments (mapcar #'ejira--parse-comment
                       (ejira--alist-get item 'fields 'comment 'comments)))))

(defun ejira--update-project (key)
  "Pull the project KEY from the server and update it's org state."

  (let* ((existing-heading (ejira--find-heading key))
         (project (ejira--parse-project (jiralib2-get-project key)))
         (project-file-name (if (s-ends-with? "/" ejira-my-org-directory)
                               (concat ejira-my-org-directory key ".org")
                              (concat ejira-my-org-directory "/" key ".org")))
         (project-buffer (or (find-buffer-visiting project-file-name)
                             (find-file project-file-name))))

    ;; We need to write empty file so that `org-id' will start tracking it.
    (unless (f-exists-p project-file-name)
      (write-region "" nil project-file-name))

    (unless existing-heading
      (ejira--new-heading project-buffer key))
    (ejira--set-summary key (ejira-project-name project))
    (ejira--set-property key "TYPE" "ejira-project")))

(defun ejira--update-task (key)
  "Pull the task KEY from the server and update it's org state."
  (message "updating item %s..." key)
  (let* ((i (ejira--parse-item (jiralib2-get-issue key)))
         (type (cond ((equal (ejira-task-type i) ejira-epic-type-name) 'ejira-epic)
                     ((equal (ejira-task-type i) ejira-story-type-name) 'ejira-story)
                     ((equal (ejira-task-type i) ejira-subtask-type-name) 'ejira-subtask)
                     (t 'ejira-issue)))
         (status (ejira-task-status i))
         (project (ejira-task-project i))
         (epic (ejira-task-epic i))
         (parent (ejira-task-parent i)))

    ;; Ensure that the project file is there to begin with.
    (unless (ejira--find-heading project) (ejira--update-project project))

    ;; Subtasks parent needs to be updated first so we can refile
    (when parent (ejira--update-task parent))

    ;; Epic needs to be updated first, so that we can refile
    (when (and epic (not (ejira--find-heading epic))) (ejira--update-task epic))

    ;; Create a new heading if needed
    (unless (ejira--find-heading key)
      (ejira--new-heading (marker-buffer (ejira--find-heading project)) key))

    (ejira--set-todo-state key (cond ((member status ejira-done-states) 3)
                                     ((member status ejira-in-progress-states) 2)
                                     (t 1)))

    (ejira--set-property key "TYPE" (symbol-name type))
    (ejira--set-summary key (ejira-task-summary i))

    (ejira--with-point-on key
      ;; TODO: This throws away any user-set tags
      (when (ejira-task-sprint i) (org-set-tags-to (ejira-task-sprint i)))

      (when (ejira-task-deadline i) (org-deadline nil (ejira-task-deadline i)))

      ;; Set priority.
      (cond ((member (ejira-task-priority i) ejira-high-priorities)
             (org-priority ?A))
            ((member (ejira-task-priority i) ejira-low-priorities)
             (org-priority ?C))
            (t (org-priority ?B)))

      (org-set-property "Status" (ejira-task-status i))
      (org-set-property "Reporter" (ejira-task-reporter i))
      (org-set-property "Assignee" (or (ejira-task-assignee i) ""))
      (if (equal (ejira-task-assignee i) (ejira-my-fullname))
          (org-toggle-tag "Assigned" 'on)
        (org-toggle-tag "Assigned" 'off))

      (org-set-property "Issuetype" (ejira-task-type i))
      (org-set-property "Created" (format-time-string "%Y-%m-%d %H:%M:%S"
                                                      (ejira-task-created i) "UTC"))
      (org-set-property "Modified" (format-time-string "%Y-%m-%d %H:%M:%S"
                                                       (ejira-task-updated i) "UTC"))
      (when (ejira-task-estimate i)
        (let ((minutes (/ (ejira-task-estimate i) 60)))
          (org-set-property "Effort" (format "%02d:%02d" (/ minutes 60) (% minutes 60)))))
      (when (ejira-task-remaining-estimate i)
        (let ((minutes (/ (ejira-task-remaining-estimate i) 60)))
          (org-set-property "Left" (format "%02d:%02d" (/ minutes 60) (% minutes 60))))))

    (ejira--with-point-on key (ejira--goto-subheading "Description"))
    (ejira--with-point-on key (ejira--goto-subheading "Comments"))
    (ejira--set-heading-body-jira-markup (ejira--find-task-subheading key "Description")
                                         (ejira-task-description i))

    ;; Finally, refile to the correct location
    (ejira--refile key (cond (parent) (epic) (t project)))))

(defun ejira--set-heading-body-jira-markup (heading content)
  "Update body of heading HEADING to parsed JIRA markup from CONTENT.
The content will be adjusted based on the heading level."
  (let ((level (org-with-point-at heading
                 (save-match-data
                   (search-forward-regexp "^\\**" (line-end-position) t)
                   (length (or (match-data) ""))))))

    (ejira--set-heading-body heading (ejira--parse-body content (1+ level)))))

(defun ejira--find-task-subheading (id heading)
  "Return marker to the subheading HEADING of task ID."
  (ejira--with-point-on id
                        (ejira--find-child-heading heading)))

(defun ejira--parse-body (body &optional level)
  "Parse a JIRA BODY to insert it inside org header.
If LEVEL is given, shift all heading by it."
  (cl-flet ((r (a b c) (replace-regexp-in-string a b c)))
    (concat
     (s-trim
      (ejira-jira-to-org
       (r "" ""    ; Windows line-endings
          (r " " " " ; Non-breaking space, JIRA likes these, Emacs doesn't
             (decode-coding-string (or body "") 'utf-8)))
       level))
     "")))

(defmacro ejira--with-narrow-to-body (heading &rest body)
  "Execute BODY while the buffer is narrowed to the content under HEADING."
  `(org-with-point-at ,heading
     (ejira--with-expand-all
        (goto-char ,heading)
        (org-narrow-to-subtree)
        (end-of-line)
        (narrow-to-region
         (point)
         (point-max))
        ,@body)))

(defun ejira--get-heading-body (heading)
  "Get body of HEADING."
  (ejira--with-narrow-to-body heading
    (let* ((buf (buffer-string))
           (s (if (and (> (length buf) 0)
                       (s-starts-with-p "\n" buf))
                  (substring buf 1)
                buf)))

      (set-text-properties 0 (length s) nil s)
      s)))

(defun ejira--set-heading-body (heading contents)
  "Set the contents of item HEADING to CONTENTS."
  (ejira--with-narrow-to-body heading
    (when (> (point-max) (point-min))
      (delete-region (point-min) (point-max)))
    (when (and contents (> (length contents) 0))
      (insert (concat "\n" contents)))))

(defun ejira--set-todo-state (key state)
  "Set todo state of item KEY into STATE."
  (ejira--with-point-on key (org-todo state)))

(defun ejira--is-parent-p (child parent)
  "Return t if CHILD is a subheading of PARENT."
  (ejira--with-point-on child
    (condition-case nil
        (progn
          (outline-up-heading 1 t)
          (equal parent (org-entry-get (point-marker) "ID")))
      (error nil))))

(defmacro ejira--with-expand-all (&rest body)
  "Evalate BODY while all outline contents are visible."
  `(org-save-outline-visibility t
     (outline-show-all)
     ,@body))

(defun ejira--new-heading (buffer id)
  "Create a header with id ID into BUFFER and return a marker to it.
If TITLE is given, use it as header title."
  (save-window-excursion
    (with-current-buffer buffer
      (org-with-wide-buffer
       (ejira--with-expand-all
        (goto-char (point-min))
        (org-insert-heading-respect-content t)
        (insert "<ejira new heading>")
        (org-set-property "ID" id)
        (org-beginning-of-line)
        (org-id-update-id-locations nil t)
        (point-marker))))))

(defun ejira--find-heading (id)
  "Find the item ID from agenda files, or return nil."
  (org-id-find-id-in-file id (org-id-find-id-file id) t))

(defmacro ejira--with-point-on (id &rest body)
  "Execute BODY with point on the item ID header."
  `(org-with-point-at (or (ejira--find-heading ,id)
                          (error "Item %s not found" ,id))
     ,@body))

(defun ejira--refile (source-id target-id)
  "Refile item SOURCE-ID to be a child of TARGET-ID."
  (unless (ejira--is-parent-p source-id target-id)
    (let ((target-m (or (ejira--find-heading target-id)
                        (error "Target %s not found" target-id))))
      (ejira--with-point-on source-id
        (org-refile nil nil
                    `(nil ,(buffer-file-name (marker-buffer target-m)) nil
                          ,(marker-position target-m)))))))

(defun ejira--set-property (id property value)
  "Set PROPERTY of item ID into VALUE."
  (ejira--with-point-on id
    (org-set-property property value)))

(defun ejira--get-property (id property)
  "Get value of PROPERTY of item ID."
  (ejira--with-point-on id
    (org-entry-get (point-marker) property)))


(defun ejira--find-child-heading (name)
  "Get marker to child heading with title NAME or nil."
  (org-with-wide-buffer
   (if (org-goto-first-child)
       (let ((found-p nil)
             (end-p nil))
         (while (and (not found-p) (not end-p))
           (if (equal (org-get-heading t t t t) name)
               (setq found-p t)

             (unless (org-goto-sibling)
               (setq end-p t))))
         (when found-p
           (point-marker))))))

(defun ejira--goto-subheading (name)
  "Move point to subheading with NAME, and create it if it does not exist."
  (goto-char
   (or (ejira--find-child-heading name)
       (org-with-wide-buffer
        (ejira--with-expand-all
         (org-insert-heading-respect-content t)
         (insert name)
         (org-demote-subtree)
         (beginning-of-line)
         (point-marker))))))

(defun ejira--set-summary (id summary)
  "Set the heading of item ID into SUMMARY."
  (ejira--with-point-on id
    (org-narrow-to-subtree)
    (goto-char (point-min))
    (org-with-wide-buffer
     (org-narrow-to-subtree)
     (when (search-forward
            (org-get-heading t t t t))
       (replace-match
        (s-replace "\\" "" (replace-regexp-in-string "\\\\\\(.\\)" "\\1"
                                                     summary)))))))

(defun ejira--alist-get (l &rest keys)
  "Find a value from fields L recursively with KEYS."
  (let ((value (let* (key exists)
                 (while (and keys (listp l))
                   (setq key (car keys))
                   (setq exists nil)
                   (mapc (lambda (item)
                           (when (equal key (car item))
                             (setq exists t)))
                         (if (and (listp l)
                                  (listp (car l)))
                             l
                           nil))
                   (setq keys (cdr keys))
                   (if exists
                       (setq l (cdr (assoc key l)))
                     (setq l (or (cdr (assoc key l)) l))))
                 (when exists
                   l))))
    (if (stringp value)
        (decode-coding-string value 'utf-8)
      value)))

(defun ejira-get-id-under-point (&optional type)
  "Get ID of the ticket under point.
With TYPE search up until an item of the given type is found."
  (save-excursion
    (catch 'id-tag
      (while t
        (save-restriction
          (ejira--with-expand-all
           (org-narrow-to-subtree)
           (goto-char (point-min))

           (let ((found-id (org-entry-get (point-marker) "ID"))
                 (found-type (org-entry-get (point-marker) "TYPE")))

             (when (and found-id (if type
                                     (equal found-type type)
                                   (s-starts-with-p "ejira-" found-type)))
               (throw 'id-tag found-id)))))
        (org-up-element)))))

(provide 'ejira)
;;; ejira.el ends here
