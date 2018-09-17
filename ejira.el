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
;; - Archive done that are not in current sprint
;; - Modify body

;;; Code:

(require 'org)
(require 'org-id)
(require 'jiralib2)
(require 'ejira-parser)
(require 'cl-lib)
(require 's)



(defgroup ejira nil
  "JIRA synchronization for Emacs."
  :prefix "ejira-")

(defvar ejira-done-states '("Done" "Resolved" "Duplicated" "Rejected"))
(defvar ejira-in-progress-states '("In Progress" "In Review" "Under Review" "Testing"))
(defvar ejira-high-priorities '("High" "Highest"))
(defvar ejira-low-priorities '("Low" "Lowest"))
(defvar ejira-projects nil
  "ID's of JIRA projects which will be synced.")
(defvar ejira-main-project nil
  "ID of the project which will be used to sync with sprint status.")
(defvar ejira-my-org-directory "~/.ejira")

(defvar ejira-no-epic-postfix "-NO-EPIC")
(defvar ejira-sprint-length 2
  "Length of a sprint in weeks.")


(defvar ejira-sprint-field 'customfield_10001)
(defvar ejira-epic-field 'customfield_10002)
(defvar ejira-epic-summary-field 'customfield_10004)

(cl-defstruct jira-issue
  (key nil :read-only t)
  type
  (reporter nil :read-only t)
  assignee
  deadline
  status
  updated
  epic
  project
  priority
  sprint
  estimate
  remaining-estimate
  summary
  description
  comments)

(cl-defstruct jira-epic
  (key nil :read-only t)
  (type "Epic" :read-only t)
  (reporter nil :read-only t)
  priority
  project
  status
  summary
  deadline
  updated
  description
  comments)

(cl-defstruct jira-comment
  id
  author
  created
  updated
  body)

(cl-defstruct jira-sprint
  (id nil :read-only t)
  name
  state
  start-date
  end-date
  complete-date)

(defun ejira-parse-sprint (s)
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

    (make-jira-sprint
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


(defun ejira-parse-comment (comment)
  "Parse a comment structure from REST object COMMENT."
  (make-jira-comment
   :id (ejira-extract-value comment 'id)
   :author (ejira-extract-value comment 'author 'displayName)
   :created (date-to-time (ejira-extract-value comment 'fields 'created))
   :updated (date-to-time (ejira-extract-value comment 'fields 'updated))
   :body (ejira-extract-value comment 'body)))

;; JIRA handles epics just as one type of issue, but we want to handle them
;; separately.
(defun ejira-parse-issue (issue)
  "Parse an issue or epic structure from REST object ISSUE."
  (if (equal (ejira-extract-value issue 'fields 'issuetype 'name) "Epic")
      (make-jira-epic
       :key (ejira-extract-value issue 'key)
       :summary (ejira-parse-body (ejira-extract-value issue 'fields
                                                       ejira-epic-summary-field))
       :status (ejira-extract-value issue 'fields 'status 'name)
       :updated (date-to-time (ejira-extract-value issue 'fields 'updated))
       :reporter (ejira-extract-value issue 'fields 'reporter 'displayName)
       :project (ejira-extract-value issue 'fields 'project 'key)
       :priority (ejira-extract-value issue 'fields 'priority 'name)
       :description (ejira-extract-value issue 'fields 'summary)
       :comments (mapcar #'ejira-parse-comment
                         (ejira-extract-value issue 'fields 'comment 'comments)))

    (make-jira-issue
     :key (ejira-extract-value issue 'key)
     :type (ejira-extract-value issue 'fields 'issuetype 'name)
     :status (ejira-extract-value issue 'fields 'status 'name)
     :updated (date-to-time (ejira-extract-value issue 'fields 'updated))
     :reporter (ejira-extract-value issue 'fields 'reporter 'displayName)
     :assignee (ejira-extract-value issue 'fields 'assignee 'displayName)
     :deadline (ejira-extract-value issue 'fields 'duedate)
     :epic (or (ejira-extract-value issue 'fields ejira-epic-field)
               (concat (ejira-extract-value issue 'fields 'project 'key)
                       ejira-no-epic-postfix))
     :project (ejira-extract-value issue 'fields 'project 'key)
     :estimate (ejira-extract-value issue 'fields 'timetracking
                                    'originalEstimateSeconds)
     :remaining-estimate (ejira-extract-value issue 'fields 'timetracking
                                              'remainingEstimateSeconds)
     :sprint (ejira-get-sprint-name (ejira-extract-value issue 'fields
                                                         ejira-sprint-field))
     :priority (ejira-extract-value issue 'fields 'priority 'name)
     :summary (ejira-parse-body (ejira-extract-value issue 'fields 'summary))
     :description (ejira-extract-value issue 'fields 'description)
     :comments (mapcar #'ejira-parse-comment
                       (ejira-extract-value issue 'fields 'comment 'comments)))))

(defvar ejira-my-fullname nil)
(defun ejira-my-fullname ()
  "Fetch full name of the currently logged in user."
  (or ejira-my-fullname
      (setq ejira-my-fullname
            (cdr (assoc 'displayName (jiralib2-get-user-info))))))

(setq *ejira-user-alist* nil)
(defun ejira-get-users ()
  "Fetch user list from server and cache it for the session."
  (or *ejira-user-alist*
      (setq *ejira-user-alist*
            (append
             '(("Unassigned" . ""))
             (remove nil
                     (mapcar (lambda (user)
                               (let ((name (decode-coding-string
                                            (cdr (assoc 'displayName user)) 'utf-8))
                                     (key (cdr (assoc 'key user))))
                                 (unless (s-starts-with? "#" key)
                                   (cons key name))))
                             (jiralib2-get-users ejira-main-project)))))))

;; A new link-type needs to be added, otherwise the ~-character at the beginning
;; of the link fools org to think this is a file path.
(org-add-link-type "jirauser")
(defun ejira-mention-user ()
  "Insert a username link."
  (interactive)
  (let* ((jira-users (ejira-get-users))
          (fullname (completing-read "User: " (mapcar 'cdr jira-users)))
          (username (car (rassoc fullname jira-users))))
    (insert (format "[[jirauser:~%s]]" username))))

(defun ejira-parse-body (body &optional level)
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

(defun ejira--get-last-modified (issue-id)
  "Get the last-modified timestamp of ISSUE-ID or nil."
  (condition-case nil
      (ejira-with-narrow-to-issue issue-id
                                  (ejira-property-value "Modified"))
    (error nil)))

(defun ejira--guess-project-key (issue-id)
  "Try to guess the project ID from ISSUE-ID.
This works with most JIRA issues."
  (replace-regexp-in-string "^\\(.*\\)-[0-9]+" "\\1" issue-id))


(defun ejira--parse-timestamp (timestamp)
  "Convert JIRA-style TIMESTAMP to native time type."
  (parse-time-string (replace-regexp-in-string "T" " "
                                               (replace-regexp-in-string "\\+.*" ""
                                                                         timestamp))))

;;;###autoload
(defun ejira-update-issues-in-active-sprint ()
  "Retrieve issues rom JIRA that are in active sprint and update the org tree."
  (interactive)
  (ejira-update-current-sprint)
  (ejira--update-issues-jql
   (concat "project in (" (s-join ", " ejira-projects) ")"
           " and sprint in openSprints ()"))
  (message "ejira update complete"))

(setq *sprint-list* nil)
(defun ejira-select-sprint ()
  "Select sprint from all sprints in current project."
  (let* ((current nil)
         (sprints
          (append
           '((nil . "No Sprint"))
           (or *sprint-list*
               (ejira-refresh-sprint-info))))
         (selected (rassoc (completing-read (format "Sprint (Current: %s): "
                                                    current)
                                            (mapcar 'cdr sprints))
                           sprints)))

    (cons
     (car selected)
     (s-trim (first (split-string (cdr selected) "<" nil nil))))))

;;;###autoload
(defun ejira-refresh-sprint-info ()
  "Refreshes information on current sprint."
  (interactive)
  (setq *sprint-list*
        (mapcar (lambda (i)
                  (cons
                   (jira-sprint-id i)
                   (format "%s\t<%s>" (jira-sprint-name i)
                           (downcase (jira-sprint-state i)))))
                (apply #'append
                       (mapcar
                        (lambda (s)
                          (mapcar #'ejira-parse-sprint s))
                        (remove nil
                                (mapcar
                                 (lambda (l)
                                   (ejira-extract-value
                                    l 'fields ejira-sprint-field))
                                 (jiralib2-do-jql-search
                                  (concat "project in (" ejira-main-project ")")
                                  300))))))))


(defun ejira-update-issues ()
  "Retrieve issues rom JIRA and update the org tree."
  (interactive)
  (ejira--update-issues-jql
   (concat "project in (" (s-join ", " ejira-projects) ")")))

(defun ejira--update-issues-jql (query)
  "Update issues matching QUERY."
  (cl-loop
   for issue in (jiralib2-do-jql-search query 300) do

   (let ((issue-id (ejira-extract-value issue 'key)))
     ;; Modify-time optimization is currently disabled, with the new jiralib2.el
     ;; it does no longer make a huge difference and there seems to be something
     ;; weird going on when JIRA updates the field.
     (progn
       ;; If the subtree already exists and it has a timestamp that is not older
       ;; than current, it does not have to be updated.
       ;; (when
       ;;     (let ((updated (ejira--get-last-modified issue-id)))
       ;;       (or
       ;;        (not updated) ;; We don't have an org-entry
       ;;        (time-less-p  ;; Entry is out of date
       ;;         (date-to-time (ejira-extract-value issue 'fields 'updated))
       ;;         (date-to-time updated))))
       (ejira-update-issue issue-id)))))


(defun ejira-get-sprint-name (data)
  "Parse sprint name from DATA. Return NIL if not found."
  (let ((name (last (mapcar (lambda (s)
                              (jira-sprint-name s))
                            (mapcar #'ejira-parse-sprint data)))))
    (when name
      ;; Spaces are not valid in a tagname.
      (ejira--to-tagname (car name)))))

(setq *current-sprint* nil)
(defun ejira-update-current-sprint ()
  (interactive)
  (setq *current-sprint*
        (catch 'active-sprint
          (mapc (lambda (s)
                  (when (equal (jira-sprint-state s) "ACTIVE")
                    (throw 'active-sprint (jira-sprint-name s))))
                (mapcar #'ejira-parse-sprint
                        (ejira-extract-value
                         (first (jiralib2-do-jql-search
                                 (concat "project in ("
                                         ejira-main-project
                                         ") and sprint in openSprints()")))
                         'fields ejira-sprint-field))))))

(defun ejira-current-sprint ()
  "Get the active sprint in current project."
  (or *current-sprint*
      (ejira-update-current-sprint)))

(defun ejira--to-tagname (str)
  "Convert STR into a valid org tag."
  (replace-regexp-in-string "[^a-zA-Z0-9_]+" "_" str))

(defun ejira-current-sprint-tag ()
  "Convert current sprint name to a valid tag identifier."
  (ejira--to-tagname (ejira-current-sprint)))

(defun ejira-current-sprint-num ()
  "Extract just the sprint number of active sprint."
  (replace-regexp-in-string ".*?\\([0-9]+\\)" "\\1" (ejira-current-sprint)))

(defun ejira--seconds-to-HH:MM (s)
  "Convert S seconds to HH:MM time format."
  (when s
    (let ((minutes (/ issue-estimate 60)))
      (format "%02d:%02d" (/ minutes 60) (% minutes 60)))))

(defmacro ejira-with-narrow-to-issue (id &rest body)
  "Execute BODY with point on the issue ID header, narrowed to subtree."
  `(save-current-buffer
     (let* ((m (or (org-id-find-id-in-file ,id (ejira-project-file-name
                                                (ejira--guess-project-key ,id))
                                           t)
                   (error (concat "no issue: " ,id))))
            (m-buffer (marker-buffer m)))

       (with-current-buffer m-buffer
         (org-with-wide-buffer
          (save-excursion
            (goto-char m)
            (save-restriction
              (org-narrow-to-subtree)
              ,@body)))))))

(defmacro ejira-with-narrow-to-issue-under-point (&rest body)
  "Execute BODY while the buffer is narrowed to the issue under point."
  `(let ((current-issue (ejira-get-id-under-point)))
     (ejira-with-narrow-to-issue current-issue
                                 ,@body)))


;;;###autoload
(defun ejira-assign-issue (&optional to-me)
  "Assign issue under point. With prefix-argument TO-ME assign it to me."
  (interactive "P")
  (ejira-with-narrow-to-issue-under-point
   (let* ((jira-users (ejira-get-users))
          (fullname (if to-me
                        (cdr (assoc jiralib2-user-login-name jira-users))
                      (completing-read "Assignee: " (mapcar 'cdr jira-users))))
          (username (car (rassoc fullname jira-users))))
     ;; REST call
     (jiralib2-assign-issue (ejira-get-id-under-point) username)

     (org-set-property "Assignee" fullname)

     ;; If assigned to me, add tag.
     (when to-me
       (org-toggle-tag "Assigned" 'on)))))


(defun ejira-project-file-name (project-id)
  "Get the file where PROJECT-ID is located. Create if not exist."
  (let ((filename (if (s-ends-with? "/" ejira-my-org-directory)
                      (concat ejira-my-org-directory project-id ".org")
                    (concat ejira-my-org-directory "/" project-id ".org"))))
    (unless (file-exists-p ejira-my-org-directory)
      (error (concat "Directory " ejira-my-org-directory " does not exist")))
    (unless (file-exists-p filename)
      (write-region (format "
* Issues without epic
:PROPERTIES:
:ID:       %s-NO-EPIC
:END:

" project-id) nil filename))
    filename))

(defun ejira--get-project (issue)
  "Extract project key from issue or epic structure ISSUE."
  (if (jira-issue-p issue)
      (jira-issue-project issue)
    (jira-epic-project issue)))

(defun ejira-new-header-with-id (id &optional title)
  "Create a header with id ID and return a marker to it.
If TITLE is given, use it as header title."
  (save-restriction
    (org-with-wide-buffer
     (save-mark-and-excursion
       (goto-char (point-min))
       (org-insert-heading-respect-content t)
       (insert "new heading")
       (org-set-property "ID" id)
       (org-beginning-of-line)
       (point-marker)))))

(defun ejira-add-child-if-not-exist (title)
  "Insert a heading with title TITLE to current item. Return marker to new item."
  (save-restriction
    (org-narrow-to-subtree)
    (save-mark-and-excursion
      (or (ejira-find-headline-in-visible title)
          (ejira-append-child title)))))

(defun ejira-append-child (title)
  "Append a child to item under point with title TITLE."
  (save-excursion
    (org-insert-heading-respect-content t)
    (insert title)
    (org-demote-subtree)
    (org-beginning-of-line)
    (point-marker)))

(defun ejira-update-issue (issue-id)
  "Update an issue with id ISSUE-ID. Create org-tree if necessary."
  (let* ((issue (ejira-parse-issue (jiralib2-get-issue issue-id)))
         (project-id (ejira--get-project issue))
         (project-file (ejira-project-file-name project-id))
         (project-buffer (or (find-buffer-visiting project-file)
                             (find-file project-file))))

    ;; Update epic first, so that refiling can be done.
    (when (and (not (jira-epic-p issue))
               (not (org-id-find-id-in-file (jira-issue-epic issue)
                                            project-file t)))

      ;; Issues can be connected to an epic of another project. In those cases,
      ;; a duplicate epic will be created to each project file to simplify life.
      (ejira-update-epic (jira-issue-epic issue) project-buffer))

    ;; JIRA can report epics when querying for issues
    (if (jira-epic-p issue)
        (ejira-update-epic issue-id project-buffer)

      (with-current-buffer project-buffer
        (org-with-wide-buffer
         (let ((inhibit-message t)
               (issue-subtree
                (or (org-id-find-id-in-file issue-id project-file 'marker)
                    (prog1
                        (ejira-new-header-with-id issue-id)
                      (when (fboundp 'helm-ejira-invalidate-cache)
                        (helm-ejira-invalidate-cache))))))

           (save-mark-and-excursion
             (goto-char issue-subtree)
             (save-restriction
               (org-narrow-to-subtree)
               (org-save-outline-visibility t
                 (org-show-subtree)

                 ;; Set the todo-status of the issue based on JIRA status.
                 (cond ((member (jira-issue-status issue) ejira-done-states)
                        (org-todo 3))
                       ((member (jira-issue-status issue) ejira-in-progress-states)
                        (org-todo 2))
                       (t
                        (org-todo 1)))

                 ;; Set the tag to JIRA sprint so that issues can be filtered easily.
                 (when (jira-issue-sprint issue)
                   (org-set-tags-to (jira-issue-sprint issue)))

                 ;; Update heading text.
                 (save-excursion
                   (ejira-update-header-text (jira-issue-summary issue)))

                 ;; Update deadline
                 (when (jira-issue-deadline issue)
                   (org-deadline nil (jira-issue-deadline issue)))

                 ;; Set properties.
                 (org-set-property "Type" (jira-issue-type issue))
                 (org-set-property "Status" (jira-issue-status issue))
                 (org-set-property "Reporter" (jira-issue-reporter issue))
                 (org-set-property "Assignee" (or (jira-issue-assignee issue) ""))

                 (if (equal (jira-issue-assignee issue) (ejira-my-fullname))
                     (org-toggle-tag "Assigned" 'on)
                   (org-toggle-tag "Assigned" 'off))

                 (org-set-property "Modified" (format-time-string
                                               "%Y-%m-%d %H:%M:%S"
                                               (jira-issue-updated issue)
                                               "UTC"))
                 (when (jira-issue-estimate issue)
                   (let ((minutes (/ (jira-issue-estimate issue) 60)))
                     (org-set-property "Effort" (format "%02d:%02d"
                                                        (/ minutes 60)
                                                        (% minutes 60)))))
                 (when (jira-issue-remaining-estimate issue)
                   (let ((minutes (/ (jira-issue-remaining-estimate issue) 60)))
                     (org-set-property "Left" (format "%02d:%02d"
                                                      (/ minutes 60)
                                                      (% minutes 60)))))

                 ;; Set priority.
                 (cond ((member (jira-issue-priority issue) ejira-high-priorities)
                        (org-priority ?A))
                       ((member (jira-issue-priority issue) ejira-low-priorities)
                        (org-priority ?C))
                       (t (org-priority ?B)))

                 (let ((description-subtree (ejira-add-child-if-not-exist "Description"))
                       (comments-subtree (ejira-add-child-if-not-exist "Comments"))
                       (attachments-subtree (ejira-add-child-if-not-exist "Attachments")))
                   (ejira--update-body description-subtree
                                       (jira-issue-description issue))

                   (ejira--update-comments comments-subtree
                                           (jira-issue-comments issue)))))

             ;; Refile the ticket under the epic
             (let ((current-epic (save-excursion
                                   (goto-char issue-subtree)
                                   (save-restriction
                                     (org-narrow-to-subtree)
                                     (ejira-property-value "Epic")))))

               (when (not (equal current-epic (jira-issue-epic issue)))
                 (save-excursion
                   (goto-char (org-id-find-id-in-file issue-id project-file t))
                   (org-set-property "Epic" (jira-issue-epic issue))
                   (ejira-refile project-id (jira-issue-epic issue))))))))
             (message "Updated issue %s: %s" issue-id
                      (jira-issue-summary issue))))))

(defun ejira-update-epic (epic-id  buffer)
  "Update an epic with id EPIC-ID. Create org-tree if necessary.
Epic will be created in BUFFER, regardless of the project."
  (let ((epic (ejira-parse-issue (jiralib2-get-issue epic-id))))

    (with-current-buffer buffer
      (org-with-wide-buffer
       (let ((epic-subtree
              (or (org-id-find-id-in-file epic-id (buffer-file-name) 'marker)
                  (prog1
                      (ejira-new-header-with-id epic-id)
                      (when (fboundp 'helm-ejira-invalidate-cache)
                        (helm-ejira-invalidate-cache))))))

         (save-mark-and-excursion
           (goto-char epic-subtree)
           (save-restriction
             (org-narrow-to-subtree)
             (org-save-outline-visibility t
               (org-show-subtree)

               ;; Set the todo-status of the issue based on JIRA status.
               (cond ((member (jira-epic-status epic) ejira-done-states)
                      (org-todo 3))
                     ((member (jira-epic-status epic) ejira-in-progress-states)
                      (org-todo 2))
                     (t
                      (org-todo 1)))

               ;; Update heading text.
               (save-excursion
                 (ejira-update-header-text (jira-epic-summary epic)))

               ;; Update deadline
               (when (jira-epic-deadline epic)
                 (org-deadline nil (jira-epic-deadline epic)))

               ;; Set properties.
               (org-set-property "Reporter" (jira-epic-reporter epic))
               (org-set-property "Modified" (format-time-string
                                             "%Y-%m-%d %H:%M:%S"
                                             (jira-epic-updated epic)
                                             "UTC"))

               ;; Set priority.
               (cond ((member (jira-epic-priority epic) ejira-high-priorities)
                      (org-priority ?A))
                     ((member (jira-epic-priority epic) ejira-low-priorities)
                      (org-priority ?C))
                     (t (org-priority ?B)))

               (let ((description-subtree (ejira-add-child-if-not-exist "Description"))
                     (comments-subtree (ejira-add-child-if-not-exist "Comments"))
                     (attachments-subtree (ejira-add-child-if-not-exist "Attachments")))
                 (ejira--update-body description-subtree
                                     (jira-epic-description epic))
                 (ejira--update-comments comments-subtree
                                         (jira-epic-comments epic)))))))))))


(defun ejira--update-comments (tree comments)
  "Update COMMENTS in org-subtree TREE from given data list."
  (mapc
   (lambda (comment)
     (save-mark-and-excursion
       (goto-char tree)
       (save-restriction
         (org-narrow-to-subtree)
         (let* ((comment-summary (ejira--get-comment-header
                                  (jira-comment-author comment)
                                  (jira-comment-body comment)))
                (comment-subtree
                 (or (org-id-find-id-in-file (jira-comment-id comment)
                                             (buffer-file-name) t)
                     (ejira-append-child comment-summary))))
           (save-excursion
             (goto-char comment-subtree)
             (save-restriction
               (org-narrow-to-subtree)

               ;; Update heading text
               (save-excursion
                 (ejira-update-header-text comment-summary))

               (org-set-property "ID" (jira-comment-id comment))
               (org-set-property "Author" (jira-comment-author comment))
               (let ((created (jira-comment-created comment))
                     (updated (jira-comment-updated comment)))

                 (org-set-property "Created" (format-time-string
                                              "%Y-%m-%d %H:%M:%S"
                                              created "UTC"))
                 (when (not (equal created updated))
                   (org-set-property "Modified" (format-time-string
                                                 "%Y-%m-%d %H:%M:%S"
                                                 updated "UTC"))))


               (ejira--update-body comment-subtree

                                   ;; Add 2 to level for comments
                                   ;; TODO: Define proper API for this.
                                   (jira-comment-body comment) 2)))))))

   comments)
  (ejira--kill-deleted-comments tree (mapcar #'jira-comment-id comments)))

(defun ejira--kill-deleted-comments (tree comment-ids)
  "Remove comment headings in TREE which are not found in COMMENT-IDS."
  (save-mark-and-excursion
    (goto-char tree)
    (save-restriction
      (org-narrow-to-subtree)
      (when (org-goto-first-child)  ;; Move point to the first comment
        (let* ((start-pos (point))
               (pos start-pos)
               (remote-comments comment-ids))
          (while pos
            (goto-char pos)
            (if (member
                 (save-restriction
                   (org-narrow-to-subtree)
                   (ejira-property-value "ID"))
                 remote-comments)
                (setq pos (outline-get-next-sibling))
              (save-mark-and-excursion
                (org-cut-subtree)

                ;; Killing the region moves point to the next header, it is
                ;; safest to just start all the way from the beginning here.
                ;; Moving to a previous sibling would be enough but org does not
                ;; provide that method, and moving to previous header would
                ;; possibly lead us out of the subtree.
                (setq pos start-pos)))))))))

;;;###autoload
(defun ejira-update-header-text (text)
  "Replace the header text with the given TEXT.
TODO state, priority and tags will be preserved."
  (interactive)
  (goto-char (point-min))
  (when (search-forward
         (org-get-heading t t t t))
    (replace-match (replace-regexp-in-string "\\\\\\(.\\)" "\\1" text))))

(defun strip-text-properties(txt)
  "Clear formatting from TXT."
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defun ejira--get-subitem-contents (header)
  "Get the body of the subitem called HEADER from visible buffer."
  (save-excursion
    (goto-char (ejira-find-headline-in-visible header))
    (save-restriction
      (org-narrow-to-subtree)

      ;; Skip any property- and clock-drawers if found.
      (search-forward-regexp org-deadline-line-regexp nil t)
      (search-forward-regexp org-property-drawer-re nil t)
      (search-forward-regexp org-clock-drawer-re nil t)

      ;; Ensure that point is on the second line of the narrowed region.
      (end-of-line)
      (if (eobp)
          (newline)
        (beginning-of-line 2))

      ;; Ensure that there are no extra newlines after the body.
      (let ((body-begin (point))
            (body-end (or (save-excursion
                            (goto-char (point-max))
                            (unless (org-with-wide-buffer
                                     (looking-at "$"))
                              (insert "\n")
                              (end-of-line 0)
                              (point)))
                          (point-max))))
        (narrow-to-region body-begin body-end))
      (buffer-string))))

(defun ejira--set-subitem-contents (header contents)
  (save-excursion
    (goto-char (ejira-find-headline-in-visible header))
    (save-restriction
      (org-narrow-to-subtree)

      ;; Skip any property- and clock-drawers if found.
      (search-forward-regexp org-deadline-line-regexp nil t)
      (search-forward-regexp org-property-drawer-re nil t)
      (search-forward-regexp org-clock-drawer-re nil t)

      ;; Ensure that point is on the second line of the narrowed region.
      (end-of-line)
      (if (eobp)
          (newline)
        (beginning-of-line 2))

      ;; Ensure that there are no extra newlines after the body.
      (let ((body-begin (point))
            (body-end (or (save-excursion
                            (goto-char (point-max))
                            (unless (org-with-wide-buffer
                                     (looking-at "$"))
                              (insert "\n")
                              (end-of-line 0)
                              (point)))
                          (point-max))))
        (narrow-to-region body-begin body-end))

      ;; Now we have the whole buffer dedicated to the description.
      (delete-region (point-min) (point-max))
      (insert contents))))


(defun ejira--org-heading-level ()
  (save-match-data
    (length
     (replace-regexp-in-string
      "^\\(\\**\\).*$" "\\1"
      (buffer-substring (line-beginning-position)
                        (line-end-position))))))

;;;###autoload
(defun ejira-push-description ()
  (interactive)
  (ejira-with-narrow-to-issue-under-point
   (let ((contents (ejira--get-subitem-contents "Description"))

         (level (save-excursion
                  (goto-char (ejira-find-headline-in-visible "Description"))
                  (ejira--org-heading-level))))

     (ejira--set-subitem-contents "Description"

                                  (ejira-parse-body
                                   (ejira-org-to-jira contents) level)))))

(defun ejira--get-comment-header (author contents)
  "Parse a header message for comment. AUTHOR: + firts 60 chars of CONTENTS."
  (let* ((msg (concat author ": "
                      (first (split-string (s-trim contents) "\n")))))
    (if (> (length msg) 63)
        (concat (substring msg 0 60) "...")
      msg)))

(defun ejira--update-body (pos contents &optional level-offset)
  "Replace body of header at POS with CONTENTS if changed."
  (save-excursion
    (goto-char pos)

    (let ((level (+ (save-excursion
                      (save-match-data
                        (search-forward-regexp "^\\**" (line-end-position) t)
                        (length (or (match-data) ""))))
                    (or level-offset 0))))

      (save-restriction
        (org-narrow-to-subtree)

        ;; Skip any property- and clock-drawers if found.
        (search-forward-regexp org-deadline-line-regexp nil t)
        (search-forward-regexp org-property-drawer-re nil t)
        (search-forward-regexp org-clock-drawer-re nil t)

        ;; Ensure that point is on the second line of the narrowed region.
        (end-of-line)
        (if (eobp)
            (newline)
          (beginning-of-line 2))

        ;; Ensure that there are no extra newlines after the body.
        (let ((body-begin (point))
              (body-end (or (save-excursion
                              (goto-char (point-max))
                              (unless (org-with-wide-buffer
                                       (looking-at "$"))
                                (insert "\n")
                                (end-of-line 0)
                                (point)))
                            (point-max))))
          (narrow-to-region body-begin body-end))

        ;; Now we have the whole buffer dedicated to the description.
        (let ((current-contents (buffer-string))
              (new-contents (ejira-parse-body contents level)))
          (unless (equal current-contents new-contents)
            (delete-region (point-min) (point-max))
            (insert new-contents)))))))

(defun ejira--get-body ()
  "Get body of the header under point."
  (save-excursion

    (save-restriction
      (org-narrow-to-subtree)

      ;; Skip any property- and clock-drawers if found.
      (search-forward-regexp org-deadline-line-regexp nil t)
      (search-forward-regexp org-property-drawer-re nil t)
      (search-forward-regexp org-clock-drawer-re nil t)

      ;; Ensure that point is on the second line of the narrowed
      ;; region.
      (end-of-line)
      (if (eobp)
          (newline)
        (beginning-of-line 2))

      (narrow-to-region (point) (point-max))

      ;; Now we have the whole buffer dedicated to the description.
      (buffer-string))))

;;;###autoload
(defun ejira-push-issue-under-point ()
  "Push the summary and description of the issue under point to the server."
  (interactive)
  (ejira-with-narrow-to-issue-under-point
   (let ((issue-id (ejira-get-id-under-point))
         (summary (strip-text-properties
                          (org-get-heading t t t t)))
         (description (ejira--get-subitem-contents "Description")))

     (jiralib2-update-summary-description
      issue-id summary (ejira-org-to-jira description)))))

;; A modified version of org-find-exact-headline-in-buffer which does not widen
;; the buffer.
(defun ejira-find-headline-in-visible (heading &optional buffer pos-only)
  "Find node HEADING in BUFFER.
Return a marker to the heading if it was found, or nil if not.
If POS-ONLY is set, return just the position instead of a marker.

The heading text must match exact, but it may have a TODO keyword,
a priority cookie and tags in the standard locations."
  (with-current-buffer (or buffer (current-buffer))
    (goto-char (point-min))
    (let (case-fold-search)
      (when (re-search-forward
	     (format org-complex-heading-regexp-format
		     (regexp-quote heading))
             nil t)
	(if pos-only
	    (match-beginning 0)
	  (move-marker (make-marker) (match-beginning 0)))))))

;;;###autoload
(defun ejira-add-comment ()
  "Add a comment at the end of the issue under point."
  (interactive)
  (ejira-with-narrow-to-issue-under-point
   (let* ((capture-template
           (org-with-wide-buffer
            (let ((refile-target '("Comments")))
              (condition-case nil
                  (while t
                    (setq refile-target (cons
                                         (format "%s" (org-get-heading t t t t))
                                         refile-target))
                    (org-up-element))
                (error nil))
              (list
               "x" "Comment" 'entry
               (cons 'file+olp (cons (buffer-file-name) refile-target))
               "* New comment:\n%?"))))
          (org-capture-templates
           (cons capture-template org-capture-templates)))
     (org-capture nil "x"))))

;;;###autoload
(defun ejira-create-isssue ()
  "Create a new JIRA ticket with `org-capture`. into project PROJECT."
  (interactive)
  (let* ((project-name (ejira--select-project t))
         (capture-template (list
                            "x" "Issue" 'entry
                            (cons 'file (cons (concat ejira-my-org-directory "/"
                                                      project-name ".org")
                                              nil))
                            "* %?
:PROPERTIES:
:ID:  nil
:END:
* Description
"))


         (org-capture-templates
          (cons capture-template org-capture-templates)))
    (org-capture nil "x")))

(defun ejira--sync-new-comment ()
  "Synchronize the newly created comment after successful `org-capture'."
  (unless org-note-abort
    (save-excursion
      (goto-char (point-min))
      (save-restriction
        (when (equal (org-get-heading t t t t) "New comment:")
          (let ((issue-id (org-with-wide-buffer (ejira-get-id-under-point)))
                (body (ejira-org-to-jira (s-trim (ejira--get-body)))))
            (let ((comment (ejira-parse-comment (jiralib2-add-comment issue-id body))))

              (org-set-property "ID" (jira-comment-id comment))
              (org-set-property "Author" (jira-comment-author comment))
              (org-set-property "Created" (format-time-string
                                           "%Y-%m-%d %H:%M:%S"
                                           (jira-comment-created comment)
                                           "UTC"))

              (save-excursion
                (ejira-update-header-text (ejira--get-comment-header
                                           (jira-comment-author comment)
                                           (jira-comment-body comment))))
              (let ((created (jira-comment-created comment))
                    (updated (jira-comment-updated comment)))

                (when (not (equal created updated))
                  (org-set-property "Modified" (format-time-string
                                                "%Y-%m-%d %H:%M:%S"
                                                updated "UTC"))))

              (ejira--update-body (point-marker) (jira-comment-body comment)))))))))

(add-hook 'org-capture-prepare-finalize-hook #'ejira--sync-new-comment)

;;;###autoload
(defun ejira-delete-comment-under-point ()
  "Delete comment under point."
  (interactive)
  (let ((comment-id (ejira-get-id-under-point t))
        (issue-id (ejira-get-id-under-point)))
    (goto-char (org-id-find-id-in-file comment-id (buffer-file-name) t))
    (when (jiralib2-delete-comment issue-id comment-id)
      (org-cut-subtree))))

(defun ejira-property-value (key)
  "List all non-nil values of property KEY in current visble buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (re (org-re-property key))
	  values)
      (when (re-search-forward re nil t)
        (org-entry-get (point) key)))))

(defun ejira-get-id-under-point (&optional include-comment)
  "Get ID of the ticket under point.
With INCLUDE-COMMENT as t, include also numeric id's."
  (save-excursion
    (catch 'id-tag
      (while t
        (save-restriction
          (org-narrow-to-subtree)
          (let ((found-id (ejira-property-value "ID")))
            (when (and found-id (or include-comment
                                    (not (s-numeric-p found-id))))
              (throw 'id-tag found-id))))
        (org-up-element)))))

;;;###autoload
(defun ejira-update-current-issue ()
  "Update current issue details."
  (interactive)
  (ejira-update-issue (or (ejira-get-id-under-point)
                          (error "Not on a ticket"))))


(defun ejira-refile (project-id epic-id)
  "Refile the issue under epic EPIC-ID in file PROJECT-ID."
  (org-with-wide-buffer
   (org-refile nil nil
               (list "" (ejira-project-file-name project-id) ""
                     (org-id-find-id-in-file
                      epic-id (ejira-project-file-name project-id) t)))))

(defun ejira-extract-value (l &rest keys)
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

;;;###autoload
(defun ejira-focus-on-current-issue ()
  "And narrow to item under point, and expand it."
  (interactive)
  (ejira-focus-on-issue (ejira-get-id-under-point)))
;; (widen)
;; (goto-char (ejira-with-narrow-to-issue-under-point (point-marker)))
;; (org-narrow-to-subtree)
;; (org-show-subtree))

;; (defun ejira-focus-on-issue (issue-key)
;;   "Move point to issue ISSUE-KEY and narrow to it."
;;   (let ((m (ejira-with-narrow-to-issue issue-key (point-marker))))
;;     (switch-to-buffer (marker-buffer m))
;;     (widen)
;;     (goto-char m)
;;     (org-narrow-to-subtree)
;;     (org-show-subtree)))

(defun ejira-focus-on-issue (issue-key)
  "Open an indirect buffer narrowed to issue ISSUE-KEY."
  (let* ((m (or (org-id-find-id-in-file issue-key (ejira-project-file-name
                                                   (ejira--guess-project-key issue-key))
                                        t)
                (error (concat "no issue: " issue-key))))
         (m-buffer (marker-buffer m))
         (buffer-name (concat "*" issue-key "*"))
         (b (or (get-buffer buffer-name)
                (make-indirect-buffer m-buffer (concat "*" issue-key "*") t))))
    (switch-to-buffer b)
    (outline-show-all)
    (widen)
    (goto-char m)
    (org-narrow-to-subtree)
    (outline-show-subtree)
    (ejira-mode 1)))

(defun ejira-close-buffer ()
  "Close the current buffer viewing issue details."
  (interactive)
  (kill-buffer (current-buffer))

  ;; Because we are using indirect buffers, killing current buffer will not go
  ;; back to the previous buffer, but instead to the corresponding direct
  ;; buffer. Switching to previous buffer here does the trick.
  (switch-to-prev-buffer))

(define-minor-mode ejira-mode
  "Ejira Mode"
  "Minor mode for managing JIRA ticket in a narrowed org buffer."
  :init-value nil
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-S-q") #'ejira-close-buffer)
            map))

;;;###autoload
(defun ejira-focus-on-clocked-issue ()
  "Goto current or last clocked item, and narrow to it, and expand it."
  (interactive)
  (org-clock-goto)
  (ejira-focus-on-issue (ejira-get-id-under-point)))
;; (org-narrow-to-subtree)
;; (org-show-subtree))

(defvar ejira-narrow-to-issue-from-agenda t)
(defun ejira--focus-advice ()
  "Narrow and expand the issue selected from `org-agenda'."
  (when ejira-narrow-to-issue-from-agenda
    (ejira-focus-on-current-issue)))
(advice-add 'org-agenda-switch-to :after #'ejira--focus-advice)

;;;###autoload
(defun ejira-progress-current-issue ()
  "Progress the issue under point."
  (interactive)
  (ejira-with-narrow-to-issue-under-point
   (let* ((actions (jiralib2-get-actions current-issue))
          (selected (rassoc (completing-read "Action: " (mapcar 'cdr actions))
                            actions)))
     (jiralib2-do-action current-issue (car selected))
     (ejira-update-current-issue))))

(defun ejira--select-project (all-projects)
  "Select a project.
With prefix ALL-PROJECTS do not limit the selection to projects configured in
`ejira-projects`."
  (interactive "P")
  (let* ((projects (jiralib2-get-projects))
         (selected-projects
          (if all-projects
              projects
            (remove-if-not
             (lambda (m) (member (alist-get 'key m) ejira-projects))
             projects))))

    (first (split-string
            (completing-read
             "Select project: "
             (mapcar (lambda (m)
                       (format "%-15s %s"
                               (propertize (alist-get 'key m) 'face
                                           'font-lock-comment-face)
                               (alist-get 'name m)))
                     selected-projects)) " "))))

(defun ejira--select-issuetype ()
  (first (split-string
          (completing-read
           "Issue type: "
           (mapcar (lambda (m)
                     (format "%-7s\t%-15s\t%s"
                             (propertize (alist-get 'id m) 'face
                                         'font-lock-comment-face)
                             (alist-get 'name m)
                             (alist-get 'description m)

                             ))
                   (jiralib2-get-issuetypes))) "\t" t "\s*")))

(defun ejira-log-work (issue-id timestamp amount)
  "Log AMOUNT of work to issue ISSUE-ID.
TIMESTAMP and AMOUNT are in `org-clock'-format."
  (jiralib2-add-worklog issue-id (ejira-org-time-to-seconds amount)
                        (ejira-org-timestamp-to-jira timestamp)
                        "test"))

(defun ejira-insert-link-to-current-issue ()
  "Insert link to currently clocked issue into buffer."
  (interactive)
  (let ((issue-id (save-current-buffer
                    (save-window-excursion
                      (save-excursion
                        (save-restriction
                          (org-clock-goto)
                          (ejira-get-id-under-point)))))))
    (insert (format "%s/browse/%s" jiralib2-url issue-id))))

(defvar ejira-agenda-overview
  '(agenda "" ((org-agenda-overriding-header "Sprint's Schedule:")
	       (org-agenda-span 'week)
	       (org-agenda-ndays 5)
	       (org-agenda-start-on-weekday 1)
	       (org-agenda-todo-ignore-deadlines nil))))

(defvar ejira-agenda-my-issues
  '(tags (concat "Assigned+" (ejira-current-sprint-tag))
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

(defvar ejira-sprint-agenda
  `("s" "Active Sprint" (,ejira-agenda-overview
                         ,ejira-agenda-my-issues
                         ,ejira-agenda-sprint-content))
  "`org-agenda' custom command for current sprint schedule.")


(provide 'ejira)
;;; ejira.el ends here
