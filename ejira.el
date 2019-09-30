;;; ejira.el --- org-mode interface to JIRA

;; Copyright (C) 2017 - 2019 Henrik Nyman

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
;; - Sprint handling
;; - Attachments
;; - Refile to an issue in current sprint

;;; Code:

(require 'org)
(require 'dash-functional)
(require 'ejira-core)



(defvar ejira-push-deadline-changes t
  "Sync deadlines to server when updated with `ejira-set-deadline'.")

(defun ejira-add-comment (to-clocked)
  "Capture new comment to issue under point.
With prefix-argument TO-CLOCKED add comment to currently clocked issue."
  (interactive "P")
  (ejira--capture-comment (if to-clocked
                              (ejira--get-clocked-issue)
                            (ejira-issue-id-under-point))))

(defun ejira-delete-comment ()
  "Delete comment under point."
  (interactive)
  (let* ((item (ejira-get-id-under-point "ejira-comment"))
         (id (nth 1 item)))
    (when (y-or-n-p (format "Delete comment %s? " (cdr id)))
      (ejira--delete-comment (car id) (cdr id)))))

(defun ejira-pull-item-under-point ()
  "Update the issue, project or comment under point."
  (interactive)
  (let* ((item (ejira-get-id-under-point))
         (id (nth 1 item))
         (type (nth 0 item)))
    (cond ((equal type "ejira-comment")
           (ejira--update-comment
            (car id) (ejira--parse-comment (jiralib2-get-comment (car id) (cdr id)))))
          ((equal type "ejira-project")
           (ejira--update-project id))
          (t
           (ejira--update-task id)))))

(defun ejira-push-item-under-point ()
  "Upload content of issue, project or comment under point to server.
For a project, this includes the summary, for a task the summary and
description, and for the comment the body."
  (interactive)
  (let* ((item (ejira-get-id-under-point))
         (id (nth 1 item))
         (type (nth 0 item)))
    (cond ((equal type "ejira-comment")
           (jiralib2-edit-comment
            (car id) (cdr id)
            (ejira-parser-org-to-jira
             (ejira--get-heading-body
              (nth 2 item)))))
          ((equal type "ejira-project")
           (message "TODO"))
          (t
           (jiralib2-update-summary-description
            id
            (ejira--with-point-on id
              (ejira--strip-properties (org-get-heading t t t t)))
            (ejira-parser-org-to-jira
             (ejira--get-heading-body
              (ejira--find-task-subheading id ejira-description-heading-name))))))))

(defun ejira--heading-to-item (heading project-id type &rest args)
  "Create an item from HEADING of TYPE into PROJECT-ID with parameters ARGS."
  (let* ((summary (ejira--strip-properties (org-get-heading t t t t)))
         (description (ejira-parser-org-to-jira (ejira--get-heading-body heading)))
         (item (ejira--parse-item
                (apply #'jiralib2-create-issue project-id
                       type summary description args))))

    (ejira--update-task (ejira-task-key item))
    (ejira-task-key item)))

(defun ejira-heading-to-task (focus)
  "Make the current org-heading into a JIRA task.
With prefix argument FOCUS, focus the issue after creating."
  (interactive "P")
  (let* ((heading (save-excursion
                    (if (outline-on-heading-p t)
                        (beginning-of-line)
                      (outline-back-to-heading))
                    (point-marker)))
         (project-id (ejira--select-project))
         (key (when project-id (ejira--heading-to-item heading project-id "Task"))))

    (when (and key focus)
      (ejira-focus-on-issue key))))

(defun ejira-heading-to-subtask (focus)
  "Make the current org-heading into a JIRA subtask.
With prefix argument FOCUS, focus the issue after creating."
  (interactive "P")
  (let* ((heading (save-excursion
                    (if (outline-on-heading-p t)
                        (beginning-of-line)
                      (outline-back-to-heading))
                    (point-marker)))
         (story (ejira--select-story))
         (project-id (ejira--get-project story))
         (key (ejira--heading-to-item heading project-id ejira-subtask-type-name
                                      `(parent . ((key . ,story))))))
    (when focus
      (ejira-focus-on-issue key))))

(defun ejira-update-project (id &optional shallow)
  "Update all issues in project ID.
If DEEP set to t, update each issue with separate API call which pulls also
comments. With SHALLOW, only update todo status and assignee."
  (ejira--update-project id)

  ;; First, update all items that are marked as unresolved.
  ;;
  ;; Handles cases:
  ;; *local*    | *remote*
  ;; ===========+===========
  ;;            | unresolved
  ;; unresolved | unresolved
  ;; resolved   | unresolved
  ;;
  (mapc (lambda (i) (if shallow
                        (ejira--update-task-light
                         (ejira--alist-get i 'key)
                         (ejira--alist-get i 'fields 'status 'name)
                         (ejira--alist-get i 'fields 'assignee 'displayName))
                      (ejira--update-task (ejira--parse-item i))))
        (apply #'jiralib2-jql-search
               (format "project = %s and resolution = unresolved" id)
               (ejira--get-fields-to-sync shallow)))

  ;; Then, sync any items that are still marked as unresolved in our local sync,
  ;; but are already resolved at the server. This should ensure that there are
  ;; no hanging todo items in our local sync.
  ;;
  ;; Handles cases:
  ;; *local*    | *remote*
  ;; ===========+===========
  ;; unresolved | resolved
  ;;
  (mapc (lambda (i) (if shallow
                        (ejira--update-task-light
                         (ejira--alist-get i 'key)
                         (ejira--alist-get i 'fields 'status 'name)
                         (ejira--alist-get i 'fields 'assignee 'displayName))
                      (ejira--update-task (ejira--parse-item i))))
        (apply #'jiralib2-jql-search
               (format "project = %s and key in (%s) and resolution = done"
                       id (s-join ", " (mapcar #'car (ejira--get-headings-in-file
                                                      (ejira--project-file-name id)
                                                      '(:todo "todo")))))
               (ejira--get-fields-to-sync shallow)))

  ;; TODO: Handle issue being deleted from server:
  ;; *local*    | *remote*
  ;; ===========+===========
  ;; unresolved |
  ;; resolved   |
  )

;;;###autoload
(defun ejira-update-my-projects (&optional shallow)
  "Synchronize data on projects listed in `ejira-projects'.
With prefix argument SHALLOW, update only the todo state and assignee."
  (interactive "P")
  (mapc (-rpartial #'ejira-update-project shallow) ejira-projects)
  (message "ejira: operation finihed"))

;;;###autoload
(defun ejira-set-deadline (arg &optional time)
  "Wrapper around `org-deadline' which pushes the changed deadline to server.
ARG and TIME get passed on to `org-deadline'."
  (interactive "P")
  (ejira--with-point-on (ejira-issue-id-under-point)
    (org-deadline arg time)
    (when ejira-push-deadline-changes
      (let ((deadline (org-get-deadline-time (point-marker))))
        (jiralib2-update-issue (ejira-issue-id-under-point)
                               `(duedate . ,(when deadline
                                              (format-time-string "%Y-%m-%d"
                                                                  deadline))))))))

;;;###autoload
(defun ejira-set-priority ()
  "Set priority of the issue under point."
  (interactive)
  (ejira--with-point-on (ejira-issue-id-under-point)
    (let ((p (completing-read "Priority: "
                              (mapcar #'car ejira-priorities-alist))))
      (jiralib2-update-issue (ejira-issue-id-under-point)
                             `(priority . ((name . ,p))))
      (org-priority (alist-get p ejira-priorities-alist nil nil #'equal)))))

;;;###autoload
(defun ejira-assign-issue (&optional to-me)
  "Set the assignee of the issue under point.
With prefix-argument TO-ME assign to me."
  (interactive "P")
  (ejira--assign-issue (ejira-issue-id-under-point) to-me))

;;;###autoload
(defun ejira-progress-issue ()
  "Progress the issue under point with a selected action."
  (interactive)
  (ejira--progress-item (ejira-issue-id-under-point)))

;;;###autoload
(defun ejira-set-issuetype ()
  "Select a new issuetype for the issue under point."
  (interactive)
  (let* ((id (ejira-get-id-under-point nil t))
         (ejira-type (nth 0 id))
         (key (if (equal ejira-type "ejira-comment")
                  (user-error "Cannot set type of comment")
                (nth 1 id)))
         (type (ejira--select-issuetype)))
    (jiralib2-set-issue-type key  type)
    (ejira--update-task key)))

;;;###autoload
(defun ejira-set-epic ()
  "Select a new epic for issue under point."
  (interactive)
  (ejira--set-epic (ejira-issue-id-under-point)
                   (ejira--select-id-or-nil
                    "Select epic: "
                     (ejira--get-headings-in-agenda-files :type "ejira-epic"))))

;;;###autoload
(defun ejira-focus-on-issue (key)
  "Open an indirect buffer narrowed to issue KEY."
  (interactive)
  (let* ((m (or (ejira--find-heading key)
                (error (concat "no issue: " key))))
         (m-buffer (marker-buffer m))
         (buffer-name (concat "*" key "*"))
         (b (or (get-buffer buffer-name)
                (make-indirect-buffer m-buffer (concat "*" key "*") t))))
    (switch-to-buffer b)
    (widen)
    (outline-show-all)
    (goto-char m)
    (org-narrow-to-subtree)
    (outline-show-subtree)
    (ejira-mode 1)))

;;;###autoload
(defun ejira-focus-on-clocked-issue ()
  "Goto current or last clocked item, and narrow to it, and expand it."
  (interactive)
  (ejira-focus-on-issue (ejira--get-clocked-issue)))


(defun ejira-close-buffer ()
  "Close the current buffer viewing issue details."
  (interactive)
  (kill-buffer (current-buffer))

  ;; Because we are using indirect buffers, killing current buffer will not go
  ;; back to the previous buffer, but instead to the corresponding direct
  ;; buffer. Switching to previous buffer here does the trick.
  ;; (switch-to-prev-buffer)
  )

(defun ejira-insert-link-to-clocked-issue ()
  "Insert link to currently clocked issue into buffer."
  (interactive)
  (insert (format "%s/browse/%s" jiralib2-url (ejira--get-clocked-issue))))

;;;###autoload
(defun ejira-focus-item-under-point ()
  "And narrow to item under point, and expand it."
  (interactive)
  (ejira-focus-on-issue (ejira-issue-id-under-point)))

;;;###autoload
(defun ejira-focus-up-level ()
  "Try to focus the parent item of the item under point."
  (interactive)
  (ejira-focus-on-issue
   (ejira--with-point-on (ejira-issue-id-under-point)
     (org-up-element)
     (ejira-issue-id-under-point))))

(define-minor-mode ejira-mode
  "Ejira Mode"
  "Minor mode for managing JIRA ticket in a narrowed org buffer."
  :init-value nil
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c q") #'ejira-close-buffer)
            (define-key map (kbd "C-c C-d") #'ejira-set-deadline)
            (define-key map (kbd "C-c ,") #'ejira-set-priority)
            ;; (define-key map (kbd "C-c C-t") #'ejira-progress-issue)
            map))

(defun ejira-guess-epic-sprint-fields ()
  "Try to guess the custom field names for epic and sprint."
  (interactive)
  (message "Attempting to auto-configure Ejira custom fields...")
  (let* ((epic-key (alist-get 'key (nth 0 (jiralib2-jql-search
                                           (format "type = %s"
                                                   ejira-epic-type-name)
                                           "key"))))
         (issue-key (alist-get 'key (nth 0 (jiralib2-jql-search
                                            (format "type != %s"
                                                    ejira-epic-type-name)
                                            "key"))))
         (epic-meta (jiralib2-session-call
                     (format "/rest/api/2/issue/%s/editmeta" epic-key)))
         (issue-meta (jiralib2-session-call
                      (format "/rest/api/2/issue/%s/editmeta" epic-key)))

         (epic-field (caar (-filter (lambda (field)
                                      (equal (alist-get 'name field) "Epic Link"))
                                    (alist-get 'fields epic-meta))))
         (sprint-field (caar (-filter (lambda (field)
                                        (equal (alist-get 'name field) "Sprint"))
                                      (alist-get 'fields issue-meta))))
         (epic-summary-field (caar (-filter (lambda (field)
                                              (equal (alist-get 'name field) "Epic Name"))
                                            (alist-get 'fields epic-meta)))))
    (setq ejira-epic-field epic-field
          ejira-epic-summary-field epic-summary-field
          ejira-sprint-field sprint-field)
    (message "Successfully configured custom fields")))

(provide 'ejira)
;;; ejira.el ends here
