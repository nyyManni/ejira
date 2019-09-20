;;; ejira.el --- org-mode interface to JIRA

;; Copyright (C) 2017 - 2019 Henrik Nyman

;; Author: Henrik Nyman <h@nyymanni.com>
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
;; - Sprint handling
;; - Attachments
;; - Update issues in current sprint should update old open tickets, they are
;;   most likely closed, and deadlines are dnagling in agenda.
;; - Refile to an issue in current sprint
;; - Select Epic interactively
;; - Subtask creation/refile
;; - Blockers and triggers with org-depend

;;; Code:

(require 'org)
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
            (ejira-org-to-jira
             (ejira--get-heading-body
              (nth 2 item)))))
          ((equal type "ejira-project")
           (message "TODO"))
          (t
           (jiralib2-update-summary-description
            id
            (ejira--with-point-on id
              (ejira--strip-properties (org-get-heading t t t t)))
            (ejira-org-to-jira
             (ejira--get-heading-body
              (ejira--find-task-subheading id ejira-description-heading-name))))))))

(defun ejira-heading-to-task (focus)
  "Make the current org-heading into a JIRA task.
With prefix argument FOCUS, focus the issue after creating."
  (interactive "P")
  (let* ((heading (save-excursion
                    (if (outline-on-heading-p t)
                        (beginning-of-line)
                      (outline-back-to-heading))
                    (point-marker)))
         (summary (ejira--strip-properties (org-get-heading t t t t)))
         (description (ejira-org-to-jira (ejira--get-heading-body heading)))
         (project (ejira--select-project t))
         (item (ejira--parse-item (jiralib2-create-issue
                                   project "Task" summary description))))

    (ejira--update-task (ejira-task-key item))
    (ejira-focus-on-issue (ejira-task-key item))))

(defun ejira-update-project (id &optional deep)
  "Update all issues in project ID.
If DEEP set to t, update each issue with separate API call which pulls also
comments."
  (mapc
   (lambda (i)
     (ejira--update-task
      (if deep
          (ejira-task-key (ejira--parse-item i))
        (ejira--parse-item i))))
   (jiralib2-do-jql-search (format "project = %s" id))))

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
                (error (concat "no issue: " issue-key))))
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

(defun ejira-insert-link-to-current-issue ()
  "Insert link to currently clocked issue into buffer."
  (interactive)
  (insert (format "%s/browse/%s" jiralib2-url (ejira--get-clocked-issue))))

;;;###autoload
(defun ejira-focus-on-current-issue ()
  "And narrow to item under point, and expand it."
  (interactive)
  (ejira-focus-on-issue (ejira-issue-id-under-point)))

(define-minor-mode ejira-mode
  "Ejira Mode"
  "Minor mode for managing JIRA ticket in a narrowed org buffer."
  :init-value nil
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-S-q") #'ejira-close-buffer)
            (define-key map (kbd "C-c C-d") #'ejira-set-deadline)
            (define-key map (kbd "C-c ,") #'ejira-set-priority)
            ;; (define-key map (kbd "C-c C-t") #'ejira-progress-issue)
            map))

(provide 'ejira)
;;; ejira.el ends here
