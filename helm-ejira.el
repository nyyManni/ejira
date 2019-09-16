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


(defvar helm-ejira-cache-dirty t
  "Flag indicating that the issue cache needs to be refreshed.")

(defvar helm-source-ejira-issues
  (helm-build-sync-source "JIRA issues"
    :candidates 'helm-ejira-issues
    :fuzzy-match helm-ejira-fuzzy-match
    :action (lambda (candidate)
              (let ((issue-key (first (split-string candidate))))
                (ejira-focus-on-issue issue-key)))))


(defvar helm-ejira-limit-to-tag nil)
(defvar helm-ejira-limit-to-assigned nil)

(setq helm-ejira-issue-cache nil)
(defun helm-ejira-issues ()
  "Fetch all the issues from org agenda files."
  (let ((issues-list
          (if (or (not helm-ejira-issue-cache)
                  helm-ejira-cache-dirty)
              (prog1
                  (setq helm-ejira-issue-cache
                        (apply #'append
                               (mapcar #'helm-ejira--get-candidates-in-file
                                       (org-agenda-files))))
                (setq helm-ejira-cache-dirty nil))
            helm-ejira-issue-cache))
        (width (window-width (helm-window))))
    (remove nil
            (mapcar
             (lambda (issue)
               (let* ((key (nth 0 issue))
                      (heading (nth 1 issue))
                      (tags (nth 2 issue))

                      (left-side (format "%-15s %s"
                                         (propertize key 'face
                                                     'font-lock-comment-face)
                                         heading))
                      (right-side (propertize (or tags "") 'face
                                              'font-lock-type-face)))
                 (when (and (or (not helm-ejira-limit-to-tag)
                                (s-contains-p helm-ejira-limit-to-tag
                                              (or tags "")))
                            (or (not helm-ejira-limit-to-assigned)
                                (s-contains-p "Assigned" (or tags ""))))
                     (format "%s%s%s"
                             left-side
                             (make-string
                              (max 0 (- width (length left-side)
                                        (length right-side)))
                              ? )
                             right-side))))
             issues-list))))


;;;###autoload
(defun helm-ejira-invalidate-cache ()
  "Make the next Helm access to load the content from files instead of cache."
  (interactive)
  (setq helm-ejira-cache-dirty t))


(defun helm-ejira--get-candidates-in-file (filename)
  "Get all JIRA issue candidates from org file FILENAME."
  (with-current-buffer (pcase filename
                         ((pred bufferp) filename)
                         ((pred stringp) (find-file-noselect filename t)))
    (let ((match-fn #'match-string-no-properties)
          (search-fn (lambda ()
                       (re-search-forward
                        org-complex-heading-regexp nil t))))
      (save-excursion
        (save-restriction
          (unless (and (bufferp filename)
                       (buffer-base-buffer filename))
            ;; Only widen direct buffers, not indirect ones.
            (widen))
          (goto-char (point-min))
          ;; clear cache for new version of org-get-outline-path
          (and (boundp 'org-outline-path-cache)
               (setq org-outline-path-cache nil))
          (remove nil
                  (cl-loop with width = (window-width (helm-window))
                           while (funcall search-fn)
                           for beg = (point-at-bol)
                           for end = (point-at-eol)
                           do (jit-lock-fontify-now beg end)
                           for level = (length (match-string-no-properties 1))
                           for heading = (funcall match-fn 4)
                           if (and (>= level helm-org-headings-min-depth)
                                   (<= level helm-org-headings-max-depth))
                           for key = (condition-case nil
                                         (save-excursion
                                           (nth 1 (ejira-get-id-under-point nil t)))
                                       (user-error nil))
                           for tags = (nth 5 (org-heading-components))
                           collect
                           (when (and (nth 2 (org-heading-components)) key)
                             `(,key ,heading ,(or tags ""))))))))))

;;;###autoload
(defun helm-ejira (&optional prefix)
  "Goto issue with helm search. With PREFIX argument, first invalidate cache."
  (interactive "P")
  (when prefix
    (helm-ejira-invalidate-cache))
  (helm :sources '(helm-source-ejira-issues)
        :buffer "*helm jira*"
        :prompt "JIRA Issue: "))

;;;###autoload
(defun helm-ejira-sprint (&optional prefix)
  "Goto issue with helm search. Limit results to issues in active sprint.
With PREFIX argument, first invalidate cache"
  (interactive "P")
  (when prefix
    (helm-ejira-invalidate-cache))
  (let ((helm-ejira-limit-to-tag (ejira-current-sprint-tag)))
    (helm :sources '(helm-source-ejira-issues)
          :buffer "*helm jira*"
          :prompt "JIRA Issue: ")))

;;;###autoload
(defun helm-ejira-sprint-assigned (&optional prefix)
  "Goto issue with helm search.
Limit the results to issues in active sprint and assigned to me. With PREFIX
argument, first invalidate cache."
  (interactive "P")
  (when prefix
    (helm-ejira-invalidate-cache))

  (let ((helm-ejira-limit-to-tag (ejira-current-sprint-tag))
        (helm-ejira-limit-to-assigned t))
    (helm :sources '(helm-source-ejira-issues)
          :buffer "*helm jira*"
          :prompt "JIRA Issue: ")))


(provide 'helm-ejira)
;;; helm-ejira.el ends here
