;;; ejira-completion.el --- support for completions for ejira headings

;; Copyright (C) 2017-2023 Henrik Nyman

;; Author: Henrik Nyman <h@nyymanni.com>
;; URL: https://github.com/nyyManni/ejira
;; Keywords: jira, org,

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

;; Common completion features for ejira.

;; Examples:

;; (ejira-completion-find-issue "select issue: " :tags `(,ejira-assigned-tagname))
;; (ejira-completion-find-issue "select issue: " :type '("ejira-issue"))

;;; Code:

(require 'ejira-core)



(defvar ejira-completion-group-by-project t
  "When set to t the completion results are grouped by the project title.

Set to nil to disable grouping.")

(defun ejira-completion--pack-for-completion (issue)
  "Pack the ISSUE (id title tags type) into a single string."
  (concat (nth 0 issue) "\t" (nth 1 issue) "\t" (or (nth 3 issue) "")))

(defun ejira-completion--unpack-from-completion (str)
  "Unpack the issue id from STR which was created with `ejira-completion--pack-for-completion'."
  (nth 0 (split-string str "\t")))

(defun ejira-completion--format-for-completion (str)
  "Apply string properties for displaying the candidate STR to user."
  (let ((i (split-string str "\t")))
    `(,(concat
        (propertize (concat (nth 0 i) "\t") 'face 'warning)
        (nth 1 i))
      ""
      ,(concat "     " (propertize (nth 2 i) 'face 'shadow)))))

(defun ejira-completion--affixation-function (seq)
  "Completing-read affixation function.

Applies formatting to SEQ."
  (mapcar #'ejira-completion--format-for-completion seq))

(defun ejira-completion--group-function (completion transform)
  "Completing-read grouping function.

COMPLETION is returned as is, and if TRANSFORM into group name is requested the
project title is returned as a group identifier."
  (when ejira-completion-group-by-project
    (if transform
        completion
      (ejira--get-project-title (nth 0 (split-string completion "-"))))))

(defun ejira-completion-find-issue (prompt &rest args)
  "Find matching issue with Emacs completion system.

PROMPT is shown to the user as the completion promt. ARGS are given to
`ejira--get-headings-in-agenda-files' as filters."
  (let ((candidates (mapcar
                     #'ejira-completion--pack-for-completion
                     (apply #'ejira--get-headings-in-agenda-files args))))

    (ejira-completion--unpack-from-completion
     (completing-read
      prompt
      (lambda (str pred flag)

        (pcase flag
          ('metadata `(metadata
                       (affixation-function . ejira-completion--affixation-function)
                       (group-function . ejira-completion--group-function)))
          (_ (all-completions str candidates pred))))))))

(setq ejira--project-title-cache nil)
(defun ejira--get-project-title (key)
  "Fetch the project name for a proejct KEY.

The resulting string is cached in an alist. This makes this operation fast
enough to be used as a grouping criteria for the completion."
  (or (alist-get key ejira--project-title-cache nil nil #'equal)
      (setf (alist-get key ejira--project-title-cache nil nil #'equal)
            (ejira--with-point-on key
              (ejira--strip-properties (org-get-heading t t t t))))))

;;;###autoload
(defun ejira-completion-focus-issue ()
  "Focus on an issue using minibuffer completion."
  (interactive)
  (ejira-focus-on-issue
   (ejira-completion-find-issue "Issue: " :type '("ejira-issue"
                                                  "ejira-story"
                                                  "ejira-epic"
                                                  "ejira-subtask"))))

;;;###autoload
(defun ejira-completion-focus-issue-active-sprint ()
  "Focus on an issue using minibuffer completion.

Search only from issues that are in the currently active sprint."
  (interactive)
  (ejira-focus-on-issue
   (ejira-completion-find-issue "Issue: "  :tags `(,(ejira-current-sprint-tag)) )))

;;;###autoload
(defun ejira-completion-focus-issue-assigned ()
  "Focus on an issue using minibuffer completion.

Search only from issues that are assigned to you."
  (interactive)
  (ejira-focus-on-issue
   (ejira-completion-find-issue "Issue: "  :tags `(,ejira-assigned-tagname))))

(provide 'ejira-completion)
;;; ejira-completion.el ends here
