;;; helm-ejira.el --- Helm-completion for ejira.el

;; Copyright (C) 2017 Henrik Nyman

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
                 (when (or (not helm-ejira-limit-to-tag)
                           (s-contains-p helm-ejira-limit-to-tag
                                         (or tags "")))
                     
                     (format "%s%s%s"
                             left-side
                             (make-string
                              (max 0 (- width (length left-side) (length right-side)))
                              ? )
                             right-side))))
             issues-list))))


(defun helm-ejira-invalidate-cache ()
  "Make the next Helm access to load the content from files instead of cache."
  (setq helm-ejira-cache-dirty t))


(defun helm-ejira--get-candidates-in-file (filename &optional fontify
                                                    nofname parents)
  (with-current-buffer (pcase filename
                         ((pred bufferp) filename)
                         ((pred stringp) (find-file-noselect filename t)))
    (let ((match-fn (if fontify
                        #'match-string
                      #'match-string-no-properties))
          (search-fn (lambda ()
                       (re-search-forward
                        org-complex-heading-regexp nil t)))
          (file (unless nofname
                  (concat (helm-basename filename) ":"))))
      (when parents
        (add-function :around (var search-fn)
                      (lambda (old-fn &rest args)
                                (when (org-up-heading-safe)
                                  (apply old-fn args)))))
      (save-excursion
        (save-restriction
          (unless (and (bufferp filename)
                       (buffer-base-buffer filename))
            ;; Only widen direct buffers, not indirect ones.
            (widen))
          (unless parents (goto-char (point-min)))
          ;; clear cache for new version of org-get-outline-path
          (and (boundp 'org-outline-path-cache)
               (setq org-outline-path-cache nil))
          (remove nil
                  (cl-loop with width = (window-width (helm-window))
                           while (funcall search-fn)
                           for beg = (point-at-bol)
                           for end = (point-at-eol)
                           when (and fontify
                                     (null (text-property-any
                                            beg end 'fontified t)))
                           do (jit-lock-fontify-now beg end)
                           for level = (length (match-string-no-properties 1))
                           for heading = (funcall match-fn 4)
                           if (and (>= level helm-org-headings-min-depth)
                                   (<= level helm-org-headings-max-depth))
                           for key = (condition-case nil
                                         (save-excursion (ejira-get-id-under-point))
                                       (user-error nil))
                           for tags = (nth 5 (org-heading-components))
                           collect
                           (when (and (nth 2 (org-heading-components)) key)
                           (list
                            key
                            heading
                            (or tags ""))))))))))


(defun helm-ejira ()
  "Goto issue with helm search."
  (interactive)
  (helm :sources '(helm-source-ejira-issues)
        :buffer "*helm jira*"
        :prompt "JIRA Issue: "))

(defun helm-ejira-sprint ()
  "Goto issue with helm search. Limit results to issues in active sprint."
  (interactive)
  (let ((helm-ejira-limit-to-tag (ejira-current-sprint-tag)))
    (helm :sources '(helm-source-ejira-issues)
          :buffer "*helm jira*"
          :prompt "JIRA Issue: ")))


(provide 'helm-ejira)
;;; helm-ejira.el ends here
