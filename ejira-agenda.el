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

;; Provides org-agenda commands for ejira

;;; Code:
(require 's)
(require 'ejira-core)
(require 'ejira-agile)
(require 'org-agenda)
(require 'jiralib2)

(defvar ejira-narrow-to-issue-from-agenda t
  "When set, pressing <RET> in agenda opens the issue in an indirect buffer.")

(declare-function ejira-focus-item-under-point "ejira.el")
(declare-function ejira-progress-issue "ejira.el")

(defvar ejira-narrow-to-issue-from-agenda t)
(defun ejira--focus-advice ()
  "Narrow and expand the issue selected from `org-agenda'."
  (when (and ejira-narrow-to-issue-from-agenda
             (s-starts-with-p "ejira-" (org-entry-get (point-marker) "TYPE")))
    (ejira-focus-item-under-point)))
(advice-add 'org-agenda-switch-to :after #'ejira--focus-advice)
(advice-add 'org-agenda-goto :after #'ejira--focus-advice)

(defun ejira-agenda--format-item (key)
  "Format the heading with ID KEY."
  (let ((marker (or (ejira--find-heading key)
                    (progn
                      (ejira--update-task key)
                      (ejira--find-heading key)))))
    (when marker
      (org-with-point-at marker
        (let ((props (list 'face 'default
		           'done-face 'org-agenda-done
		           'undone-face 'default
		           'mouse-face 'highlight
		           'org-not-done-regexp org-not-done-regexp
		           'org-todo-regexp org-todo-regexp
		           'org-complex-heading-regexp org-complex-heading-regexp
		           'help-echo
		           (format "mouse-2 or RET jump to Org file %S"
			           (abbreviate-file-name
			            (or (buffer-file-name (buffer-base-buffer))
				        (buffer-name (buffer-base-buffer))))))))
          (let* ((level (org-reduced-level (org-outline-level)))
                 (tags (org-get-tags))
                 (category (org-get-category))
                 (heading (org-agenda-format-item
                           ""
			   (concat
			    (if (eq org-tags-match-list-sublevels 'indented)
			        (make-string (1- level) ?.) "")
			    (org-get-heading))
			   (make-string level ?\s)
                           category
                           tags))
                 (priority (org-get-priority heading))
                 (todo (org-get-todo-state))
                 (org-marker (org-agenda-new-marker)))
            (org-add-props heading props
              'org-marker org-marker
              'org-hd-marker org-marker
              'org-clock-hd-marker org-marker
              'org-category category
              'priority priority)
            heading))))))

(defun ejira-agenda-view (keys)
  "Generate agenda view from JIRA identifier list KEYS.
A function similar to `org-tags-view' but instead of a tag search it uses
a list of JIRA keys and `org-id' to perform the search."
  (catch 'exit
    (org-agenda-prepare "Ejira agenda")
    (org-compile-prefix-format 'tags)
    (org-set-sorting-strategy 'tags)
    (setq org-agenda-redo-command (list 'ejira-agenda-view keys))
    (let ((headings (remq nil (mapcar #'ejira-agenda--format-item keys))))
      (org-agenda--insert-overriding-header
        "Headlines from selected JIRA keys")

      (org-agenda-mark-header-line (point-min))
      (when headings
        (insert (org-agenda-finalize-entries headings 'tags) "\n"))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer)))
    (org-agenda-finalize)
    (setq buffer-read-only t)))

(defvar ejira-agenda--jql-cache nil
  "Cache for JQL searches made by ejira agenda.
Association list ((<jql> . (<key1> <key2> <key3> ...)) ...)")

;;;###autoload
(defun ejira-jql (jql)
  "`org-agenda' -type which filters the issues with JQL.
Prefix argument causes discarding the cached issue key list."
  (when (equal current-prefix-arg '(16))
    (mapc #'ejira--update-task
          (mapcar #'ejira--parse-item
                  (apply #'jiralib2-jql-search jql (ejira--get-fields-to-sync)))))
  (when (or current-prefix-arg (not (alist-get jql ejira-agenda--jql-cache nil nil #'equal)))
    (setf (alist-get jql ejira-agenda--jql-cache nil nil 'equal)
          (mapcar (-partial #'alist-get 'key) (jiralib2-jql-search jql "key"))))

  (ejira-agenda-view (alist-get jql ejira-agenda--jql-cache nil nil #'equal)))

(defun ejira-agenda--cmd (fun &rest args)
  "Call function FUN from agenda.
The first parameter to FUN is the key of the issue, ARGS are given as additional
parameters."
  (org-agenda-check-no-diary)
  (let* ((col (current-column))
	 (marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (inhibit-read-only t)
	 (hdmarker (org-get-at-bol 'org-hd-marker))
         newheading m props)
    (org-with-remote-undo buffer
      (let ((key (with-current-buffer buffer
                   (save-restriction
                     (widen)
                     (save-excursion
                       (goto-char pos)
                       (ejira-issue-id-under-point))))))

        (apply fun key args)
        (setq newheading (with-current-buffer (marker-buffer hdmarker)
		           (org-with-wide-buffer
                            (ejira-agenda--format-item key)))))
      (save-excursion
        (goto-char (point-max))
        (beginning-of-line 1)
        (while (not (bobp))
          (when (and (setq m (org-get-at-bol 'org-hd-marker))
                     (equal m hdmarker))
            (beginning-of-line 1)
	    (cond
	     ((equal newheading "") (delete-region (point) (line-beginning-position 2)))
	     ((looking-at ".*")
	      ;; When replacing the whole line, preserve bulk mark
	      ;; overlay, if any.
	      (let ((mark (catch :overlay
			    (dolist (o (overlays-in (point) (+ 2 (point))))
			      (when (eq (overlay-get o 'type)
				        'org-marked-entry-overlay)
			        (throw :overlay o))))))
	        (replace-match newheading t t)
	        (beginning-of-line)
	        (when mark (move-overlay mark (point) (+ 2 (point)))))
	      (org-agenda-highlight-todo 'line)
	      (beginning-of-line 1))
	     (t (error "Line update did not work")))

	    (save-restriction
	      (narrow-to-region (point-at-bol) (point-at-eol))
	      (org-agenda-finalize)))
          (beginning-of-line 0))))

    (org-move-to-column col)
    (org-agenda-mark-clocking-task)))

(defun ejira-agenda-pull-item ()
  "Update the item under point."
  (interactive)
  (ejira-agenda--cmd #'ejira--update-task))

(defun ejira-agenda-progress-item ()
  "Progress the item under point by interactively selecing an action."
  (interactive)
  (ejira-agenda--cmd (lambda (key)
                       (ejira--with-point-on key (ejira-progress-issue)))))

(provide 'ejira-agenda)
;;; ejira-agenda.el ends here
