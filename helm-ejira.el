;;; helm-ejira.el --- Helm-completion for ejira.el

;; Copyright (C) 2017-2019 Henrik Nyman

;; Author: Henrik Nyman <henrikjohannesnyman@gmail.com>
;; URL: https://github.com/nyyManni/ejira
;; Keywords: jira, org, helm
;; Version: 1.0
;; Package-Requires: ((ejira "1.0") (helm "1.0") (org "8.3") (s "1.0") (dash "2.19.1"))

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
(require 'dash)
(require 's)
(require 'ejira)
(require 'ejira-core)
(require 'ejira-agile)
(require 'org-agenda)

(defgroup helm-ejira nil
  "Helm support for ejira."
  :prefix "helm-ejira-"
  :group `ejira)

;;;###autoload
(defcustom helm-ejira-fuzzy-match t
  "Enable fuzzy matching for Helm Ejira.
The search will be matched against the title, issue key and tags."
  :type 'boolean
  :group 'helm-ejira)

(defun ejira--select-id-or-nil (prompt candidates)
  "Select a candidate from CANDIDATES or nil. Display PROMPT."
  (let ((choice (completing-read
                 prompt
                 (mapcar (-partial #'nth 0)
                         (cons '("*nil*", "" ()) candidates)))))
    (unless (equal choice "*nil*")
      choice)))

(defun helm-ejira--format-entry (item width)
  "Format item ITEM for displaying with `completing-read' buffer of size WIDTH."
  (let* ((key (nth 0 item))
         (heading (nth 1 item))
         (tags (nth 2 item))
         (type (substring (nth 3 item) (min 6 (length (nth 3 item)))))
         (left-side (format "%-15s %-7s %s"
                            (propertize key 'face 'font-lock-comment-face)
                            (propertize type 'face 'font-lock-keyword-face)
                            heading))
         (right-side (propertize (s-join "," tags) 'face 'font-lock-type-face)))

    (format "%s%s%s"
            left-side
            (make-string (max 0 (- width (length left-side) (length right-side))) ? )
            right-side)))

(defmacro helm-ejira--define (cmd doc &rest plist)
  "Define helm-completer CMD for ejira with docstring DOC.
PLIST can have following options:
  :headings-fn  expression to get candidates (required)
  :prompt       the helm prompt (defaults to \"Ejira item: \")
  :allow-nil    include nil as a selectable option
  :action       the action to perform for the selected item. (defaults to focus)
                should be a function taking an issue id as a parameter."
  `(progn
     (defun ,(intern (concat (symbol-name cmd) "--candidates")) ()
       (mapcar
        (-rpartial #'helm-ejira--format-entry (window-width (helm-window)))
        ,(if (plist-get plist :allow-nil)
           `(cons '("*nil*" "*nil*" () "*nil*")
                  ,(plist-get plist :headings-fn))
           (plist-get plist :headings-fn))))

     (defun ,(intern (concat (symbol-name cmd) "--action")) (c)
       (let ((id (nth 0 (split-string c)))
             (action ,(or (plist-get plist :action)
                          (quote #'identity))))
         (if (equal id "*nil*")
             (funcall action nil)
           (funcall action id))))

     (defvar ,(intern (concat (symbol-name cmd) "--source"))
       (helm-build-sync-source ,(symbol-name cmd)
         :candidates ',(intern (concat (symbol-name cmd) "--candidates"))
         :fuzzy-match helm-ejira-fuzzy-match
         :action ',(intern (concat (symbol-name cmd) "--action")))
       "Auto-generated helm-sync-soure for `helm-ejira'.")
     (defun ,cmd ()
       ,doc
       (interactive)
       (helm :sources ,(intern (concat (symbol-name cmd) "--source"))
             :buffer "*helm ejira*"
             :prompt ,(or (plist-get plist :prompt) "Ejira item: ")))))
(function-put #'helm-ejira--define 'lisp-indent-function 'defun)

;;;###autoload (autoload 'helm-ejira--define "helm-ejira" nil t)
(helm-ejira--define helm-ejira-focus-issue
  "Select an issue."
  :prompt "Issue: "
  :action #'ejira-focus-on-issue
  :headings-fn (ejira--get-headings-in-agenda-files :type '("ejira-issue"
                                                            "ejira-story"
                                                            "ejira-epic"
                                                            "ejira-subtask")))

;;;###autoload (autoload 'helm-ejira--define "helm-ejira" nil t)
(helm-ejira--define helm-ejira-focus-issue-assigned
  "Select an issue that is assigned to me."
  :prompt "Issue: "
  :action #'ejira-focus-on-issue
  :headings-fn (ejira--get-headings-in-agenda-files :tags `(,ejira-assigned-tagname)))

;;;###autoload (autoload 'helm-ejira--define "helm-ejira" nil t)
(helm-ejira--define helm-ejira-focus-issue-active-sprint
  "Select an issue that is assigned to me."
  :prompt "Issue: "
  :action #'ejira-focus-on-issue
  :headings-fn (ejira--get-headings-in-agenda-files
                :tags `(,(ejira-current-sprint-tag))))

;;;###autoload (autoload 'helm-ejira--define "helm-ejira" nil t)
(helm-ejira--define helm-ejira-refile
  "Refile a heading under a ticket."
  :propt "Issue: "
  :action #'ejira-refile
  :headings-fn (ejira--get-headings-in-agenda-files :type '("ejira-issue"
                                                            "ejira-story"
                                                            "ejira-epic"
                                                            "ejira-subtask")))

;;;###autoload (autoload 'helm-ejira--define "helm-ejira" nil t)
(helm-ejira--define helm-ejira-set-epic
  "Select a new epic for issue under point."
  :prompt "Select epic: "
  :allow-nil t
  :headings-fn (ejira--get-headings-in-agenda-files :type "ejira-epic")
  :action (lambda (id)
            (ejira--set-epic (ejira-issue-id-under-point) id)))

;;;###autoload (autoload 'helm-ejira--define "helm-ejira" nil t)
(helm-ejira--define helm-ejira-select-project
  "Select a project."
  :prompt "Select project: "
  :headings-fn (ejira--get-headings-in-agenda-files :type "ejira-project"))

(defun helm-ejira--select-project-advice (_orig-fun &rest _args)
  "Advice for `ejira--select-project'."
  (helm-ejira-select-project))

(defun helm-ejira-advice ()
  "Set advices so that ejira will use helm-functions."
  (interactive)
  (eval-after-load 'ejira
    (advice-add 'ejira--select-project :around #'helm-ejira--select-project-advice)))

(provide 'helm-ejira)
;;; helm-ejira.el ends here
