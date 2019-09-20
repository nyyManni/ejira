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
(require 'ejira)
(require 'ejira-core)
(require 'org-agenda)
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
         (left-side (format "%-15s %s" (propertize key 'face 'font-lock-comment-face) heading))
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
  :action       the action to perform for the selected item. (defaults to focus)"
  `(progn
     (defun ,(intern (concat (symbol-name cmd) "--candidates")) ()
       (mapcar
        (-rpartial #'helm-ejira--format-entry (window-width (helm-window)))
        ,(if (plist-get plist :allow-nil)
           `(cons '("*nil*" "*nil*" ())
                  ,(plist-get plist :headings-fn))
           (plist-get plist :headings-fn))))

     (defun ,(intern (concat (symbol-name cmd) "--action")) (c)
       (let ((id (nth 0 (split-string c)))
             (action ,(or (plist-get plist :action)
                          (quote #'ejira-focus-on-issue))))
         (if (equal id "*nil*")
             (funcall action nil)
           (funcall action id))))

     (setq ,(intern (concat (symbol-name cmd) "--source"))
       (helm-build-sync-source ,(symbol-name cmd)
         :candidates ',(intern (concat (symbol-name cmd) "--candidates"))
         :fuzzy-match helm-ejira-fuzzy-match
         :action ',(intern (concat (symbol-name cmd) "--action"))))
     (defun ,cmd ()
       ,doc
       (interactive)
       (helm :sources ,(intern (concat (symbol-name cmd) "--source"))
             :buffer "*helm ejira*"
             :prompt ,(or (plist-get plist :prompt) "Ejira item: ")))))
(function-put #'helm-ejira--define 'lisp-indent-function 'defun)

;;;###autoload
(helm-ejira--define helm-ejira-focus-issue
  "Select an issue."
  :prompt "Issue: "
  :headings-fn (ejira--get-headings-in-agenda-files))

;;;###autoload
(helm-ejira--define helm-ejira-focus-issue-assigned
  "Select an issue that is assigned to me."
  :prompt "Issue: "
  :headings-fn (ejira--get-headings-in-agenda-files :tags '("Assigned")))

(helm-ejira--define helm-ejira-set-epic
  "Select a new epic for issue under point."
  :prompt "Select epic: "
  :allow-nil t
  :headings-fn (ejira--get-headings-in-agenda-files :type "ejira-epic")
  :action (lambda (id)
            (ejira--set-epic (ejira-issue-id-under-point) id)))


(provide 'helm-ejira)
;;; helm-ejira.el ends here
