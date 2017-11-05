;;; ejira.el --- Syncing between Jira and Org-mode.

;;; Commentary:

;; TODO:
;; - syncing from org to remote
;; - deleting comments

;;; Code:

(require 'org)
(require 'org-id)
(require 'jiralib)
(require 'ox-jira)
(require 'cl-lib)
(require 'language-detection)
(require 's)


(defvar ejira-done-states '("Done"))
(defvar ejira-in-progress-states '("In Progress" "In Review" "Testing"))
(defvar ejira-high-priorities '("High" "Highest"))
(defvar ejira-low-priorities '("Low" "Lowest"))
(defvar ejira-projects '("CT" "USFUSI4442" "PSM2228")
  "ID's of JIRA projects which will be synced.")
(defvar ejira-user-project "CT"
  "Project to use for fetching a list of users.")
(defvar ejira-my-username "hnyman")


(cl-defstruct jira-issue
  (key nil :read-only)
  type
  (reporter nil :read-only)
  assignee
  updated
  (epic "CT-NO-EPIC")
  sprint)

(defstruct jira-comment
  id author created updated )

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
                                   (cons name key))))
                             (jiralib-get-users "CT")))))))

(defun ejira-parse-body (body &optional level)
  "Parse a JIRA BODY to insert it inside org header. If LEVEL is given, shift
all heading by it."
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
  (let* ((issue-tree (org-id-find-id-in-file issue-id (buffer-file-name) t))
         (updated (when issue-tree (save-excursion
                                     (goto-char issue-tree)
                                     (save-restriction
                                       (org-narrow-to-subtree)
                                       (ejira-property-value "Modified"))))))
    updated))

(defun ejira--parse-timestamp (timestamp)
  "Convert JIRA-style TIMESTAMP to native time type."
  ;; (message timestamp)
  (parse-time-string (replace-regexp-in-string "T" " " timestamp)))

(defun ejira-update-issues-in-active-sprint ()
  "Retrieve issues rom JIRA that are in active sprint and update the org tree."
  (interactive)
  (ejira--update-issues-jql
   (concat "project in (" (s-join ", " my-jira-projects) ")"
           " and sprint in openSprints ()")))

(defun ejira-update-issues ()
  "Retrieve issues rom JIRA and update the org tree."
  (interactive)
  (ejira--update-issues-jql
   (concat "project in (" (s-join ", " my-jira-projects) ")")))

(defun ejira--update-issues-jql (query)
  "Update issues matching QUERY."
  (cl-loop
   for issue in (jiralib-do-jql-search query 300) do

   (let ((issue-id (ejira-extract-value issue 'key)))
     ;; If the subtree already exists and it has a timestamp that is not older
     ;; than current, it does not have to be updated.
     (when
         (let ((updated (ejira--get-last-modified issue-id)))
           (or
            (not updated) ;; We don't have an org-entry
            (time-less-p  ;; Entry is out of date
             (date-to-time (ejira-extract-value issue 'fields 'updated))
             (date-to-time updated))))
       (ejira-update-issue issue-id issue)))))

(defun ejira-get-sprint-name (data)
  "Parse sprint name from DATA. Return NIL if not found."
  (let ((name (last (mapcar (lambda (s)
                              (cdr (assoc "name" s)))
                            (mapcar #'ejira-parse-sprint data)))))
    (when name
      (replace-regexp-in-string " " "_" (car name)))))

(defun ejira-parse-sprint (s)
  "Parse a sprint object S. Return it as an assoc list."
  (mapcar
   (lambda (p)
     (apply 'cons (split-string p "=")))
   (split-string
    (replace-regexp-in-string "^.*@[0-9a-f]*\\[\\(.*\\)\\]$" "\\1" s) ",")))

(defun ejira--seconds-to-HH:MM (s)
  "Converts S seconds to HH:MM time format."
  (when s
    (let ((minutes (/ issue-estimate 60)))
      (format "%02d:%02d" (/ minutes 60) (% minutes 60)))))

(defmacro ejira-with-narrow-to-issue (id &rest body)
  "Point on the issue ID header, narrowed to subtree."
  `(org-with-wide-buffer
    (save-excursion
      (goto-char (or (org-id-find-id-in-file ,id (buffer-file-name) t)
                     (error (concat "no issue: " ,id))))
      (beginning-of-line)
      (save-restriction
        (org-narrow-to-subtree)
        ,@body))))

(defmacro ejira-with-narrow-to-issue-under-point (&rest body)
  "Execute body while the buffer is narrowed to the issue under point."
  `(ejira-with-narrow-to-issue (ejira-get-id-under-point)
    ,@body))

(defun ejira-assign-issue (&optional to-me)
  "Assign issue under point. With prefix-argument assign it to me."
  (interactive)
  (ejira-with-narrow-to-issue-under-point
   (let* ((jira-users (unless to-me (ejira-get-users)))
          (username (if to-me
                        ejira-my-username
                      (completing-read "Assignee: " (mapcar 'car jira-users)))))
     (org-set-property "Assignee" username))))

(defun ejira-update-issue (issue-id &optional issue-data)
  "Update an issue with id ISSUE-ID. If ISSUE-DATA is given, no call to API is made."

  (org-with-wide-buffer
   (let* ((issue (or nil (jiralib-get-issue issue-id)))
          (issue-type (ejira-extract-value issue 'fields 'issuetype 'name))
          (epic-p (equal issue-type "Epic"))
          (issue-summary (if epic-p
                             (ejira-extract-value issue 'fields 'customfield_10004) 
                           (ejira-extract-value issue 'fields 'summary)))
          (issue-epic (when (not epic-p)
                        (or (org-jira-find-value issue 'fields 'customfield_10002)
                            "CT-NO-EPIC")))
          (issue-estimate (ejira-extract-value issue 'fields 'timetracking
                                               'originalEstimateSeconds))
          (issue-remaining-estimate (ejira-extract-value issue 'fields 'timetracking
                                                         'remainingEstimateSeconds))
          (issue-assignee (ejira-extract-value issue 'fields 'assignee 'displayName))
          (issue-sprint (unless epic-p (ejira-get-sprint-name
                                        (ejira-extract-value issue 'fields
                                                             'customfield_10001))))
          (issue-reporter (ejira-extract-value issue 'fields 'reporter 'displayName))
          (issue-priority (ejira-extract-value issue 'fields 'priority 'name))
          (issue-description (ejira-extract-value issue 'fields 'description))
          (issue-updated (date-to-time (ejira-extract-value issue 'fields 'updated)))
          (issue-status (ejira-extract-value issue 'fields 'status 'name))
          (issue-deadline (ejira-extract-value issue 'fields 'duedate))
          (issue-tree (or (org-id-find-id-in-file issue-id (buffer-file-name) t)
                          (save-excursion
                            (goto-char (point-min))
                            (org-insert-todo-heading t t)
                            (insert issue-summary)
                            (org-set-property "ID" issue-id)
                            (forward-line -1)  ; FIXME: explain why needed
                            (point-marker)))))

     ;; Issue's epic needs to be created first so that refiling can be done.
     (when (and issue-epic
                (not (org-id-find-id-in-file issue-epic (buffer-file-name) t)))
       (ejira-update-issue issue-epic)
       ;; Update issue-tree, the file may have been changed.
       (setq issue-tree (org-id-find-id-in-file issue-id (buffer-file-name) t)))

     (save-excursion
       (goto-char issue-tree)
       (save-restriction
         (org-narrow-to-subtree)

         ;; Set the todo-status of the issue based on JIRA status.
         (cond ((member issue-status ejira-done-states) (org-todo 3))
               ((member issue-status ejira-in-progress-states) (org-todo 2)))

         ;; Set the tag to JIRA sprint so that issues can be filtered easily.
         (when issue-sprint
           (org-set-tags-to issue-sprint))

         ;; Update heading text.
         (save-excursion
           (ejira-update-header-text issue-summary))

         ;; Update deadline
         (when issue-deadline
           (org-deadline nil issue-deadline))

         ;; Set properties.
         (org-set-property "Type" issue-type)
         (org-set-property "Reporter" issue-reporter)
         (org-set-property "Assignee" (or issue-assignee ""))
         (org-set-property "Modified" (format-time-string "%Y-%m-%d %H:%M:%S"
                                                          issue-updated))
         (when issue-estimate
           (let ((minutes (/ issue-estimate 60)))
             (org-set-property "Effort" (format "%02d:%02d"
                                                (/ minutes 60)
                                                (% minutes 60)))))
         (when issue-remaining-estimate
           (let ((minutes (/ issue-remaining-estimate 60)))
             (org-set-property "Remaining" (format "%02d:%02d"
                                                   (/ minutes 60)
                                                   (% minutes 60)))))

         ;; Set priority.
         (cond ((member issue-priority ejira-high-priorities)
                (org-priority ?A))
               ((member issue-priority ejira-low-priorities)
                (org-priority ?C))
               (t (org-priority ?B)))

         ;; Update description.
         (let ((description-tree (or (ejira-find-headline-in-visible "Description")
                                     (save-excursion
                                       (org-insert-heading-respect-content)
                                       (insert "Description")
                                       (org-demote-subtree)
                                       (point-marker)))))

           (ejira--update-body description-tree issue-description))

         ;; Update comments.
         (let ((comments-tree (or (ejira-find-headline-in-visible "Comments")
                                  (save-excursion
                                    (org-insert-heading-respect-content)
                                    (insert "Comments")
                                    (org-demote-subtree)
                                    (point-marker))))
               (comments (ejira-extract-value issue 'fields 'comment 'comments)))
           (save-excursion
             (goto-char comments-tree)
             (save-restriction
               (org-narrow-to-subtree)
               (mapc
                (lambda (comment)
                  (let* ((comment-id (ejira-extract-value comment 'id))
                         (comment-author (ejira-extract-value comment 'author
                                                              'displayName))
                         (comment-created (date-to-time (ejira-extract-value comment
                                                                             'created)))
                         (comment-updated (date-to-time (ejira-extract-value comment
                                                                             'updated)))
                         (comment-contents (ejira-extract-value comment 'body))
                         (comment-summary (ejira--get-comment-header comment-author
                                                                     comment-contents))

                         (comment-tree (or (org-id-find-id-in-file comment-id
                                                                   (buffer-file-name) t)
                                           (save-excursion
                                             (org-insert-heading-respect-content)
                                             (insert comment-summary)
                                             (org-demote-subtree)
                                             (point-marker)))))

                    (save-excursion
                      (goto-char comment-tree)
                      (save-restriction
                        (org-narrow-to-subtree)

                        ;; Update heading text
                        (save-excursion
                          (ejira-update-header-text comment-summary))

                        (org-set-property "ID" comment-id)
                        (org-set-property "Author" comment-author)
                        (org-set-property "Created" (format-time-string
                                                     "%Y-%m-%d %H:%M:%S"
                                                     comment-created))
                        (when (not (equal comment-created comment-updated))
                          (org-set-property "Modified" (format-time-string
                                                        "%Y-%m-%d %H:%M:%S"
                                                        comment-updated)))

                        (ejira--update-body comment-tree comment-contents)))))
                comments))))

         ;; Refile the ticket under the epic
         (unless epic-p
           (let ((current-epic (ejira-property-value "Epic")))
             (when (not (equal current-epic issue-epic))
               (when issue-epic (org-set-property "Epic" issue-epic))
               (save-excursion
                 (goto-char issue-tree)
                 (ejira-refile issue-epic))))))))))

(defun ejira-update-header-text (text)
  "Replace the header text with the given TEXT.
TODO state, priority and tags will be preserved."
  (interactive)
  (goto-char (point-min))
  (when (search-forward
         (org-get-heading t t t t))
    (replace-match text)))

(defun ejira--get-comment-header (author contents)
  "Parse a header message for comment. AUTHOR: + firts 60 chars of CONTENTS."
  (let* ((msg (concat author ": "
                          (first (split-string (s-trim contents) "\n")))))
    (if (> (length m
        (concat (substring msg 0 60) "...")
      msg)))

(defun ejira--update-body (pos contents)
  "Replace body of header at POS with CONTENTS if changed."
  (save-excursion
    (goto-char pos)

    (let ((level (save-mark-and-excursion
                   (save-match-data
                     (search-forward-regexp "^\\**" (line-end-position) t)
                     (length (or (match-data) ""))))))
      
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
            (message "'%s'" current-contents)
            (message "'%s'" new-contents)
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
     (org-with-wide-buffer
      (org-capture nil "x")))))

(defun ejira--sync-new-comment ()
  (unless org-note-abort
    (save-excursion
      (goto-char (point-min))
      (save-restriction
        (when (equal (org-get-heading t t t t) "New comment:")
          (let ((issue-id (org-with-wide-buffer (ejira-get-id-under-point)))
                (body (ejira-org-to-jira (s-trim (ejira--get-body)))))
            (let ((comment (jiralib-add-comment issue-id body)))
              (let* ((comment-id (ejira-extract-value comment 'id))
                     (comment-author (ejira-extract-value comment 'author 'displayName))
                     (comment-created (date-to-time (ejira-extract-value comment 'created)))
                     (comment-updated (date-to-time (ejira-extract-value comment 'updated)))
                     (comment-contents (ejira-parse-body (ejira-extract-value comment 'body)))
                     (comment-summary (ejira--get-comment-header comment-author
                                                                 comment-contents)))

                (org-set-property "ID" comment-id)
                (org-set-property "Author" comment-author)
                (org-set-property "Created" (format-time-string
                                             "%Y-%m-%d %H:%M:%S"
                                             comment-created))

                (save-excursion
                  (ejira-update-header-text comment-summary))

                (when (not (equal comment-created comment-updated))
                  (org-set-property "Modified" (format-time-string
                                                "%Y-%m-%d %H:%M:%S"
                                                comment-updated)))

                (ejira--update-body (point-marker) comment-contents)))))))))

(add-hook 'org-capture-prepare-finalize-hook #'ejira--sync-new-comment)


  (let ((pos (ejira-with-narrow-to-issue-under-point
              (goto-char (ejira-find-headline-in-visible "Comments"))
              (if (org-goto-first-child)
                  (progn
                    (org-end-of-subtree)
                    (org-insert-heading-respect-content))
                (progn
                  (org-insert-heading-respect-content)
                  (org-demote)))
              (insert "New comment:\n  ")
              (point-marker))))
    (goto-char pos)
    (evil-insert-state)))

(defun ejira-property-value (key)
  "List all non-nil values of property KEY in current visble buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (re (org-re-property key))
	  values)
      (when (re-search-forward re nil t)
        (org-entry-get (point) key)))))

(defun ejira-get-id-under-point ()
  "Get ID of the ticket under point."
  (save-excursion
    (catch 'id-tag
      (while t
        (save-restriction
          (org-narrow-to-subtree)
          (let ((found-id (ejira-property-value "ID")))
            (when (and found-id (not (s-numeric-p found-id)))
              (throw 'id-tag found-id))))
        (org-up-element)))))

(defun ejira-update-current-issue ()
  "Update current issue details."
  (interactive)
  (ejira-update-issue (or (ejira-get-id-under-point)
                          (error "Not on a ticket"))))


(defun ejira-refile (epic-id)
  "Refile the issue under epic EPIC-ID."
  (org-with-wide-buffer
   (org-refile nil nil
               (list "" (buffer-file-name) ""
                     (org-id-find-id-in-file epic-id (buffer-file-name) t)))))

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
                  l)))
    (if (stringp value)
        (decode-coding-string value 'utf-8)
      value)))



(setq ejira-jira-to-org-patterns
      '(

        ;; Code block
        ("^{code\\(?::.*language=\\(?1:[a-z]+\\)\\)?.*}\\(?2:.*\\(?:
.*\\)*?\\)?
?{code}"
         . (lambda ()
             (let ((lang (match-string 1))
                   (body (match-string 2))
                   (md (match-data)))
               (when (equal lang "none")
                 (setq lang (symbol-name (language-detection-string body)))
                 (when (equal lang "awk")
                   ;; Language-detection seems to fallback to awk. In that case
                   ;; it most likely is not code at all. (Some coworkers like to
                   ;; use source blocks for other things than code...)
                   (setq lang ""))
                 )
               (prog1
                   (concat
                    "#+BEGIN_SRC " lang "\n"
                    (replace-regexp-in-string "^" "  " body)
                    "\n#+END_SRC")

                 ;; Auto-detecting language alters match data, restore it.
                 (set-match-data md)))))

        ;; Link
        ("\\[\\(?:\\(.*\\)|\\)?\\(.*\\)\\]"
         . (lambda ()
             (let ((url (format "[%s]" (match-string 2)))
                   (placeholder (if (match-string 1)
                                    (format "[%s]" (match-string 1))
                                  "")))
               (format "[%s%s]" url placeholder))))
        
        ;; Table
        ("\\(^|| .*||\\)\\(\\(?:
| .*|\\)*$\\)"
         . (lambda ()
             (let ((header (match-string 1))
                   (body (match-string 2))
                   (md (match-data)))
               (with-temp-buffer
                 (insert
                  (concat
                   (replace-regexp-in-string "||" "|" header)
                   "\n|"
                   (replace-regexp-in-string
                    "" "-"
                    (make-string (- (s-count-matches "||" header) 1) ?+)
                    nil nil nil 1)
                   "-|"
                   body))
                 (org-table-align)

                 (prog1
                     ;; Get rid of final newline that may be injected by org-table-align
                     (replace-regexp-in-string "\n$" "" (buffer-string)) 

                   ;; org-table-align modifies match data, restore it.
                   (set-match-data md))))))

        ;; Bullet- or numbered list
        ("^\\([#*]+\\) "
         . (lambda ()
             (let* ((prefixes (match-string 1))
                    (level (- (length prefixes) 1))
                    (indent (make-string (max 0 (* 4 level)) ? )))
               ;; Save numbered lists with a placeholder, they will be calculated
               ;; later.
               (concat indent (if (s-ends-with? "#" prefixes) "########" "-") " "))))

        ;; Heading
        ("^h\\([1-6]\\)\\. "
         . (lambda ()
             (concat
              ;; NOTE: Requires dynamic binding to be active.
              (make-string (+ (if (boundp 'jira-to-org--convert-level)
                                  jira-to-org--convert-level
                                0)
                              (string-to-number (match-string 1))) ?*) " ")))

        ;; Verbatim text
        ("{{\\(.*\\)}}"
         . (lambda () (concat "=" (match-string 1) "=")))

        ;; Italic text
        ("_\\([^_]*\\)_"
         . (lambda () (concat "/" (match-string 1) "/")))

        ))


(defun ejira-org-to-jira (s)
  "Transform org-style string S into JIRA format."
  (org-export-string-as s 'jira t))

(defun random-alpha ()
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i))))


(defun ejira-jira-to-org (s &optional level)
  "Transform JIRA-style string S into org-style. If LEVEL is given, shift all
headings to the right by that amount."
  (with-temp-buffer
    (let ((replacements (make-hash-table :test 'equal))
          (jira-to-org--convert-level (or level 0)))
      (insert (decode-coding-string s 'utf-8))
      (cl-loop
       for (pattern . replacement) in ejira-jira-to-org-patterns do
       (goto-char (point-min))
       (while (re-search-forward pattern nil t)
         (let ((identifier (concat (random-alpha) (random-alpha)
                                   (random-alpha) (random-alpha)
                                   (random-alpha) (random-alpha)
                                   (random-alpha) (random-alpha)
                                   (random-alpha) (random-alpha)
                                   (random-alpha) (random-alpha)
                                   (random-alpha) (random-alpha)
                                   (random-alpha) (random-alpha)))
               (rep (funcall replacement)))
           (replace-match identifier)
           (puthash identifier rep replacements))))
      (maphash (lambda (key val)
                 (goto-char (point-min))
                 (search-forward key)
                 (replace-match val))
               replacements)
      ;; Calculate numbered list indices
      (goto-char (point-min))
      (let ((counters (make-list 6 1)))  ; JIRA Supports 6 levels of headings
        (dolist (n (split-string (buffer-string) "\n")) 
          (cond ((search-forward-regexp "^\\([[:blank:]]*\\)########"
                                        (line-end-position) t)
                 (let ((level (/ (length (match-string 1)) 4)))
                   (replace-match (format "%s%i." (match-string 1) (nth level counters)))
                   (setcar (nthcdr level counters) (1+ (nth level counters)))))
                ((search-forward-regexp "^\\([[:blank:]]*\\)- " (line-end-position) t)
                 nil)
                (t (setq counters (make-list 6 1))))
          (forward-line 1)))
      (delete-trailing-whitespace))
    (setq my-contents (buffer-string)) 
    (buffer-string)))


;; (setq org-buffer "* This is a test
;; ** Subheader 1
;; - A bullet
;; - list of
;; - three items

;; #+BEGIN_SRC python
;;   def my_test_function():
;;       print('hello world')
;; #+END_SRC

;; #+BEGIN_SRC java
;;   static void main(String a) {
;;      System.out.println(\"Hello World!\");
;;   }
;; #+END_SRC

;; #+BEGIN_SRC
;;   test
;; #+END_SRC

;; Text with =verbatim=, /italic/ and *bold* text

;; 1. A numbered
;; 2. list of
;; 3. three items
;;     - with
;;     - a sublist
;;     - and a link [[http://www.google.com][google]]
;;     - and a link [[http://www.google.com]]

;; | a | b |
;; |---+---|
;; | c | d |

;; ** Here, a table
;; | test | table | contents | here |  . |
;; |------+-------+----------+------+----|
;; |    3 |     2 |        1 |    0 | -1 |

;; ** End
;; 1. test
;; 2. test
;;     1. test
;;     2. test
;;         - test
;;         - test
;;     3. test
;; 3. test
;; 4. test
;; ")

;; (message (replace-regexp-in-string "^{code\\(?::.*language=\\(?1:[a-z]+\\)\\)?.*}\\(?2:.*\\(?:
;; .*\\)*?\\)?
;; ?{code}" "#+BEGIN_SRC \\1
;; \\2
;; #+END_SRC" 


;; "{code:title=|langage=none|collapse=false}def my_test_function():
;;     print(’hello world’)
;; {code}"))


;; (while (re-search-forward "’" nil t)
;;   ;; TODO: Not working yet
;;   (replace-match "'"))


;; (replace-regexp-in-string "’" "'" "


;; print(’hello world’)


;; ")

;; (search-forward-regexp "\\[\\(?:\\(?1:.*\\)|\\)?\\(?2:.*\\)\\]"
;;                        )
;;     - and a link [google|http://www.google.com]




;; (setq my-list (make-list 8 0))

;; (message "%s" my-list)

;; (setcar (nthcdr 3 my-list) (1+ (nth 3 my-list)))



;; (equal org-buffer (ejira-jira-to-org (ejira-org-to-jira org-buffer)))

;; (insert org-buffer)
;; (insert (ejira-jira-to-org (ejira-org-to-jira org-buffer)))





;; (message
;;  (concat
;;   "==============================================\n"
;;   (ejira-jira-to-org (ejira-org-to-jira org-buffer))
;;   "\n=============================================="))

;; (message (replace-regexp-in-string "a" "_" my-contents))

;; ("‘")
;; ()

;; (body (replace-regexp-in-string "’" "'" (match-string 2))))

;; (setq _table "| a | b |
;; |-+-|
;; | c | d |
;; | c | d |")

;; (defun my-test ()
;;   (interactive)
;;   (message
;;    (with-temp-buffer
;;      (insert _table)
;;      (org-table-align)
;;      (buffer-string))))

;; (message "'test'")

;; (message *from*)

;; (provide 'ejira)
;; ;;; ejira.el ends here

;; "^[ 	]*:PROPERTIES:[ 	]*
;; \(?:[ 	]*:\S-+:\(?: .*\)?[ 	]*
;; \)*?[ 	]*:END:[ 	]*$"

;; (search-forward-regexp "^{code.*\\(?:language=\\([\\w]+\\)\\)?.*}.*\\(?:
;; .*\\)*?
;; ?{code}")

;; {code:title=|language=none|collapse=false}def my_test_function():
;;     print(’hello world’)
;; {code}

;; (progn
;;   (search-forward-regexp "\\(^|| .*||\\)\\(\\(?:
;; | .*|\\)*$\\)")
;;   (replace-match
;;    (let ((header (match-string 1))
;;          (body (match-string 2)))
;;      (concat
;;       (replace-regexp-in-string "||" "|" header)
;;       "\n|"
;;       (replace-regexp-in-string
;;        "" "-" 
;;        (make-string (- (s-count-matches "||" header) 1) ?+)
;;        nil nil nil 1)
;;       "-|"
;;       body))

;;    )

;;   )
;; (make-string 3 "s")


;; (progn
;;   (search-forward-regexp "\\(^|| .*||\\)\\(\\(?:
;; | .*|\\)*$\\)")
;;   (replace-match 
   

;;    (let ((header (match-string 1))
;;          (body (match-string 2))
;;          (md (match-data)))
;;      (with-temp-buffer
;;        (insert
;;         (concat
;;          (replace-regexp-in-string "||" "|" header)
;;          "\n|"
;;          (replace-regexp-in-string
;;           "" "-"
;;           (make-string (- (s-count-matches "||" header) 1) ?+)
;;           nil nil nil 1)
;;          "-|"
;;          body))
;;        (org-table-align)

;;        (prog1
;;            ;; Get rid of final newline that may be injected by org-table-align
;;            (replace-regexp-in-string "\n$" "" (buffer-string)) 

;;          ;; org-table-align modifies match data, restore it.
;;          (set-match-data md))
;;        ))))





;; || a || b ||
;; | c | d |
;; | c | d |
;; | c | d |
;; | c | d |

;; || test || table || contents || here || . ||
;; | 3 | 2 | 1 | 0 | -1 |

;; | a | b |
;; |-+-|
;; | c | d |
;; | c | d |

;; | c | d |
;; | c | d |



;; sadfaf
;; o

