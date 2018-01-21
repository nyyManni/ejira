;;; ejira-hourmarking.el --- Syncing org-clock entries to JIRA.

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

;; Based on org-clock-csv.el by Aaron Jacobs

;; Parses timestamps from org files and generates a report buffer.
;; Report buffer can be modified and then posted to JIRA Tempo.

;;; Code:

(require 'org)


(defun ejira-hourmarking--find-headlines (element)
  "Get a list of headline ancestors of ELEMENT from closest parent to the farthest."
  (let ((ph (org-element-lineage element '(headline))))
    (if ph
      (cons ph (ejira-hourmarking--find-headlines ph)))))

(defun ejira-hourmarking--get-entries (date-str)
  "Get entries matching DATE-STR."
  (sort
   (let ((ejira-hourmarking--date-filter date-str))
     (cl-loop
      for file in (org-agenda-files) append
      (with-current-buffer (or (find-buffer-visiting file)
                               (find-file file))
        (save-mark-and-excursion
          (save-restriction
            (widen)
            (org-element-map (org-element-parse-buffer) 'clock
              #'ejira-hourmarking--parse-clock-data nil nil))))))

   ;; Sort entries based on start time.
   (lambda (a b)
     (time-less-p
      (plist-get a ':start)
      (plist-get b ':start)))))

(defun ejira-hourmarking--parse-org-clock-stamp (s)
  "Parse org timestamp S to to elisp time objects."
  (mapcar #'date-to-time (split-string s "--")))

(defun ejira-hourmarking--parse-clock-data (element)
  "Parse clock data of ELEMENT. Return plist."
  (when (and (equal (org-element-type element) 'clock)
             ;; Only ingest closed, inactive clock elements.
             (equal (org-element-property :status element) 'closed)
             (equal (org-element-property
                     :type (org-element-property :value element))
                    'inactive-range))

    (let* ((timestamp (org-element-property :value element))
           (headlines (ejira-hourmarking--find-headlines element))
           (headlines-values (mapcar (lambda (h) (org-element-property :raw-value h)) headlines))
           (task-headline (car headlines))
           (task (car headlines-values))
           (subtask-p (unless (org-element-property :ID (car headlines)) t))

           (task-key (ejira-hourmarking--get-id headlines))

           (times (ejira-hourmarking--parse-org-clock-stamp
                   (org-element-property :raw-value timestamp)))
           (start (car times))
           (end (cadr times))
           (duration (time-subtract end start)))
      (when (equal (format-time-string "%Y-%m-%d" start)

                   ;; Dynamically bound.
                   ejira-hourmarking--date-filter)

        (list :start start
              :start-r start ;; TODO: Maybe round this as well
              :end end
              :duration (ejira-hourmarking-round duration 1)
              :duration-r (ejira-hourmarking-round duration 15)
              :key task-key
              :title task
              :subtask-p subtask-p)))))

(defun ejira-hourmarking-round (time round-by)
  "Round TIME to nearest ROUND-BY."
  ;; Account for stupid time zone issues.
  (let* ((hours (- (string-to-number (format-time-string "%H" time)) 2))
         (minutes (string-to-number (format-time-string "%M" time)))
         (total (+ (* 60 hours) minutes))
         (rounded (round total round-by))
         (hours-r (/ rounded (/ 60 round-by)))
         (minutes-r (* round-by (% rounded (/ 60 round-by)))))
     (+ (* 3600 hours-r) (* 60 minutes-r))))

(defun ejira-hourmarking-export-row (entry)
  (let* ((d (plist-get entry :duration))
         (duration-r (if (plist-get entry :subtask-p)
                         ;; Round harvays-tasks by 15 minutes and project tasks by 30
                         (ejira-hourmarking-round d 15)
                       (ejira-hourmarking-round d 30)))

         ;; Accurate duration
         (duration (ejira-hourmarking-round d 1)))
    nil
  ))


(defun ejira--format-h-m (seconds)
  "Format number of SECONDS into JIRA format %Hm %Mm."
  (if (>= seconds 3600)
      ;; More than hour
      (format "% 1dh% 3dm" (/ seconds 3600) (/ (% seconds 3600) 60))
    ;; Less than hour
    (format "   % 3dm" (/ (% seconds 3600) 60))))

(defun ejira-hourmarking-format-row (entry)
  "Pretty-print ENTRY."
  `(,(format-time-string "%H:%M" (plist-get entry :start))
    ;; ,(format-time-string "%Y-%m-%d %H:%M:%S" (plist-get entry :end))
    ;; ,duration
    ;; ,duration-r
    ;; ,(format-time-string "%Hh %Mm" (plist-get entry :duration))
    ,(ejira--format-h-m (plist-get entry :duration))
    ,(ejira--format-h-m (plist-get entry :duration-r))
    ;; ,(format "% 1dh% 3dm" (/ d 3600) (/ (% d 3600) 60))
    ;; ,(format "% 1dh% 3dm" (/ dr 3600) (/ (% dr 3600) 60))
    ,(format "%-15s" (plist-get entry :key))
    ,(plist-get entry :title)))

(defun ejira-hourmarking--get-id (headlines)
  "Return the ID of the given headline.
If headline does not have an id, use it's parents id in HEADLINES."
  (condition-case nil
      (or (org-element-property :ID (car headlines))
          (org-element-property :ID (cadr headlines)))
    (error nil)))

(defun ejira-hourmarking--redraw (rows)
  "Redraw buffer with content from ROWS."
  (unless (equal (buffer-name) "*ejira-hourlog*")
    (error "Not in a hourlog buffer"))

  (let ((inhibit-read-only t))
  (goto-char 0)
  (erase-buffer)

  ;; Print header
  (if (> (length rows) 0)
      (insert (format-time-string "  JIRA Hourlog %A, %h %e\n\n"
                                  (plist-get (nth 0 rows) :start)))
    (insert "\n\n"))


  (let ((sum (apply #'+ (mapcar (lambda (e) (plist-get e :duration)) rows)))
        (sum-r (apply #'+ (mapcar (lambda (e) (plist-get e :duration-r)) rows))))

    (mapc (lambda (r)
            (insert (concat " " (s-join " | " r) "\n")))

       (mapcar #'ejira-hourmarking-format-row ejira-hourlog-entries))
    (insert (format "\nTOTAL:   %dh %dm" (/ sum-r 3600) (/ (% sum-r 3600) 60)))
    (insert (format "\nCLOCKED: %dh %dm\n" (/ sum 3600) (/ (% sum 3600) 60))))))

;;;###autoload
(defun ejira-hourmarking-get-hourlog (date-str)
  "Open hourlog from today into an ejira-hourlog -buffer.
Limit entries to DATE-STR."
  (interactive
   (last (split-string
          (completing-read
           "Date (YYYY-MM-DD): "
           (mapcar (lambda (d)
                     (let ((time (time-subtract (current-time)
                                                (days-to-time d))))
                       (format "%-15s %s"
                               (format-time-string "%A" time)
                               (format-time-string "%Y-%m-%d" time))))
                   (number-sequence 0 6))) " " t)))
  (let* ((buffer (get-buffer-create "*ejira-hourlog*"))
         (entries (ejira-hourmarking--get-entries date-str)))
    (with-current-buffer buffer

      (let ((mode 'ejira-hourlog-mode))
        (funcall mode))

      (make-local-variable 'ejira-hourlog-entries)
      (setq-local ejira-hourlog-entries entries)
      (ejira-hourmarking--redraw ejira-hourlog-entries)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defvar ejira-hourlog-mode-map
  (let ((map (make-sparse-keymap))) map))

(defun ejira-hourlog--adjust-current-row (amount)
  "Adjust the rounded duration of current row by AMOUNT minutes."
  (let ((index (- (line-number-at-pos) 3))
        (p (point)))
    (when (or (>= index (length ejira-hourlog-entries))
              (< index 0))
      (user-error "Outside hourlog"))

    (plist-put (nth index ejira-hourlog-entries) :duration-r
               (max
                0
                (+ (plist-get (nth index ejira-hourlog-entries) :duration-r)
                   (* 60 amount))))

    (ejira-hourmarking--redraw ejira-hourlog-entries)
    (goto-char p)))

(defun ejira-hourlog-increase ()
  "Increase the duration of item under point by 15 min."
  (interactive)
  (ejira-hourlog--adjust-current-row 15))

(defun ejira-hourlog-decrease ()
  "Decrease the duration of item under point by 15 min."
  (interactive)
  (ejira-hourlog--adjust-current-row -15))

(defun ejira-hourlog-commit ()
  "Confirm hourlog and push it to Tempo."
  (interactive)
  (dolist (entry ejira-hourlog-entries)
    (let ((key (plist-get entry :key))
          (start (format-time-string "%Y-%m-%dT%H:%M:%S.000+0300" (plist-get entry :start)))
          (duration (plist-get entry :duration-r))
          (comment (plist-get entry :title)))
      (when (> duration 0)
        (jiralib2-add-worklog key start duration comment)
        ;; (message "updating worklog")
        )))
  (ejira-hourlog-quit)
  (message "Successfully updated worklog"))

(defun ejira-hourlog-quit ()
  "Quit ejira-hourlog and close window."
  (interactive)
  (kill-this-buffer)
  (delete-window))

;;;###autoload
(define-derived-mode ejira-hourlog-mode special-mode "JIRA Hourlog"
  (local-set-key (kbd "C-k") 'ejira-hourlog-increase)
  (local-set-key (kbd "C-j") 'ejira-hourlog-decrease)
  (local-set-key (kbd "C-c C-c") 'ejira-hourlog-commit)
  (local-set-key (kbd "q") 'ejira-hourlog-quit))

(defun ejira-hourmarking--hourlog-mode-hook ()
  "Set up font-locks for hourlog-mode."
  (font-lock-add-keywords nil
    '(("^  JIRA Hourlog .*$" . font-lock-warning-face)
      ("^.*| *0m *|.*$" . font-lock-comment-face)
      (" | " . font-lock-comment-face)
      )))
(add-hook 'ejira-hourlog-mode-hook #'ejira-hourmarking--hourlog-mode-hook)

(provide 'ejira-hourmarking)
;;; ejira-hourmarking.el ends here
