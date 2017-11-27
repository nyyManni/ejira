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

;;; Code:

(require 'org)


(defun ejira-hourmarking--find-headlines (element)
  "Get a list of headline ancestors of ELEMENT from closest parent to the farthest."
  (let ((ph (org-element-lineage element '(headline))))
    (if ph
      (cons ph (ejira-hourmarking--find-headlines ph)))))

(defun ejira-hourmarking--get-entries ()
  (sort
   (cl-loop
    for file in (org-agenda-files) append
    (with-current-buffer (or (find-buffer-visiting file)
                             (find-file file))
      (save-mark-and-excursion
        (save-restriction
          (widen)
          (org-element-map (org-element-parse-buffer) 'clock
            #'ejira-hourmarking--parse-clock-data nil nil)))))

   ;; Sort entries based on start time.
   (lambda (a b)
     (time-less-p
      (plist-get a ':start)
      (plist-get b ':start)))))



(defun ejira-hourmarking--parse-org-clock-stamp (s)
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

           ;; (task-key (org-clock-csv--get-id headlines))
           (task-key (ejira-hourmarking--get-id headlines))

           (times (ejira-hourmarking--parse-org-clock-stamp
                   (org-element-property :raw-value timestamp)))
           (start (car times))
           (end (cadr times))
           (duration (time-subtract end start)))
      (when (equal (format-time-string "%Y-%m-%d" start)
                   (format-time-string "%Y-%m-%d"))

        (list :start start
              :end end
              :duration duration
              :key task-key
              :title task
              :subtask-p subtask-p)))))

(defun ejira-hourmarking-round (time round-by)
  "Round TIME to nearest ROUND-BY."
  (let* ((hours (- (string-to-number (format-time-string "%H" time)) 2))
         (minutes (string-to-number (format-time-string "%M" time))) 
         (total (+ (* 60 hours) minutes))
         (rounded (round total round-by))
         (hours-r (/ rounded (/ 60 round-by)))
         (minutes-r (* round-by (% rounded (/ 60 round-by)))))
    (number-to-string (+ (* 3600 hours-r) (* 60 minutes-r)))))
    ;; (format "%dh %dm" hours-r minutes-r)))


(defun ejira-hourmarking-format-row (entry)
  `(,(format-time-string "%Y-%m-%d %H:%M:%S" (plist-get entry :start))
    ;; ,(format-time-string "%Y-%m-%d %H:%M:%S" (plist-get entry :end))
    ,(if (plist-get entry :subtask-p)

         ;; Round harvays-tasks by 15 minutes and project tasks by 30
         (ejira-hourmarking-round (plist-get entry :duration) 10)
       (ejira-hourmarking-round (plist-get entry :duration) 30))

    ;; ,(format-time-string "%Hh %Mm" (plist-get entry :duration))
    ,(plist-get entry :key)
    ,(plist-get entry :title)))

(defun ejira-hourmarking--get-id (headlines)
  "Return the ID of the given headline.
If headline does not have an id, use it's parents id in HEADLINES."
  (condition-case nil
      (or (org-element-property :ID (car headlines))
          (org-element-property :ID (cadr headlines)))
    (error nil)))

(defun ejira-hourmarking-get-hourlog ()
  (interactive)
  ;; TODO: Handle an OUTFILE argument.
  (let* ((buffer (get-buffer-create "*ejira-hourlog*"))
         (entries (ejira-hourmarking--get-entries)))
    (message "entries found: %d" (length entries))
    (with-current-buffer buffer
      (goto-char 0)
      (erase-buffer)
      (let* ((rows (mapcar #'ejira-hourmarking-format-row entries))
             ;; (sum (redu))
             (sum (apply #'+ (mapcar (lambda (e) (string-to-number (nth 1 e))) rows))))
        
        
        (mapc (lambda (r)
                (insert
                 (concat
                  (s-join " | " r)
                  ;; r
                  "\n")))
              rows)
        (insert (format "TOTAL: %dh %dm" (/ sum 3600) (/ (% sum 3600) 60)))
        ))
    (switch-to-buffer buffer)))

(provide 'ejira-hourmarking)
;;; ejira-hourmarking.el ends here
