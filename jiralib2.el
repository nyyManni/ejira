;;; jiralib2.el -- Provide connectivity to JIRA REST services.

;; Author: Henrik Nyma (henrikjohannesnyman@gmail.com)
;; Created: December, 2017
;; Keywords: rest, web-services, jira

;;; Commentary:

;; This file provides a programatic interface to JIRA.  It provides access to
;; JIRA from other programs, but no user level functionality.

;; jiralib2.el uses cookie authentication instead of basic auth for performance
;; reasons. JIRA API has an artificial delay of ~second in basic auth queries.
;; The session cookie is stored in an Emacs global variable, and it is
;; automatically used in each query. If the user has not logged in, or the
;; session has expired, a new login is performed and the password queried from
;; the user. jiralib2 DOES NOT store user's password anywhere like jiralib did.
;; Only the session token is saved, and user credentials cannot be extracted
;; from it.

;; Jira References:

;; Primary reference (on current Jira, only REST is supported):
;; https://docs.atlassian.com/jira/REST/cloud/

;;; Code:
(eval-when-compile (require 'cl))
(require 'request)
(require 'json)
(require 'url-parse)

(defgroup jiralib2 nil
  "Jiralib2 customization group."
  :group 'applications)

(defcustom jiralib2-url "http://localhost:8081/"
  "The address of the jira host."
  :type 'string
  :group 'jiralib2)

(defvar jiralib2-user-login-name nil
  "The name of the user logged into JIRA.
This is maintained by `jiralib2-login'.")

(defvar *JIRA-SESSION* nil
  "Contains the cookie of the active JIRA session.")

(defun jiralib2-session-login (&optional username password)
  "Login to JIRA with USERNAME and PASSWORD. Save cookie in *JIRA-SESSION*."
  (interactive)
  (setq *JIRA-SESSION*
        (let* ((username (or username
                             jiralib2-user-login-name
                             (read-string "Username: ")))
               (password (or password
                             (read-passwd (format "Password for user %s: "
                                                  username))))
               (reply-data (request (concat jiralib2-url "/rest/auth/1/session")
                                    :type "POST"
                                    :headers `(("Content-Type" . "application/json"))
                                    :parser 'json-read
                                    :sync t
                                    :data (json-encode `((username . ,username)
                                                         (password . ,password)))))
               (auth-info (cdar (request-response-data reply-data)))
               (session-token (format "%s=%s"
                                      (cdr (assoc 'name auth-info))
                                      (cdr (assoc 'value auth-info)))))
          session-token)))

(defun jiralib2-get-user-info ()
  "Fetch information on currently logged in user."
  (jiralib2-session-call "/rest/api/2/myself"))

(defun jiralib2--session-call (path args)
  "Do a call to PATH with ARGS using current session.
Does not check for session validity."
  (apply #'request (concat jiralib2-url path)
         :headers `(("Content-Type" . "application/json")
                    ("cookie" . ,*JIRA-SESSION*))
         :sync t
         :parser 'json-read
         args))

(defun jiralib2-session-call (path &rest args)
  "Do a call to PATH with ARGS using current session.
If no session exists, or it has expired, login first."
  (unless *JIRA-SESSION*
    (jiralib2-session-login))

  (let ((response (jiralib2--session-call path args)))

    ;; The session has probably expired. Login and try again.
    (when (= (request-response-status-code response) 401)
      (jiralib2-session-login)
      (setq response (jiralib2--session-call path args)))
    (request-response-data response)))

(defun jiralib2-get-issue (issue-key)
  "Get the issue with key ISSUE-KEY."
  (jiralib2-session-call (format "/rest/api/2/issue/%s" issue-key)))

(defun jiralib2-add-comment (issue-key body)
  "Add comment to issue ISSUE-KEY with contents BODY."
  (jiralib2-session-call (format "/rest/api/2/issue/%s/comment" issue-key)
                         :type "POST"
                         :data (json-encode `((body . ,body)))))


(defun jiralib2-delete-comment (issue-key comment-id)
  "Remove comment COMMENT-ID from issue ISSUE-KEY."
  (jiralib2-session-call (format "/rest/api/2/issue/%s/comment/%s"
                                 issue-key comment-id)
                         :type "DELETE"))

(defun jiralib2-edit-comment (issue-key comment-id body)
  "Update comment COMMENT-ID from issue ISSUE-KEY with body BODY."
  (jiralib2-session-call (format "/rest/api/2/issue/%s/comment/%s"
                                 issue-key comment-id)
                         :type "PUT"
                         :data (json-encode `((body . ,body)))))

(defvar *jiralib2-users-cache* nil)
(defun jiralib2-get-users (project-key)
  "Return assignable users information given the PROJECT-KEY."
  (or *jiralib2-users-cache*
      (jiralib2-session-call
       (format "/rest/api/2/user/assignable/search?project=%s&maxResults=10000"
               project-key))))

(defun jiralib2-assign-issue (issue-key username)
  "Assign issue with ISSUE-KEY to USERNAME."
  (jiralib2-session-call (format "/rest/api/2/issue/%s/assignee" issue-key)
                         :type "PUT"
                         :data (json-encode `((name . ,username)))))

(defun jiralib2-do-jql-search (jql &optional limit)
  "Run a JQL query and return the list of issues that matched.
LIMIT is the maximum number of queries to return.  Note that JIRA
has an internal limit of how many queries to return, as such, it
might not be possible to find *ALL* the issues that match a
query."
  (unless (or limit (numberp limit))
    (setq limit 100))
  (append
   (cdr
    (assoc 'issues
           (jiralib2-session-call "/rest/api/2/search"
                                  :type "POST"
                                  :data (json-encode
                                         `((jql . ,jql)
                                           (maxResults . ,limit))))))
   nil))


(defun jiralib2-get-actions (issue-key)
  "Get available actions for the issue ISSUE-KEY.
The issues are returned as a list of ((name . <name>) (id . <id>)) alists."
  (mapcar
   (lambda (trans)
     `(,(cdr (assoc 'id trans)) . ,(cdr (assoc 'name trans))))
   (cdadr
    (jiralib2-session-call (format "/rest/api/2/issue/%s/transitions" issue-key)))))

(defun jiralib2-do-action (issue-key action-id)
  "Move the issue ISSUE-KEY to another state with action ACTION-ID."
  (jiralib2-session-call (format "/rest/api/2/issue/%s/transitions" issue-key)
                         :type "POST"
                         :data (json-encode `((transition . ((id . ,action-id)))))))


(defun jiralib2-get-worklog (issue-key &optional only-mine)
  "Get worklogs of the issue ISSUE-KEY.
With ONLY-MINE set to t, only return worklogs logged by me."
  (jiralib2-session-call (format "/rest/api/2/issues/%s/worklog" issue-key)))

(defun jiralib2-add-worklog (issue-key timestamp seconds message)
  "Add a worklog to issue ISSUE-KEY with message MESSAGE.
Use TIMESTAMP as start time and SECONDS as amount of logged work in seconds."
  (jiralib2-session-call (format "/rest/api/2/issues/%s/worklog" issue-key)
                         :type "POST"
                         :data (json-encode `((comment . ,message)
                                              (started . ,timestamp)
                                              (timeSpentSeconds . ,seconds)))))


(provide 'jiralib2)
;;; jiralib2.el ends here
