;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DIDIT; Base: 10 -*-

#|

Copyright (C) 2021  Anthony Green <green@moxielogic.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with this program.  If not, see
<http://www.gnu.org/licenses/>.

|#

;; Load config from git

(in-package :didit)

(defun load-config (etcd)
  "Load all git-hosted config."

  (log:info "Loading config")

  (flet ((get-config-value (key)
	   (let ((value (or (gethash key *config*)
			    (gethash key *default-config*)
			    (error "config does not contain key '~A'" key))))
	     ;; Some of the users of these values are very strict
	     ;; when it comes to string types... I'm looking at you,
	     ;; SB-BSD-SOCKETS:GET-HOST-BY-NAME.
	     (if (subtypep (type-of value) 'vector)
		 (coerce value 'simple-string)
		 value))))

    (log:info "Pulling config repo ~A" (get-config-value "config-repo"))
    (pull-repo *config-dir* (get-config-value "config-repo"))

    (let* ((repo.ini-filename
             (merge-pathnames *config-dir* "repos.ini")))

      (if (fad:file-exists-p repo.ini-filename)
          (let* ((file-contents (alexandria:read-file-into-string repo.ini-filename
                                                                  :external-format :latin-1))
                 (config (cl-toml:parse file-contents))
                 (repos (gethash "repos" config)))

            (when repos

              ;; Pull all of the config content before acting on it in
              ;; case there are pull problems.
              (maphash
               (lambda (key value)
                 (let ((repo (gethash "repo" value)))
                   (pull-repo (str:concat (namestring (get-config-value "root-dir"))
                                          (short-hash repo))
                              repo)))
               repos)

              ;; Clear all scheduled tasks.
              (log:info "Clearing all scheduled tasks")
              (let ((tasks (scheduler:list-scheduler-tasks *scheduler*)))
                (dolist (task tasks)
                  (scheduler:delete-scheduler-task *scheduler* task)))

              (maphash
               (lambda (key value)
                 (let* ((repo (gethash "repo" value))
                        (prefix (gethash "prefix" value))
                        (repo-dirname (str:concat (namestring (get-config-value "root-dir"))
                                                  (short-hash repo))))
                   (let* ((didit.ini-filename (concatenate 'string repo-dirname "/didit.ini"))
                          (file-contents (if (fad:file-exists-p didit.ini-filename)
                                             (alexandria:read-file-into-string didit.ini-filename
                                                                               :external-format :latin-1)
                                             ""))
                          (didit.ini (cl-toml:parse file-contents)))

                     ;; Process all of the alerts entries
                     (let ((alerts (gethash "alerts" didit.ini)))
                       (maphash (lambda (key value)
                                  (setf (gethash (format nil "~A/~A" prefix key) *alerts-table*)
                                        (make-instance (read-from-string
                                                        (str:concat "didit:alert/" (gethash "type" value))) :config value)))
                                alerts))

                     ;; Process all of the didit entries
                     (let ((didits (gethash "didit" didit.ini)))
                       (maphash (lambda (key value)
                                  (let ((cron (gethash "cron" value))
                                        (token (gethash "token" value)))
                                    (if (not (= 5 (length (split-sequence:split-sequence #\Space cron))))
                                        (error "Invalid schedule format: ~A" cron))
                                    (let ((didit-key (str:concat "/didit/" prefix "/" token)))
                                      (log:info didit-key)
                                      (setf (cl-etcd:get-etcd didit-key etcd) "1")
                                      (log:info ">>>>>>>>>>>>>>>>>>>>>>>> ~A" (format nil "~A/~A" prefix (gethash "alert" value)) *alerts-table*)
                                      (setf (gethash (format nil "/didit/~A/~A" prefix token) *didit-table*)
                                            (make-didit
                                             :name (gethash "name" value)
                                             :alert (gethash (format nil "~A/~A" prefix (gethash "alert" value)) *alerts-table*)
                                             :token token
                                             :scheduler-task (scheduler:create-scheduler-task
                                                              *scheduler*
                                                              (format nil "~A (didit:check-didit \"~A\")" cron didit-key))))
                                      (log:info ">> ~A" (gethash (format nil "/didit/~A/~A" prefix token) *didit-table*)))))
                                didits)))))
               repos))))))
  "loaded")
