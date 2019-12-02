;;; magit-buildkite.el --- Buildkite build information inside magit-status  -*- lexical-binding: t -*-

;; Copyright (C) 2019  Carlo Zancanaro

;; Author: Carlo Zancanaro <carlo@zancanaro.id.au>
;; Homepage: https://github.com/czan/magit-buildkite
;; Package-Requires: ((request "0.3.1") (magit "2.90.1"))

;; magit-buildkite is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License version
;; 3 as published by the Free Software Foundation.
;;
;; magit-buildkite is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with magit-buildkite.  If not, see http://www.gnu.org/licenses.

;;; Code:

(require 'ansi-color)
(require 'cl)
(require 'hideshow)
(require 'magit)
(require 'request)

(defvar magit-buildkite-auth-token nil)
(put 'magit-buildkite-auth-token 'safe-local-variable #'stringp)

(defvar magit-buildkite-organization nil)
(put 'magit-buildkite-organization 'safe-local-variable #'stringp)

(defvar magit-buildkite-pipeline nil)
(put 'magit-buildkite-pipeline 'safe-local-variable #'stringp)

(defface magit-buildkite-job-status-good
  '((t (:inherit magit-diff-added)))
  "Face to use for successful job status information")

(defface magit-buildkite-job-status-bad
  '((t (:inherit magit-diff-removed)))
  "Face to use for failing job status information")

(defface magit-buildkite-job-status-neutral
  '((t (:inherit magit-dimmed)))
  "Face to use for job status information that is neither successful nor failing")

(defface magit-buildkite-job-type
  '((t (:inherit magit-dimmed)))
  "Face to use for job type information")

(defun magit-buildkite--json-read ()
  (let ((json-object-type 'plist)
        (json-key-type 'symbol)
        (json-false :json-false))
    (json-read)))

(defvar-local magit-buildkite--buffer-refresh-token
  nil)

(defmacro plist-format (string plist)
  (let* ((plist-sym (gensym "plist"))
         (arguments nil)
         (format-string (replace-regexp-in-string "{[^}]+}"
                                                  (lambda (text)
                                                    (let ((name (substring text 1 (1- (length text)))))
                                                      (push `(plist-get ,plist-sym ',(intern name))
                                                            arguments)
                                                      "%s"))
                                                  string
                                                  :fixedcase
                                                  :literal)))
    `(let ((,plist-sym ,plist))
       (format ,format-string ,@(reverse arguments)))))

(defun magit-buildkite-visit ()
  (interactive)
  (let* ((properties (text-properties-at (point)))
         (url (plist-get properties 'magit-buildkite-web-url)))
    (when url
      (browse-url url))))

(defun magit-buildkite-log-toggle-all ()
  "If any outline sections are shown, hide them all. Otherwise,
show them all."
  (interactive)
  (save-mark-and-excursion
    (let ((all-visible? t))
      (outline-map-region (lambda ()
                            (end-of-line)
                            (setq all-visible? (and all-visible?
                                                    (not (outline-invisible-p)))))
                          (point-min) (point-max))
      (outline-map-region (lambda ()
                            (if all-visible?
                                (outline-hide-subtree)
                              (outline-show-children)
                              (outline-show-entry)))
                          (point-min) (point-max)))))

(defun magit-buildkite-automatically-open ()
  (save-match-data
    (save-mark-and-excursion
      (beginning-of-buffer)
      (while (re-search-forward (rx bol (? "^^^ ") "+++") nil :noerror)
        (outline-show-entry)))))

(defvar magit-buildkite-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'bury-buffer)
    (define-key map (kbd "<tab>") #'outline-toggle-children)
    (define-key map (kbd "<backtab>") #'magit-buildkite-log-toggle-all)
    (define-key map (kbd "C-c C-n") #'outline-next-visible-heading)
    (define-key map (kbd "C-c C-p") #'outline-previous-visible-heading)
    map))

(define-derived-mode magit-buildkite-log-mode special-mode "Buildkite[Log]"
  "A mode for viewing log files from Buildkite."
  (ansi-color-apply-on-region (point-min) (point-max))
  (view-mode)
  (outline-minor-mode)
  (setq-local outline-regexp "~~~ \\|\\$ \\|--- \\|\\+\\+\\+ ")
  (setq-local outline-heading-alist '(("~~~ " . 1)
                                      ("$ " . 2)
                                      ("--- " . 3)
                                      ("+++ " . 3)))
  (outline-hide-sublevels 1)
  (visual-line-mode -1)
  (magit-buildkite-automatically-open))

(defun magit-buildkite--maintaining-section (func)
  (declare (indent defun))
  (let ((refresh-token magit-buildkite--buffer-refresh-token)
        (current magit-insert-section--current)
        (parent magit-insert-section--parent)
        (oldroot magit-insert-section--oldroot))
    (lambda (&rest args)
      (when (eq magit-buildkite--buffer-refresh-token refresh-token)
        (let ((magit-insert-section--current current)
              (magit-insert-section--parent parent)
              (magit-insert-section--oldroot oldroot)
              (hide-at-end (when (magit-section-invisible-p current)
                             (magit-section-show current)
                             t)))
          (unwind-protect (apply func args)
            (when hide-at-end
              (magit-section-hide current))))))))

(defun magit-buildkite-get-request-raw (url type success error)
  (declare (indent defun))
  (setq type (or type 'json))
  (request url
           :headers `(("Authorization" . ,(concat "Bearer " magit-buildkite-auth-token))
                      ("Accept" . ,(case type
                                     (json "application/json")
                                     (text "text/plain"))))
           :parser (case type
                     (json #'magit-buildkite--json-read)
                     (text #'buffer-string))
           :success (lambda (&rest args)
                      (condition-case err
                          (apply success args)
                        (error (message "error in request success handler: %s (from %s)" err url))))
           :error (lambda (&rest args)
                    (condition-case err
                        (apply error args)
                      (error (message "error in request error handler: %s" err))))))

(defun magit-buildkite-get-request (url type success error)
  (declare (indent defun))
  (magit-buildkite-get-request-raw url type
    (magit-buildkite--maintaining-section success)
    (magit-buildkite--maintaining-section error)))

(defun magit-buildkite-update-heading* (properties thunk)
  (with-slots (start content end) magit-insert-section--current
    (with-current-buffer (marker-buffer start)
      (save-mark-and-excursion
        (save-restriction
          (narrow-to-region start content)
          (beginning-of-buffer)
          (let ((inhibit-read-only t)
                (base-properties (text-properties-at (point))))
            (unwind-protect (funcall thunk)
              (set-marker start (point-min))
              (set-marker content (point-max))
              (add-text-properties start content properties))))))))

(defmacro magit-buildkite-update-heading (properties &rest body)
  (declare (indent defun))
  `(magit-buildkite-update-heading* ,properties (lambda () ,@body)))

(defmacro magit-buildkite-replace-heading (properties &rest body)
  (declare (indent defun))
  `(magit-buildkite-update-heading ,properties
     (unwind-protect (progn ,@body)
       (delete-region (point) (point-max)))))

(defun magit-buildkite--update-parent-ends (section)
  (with-slots (end parent) section
    (message "%s" (eq (car (last (slot-value parent 'children)))
                      section))
    (when (eq (car (last (slot-value parent 'children)))
              section)
      (set-marker (slot-value parent 'end)
                  (marker-position end))
      (magit-buildkite--update-parent-ends parent))))

(defun magit-buildkite-update-body* (properties thunk)
  (declare (indent defun))
  (with-slots (start content end) magit-insert-section--current
    (with-current-buffer (marker-buffer start)
      (save-mark-and-excursion
        (save-restriction
          (narrow-to-region content end)
          (beginning-of-buffer)
          (let ((inhibit-read-only t))
            (unwind-protect (funcall thunk)
              (set-marker content (point-min))
              (set-marker end (point-max))
              (add-text-properties content end properties)
              (magit-buildkite--update-parent-ends magit-insert-section--current))))))))

(defmacro magit-buildkite-update-body (properties &rest body)
  (declare (indent defun))
  `(magit-buildkite-update-body* ,properties (lambda () ,@body)))

(defmacro magit-buildkite-replace-body (properties &rest body)
  (declare (indent defun))
  `(magit-buildkite-update-body ,properties
     (unwind-protect (progn ,@body)
       (delete-region (point) (point-max)))))

(defmacro magit-buildkite-insert-section-body (&rest body)
  "Exactly the same as `magit-insert-section-body', but
additionally maintains the necessary references for the family of
update/replace heading/body functions."
  (declare (indent defun))
  `(let ((thunk (magit-buildkite--maintaining-section (lambda () ,@body))))
     (magit-insert-section-body (funcall thunk))))

(defun magit-buildkite-visit-log ()
  (interactive)
  (let* ((properties (text-properties-at (point)))
         (url (plist-get properties 'magit-buildkite-raw-log-url)))
    (when url
      (magit-buildkite-get-request-raw url 'text
        (lambda (&key data &rest args)
          (with-current-buffer
              (get-buffer-create
               (plist-format "*buildkite log: {magit-buildkite-job-name} (build #{magit-buildkite-build-number})*"
                             properties))
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert data)
              ;; hackity hack-hack! :)
              (replace-string "" "" nil (point-min) (point-max))
              (replace-string "\n" "\n" nil (point-min) (point-max))
              (replace-string "" "\n" nil (point-min) (point-max))
              (magit-buildkite-log-mode)
              (beginning-of-buffer))
            (pop-to-buffer (current-buffer))))
        (lambda (&rest args)
          (message "Error retrieving log for job %s" (plist-get properties 'magit-buildkite-job-name)))))))

(defmacro magit-buildkite--with-advised (advice &rest body)
  "

\(fn (SYMBOL WHERE FUNCTION &rest PROPS) &rest BODY)"
  (declare (indent 1))
  (let ((function-sym (gensym "function")))
    (destructuring-bind (symbol where function &rest props) advice
      `(let ((,function-sym ,function))
         (unwind-protect (progn (advice-add ',symbol ,where ,function-sym ,@props)
                                ,@body)
           (advice-remove ',symbol ,function-sym))))))

(defun magit-buildkite-visit-artifact ()
  (interactive)
  (let* ((properties (text-properties-at (point)))
         (url (plist-get properties 'magit-buildkite-download-url)))
    (when url
      ;; this is one of the hackiest things ever, but it's to stop
      ;; `request' from following redirects (which is done by removing
      ;; the "--location" flag from the underlying curl command
      (magit-buildkite--with-advised
          (request--curl-command :filter-return (lambda (args) (remove "--location" args)))
        (magit-buildkite-get-request-raw url 'json
          (lambda (&rest args)
            (let ((artifact-url (plist-get (plist-get args :data) 'url)))
              (browse-url artifact-url)))
          (lambda (&rest args)
            (message "Error retrieving artifact")))))))

(defvar magit-buildkite-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magit-buildkite-visit)
    map))

(defvar magit-buildkite-job-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magit-buildkite-visit)
    map))

(defvar magit-buildkite-job-log-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magit-buildkite-visit-log)
    map))

(defvar magit-buildkite-artifact-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magit-buildkite-visit-artifact)
    map))

(defun magit-buildkite-update-triggered-job (job-state triggered-build indent)
  (let ((triggered-build-url (plist-get triggered-build 'url)))
    (when triggered-build-url
      (magit-buildkite-insert-section-body
        (insert (loop for i below (+ indent 2) concat " ")
                "loading triggered jobs...\n")
        (magit-buildkite-get-request triggered-build-url 'json
          (lambda (&rest args)
            (let ((data (plist-get args :data)))
              (magit-buildkite-update-heading ()
                (let ((build-state (plist-get data 'state)))
                  (when (search-forward (format "[%s]" job-state) nil t)
                    (replace-match (propertize (format "[%s]" build-state)
                                               'face (pcase build-state
                                                       ("passed" 'magit-buildkite-job-status-good)
                                                       ("failed" 'magit-buildkite-job-status-bad)
                                                       (_ 'magit-buildkite-job-status-neutral))
                                               'magit-buildkite-web-url (plist-get triggered-build 'web_url))
                                   nil t))))
              (magit-buildkite-replace-body `(magit-buildkite-build-number ,(plist-get data 'number))
                (map nil (lambda (job)
                           (magit-buildkite-insert-job job (+ indent 2)))
                     (plist-get data 'jobs)))))
          (lambda (&rest args)
            (magit-buildkite-replace-body `(magit-buildkite-build-number ,(plist-get data 'number))
              (magit-insert-section (buildkite-error nil t)
                (insert
                 (loop for i below (+ indent 2)
                       concat " ")
                 "Error fetching build data\n")))))))))

(defun magit-buildkite-insert-artifact-list (url indent)
  (insert (loop for i below (+ indent 2) concat " ")
          "loading artifact list...\n")
  (magit-buildkite-get-request url 'json
    (lambda (&rest args)
      (let* ((data (plist-get args :data))
             (link (request-response-header (plist-get args :response) "link"))
             (next (save-match-data
                     (when (and link (string-match "<\\([^>]*\\)>; rel=\"next\"" link))
                       (match-string 1 link)))))
        (magit-buildkite-update-body ()
          (end-of-buffer)
          (previous-line)
          (beginning-of-line)
          (delete-region (point) (point-max))
          (map nil
               (lambda (artifact)
                 (magit-insert-section artifact-section (buildkite-artifact nil t)
                   (insert (loop for i below (+ indent 2) concat " ")
                           (plist-format "{filename}\n" artifact))
                   (add-text-properties (slot-value artifact-section 'start) (point)
                                        (list 'magit-buildkite-download-url (plist-get artifact 'download_url)))))
               data)
          (when next
            (magit-buildkite-insert-artifact-list next indent)))))
    (lambda (&rest args)
      (magit-buildkite-update-body ()
        (end-of-buffer)
        (magit-insert-section (buildkite-error nil t)
          (insert (loop for i below (+ indent 2) concat " ")
                  "Error fetching artifact information\n"))))))

(defun magit-buildkite-insert-build-information (job indent)
  (magit-buildkite-insert-section-body
    (magit-insert-section log-section (buildkite-job-log nil t)
      (insert (loop for i below (+ indent 2) concat " ")
              "View build log\n")
      (add-text-properties (slot-value log-section 'start) (point)
                           (list 'magit-buildkite-raw-log-url (plist-get job 'raw_log_url)
                                 'magit-buildkite-web-url (plist-get job 'web_url)
                                 'magit-buildkite-job-name (plist-get job 'name))))
    (magit-buildkite-insert-artifact-list (concat (plist-get job 'artifacts_url) "?per_page=100") indent)))

(defun magit-buildkite-insert-job (job &optional indent)
  (setq indent (or indent 0))
  (when (plist-get job 'name)
    (let ((trigger? (string= (plist-get job 'type) "trigger"))
          (state (if (eq (plist-get job 'soft_failed) t)
                     "failed"
                   (plist-get job 'state))))
      (magit-insert-section section (buildkite-job (plist-get job 'id) t)
        (magit-insert-heading
          (loop for i below indent
                concat " ")
          (propertize (format "[%s]" state)
                      'face (pcase state
                              ("passed" 'magit-buildkite-job-status-good)
                              ("failed" 'magit-buildkite-job-status-bad)
                              (_ 'magit-buildkite-job-status-neutral)))
          " "
          (plist-get job 'name)
          (when trigger?
            (concat " "
                    (propertize (plist-format "[{type}]" job)
                                'face 'magit-buildkite-job-type))))
        (if trigger?
            (let ((triggered-build (plist-get job 'triggered_build)))
              (add-text-properties (slot-value section 'start) (point)
                                   (list 'magit-buildkite-web-url
                                         (plist-get triggered-build 'web_url)))
              (magit-buildkite-update-triggered-job state triggered-build indent))
          (add-text-properties (slot-value section 'start) (point)
                               (list 'magit-buildkite-web-url (plist-get job 'web_url)))
          (magit-buildkite-insert-build-information job indent))))))

(defun magit-buildkite-insert-recent-builds ()
  (when (and magit-buildkite-auth-token
             magit-buildkite-organization
             magit-buildkite-pipeline)
    (setq-local magit-buildkite--buffer-refresh-token (gensym "refresh-token"))
    (let ((url (format "https://api.buildkite.com/v2/organizations/%s/pipelines/%s/builds?per_page=1&commit=%s"
                       magit-buildkite-organization
                       magit-buildkite-pipeline
                       (magit-rev-format "%H"))))
      (magit-insert-section (buildkite nil t)
        (magit-insert-heading "Buildkite")
        (magit-buildkite-insert-section-body
          (insert "loading...\n\n"))
        (magit-buildkite-get-request url 'json
          (lambda (&rest args)
            (let ((data (plist-get args :data)))
              (if (> (length data) 0)
                  (let* ((build (aref data 0))
                         (build-number (plist-get build 'number)))
                    (magit-buildkite-replace-heading `(magit-buildkite-web-url ,(plist-get build 'web_url))
                      (magit-insert-heading
                        (format "Buildkite build %s [%s]\n"
                                build-number
                                (if (eq (plist-get build 'blocked) :json-false)
                                    (plist-get build 'state)
                                  "blocked"))))
                    (magit-buildkite-replace-body `(magit-buildkite-build-number ,build-number)
                      (let* ((build (aref data 0))
                             (build-number (plist-get build 'number)))
                        (map nil #'magit-buildkite-insert-job
                             (plist-get build 'jobs))
                        (insert "\n"))))
                (magit-buildkite-replace-body ()
                  (insert "No build data found\n")
                  (insert "\n")))))
          (lambda (&rest args)
            (magit-buildkite-replace-body ()
              (magit-insert-section (buildkite-error nil t)
                (insert "Error fetching build data\n\n")))))))))

(define-minor-mode magit-buildkite-mode
  "Add a new section to the magit status screen which loads
  Buildkite build information for the current commit."
  :init-value nil
  :lighter " Buildkite"
  (when (eq major-mode 'magit-status-mode)
    (if magit-buildkite-mode
        (magit-add-section-hook 'magit-status-sections-hook
                                #'magit-buildkite-insert-recent-builds
                                'magit-insert-recent-commits
                                :append)
      (setq magit-status-sections-hook
            (remove #'magit-buildkite-insert-recent-builds magit-status-sections-hook)))))

(provide 'magit-buildkite)
