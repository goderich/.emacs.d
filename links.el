;;; ~/.emacs.d/links.el -*- lexical-binding: t; -*-

;; Org-mode links
(require 'org)
(require 'ol)
(require 'consult)
(require 'dash)
(require 'f)

(defun +insert-link (address &optional name)
  "Insert an Org link to ADDRESS.
Prompts for a link name (the string that will be visible
as the hyperlink text). If the prompt is left blank,
uses NAME if it's provided, and ADDRESS otherwise.

If on a whitespace at the end of the line,insert an extra
space before the link. This is useful when inserting links
in normal mode."
  (let* ((default (substring-no-properties (or name address)))
         (prompt (concat "Link name (default \"" default "\"): "))
         (link-name (read-string prompt "" nil default))
         (link-string (org-link-make-string address link-name)))
    (if (and (eolp) (not (eobp)) (= (char-after) 32))
        (insert " " link-string)
      (insert link-string))))

(defun +consult-org-get-heading-text ()
  "Get the text of an Org heading with completion, using `consult'."
  (save-excursion
    (consult-org-heading)
    (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment)))

(defun +org-insert-link ()
  "Insert link to org-mode heading with completion."
  (interactive)
  (+insert-link (+consult-org-get-heading-text)))

(defun +org-insert-link-from-clipboard ()
  "Insert org link from clipboard.
Prompts for link name."
  (interactive)
  (let ((address (substring-no-properties (current-kill 0))))
    (+insert-link address)))

(defun +org--get-id-heading ()
  "Get the text and ID of an org heading.
Creates the ID if one isn't already present."
  (save-excursion
    (consult-org-heading)
    (let ((heading (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment))
          (id (concat "id:" (org-id-get-create))))
      (list id heading))))

(defun +org-insert-link-with-id ()
  "Insert a link to a heading with completion, using a unique ID."
  (interactive)
  (apply #'+insert-link (+org--get-id-heading)))

(defun +org-insert-last-stored-link ()
  "Assumes that `org-stored-links' is non-nil."
  (let ((link (pop org-stored-links)))
    (+insert-link (car link) (cadr link))))

(defun +org-insert-file-path (&optional absolute?)
  "Insert file path at point as an org-link, with autocompletion."
  ;; see `org-link-complete-file'
  (interactive)
  (let* ((result (find-file-read-args "Copy file path: "
                                     (confirm-nonexistent-file-or-buffer)))
         (link (if absolute?
                   (-first-item result)
                 (f-relative (-first-item result)))))
    (if (-second-item result)
        (+insert-link (concat "file:" link))
      (message "Error retrieving file path."))))

(defun +org-insert-absolute-file-path ()
  (interactive)
  (+org-insert-file-path t))

(defun +org-insert-image-path (&optional absolute?)
  (interactive)
  (let* ((result (find-file-read-args "Copy file path: "
                                     (confirm-nonexistent-file-or-buffer)))
         (link (if absolute?
                   (-first-item result)
                 (f-relative (-first-item result)))))
    (if (-second-item result)
        (insert (concat "[[./" link "]]"))
      (message "Error retrieving file path."))))

(defun +org-link-dwim ()
  "Do-what-I-mean for linking.
If on a heading, link it. If on a link, edit it.
Elsewhere, insert last stored link:
either from `org-stored-links', or from the clipboard.
This is a convenience function to bind it to a single keystroke."
  (interactive)
  (cond
   ((org-at-heading-p) (org-store-link nil 1))
   ((org-in-regexp org-link-bracket-re) (org-insert-link))
   ((and org-stored-links) (+org-insert-last-stored-link))
   (t (+org-insert-link-from-clipboard))))
