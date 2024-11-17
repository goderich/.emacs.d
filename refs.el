;;; ~/.emacs.d/refs.el -*- lexical-binding: t; -*-

;; Inserting pandoc references in org-mode with narrowing.
;; Thinking of a package name.
;; Maybe prior: Pandoc References In Org Rendition
;; Another idea: pox (pandoc/org X-ref)

(defun +ref--insert-reference (str &optional capitalize?)
  (let ((str (if capitalize? (s-capitalize str) str)))
    (insert "[cite: @" str "]")))

(defun +ref--org-set-custom-id ()
  "Create a new custom ID property at the current org heading.
Prompts for user input, converts it to lisp-case, and
sets that as the new CUSTOM_ID. If the input is left
blank, uses the heading text itself.

Returns the new CUSTOM_ID value as a string."
  (let ((custom-id
         (->> (org-get-heading)
              (read-string "Create new custom ID: " "" nil)
              (s-dashed-words)
              (concat "sec:"))))
    (org-set-property "CUSTOM_ID" custom-id)
    custom-id))

(defun +ref--org-get-custom-id ()
  "Retrieve the custom_id of a heading.
If one does not exist, create it.

The function prompts the user for a new custom ID. By default,
the heading name is used. The user input or heading is then
transformed into a lisp-case string."
  (save-excursion
    (consult-org-heading)
    (let* ((props (org-entry-properties))
           (custom-id (or (map-elt props "CUSTOM_ID")
                          (+ref--org-set-custom-id))))
      custom-id)))

(defun +ref-insert-ref-heading (&optional capitalize?)
  "Insert a pandoc reference to a heading, with completion.
We use narrowing to find the required heading, and then insert a link
using its CUSTOM_ID property. If the property isn't set, it is
created."
  (interactive)
  (let ((custom-id (+ref--org-get-custom-id)))
    (+ref--insert-reference custom-id capitalize?)))

(defun +ref-insert-ref-heading-capitalized ()
  (interactive)
  (+ref-insert-ref-heading 'capitalize))

(defun +ref--extract-latex-label (str)
  "Extract the text from inside a LaTeX label STR."
  (let ((regex (rx "\\label{" (group (1+ (or alpha ?: ?- ?_))) "}")))
    (string-match regex str)
    (match-string 1 str)))

(defun +ref--get-labels-latex (type)
  "Get a list of all LaTeX labels in this file beginning with the string TYPE."
  (->> (f-this-file)
       (shell-quote-argument)
       (concat "grep \"\\\\\\label{\"" type " ")
       (shell-command-to-string)
       (s-trim-right)
       (s-split "\n")
       (-map #'+ref--extract-latex-label)))

(defun +ref--get-labels-org (type)
  "Get a list of all org labels in this file beginning with the string TYPE."
  (->> (f-this-file)
       (shell-quote-argument)
       (concat "awk \'/^#\\+label:\\s+" type "/ {print $2}\' ")
       (shell-command-to-string)
       (s-trim-right)
       (s-split "\n")))

(defun +ref--get-labels (type)
  "Get a list of all labels in this file beginning with the string TYPE.
Searches for both org-mode and LaTeX style labels."
  (-remove #'s-blank? ; removes nil as well
   (-concat
    (+ref--get-labels-org type)
    (+ref--get-labels-latex type))))

(defun +ref--insert-crossref (type &optional capitalize?)
  "Insert an org-cite reference of a given TYPE.
Optionally capitalize it."
  (let ((choice (->> (+ref--get-labels type)
                     (completing-read "Choose candidate:"))))
    (+ref--insert-reference choice capitalize?)))

(defun +ref-insert-ref-table ()
  (interactive)
  (+ref--insert-crossref "tbl"))

(defun +ref-insert-ref-table-capitalized ()
  (interactive)
  (+ref--insert-crossref "tbl" 'capitalize))

(defun +ref-insert-ref-figure ()
  (interactive)
  (+ref--insert-crossref "fig"))

(defun +ref-insert-ref-figure-capitalized ()
  (interactive)
  (+ref--insert-crossref "fig" 'capitalize))
