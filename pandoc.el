;;; ~/.emacs.d/pandoc.el -*- lexical-binding: t; -*-

;; Elisp functions to call Pandoc from Emacs
(require 'f)

(defun pandoc--defaults-option (extension)
  (pcase extension
    ("pdf" "-dpdf")
    ("html" "-drev")
    ("docx" "-ddoc")))

(defun pandoc--find-csl (dir)
  "Find a single CSL file in DIR or supply a default."
  (let ((fs (f-glob "*.csl" dir))
        (default (f-full "~/dotfiles/pandoc/.local/share/pandoc/defaults/linguistics.csl")))
    (when (length> fs 1)
      ;; TODO: allow user to select CSL file?
      (error "Error: more than one CSL file in current directory!"))
    (or (-first-item fs) default)))

(defun pandoc--output-name (input extension handout?)
  "Generate output file name from the INPUT."
  (if handout?
      (-> input (f-no-ext) (s-concat "-handout." extension))
    (f-swap-ext input extension)))

(cl-defun pandoc-org--convert (&key extension self-contained numbered handout empty)
  "Convert the current file using pandoc.
The format and the defaults file need to be supplied by the caller."
  (save-buffer)
  (let* ((args (pandoc-org--get-args extension self-contained numbered handout empty)))
    (message "Calling: %s" args)
    (set-process-sentinel
     (apply #'start-process "pandoc" "*pandoc*" args)
     #'pandoc-process-sentinel)))

(defun pandoc-org--get-args (extension self-contained numbered handout empty)
  "Helper function to construct the correct pandoc call."
  (let* ((input (f-this-file))
         (dir (f-dirname input))
         (output (pandoc--output-name input extension handout))
         (defaults (pandoc--defaults-option extension))
         (metadata (f-join dir "metadata.yaml"))
         (style? (f-exists? (f-join dir "style.css")))
         (csl (pandoc--find-csl dir)))
    `("pandoc" ,input ,defaults
      ,@(when (f-exists? metadata) `("--metadata-file" ,metadata))
      ;; Temporary bugfix for https://github.com/jgm/pandoc/issues/6431
      ,@(when (string= extension "html")
          '("-V" "revealjs-url=https://unpkg.com/reveal.js@^4"))
      ,@(when (and (string= extension "html") style?)
          '("--css" "./style.css"))
      ,@(when (string= extension "html")
          (if handout
              '("--metadata=handout" "--incremental=false")
            '("--incremental=true")))
      ,@(when (and (string= extension "html") self-contained)
          '("--embed-resources=true" "--standalone"))
      ,@(when (and (member extension '("pdf" "docx")) numbered)
          '("--number-sections"))
      ,@(when (and (string= extension "pdf") empty)
          '("--variable=pagestyle:empty"))
      "--csl" ,csl
      "-o" ,output)))

(defvar pandoc-org->pdf-hook nil
  "Hook to run before converting from org-mode to PDF.")

(defun pandoc-org->pdf ()
  "Convert the current file to pdf using pandoc.
Works only on org files using my pdf template."
  (interactive)
  (run-hooks 'pandoc-org->pdf-hook)
  (let ((num? (transient-arg-value "number-sections" (transient-args 'pandoc-transient)))
        (empty? (transient-arg-value "empty" (transient-args 'pandoc-transient))))
    (pandoc-org--convert :extension "pdf" :numbered num? :empty empty?)))

(defun pandoc-org->revealjs ()
  "Convert the current file to revealjs using pandoc.
Works only on org files using my revealjs template."
  (interactive)
  (let ((handout? (transient-arg-value "handout" (transient-args 'pandoc-transient)))
        (self-con? (transient-arg-value "self-contained" (transient-args 'pandoc-transient))))
    (pandoc-org--convert :extension "html" :handout handout? :self-contained self-con?)))

(defun pandoc-org->docx ()
  "Convert the current file to pdf using pandoc.
Works only on org files using my docx template."
  (interactive)
  (pandoc-org--convert :extension "docx"))

(transient-define-infix pandoc--handout? ()
  :argument "handout"
  :shortarg "h"
  :class 'transient-switch
  :description "Toggle handout mode (reveal.js only)."
  :init-value (lambda (obj)
                (oset obj value nil)))

(transient-define-infix pandoc--number-sections? ()
  :argument "number-sections"
  :shortarg "n"
  :class 'transient-switch
  :description "Toggle section numbering (pdf/doc)."
  :init-value (lambda (obj)
                (oset obj value "number-sections")))

(transient-define-infix pandoc--self-contained? ()
  :argument "self-contained"
  :shortarg "s"
  :class 'transient-switch
  :description "Toggle self-contained file (reveal.js only)."
  :init-value (lambda (obj)
                (oset obj value nil)))

(transient-define-infix pandoc--empty? ()
  :argument "empty"
  :shortarg "e"
  :class 'transient-switch
  :description "Toggle empty style (no page numbers, pdf only)."
  :init-value (lambda (obj)
                (oset obj value nil)))

(transient-define-prefix pandoc-transient ()
  ["Convert this file with pandoc..."
   [("p" "to pdf" pandoc-org->pdf)
    ("r" "to revealjs" pandoc-org->revealjs)
    ("d" "to docx" pandoc-org->docx)]
   [("q" "quit" transient-quit-all)]]
  ["Options"
   [(pandoc--self-contained?)
    (pandoc--number-sections?)
    (pandoc--handout?)
    (pandoc--empty?)]])

(defun pandoc-process-sentinel (process event)
  "Sentinel for use by this module.
Sends a notification in Emacs and the system upon completion,
successful or otherwise."
  (when (eq (process-status process) 'exit)
    (if (string-match "finished" event)
        (progn
          (shell-command "notify-send 'Pandoc' 'Finished successfully.' --icon=dialog-information --expire-time=10000")
          (message "Process pandoc finished successfully!"))
      (shell-command "notify-send 'Pandoc' 'Error, could not compile.' --icon=dialog-information --expire-time=10000")
      (message "Error: pandoc could not compile!"))))
