;;; ~/.emacs.d/pandoc.el -*- lexical-binding: t; -*-

;; Elisp functions to call Pandoc from Emacs
(require 'f)

(defun pandoc--extension (format)
  (pcase format
    ("latex" "pdf")
    ("typst" "pdf")
    ("revealjs" "html")
    ("docx" "docx")))

(defun pandoc--defaults-option (format)
  (pcase format
    ("latex" "-dpdf")
    ("typst" "-dtypst")
    ("revealjs" "-drev")
    ("docx" "-ddoc")))

(defun pandoc--find-csl (dir)
  "Find a single CSL file in DIR or supply a default."
  (let ((fs (f-glob "*.csl" dir))
        (default (f-full "~/dotfiles/pandoc/.local/share/pandoc/defaults/linguistics.csl")))
    (pcase (length fs)
      (0 default)
      (1 (-first-item fs))
      (_ (completing-read "Choose CSL file: " fs)))))

(defun pandoc--output-name (input extension handout?)
  "Generate output file name from the INPUT."
  (if handout?
      (-> input (f-no-ext) (s-concat "-handout." extension))
    (f-swap-ext input extension)))

(cl-defun pandoc-org--convert (&key format self-contained numbered handout empty)
  "Convert the current file using pandoc.
The format and the defaults file need to be supplied by the caller."
  (save-buffer)
  (let* ((input (f-this-file))
         (dir (f-dirname input))
         (extension (pandoc--extension format))
         (outfile (pandoc--output-name input extension handout))
         (defaults (pandoc--defaults-option format))
         (metadata (f-join dir "metadata.yaml"))
         (style? (f-exists? (f-join dir "style.css")))
         (csl (pandoc--find-csl dir))
         (command
          `("pandoc" ,input ,defaults
            ,@(when (f-exists? metadata) `("--metadata-file" ,metadata))
            ;; Temporary bugfix for https://github.com/jgm/pandoc/issues/6431
            ,@(when (string= format "revealjs")
                '("-V" "revealjs-url=https://unpkg.com/reveal.js@^4"))
            ,@(when (and (string= format "revealjs") style?)
                '("--css" "./style.css"))
            ,@(when (string= format "revealjs")
                (if handout
                    '("--metadata=handout" "--incremental=false")
                  '("--incremental=true")))
            ,@(when (and (string= format "revealjs") self-contained)
                '("--embed-resources=true" "--standalone"))
            ,@(when (and (member extension '("pdf" "docx")) numbered)
                '("--number-sections"))
            ,@(when (and (string= format "latex") empty)
                '("--variable=pagestyle:empty"))
            ;; Temporarily disable CSL styles in typst due to bug
            ,@(unless (string= format "typst")
                `("--csl" ,csl))
            "-o" ,outfile)))
    (message "Calling: %s" command)
    (make-process :name "pandoc"
                  :buffer "*pandoc*"
                  :command command
                  :sentinel (pandoc-process-sentinel outfile handout))))

(defvar pandoc--org->pdf-pre-hook nil
  "Hook to run before converting from org-mode to PDF.")

(defvar pandoc--org->revealjs-handout-post-hook nil
  "Hook to run after converting from org-mode to revealjs with the handout flag enabled.")

(defun pandoc--org->pdf-latex ()
  "Convert org file to PDF via LaTeX with chosen settings."
  (interactive)
  (run-hooks 'pandoc--org->pdf-pre-hook)
  (let ((num? (transient-arg-value "number-sections" (transient-args 'pandoc--pdf-latex-transient)))
        (empty? (transient-arg-value "empty" (transient-args 'pandoc--pdf-latex-transient))))
    (pandoc-org--convert :format "latex" :numbered num? :empty empty?)))

(transient-define-prefix pandoc--pdf-latex-transient ()
  ["Convert to PDF via LaTeX..."
   [("c" "convert" pandoc--org->pdf-latex)
    ("q" "quit" transient-quit-all)]]
  ["Options"
   [(pandoc--number-sections?)
    (pandoc--empty?)]])

(defun pandoc--org->pdf-typst ()
  "Convert the current file to pdf using pandoc.
Works only on org files using my pdf template."
  (interactive)
  (run-hooks 'pandoc-org->pdf-pre-hook)
  (let ((num? (transient-arg-value "number-sections" (transient-args 'pandoc--pdf-typst-transient)))
        (empty? (transient-arg-value "empty" (transient-args 'pandoc--pdf-typst-transient))))
    (pandoc-org--convert :format "typst" :numbered num? :empty empty?)))

(transient-define-prefix pandoc--pdf-typst-transient ()
  ["Convert to PDF via LaTeX..."
   [("c" "convert" pandoc--org->pdf-typst)
    ("q" "quit" transient-quit-all)]]
  ["Options"
   [(pandoc--number-sections?)
    (pandoc--empty?)]])

(defun pandoc--org->revealjs ()
  "Convert the current file to revealjs using pandoc.
Works only on org files using my revealjs template."
  (interactive)
  (let ((handout? (transient-arg-value "handout" (transient-args 'pandoc--revealjs-transient)))
        (self-con? (transient-arg-value "self-contained" (transient-args 'pandoc--revealjs-transient))))
    (pandoc-org--convert :format "revealjs" :handout handout? :self-contained self-con?)))

(transient-define-prefix pandoc--revealjs-transient ()
  ["Convert to PDF via LaTeX..."
   [("c" "convert" pandoc--org->revealjs)]
   [("q" "quit" transient-quit-all)]]
  ["Options"
   [(pandoc--handout?)
    (pandoc--self-contained?)]])

(defun pandoc--org->docx ()
  "Convert the current file to pdf using pandoc.
Works only on org files using my docx template."
  (interactive)
  (let ((num? (transient-arg-value "number-sections" (transient-args 'pandoc--docx-transient))))
    (pandoc-org--convert :format "docx" :numbered num?)))

(transient-define-prefix pandoc--docx-transient ()
  ["Convert to PDF via LaTeX..."
   [("c" "convert" pandoc--org->docx)
    ("q" "quit" transient-quit-all)]]
  ["Options"
   [(pandoc--number-sections?)]])

(transient-define-infix pandoc--handout? ()
  :argument "handout"
  :shortarg "h"
  :class 'transient-switch
  :description "Toggle handout mode."
  :init-value (lambda (obj)
                (oset obj value nil)))

(transient-define-infix pandoc--number-sections? ()
  :argument "number-sections"
  :shortarg "n"
  :class 'transient-switch
  :description "Toggle section numbering."
  :init-value (lambda (obj)
                (oset obj value "number-sections")))

(transient-define-infix pandoc--self-contained? ()
  :argument "self-contained"
  :shortarg "s"
  :class 'transient-switch
  :description "Toggle self-contained file."
  :init-value (lambda (obj)
                (oset obj value nil)))

(transient-define-infix pandoc--empty? ()
  :argument "empty"
  :shortarg "e"
  :class 'transient-switch
  :description "Toggle empty style."
  :init-value (lambda (obj)
                (oset obj value nil)))

(transient-define-prefix pandoc-transient ()
  ["Convert this file with pandoc..."
   [("p" "to pdf (LaTeX)" pandoc--pdf-latex-transient)
    ("t" "to pdf (typst)" pandoc--pdf-typst-transient)
    ("r" "to revealjs" pandoc--revealjs-transient)
    ("d" "to docx" pandoc--docx-transient)]
   [("q" "quit" transient-quit-all)]])

(defun pandoc-process-sentinel (outfile handout?)
  "Sentinel for use by this module.

Sends a notification in Emacs and the system upon completion,
successful or otherwise.

The current iteration is a closure over the HANDOUT? flag,
so that it can be passed as an additional argument."
  (lambda (process event)
    (when (eq (process-status process) 'exit)
      (if (string-match "finished" event)
          (progn
            (shell-command "notify-send 'Pandoc' 'Finished successfully.' --icon=dialog-information --expire-time=5000")
            (message "Process pandoc finished successfully!")
            (when handout?
              (run-hook-with-args 'pandoc--org->revealjs-handout-post-hook outfile)))
        (shell-command "notify-send 'Pandoc' 'Error, could not compile.' --icon=dialog-information --expire-time=5000")
        (message "Error: pandoc could not compile!")))))
