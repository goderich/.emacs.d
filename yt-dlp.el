;;; ~/.emacs.d/yt-dlp.el -*- lexical-binding: t; -*-

(defun +yt-dlp-link ()
  (interactive)
  (let ((link (+yt-dlp--get-link)))
    (if link
        (progn
          (message "Starting download...")
          (set-process-sentinel
           (start-process "yt-dlp" "*yt-dlp*" "yt-dlp" link)
           #'yt-dlp-process-sentinel))
      (user-error "No links available!"))))

(defun +yt-dlp--get-link ()
  "Get a link as a string (dwim).
If on a link, copies that link.
If not on a link, prompts the user to select one with avy."
  (let ((link)))
  (if (org-in-regexp org-link-any-re) 
      (-some-> (link-hint--get-link-at-point)
        (map-elt :args))
    (let ((link-hint-types '(link-hint-text-url link-hint-org-link)))
      (-some-> (link-hint--get-links)
        (link-hint--process)
        (map-elt :args)))))

(defun yt-dlp-process-sentinel (process event)
  "Sentinel for use by this module.
Sends a notification in Emacs and the system upon completion,
successful or otherwise."
  (when (eq (process-status process) 'exit)
    (if (string-match "finished" event)
        (progn
          (shell-command "notify-send 'yt-dlp' 'Finished successfully.' --icon=dialog-information --expire-time=5000")
          (message "Process yt-dlp finished successfully!"))
      (shell-command "notify-send 'yt-dlp' 'Error, could not compile.' --icon=dialog-information --expire-time=5000")
      (message "Error: yt-dlp could not download"))))
