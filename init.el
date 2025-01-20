;; Bootstrap elpaca
(load "~/.emacs.d/elpaca-bootstrap")

;; Load literate config
(org-babel-load-file (concat user-emacs-directory "config.org"))
