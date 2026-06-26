((nil . ((eval . (let ((project-root (locate-dominating-file default-directory ".git")))
                   (if project-root
                       (progn
                         (setq-local default-directory project-root)
                         (setq-local compile-command "make")
                         (setq-local project-compile-command "make"))rr
                     (message "No .git found, using current directory")))))))
