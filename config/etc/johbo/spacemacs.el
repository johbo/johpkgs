;; Write a PID file if we are a daemon, so that supervisord can manage us with
;; pidproxy
(defun my/write-pid-file ()
  "Write the Emacs PID into a file"
  (let ((pid-file "~/var/run/emacs-server.pid")
        (pid (number-to-string (emacs-pid))))
    (with-temp-file pid-file (insert pid))))

(if (daemonp)
    (my/write-pid-file))

(setq tramp-shell-prompt-pattern
      "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

(setq org-agenda-files (quote ("~/n" "~/n/hr")))
(setq org-directory "~/n")
(setq org-default-notes-file (concat org-directory "/capture.org"))

(setq org-export-backends '(ascii jira html icalendar latex beamer man md odt))

(setq org-capture-templates
 '(("t" "Tasks" entry (file+headline (concat org-directory "/capture.org")
                                     "Refile Tasks")
    "* TODO %?\n  %i\n  %a")
   ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
    "* %?\nEntered on %U\n  %i\n  %a")
   ("f" "Feedback" entry (file+datetree (concat org-directory "/hr/feedback.org"))
    "* %?\nEntered on %U\n  %i\n  %a")
   ("d" "Daily" entry (file+headline (concat org-directory "/daily.org")
                                     "Since the last daily"))
   ))



(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps t)
(setq org-refile-allow-creating-parent-nodes nil)
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))

;;  (org-babel-do-load-languages
;;   'org-babel-load-languages
;;   '((dot . t)
;;     (emacs-lisp . t)
;;     (js . t)
;;     (python . t)
;;     (sh . t)
;;     (sql . t)))

(setq flycheck-flake8rc "~/.nix-profile/etc/johbo/flake8rc")
(setq python-fill-docstring-style (quote django))

;;(push '((d-mode . "stroustrup")) c-default-style)
