
(defconst config-johbo-packages
  '(
    org
    ))

(defun config-johbo/post-init-org ()
  ;; Agenda configuration
  (setq org-agenda-files (quote ("~/n" "~/n/hr")))
  (setq org-directory "~/n")
  (setq org-default-notes-file (concat org-directory "/capture.org"))

  ;; Export backends
  (setq org-export-backends '(ascii jira html icalendar beamer md odt))

  ;; Capturing
  (setq org-capture-templates
        '(("t" "Tasks" entry (file+headline (concat org-directory "/capture.org")
                                            "Refile Tasks")
           "* TODO %?\n  %i\n  %a")
          ("b" "Blog" entry (file+headline (concat org-directory "/capture.org")
                                           "Blog Topics")
           "* TODO %?\n\nEntered on %U\n  %i\n  %a")

          ("l" "Lead" entry (file+headline (concat org-directory "/capture.org")
                                           "Leads")
           "* TODO %?\n\nEntered on %U\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
           "* %?\nEntered on %U\n  %i\n  %a")
          ("f" "Feedback" entry (file+datetree (concat org-directory "/hr/feedback.org"))
           "* %?\nEntered on %U\n  %i\n  %a")
          ))

  ;; Refiling
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps t)
  (setq org-refile-allow-creating-parent-nodes nil)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
  )
