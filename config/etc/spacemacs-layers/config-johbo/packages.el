(defconst config-johbo-packages
  '(
    editorconfig
    flycheck
    imenu-list
    org
    ox-jira
    ox-rst
    python
    rst
    ;; sr-speedbar
    ))

(defun config-johbo/init-editorconfig ()
  (use-package editorconfig
    :config
    (editorconfig-mode 1)
    )
  )

(defun config-johbo/post-init-flycheck ()
  (setq flycheck-flake8rc "~/.nix-profile/etc/johbo/flake8rc")
  )

(defun config-johbo/post-init-imenu-list ()
  (setq imenu-list-focus-after-activation nil)
  (setq imenu-list-auto-resize nil)
  ;; Imenu window position ['left|'right|'above|'below]
  (setq imenu-list-position 'right)
  ;; Imenu list size. Should be a positive integer or a percentage. If integer,
  ;; decides the total number of rows/columns the window has. If percentage (0 <
  ;; imenu-list-size < 1), decides the number of rows/columns relative to the
  ;; total number of rows/columns in the frame.
  (setq imenu-list-size 0.25)
  ;; Adapt imenu-list to reposition the window on display and goto actions.
  (defun my-reposition-wrapper (&rest args)
    "Reposition window wrapper that accepts arguments"
    (reposition-window))
  (defun my-imenu-list-display-entry ()
    "Display in original buffer the entry under point.
Additionally reposition the window."
    (interactive)
    (let ((entry (imenu-list--find-entry)))
      (save-selected-window
        (pop-to-buffer imenu-list--displayed-buffer)
        (imenu entry)
        (reposition-window) ;; Customized line
        (imenu-list--show-current-entry)
        )))
  (advice-add 'imenu-list-display-entry :override 'my-imenu-list-display-entry)
  (advice-add 'imenu-list-goto-entry :after 'my-reposition-wrapper)
  )

(defun config-johbo/post-init-org ()
  ;; Keys
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ob" 'org-babel-demarcate-block)

  ;; Agenda configuration
  (setq org-agenda-files (quote ("~/n" "~/n/hr")))
  (setq org-directory "~/n")
  (setq org-default-notes-file (concat org-directory "/capture.org"))

  ;; Export backends
  (setq org-export-backends '(ascii html icalendar beamer md odt))

  ;; Capturing
  (setq org-capture-templates
        '(("t" "Tasks" entry (file+headline (concat org-directory "/capture.org")
                                            "Refile Tasks")
           "* TODO %?\n  %i\n  %a")
          ("b" "Blog" entry (file+headline (concat org-directory "/capture.org")
                                           "Blog Topics")
           "* TODO %?\n\nEntered on %U\n  %i\n  %a")
          ("w" "Twitter" entry (file+headline (concat org-directory "/capture.org")
                                              "Twitter out")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("l" "Lead" entry (file+headline (concat org-directory "/capture.org")
                                           "Leads")
           "* TODO %?\n\nEntered on %U\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
           "* %?\nEntered on %U\n  %i\n  %a")
          ("f" "Feedback" entry (file+datetree (concat org-directory "/hr/feedback.org"))
           "* %?\nEntered on %U\n  %i\n  %a")
          ))

  ;; Clocking
  (setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes
                                                ":%02d" :require-minutes t)))

  ;; Refiling
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes nil)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
  )

(defun config-johbo/init-ox-jira ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-jira)))

(defun config-johbo/init-ox-rst ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-rst)))

(defun config-johbo/post-init-python ()
  (setq python-fill-docstring-style (quote django))
  )

(defun config-johbo/init-rst ())
