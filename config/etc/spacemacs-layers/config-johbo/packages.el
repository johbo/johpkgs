(defconst config-johbo-packages
  '(
    editorconfig
    flycheck
    helm
    imenu-list
    jabber
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

(defun config-johbo/post-init-helm ()
  (setq helm-buffer-max-length 70)
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

(defun config-johbo/post-init-jabber ()
  (spacemacs/set-leader-keys "aj" nil)
  (spacemacs/declare-prefix "aj" "jabber-prefix")
  (spacemacs/set-leader-keys
    "ajA" 'jabber-send-default-presence
    "aja" 'jabber-send-presence
    "aji" 'jabber-activity-switch-to
    "ajj" 'jabber-connect-all
    "ajr" 'jabber-switch-to-roster-buffer)
  (spacemacs|use-package-add-hook jabber
    :post-config
    (progn
      (setq jabber-auto-reconnect t)
      ;; SPC TAB
      (add-to-list 'spacemacs-useful-buffers-regexp "\\*-jabber.*\\-*")
      (add-hook 'jabber-post-connect-hooks 'jabber-muc-autojoin t)

      (spacemacs/set-leader-keys-for-major-mode 'jabber-chat-mode
        "A" 'jabber-send-default-presence
        "a" 'jabber-send-presence
        "q" 'bury-buffer
        "r" 'jabber-switch-to-roster-buffer)

      ;; notifications for jabber on osx
      (setq my/jabber-terminal-notifier-sound "Pop")
      (defun my/msg-via-notifier (title msg group sound)
        (shell-command
         (format "%s -sender org.gnu.Emacs -title %s -message %s -group %s -sound %s"
                 my/cmd-terminal-notifier
                 (shell-quote-argument title)
                 (shell-quote-argument msg)
                 (shell-quote-argument group)
                 (shell-quote-argument sound)
                 )))
      (defun my/do-notify (from buffer text title) t)
      (defun my/do-notify-muc (from group buffer text title)
        (when (string-match (concat ".*" (jabber-my-nick group) ".*") text) t)
        ;; (if (jabber-muc-looks-like-personal-p text group) t nil)
        )
      (defun my/notify-jabber-message (from buffer text title)
        (when (my/do-notify from buffer text title)
          (my/msg-via-notifier from text from my/jabber-terminal-notifier-sound)
          )
        )
      (add-hook 'jabber-alert-message-hooks 'my/notify-jabber-message)
      (defun my/notify-jabber-muc-message (from group buffer text title)
        (when (my/do-notify-muc from group buffer text title)
          (my/msg-via-notifier from text group my/jabber-terminal-notifier-sound))
        )
      (add-hook 'jabber-alert-muc-hooks 'my/notify-jabber-muc-message)
      )
    )
  )

(defun config-johbo/post-init-org ()
  ;; Keys
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "ob" 'org-babel-demarcate-block
    "oc" 'org-copy-special
    "od" 'org-cut-special
    "op" 'org-paste-special)

  ;; Agenda configuration
  (setq org-agenda-files (quote ("~/n")))
  (setq org-directory "~/n")
  (setq org-default-notes-file (concat org-directory "/capture.org"))

  ;; Export backends
  (setq org-export-backends '(ascii html icalendar beamer md org odt reveal))
  ;; TODO: Use a different value
  (setq org-reveal-root "./reveal.js")

  ;; Link handling
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; Capturing
  (setq org-capture-templates
        '(("t" "Tasks" entry (file+headline (lambda () (concat org-directory "/capture.org"))
                                            "Refile Tasks")
           "* TODO %?\n  %i\n  %a")
          ("b" "Blog" entry (file+headline (lambda () (concat org-directory "/capture.org"))
                                           "Blog Topics")
           "* TODO %?\nEntered on %U\n  %i\n  %a")
          ("w" "Twitter" entry (file+headline (lambda () (concat org-directory "/capture.org"))
                                              "Twitter out")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("l" "Lead" entry (file+headline (lambda () (concat org-directory "/capture.org"))
                                           "Leads")
           "* TODO %?\nEntered on %U\n  %i\n  %a")
          ("r" "Read" entry (file+headline (lambda () (concat org-directory "/capture.org"))
                                           "Reading")
           "* TODO %?\nEntered on %U\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree (lambda () (concat org-directory "/journal.org")))
           "* %?\nEntered on %U\n  %i\n  %a")
          ("f" "Feedback" entry (file+datetree (lambda () (concat org-directory "/hr/feedback.org")))
           "* %?\nEntered on %U\n  %i\n  %a")
          ))

  ;; Clocking
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes
                                                ":%02d" :require-minutes t)))

  ;; Refiling
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes nil)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))

  ;; Misc
  (setq org-startup-indented t)
  )

(defun config-johbo/init-ox-jira ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-jira)))

(defun config-johbo/init-ox-rst ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-rst)))

(defun config-johbo/post-init-python ()
  (setq python-fill-docstring-style (quote django))
  )

(defun config-johbo/init-rst ())
