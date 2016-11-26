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

(setq flycheck-flake8rc "~/.nix-profile/etc/johbo/flake8rc")
(setq python-fill-docstring-style (quote django))

;;(push '((d-mode . "stroustrup")) c-default-style)
