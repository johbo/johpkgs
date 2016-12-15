
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

;;(push '((d-mode . "stroustrup")) c-default-style)

;; Less mode uses this setting as offset
(setq css-indent-offset 2)

;; Configure indentation for web work
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; TODO: Think I don't event use this, need to find out its role
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq js2-strict-missing-semi-warning nil)

;; JavaScript, JSX
(setq js2-highlight-level 2)
;; Based on file size, parse less often
(setq js2-dynamic-idle-timer-adjust 10000)
;;  (setq js2-concat-multiline-strings eol)
(setq js2-language-version 200)
