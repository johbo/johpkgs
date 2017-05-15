
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
;; HACK: Disable tramps inline compression to avoid "gzip: (stdin): unexpected
;; end of file" for some files.
(setq tramp-inline-compress-start-size 10000000)

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

;; Layout for Chat handling
(defvar chat-spacemacs-layout-name "@Chat"
  "Name used for the chats layout")
(defvar chat-spacemacs-layout-bindind "c"
  "Binding key to access the custom chat layout.")

(spacemacs|define-custom-layout chat-spacemacs-layout-name
  :binding "c"
  :body
  (progn
    (defun spacemacs-layouts/add-chat-buffer-to-persp ()
      (persp-add-buffer (current-buffer)
                        (persp-get-by-name
                         chat-spacemacs-layout-name)))
    (add-hook 'jabber-chat-mode-hook #'spacemacs-layouts/add-chat-buffer-to-persp)
    (add-hook 'jabber-roster-mode-hook #'spacemacs-layouts/add-chat-buffer-to-persp)
    (jabber-connect-all)
    (add-hook 'erc-mode-hook #'spacemacs-layouts/add-chat-buffer-to-persp)
    (erc-tls)
    ))
