
;; Common emacs configuration


;;; Code:

;; Set up the package repos for emacs
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
;; Integration for emacsPackagesNg
(add-to-list 'package-directory-list "~/.nix-profile/share/emacs/site-lisp/elpa")
(package-initialize)

;; Various configuration settings
(custom-set-variables
  '(column-number-mode t)
  '(custom-enabled-themes (quote (tango-dark)))
  '(display-time-mode t)
  '(feature-indent-level 4)
  '(feature-indent-offset 4)
  '(fill-column 79)
  '(indent-tabs-mode nil)
  '(js2-mode-escape-quotes nil)
  '(js2-mode-indent-ignore-first-tab t)
  '(menu-bar-mode nil)
  '(po-auto-edit-with-msgid t)
  '(po-auto-replace-revision-date t)
  '(python-fill-docstring-style (quote django))
  '(safe-local-variable-values (quote ((encoding . utf-8))))
  '(scroll-bar-mode nil)
  '(show-paren-mode t)
  '(tool-bar-mode nil)
  '(tramp-default-method "ssh")
)

(setq font-lock-maximum-decoration t)
(setq-default ispell-program-name "aspell")
(setq ns-pop-up-frames nil)
(setq search-highlight t)
(setq x-select-enable-clipboard t)

;; Increase the threshold when GC will be initiated, suggested by flx
(setq gc-cons-threshold 20000000)

(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp")

;; Write a PID file if we are a daemon, so that supervisord can manage us with
;; pidproxy
(defun my/write-pid-file ()
  "Write the Emacs PID into a file"
  (let ((pid-file "~/var/run/emacs-server.pid")
        (pid (number-to-string (emacs-pid))))
    (with-temp-file pid-file (insert pid))))

(if (daemonp)
    (my/write-pid-file))

(setq inhibit-startup-message t)

(put 'narrow-to-region 'disabled nil)

;; YaSnippets
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

;; IDO mode
(require 'ido)
(require 'flx-ido)
(ido-mode t)
;; TODO: find out what this does
;; (ido-everywhere t)
(flx-ido-mode t)
;; disable ido faces to see flx highlights
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;(setq-default show-trailing-whitespace t)

(setq tramp-shell-prompt-pattern
      "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

(setq org-agenda-files (quote ("~/n" "~/n/hr")))
(setq org-directory "~/n")
(setq org-default-notes-file (concat org-directory "/capture.org"))

(setq org-export-backends '(ascii html icalendar latex beamer man md odt))

;; Org mode configuration
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (emacs-lisp . t)
   (js . t)
   (python . t)
   (sh . t)
   (sql . t)))

(setq-default c-basic-offset 4)

;; Projectile
(require 'projectile)
(projectile-global-mode)

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(setq flycheck-flake8rc "~/.nix-profile/etc/johbo/flake8rc")

;; Autocomplete mode
(require 'auto-complete-config)
(ac-config-default)

;; Register YAML mode
(autoload 'yaml-mode "yaml-mode" "Major mode for editing YAML files." t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Register D mode
(autoload 'd-mode "d-mode" "Major mode for editing D source code." t)
(push '("\\.d$" . d-mode) auto-mode-alist)
;; TODO: currently need this so that c-default-style is defined.
(require 'cc-mode)
(push '((d-mode . "stroustrup")) c-default-style)

(require 'ac-dcd)
(add-hook
 'd-mode-hook
 (lambda ()
   (auto-complete-mode t)
   (when (featurep 'yasnippet) (yas-minor-mode-on))
   (ac-dcd-maybe-start-server)
   (ac-dcd-add-imports)
   (add-to-list 'ac-sources 'ac-source-dcd)
   (define-key d-mode-map (kbd "C-c ?") 'ac-dcd-show-ddoc-with-buffer)
   (define-key d-mode-map (kbd "C-c .") 'ac-dcd-goto-definition)
   (define-key d-mode-map (kbd "C-c ,") 'ac-dcd-goto-def-pop-marker)
   (define-key d-mode-map (kbd "C-c s") 'ac-dcd-search-symbol)

   (when (featurep 'popwin)
     (add-to-list 'popwin:special-display-config
                  `(,ac-dcd-error-buffer-name :noselect t))
     (add-to-list 'popwin:special-display-config
                  `(,ac-dcd-document-buffer-name :position right :width 80))
     (add-to-list 'popwin:special-display-config
                  `(,ac-dcd-search-symbol-buffer-name :position bottom :width 5)))))

(add-to-list 'ac-sources 'ac-source-jedi-direct)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:server-command '("jediepcserver"))
;; (setq jedi:server-args
;;       '("--log" "/tmp/jediepcserver.log"
;;         "--log-traceback"
;;         "--log-level" "DEBUG"))

(autoload 'nix-mode "nix-mode" "Major mode for editing Nix expressions." t)
(push '("\\.nix\\'" . nix-mode) auto-mode-alist)
(push '("\\.nix.in\\'" . nix-mode) auto-mode-alist)

(require 'magit)

(require 'monky)
