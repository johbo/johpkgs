
;; Common emacs configuration


;; Write a PID file if we are a daemon, so that supervisord can manage us with
;; pidproxy
(defun write-pid-file ()
  "Write the Emacs PID into a file"
  (let ((pid-file "~/var/run/emacs-server.pid")
        (pid (number-to-string (emacs-pid))))
    (with-temp-file pid-file (insert pid))))

(if (daemonp)
    (write-pid-file))



;; Nix integration
(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp")

;; Set up the package repos for emacs
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
;; Integration for emacsPackagesNg
(add-to-list 'package-directory-list "~/.nix-profile/share/emacs/site-lisp/elpa")
(package-initialize)


;; Allow narrowing, "C-x n n" and "C-x n w"
(put 'narrow-to-region 'disabled nil)


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
  '(org-agenda-files (quote ("~/orgfiles/")))
  '(org-directory "~/orgfiles/")
  '(po-auto-edit-with-msgid t)
  '(po-auto-replace-revision-date t)
  '(safe-local-variable-values (quote ((encoding . utf-8))))
  '(scroll-bar-mode nil)
  '(show-paren-mode t)
  '(tool-bar-mode nil)
  '(tramp-default-method "ssh")
)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq font-lock-maximum-decoration t)
(setq inhibit-startup-message t)
(setq-default ispell-program-name "aspell")
(setq ns-pop-up-frames nil)
(setq search-highlight t)
(setq x-select-enable-clipboard t)
;(setq-default show-trailing-whitespace t)

;; Increase the threshold when GC will be initiated, suggested by flx
(setq gc-cons-threshold 20000000)


;; Register D mode
(autoload 'd-mode "d-mode" "Major mode for editing D source code." t)
(push '("\\.d$" . d-mode) auto-mode-alist)
;; TODO: currently need this so that c-default-style is defined.
(require 'cc-mode)
(push '((d-mode . "stroustrup")) c-default-style)



;; Register nix mode
(autoload 'nix-mode "nix-mode" "Major mode for editing Nix expressions." t)
(push '("\\.nix\\'" . nix-mode) auto-mode-alist)
(push '("\\.nix.in\\'" . nix-mode) auto-mode-alist)


;; Register YAML mode
(autoload 'yaml-mode "yaml-mode" "Major mode for editing YAML files." t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


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


;; Autocomplete mode
(require 'auto-complete-config)
(ac-config-default)

;; JEDI as completion plugin for Python
(require 'jedi)
(add-to-list 'ac-sources 'ac-source-jedi-direct)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:server-command '("jediepcserver"))

;; Projectile
(require 'projectile)
(projectile-global-mode)


;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)



;; run the server by default
;; Note: switching to launchd managed emacs daemon
;;(load "server")
;;(unless (server-running-p) (server-start))
