
;; Common emacs configuration


;; Nix integration
;; TODO: Check why this is needed on darwin
(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp")



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
  '(python-fill-docstring-style (quote django))
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



;; Register D mode
(autoload 'd-mode "d-mode" "Major mode for editing D source code." t)
(push '("\\.d$" . d-mode) auto-mode-alist)


;; Register nix mode
(autoload 'nix-mode "nix-mode" "Major mode for editing Nix expressions." t)
(push '("\\.nix\\'" . nix-mode) auto-mode-alist)
(push '("\\.nix.in\\'" . nix-mode) auto-mode-alist)


;; Register YAML mode
(autoload 'yaml-mode "yaml-mode" "Major mode for editing YAML files." t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;; Set up the package repos for emacs
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)


;; run the server by default
;; Note: switching to launchd managed emacs daemon
;;(load "server")
;;(unless (server-running-p) (server-start))
