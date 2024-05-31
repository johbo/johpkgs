
(configuration-layer/declare-layers
 '(
   (org :variables
        org-enable-reveal-js-support t
        org-want-todo-bindings t)
   (elfeed :variables
           rmh-elfeed-org-files (list "~/n/rssfeeds.org"))
   ;; jabber
   docker
   csv
   ;; ruby
   ;; php
   auto-completion
   ;; better-defaults
   emacs-lisp
   git
   markdown
   multiple-cursors
   d
   ;; This screws with my holy org files
   ;; See https://github.com/syl20bnr/emacs-emoji-cheat-sheet-plus/issues/5
   ;; emoji
   imenu-list
   (python :variables
           python-sort-imports-on-save t
           python-test-runner 'pytest)
   javascript
   typescript
   nixos
   react
   vue
   yaml
   html
   restructuredtext
   shell
   ;; (shell :variables
   ;;        shell-default-height 30
   ;;        shell-default-position 'bottom)
   spell-checking
   sphinx
   syntax-checking
   sql
   terraform
   version-control
   erc
   shell-scripts
   ansible

   (osx :variables
        osx-right-option-as 'none)
   botech
   ))
