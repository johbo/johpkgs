
(configuration-layer/declare-layers
 '(
   org
   (elfeed :variables
           rmh-elfeed-org-files (list "~/n/rssfeeds.org"))
   jabber
   csv
   ruby
   php
   auto-completion
   ;; better-defaults
   emacs-lisp
   git
   markdown
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
   yaml
   html
   shell
   ;; (shell :variables
   ;;        shell-default-height 30
   ;;        shell-default-position 'bottom)
   spell-checking
   syntax-checking
   sql
   ;; version-control
   erc
   shell-scripts
   ansible

   (osx :variables
        mac-right-option-modifier nil)
   botech
   ))
