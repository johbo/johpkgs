
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
   emoji
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
   ;; version-control
   erc
   shell-scripts

   (osx :variables
        mac-right-option-modifier nil)
   ))