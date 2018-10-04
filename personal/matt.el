;; Packages
(prelude-require-packages '(visual-fill-column
                            clj-refactor deft
                            leuven-theme
                            dired-sidebar
                            all-the-icons-dired
                            magit-gh-pulls
                            slime
                            org-plus-contrib
                            xml-rpc
                            metaweblog
                            htmlize
                            terraform-mode
                            dockerfile-mode
                            puppet-mode
                            counsel-projectile
                            company-terraform
                            company-shell
                            ag
                            leuven-theme
                            ;;ob-async
                            fold-dwim
                            fold-dwim-org
                            org2blog
                            deft
                            all-the-icons
                            all-the-icons-dired
                            ;;all-the-icons-ivy
                            ;;doom-themes
                            ;;leuven-theme
                            pivotal-tracker
                            ;;org-gcal
                            ;;unicode-fonts
                            browse-at-remote
                            ;;calfw
                            ;;calfw-org
                            smart-mode-line
                            ;;smart-mode-line-powerline-theme
                            neotree
                            pinentry
                            ;;twittering-mode
                            mustache-mode
                            ipcalc
                            ob-async
                            pyvenv
                            ein
                            ))

;; GUI
(server-start)
(setq whitespace-style '(face tabs empty))
(sml/setup)

(let ((font-size (if (string-equal system-type "darwin")
                     "14"
                   "11")))
  (setq default-frame-alist (list (cons 'font (concat "Monospace-" font-size)))))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(global-set-key [f9] 'neotree-toggle)
(global-set-key [f8] 'compose-mail)
(global-set-key [f7] 'org-agenda)
(global-set-key [f6] 'deft)
(global-set-key [f5] 'eshell)

(setq epa-pinentry-mode 'loopback)
(pinentry-start)
(nyan-mode)

;; Browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Who the hell are we?
(setq user-full-name "Matt Ford"
      user-mail-address "matt@dancingfrog.co.uk")

;; Markdown
;; (add-hook 'markdown-mode-hook 'visual-line-mode)
;; (add-hook 'markdown-mode-hook 'visual-fill-column-mode)

;; Clojure
(setq cljr-warn-on-eval t)

(add-hook 'clojure-mode-hook 'hs-minor-mode)
(add-hook 'clojure-mode-hook 'fold-dwim-org/minor-mode)

;; Python
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; Deft
(setq deft-directory "~/src/keybase/feynman"
      deft-extensions '("org" "md")
      deft-recursive t
      deft-use-filename-as-title nil
      deft-use-filter-string-for-filename t)

(setq deft-file-naming-rules
      '((noslash . "-")
        (nospace . "-")
        (case-fn . downcase)))

;; Dired Tree
;;(all-the-icons-dired-mode)

;; Projectile
(setq projectile-switch-project-action 'projectile-find-file)

;; Magit
(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; Org
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (clojure . t)))

(setq org-babel-clojure-backend 'cider)
(setq org-export-babel-evaluate nil)
(setq org-confirm-babel-evaluate nil)

(setq load-path (cons "~/.emacs.d/org2blog/" load-path))
(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
      '(("Witan"
         :url "https://witanblog.wordpress.com/xmlrpc.php"
         :username "mattford63")
        ("Dancingfrog"
         :url "https://dancingfrogsite.wordpress.com/xmlrpc.php"
         :username "mattford63")))

(setq org2blog/wp-shortcode-langs-map '(("elisp" . "text")))
(setq org2blog/wp-use-sourcecode-shortcode t)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "68D8501429C42E01")

(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE")))

;; EPA Encryption
(setq epg-gpg-program "gpg2")

;; Email
(setq smtpmail-smtp-default-server "smtp.gmail.com")
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
(setq smtpmail-smtp-user "mattford63@gmail.com")
(setq smtpmail-smtp-service 587)

;; Pivotal
(setq pivotal-credentials (auth-source-user-and-password "pivotal"))
(setq pivotal-api-token (cadr pivotal-credentials))

;;Twitter
;;(setq twittering-connection-type-order '(wget curl urllib-http native urllib-https))
;;(setq twittering-use-master-password t)
;;(setq twittering-icon-mode t)
;;(setq twittering-convert-fix-size 64)
;;(setq twittering-use-icon-storage t)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
