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
                            ob-async
                            fold-dwim
                            fold-dwim-org))

;; GUI
(setq whitespace-style '(face tabs empty))
;;(disable-theme 'zenburn)
;;(load-theme 'leuven)

(let ((font-size (if (string-equal system-type "darwin")
                     "14"
                   "10")))
  (setq default-frame-alist (list (cons 'font (concat "Monospace-" font-size)))))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Browser
(setq browse-url-browser-function 'browse-url-chrome)

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
(all-the-icons-dired-mode)

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

;; EPA Encryption
(setq epg-gpg-program "gpg2")

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
