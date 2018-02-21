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
                            htmlize))

;; GUI
(setq default-frame-alist '((font . "DejaVu Sans Mono-10")))
(setq whitespace-style '(face trailing tabs empty))
;; (disable-theme 'zenburn)

;; Markdown
;; (add-hook 'markdown-mode-hook 'visual-line-mode)
;; (add-hook 'markdown-mode-hook 'visual-fill-column-mode)

;; Clojure
(setq cljr-warn-on-eval nil)

;; Deft
(setq deft-directory "~/src/github/mattford63/feynman"
      deft-extensions '("org" "md")
      deft-recursive t
      deft-use-filename-as-title nil
      deft-use-filter-string-for-filename t)

(global-set-key [f8] 'deft)

;; Dired Tree
(all-the-icons-dired-mode)

;; Magit
(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; Org
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (clojure . t)))

(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)

(setq load-path (cons "~/.emacs.d/org2blog/" load-path))
(require 'org2blog-autoloads)
(setq org2blog/wp-blog-alist
      '(("Witan"
         :url "https://witanblog.wordpress.com/xmlrpc.php"
         :username "mattford63"
         :default-title "Just Another Default Title"
         :default-categories ("org2blog" "emacs")
         :tags-as-categories nil)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
