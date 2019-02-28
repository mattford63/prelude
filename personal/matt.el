(defadvice terminal-init-screen
    ;; The advice is named `tmux', and is run before `terminal-init-screen' runs.
    (before tmux activate)
  ;; Docstring.  This describes the advice and is made available inside emacs;
  ;; for example when doing C-h f terminal-init-screen RET
  "Apply xterm keymap, allowing use of keys passed through tmux."
  ;; This is the elisp code that is run before `terminal-init-screen'.
  (if (getenv "TUX")
      (let ((map (copy-keymap xterm-function-map)))
        (set-keymap-parent map (keymap-parent input-decode-map))
        (set-keymap-parent input-decode-map map))))

;; Packages
(prelude-require-packages '(visual-fill-column
                            clj-refactor
                            deft
                            dired-sidebar
                            all-the-icons-dired
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
                            ;;leuven-theme
                            ;;ob-async
                            fold-dwim
                            fold-dwim-org
                            org2blog
                            deft
                            all-the-icons
                            all-the-icons-dired
                            ;;all-the-icons-ivy
                            doom-themes
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
                            nyan-mode
                            realgud
                            ess
                            doom-modeline
                            perspective
                            persp-projectile
                            avy
                            ))

;; GUI
;;(load-theme 'doom-vibrant)
(server-start)
;;(setq whitespace-style '(face tabs empty))
;;(set-face-attribute 'region nil :background "#eee")
;; (sml/setup)

(setq doom-modeline-buffer-file-name-style 'truncate-except-project)
(setq doom-modeline-lsp nil)
(setq doom-modeline-major-mode-color-icon t)
(doom-modeline-mode 1)
(doom-modeline-def-modeline 'my-simple-line
  '(bar matches buffer-info remote-host parrot selection-info)
  '(minor-modes input-method major-mode process vcs checker))

(doom-modeline-set-modeline 'my-simple-line 'default)





(let ((font-size (if (string-equal system-type "Darwin")
                     "14"
                   "11")))
  (setq default-frame-alist (list (cons 'font (concat "Monospace-" font-size)))))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(global-set-key [f9] 'neotree-toggle)
;;(global-set-key [f8] 'compose-mail)
(global-set-key [f7] 'org-agenda)
;;(global-set-key [f6] 'deft)
(global-set-key [f5] 'eshell)

(global-set-key (kbd "C-'") 'avy-goto-char)
(global-set-key (kbd "C-#") 'avy-goto-line)

(setq epa-pinentry-mode 'loopback)
(pinentry-start)

;;(nyan-mode)

(persp-mode)
(require 'persp-projectile)
(define-key projectile-mode-map (kbd "C-S-s") 'projectile-persp-switch-project)
(global-set-key (kbd "C-S-a") 'persp-switch)

(require 'symbol-overlay)
(global-set-key (kbd "M-i") 'symbol-overlay-put)

;; Browser
(setq browse-url-browser-function 'browse-url-chrome)

;; Who the hell are we?
(setq user-full-name "Matt Ford"
      user-mail-address "matt@dancingfrog.co.uk")

;; Markdown
;; (add-hook 'markdown-mode-hook 'visual-line-mode)
;; (add-hook 'markdown-mode-hook 'visual-fill-column-mode)

;; Conda
(setq conda-anaconda-home "~/miniconda3")
(setq conda-env-home-directory "~/miniconda3/")

;; Clojure
(setq cljr-warn-on-eval t)
(require 'flycheck-joker)
(add-hook 'clojure-mode-hook 'hs-minor-mode)
(add-hook 'clojure-mode-hook 'fold-dwim-org/minor-mode)

;; Python
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'fold-dwim-org/minor-mode)
;;(pyvenv-tracking-mode 1)

;; Other Languages
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'c-mode-common-hook   'fold-dwim-org/minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'fold-dwim-org/minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'java-mode-hook       'fold-dwim-org/minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'fold-dwim-org/minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'fold-dwim-org/minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'sh-mode-hook         'fold-dwim-org/minor-mode)


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
;; (require 'magit-gh-pulls)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

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
