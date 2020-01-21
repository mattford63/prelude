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
                            docker-cli
                            docker-compose-mode
                            dockerfile-mode
                            puppet-mode
                            ag
                            fold-dwim
                            fold-dwim-org
                            org2blog
                            org-gcal
                            all-the-icons
                            all-the-icons-dired
                            ;;doom-themes
                            ;;pivotal-tracker
                            org-gcal
                            browse-at-remote
                            neotree
                            pinentry
                            twittering-mode
                            mustache-mode
                            ipcalc
                            ob-async
                            pyvenv
                            ;;ein
                            nyan-mode
                            realgud
                            ess
                            ;;doom-modeline
                            perspective
                            avy
                            ess
                            symbol-overlay
                            flycheck-joker
                            elfeed
                            elfeed-org
                            conda
                            markdown-preview-mode
                            ))

;; GUI
;; (disable-theme 'zenburn)
(server-start)

(setq lsp-prefer-flymake nil)

;; (setq whitespace-style '(face tabs empty))
;; (set-face-attribute 'region nil :background "#eee")

;; (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
;; (setq doom-modeline-lsp nil)
;; (setq doom-modeline-major-mode-color-icon t)
;; (doom-modeline-mode 1)
;; (doom-modeline-def-modeline 'my-simple-line
;;   '(bar matches buffer-info remote-host parrot selection-info)
;;   '(minor-modes input-method major-mode process vcs checker))
;; (doom-modeline-set-modeline 'my-simple-line 'default)

(if (eq system-type 'gnu/linux)
    (set-frame-font "inconsolata-13" nil t))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(global-set-key [f9] 'neotree-toggle)
(global-set-key [f8] 'org-agenda)
;;(global-set-key [f7] 'mu4e)
(global-set-key [f5] 'ansi-term)
(global-set-key [f6] 'eshell)

(global-set-key (kbd "C-'") 'avy-goto-char)
(global-set-key (kbd "C-#") 'avy-goto-line)

(when (not (eq system-type 'gnu/linux))
  se-url-browser-function  (setq epg-pinentry-mode 'loopback)
  (pinentry-start))

(require 'symbol-overlay)
(global-set-key (kbd "M-i") 'symbol-overlay-put)

;; Browser
(setq brow 'browse-url-default-browser)

;; Who the hell are we?
(setq user-full-name "Matt Ford"
      user-mail-address "matt@dancingfrog.co.uk")

;; Markdown
;; (add-hook 'markdown-mode-hook 'visual-line-mode)
;; (add-hook 'markdown-mode-hook 'visual-fill-column-mode)

;; Conda
;; (setq conda-anaconda-home "~/miniconda3")
;; (setq conda-env-home-directory "~/miniconda3/")

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
;; (add-hook 'c-mode-common-hook   'hs-minor-mode)
;; (add-hook 'c-mode-common-hook   'fold-dwim-org/minor-mode)
;; (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
;; (add-hook 'emacs-lisp-mode-hook 'fold-dwim-org/minor-mode)
;; (add-hook 'java-mode-hook       'hs-minor-mode)
;; (add-hook 'java-mode-hook       'fold-dwim-org/minor-mode)
;; (add-hook 'lisp-mode-hook       'hs-minor-mode)
;; (add-hook 'lisp-mode-hook       'fold-dwim-org/minor-mode)
;; (add-hook 'perl-mode-hook       'hs-minor-mode)
;; (add-hook 'perl-mode-hook       'fold-dwim-org/minor-mode)
;; (add-hook 'sh-mode-hook         'hs-minor-mode)
;; (add-hook 'sh-mode-hook         'fold-dwim-org/minor-mode)

(setq markdown-preview-stylesheets (list "http://thomasf.github.io/solarized-css/solarized-light.min.css"))
(add-to-list 'markdown-preview-stylesheets "https://raw.githubusercontent.com/richleland/pygments-css/master/emacs.css")

;; Projectile
(setq projectile-switch-project-action 'projectile-find-file)

;; Magit
(setq auto-revert-check-vc-info t)

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
      `(("Dancingfrog"
         :url "https://dancingfrogsite.wordpress.com/xmlrpc.php"
         :username ,(car (auth-source-user-and-password "dancingfrogsite.wordpress.com"))
         :password ,(cadr (auth-source-user-and-password "dancingfrogsite.wordpress.com")))))

(setq org2blog/wp-shortcode-langs-map '(("elisp" . "text")))
(setq org2blog/wp-use-sourcecode-shortcode t)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "68D8501429C42E01")

(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE")))

;; (setq org-gcal-client-id "102412122628-egcfksdub9jcui6q8mug49e6mdktfsqq.apps.googleusercontent.com"
;;       org-gcal-client-secret (cadr (auth-source-user-and-password "org.gcal"))
;;       org-gcal-file-alist '(("matt.ford@mastodonc.com" . "~/org/mc-gcal.org")))

;;(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
;;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))

(setq org-agenda-files '("~/org"))

(require 'elfeed-org)
(elfeed-org)
(setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))

;; EPA Encryption
(setq epg-gpg-program "gpg")

;; Email
(when (eq system-type 'darwin)
  (add-to-list 'load-path "~/.emacs.d/elisp/mu4e"))

;; (require 'mu4e)
;; (setq mu4e-maildir "~/Maildir")
;; (setq mu4e-view-show-images t)
;; (setq mu4e-view-actions
;;       '(("capture message" . mu4e-action-capture-message)
;;         ("show this thread" . mu4e-action-show-thread)
;;         ("view in browser" . mu4e-action-view-in-browser)))
;; (setq mu4e-change-filenames-when-moving t)
;; (setq mu4e-get-mail-command "mbsync -a")

;; (setq mu4e-contexts
;;     `( ,(make-mu4e-context
;;           :name "Private"
;;           :enter-func (lambda () (mu4e-message "Entering Private context"))
;;           :leave-func (lambda () (mu4e-message "Leaving Private context"))
;;           :match-func (lambda (msg)
;;                         (when msg
;;                           (string-match-p "^/df" (mu4e-message-field msg :maildir))))
;;           :vars '( ( user-mail-address      . "matt@dancingfrog.co.uk"  )
;;                    ( user-full-name         . "Matt Ford" )
;;                    ( mu4e-compose-signature .
;;                      (concat
;;                       "Matt\n"))
;;                    ( mu4e-draft-folder  . "/df/Drafts")
;;                    ( mu4e-sent-folder . "/df//SentMail")
;;                    ( mu4e-trash-folder . "/df/Trash")
;;                    ( mu4e-refile-folder . "/df/Archive")
;;                    (smtpmail-smtp-server . "smtp.ionos.co.uk")
;;                    (smtpmail-smtp-service . 587)
;;                    (smtpmail-smtp-user . "matt@dancingfrog.co.uk")))
;;        ,(make-mu4e-context
;;           :name "Work"
;;           :enter-func (lambda () (mu4e-message "Switch to the Work context"))
;;           :match-func (lambda (msg)
;;                         (when msg
;;                           (string-match-p "^/mc" (mu4e-message-field msg :maildir))))
;;           :vars '( ( user-mail-address       . "matt@mastodonc.com" )
;;                    ( user-full-name          . "Matt Ford" )
;;                    ( mu4e-compose-signature  .
;;                      (concat
;;                        "Matt Ford\n"
;;                        "Mastodon C\n"))
;;                    ( mu4e-drafts-folder . "/mc/[Gmail].Drafts")
;;                    ( mu4e-sent-folder . "/mc/[Gmail].Sent Mail")
;;                    ( mu4e-trash-folder . "/mc/[Gmail].Trash")
;;                    ( mu4e-refile-folder . "/mc/[Gmail].All Mail")
;;                    ( smtpmail-smtp-server . "smtp.gmail.com")
;;                    ( smtpmail-smtp-service . 587)
;;                    ( smtpmail-smtp-user "matt.ford@mastodonc.com")))))

;; (setq mu4e-user-mail-address-list
;;       (delq nil
;;             (mapcar (lambda (context)
;;                       (when (mu4e-context-vars context)
;;                         (cdr (assq 'user-mail-address (mu4e-context-vars context)))))
;;                     mu4e-contexts)))

;; (setq mu4e-bookmarks
;;       (list (make-mu4e-bookmark
;;              :name  "Unread messages"
;;              :query "flag:unread AND NOT flag:trashed"
;;              :key ?u)
;;             (make-mu4e-bookmark
;;              :name "Today's messages"
;;              :query "date:today..now"
;;              :key ?t)
;;             (make-mu4e-bookmark
;;              :name "Last 7 days"
;;              :query "date:7d..now"
;;              :key ?w)
;;             (make-mu4e-bookmark
;;              :name "Messages with images"
;;              :query "mime:image/*"
;;              :key ?p)
;;             (make-mu4e-bookmark
;;              :name "Mastodonc (current)"
;;              :query "(maildir:/mc/INBOX or maildir:/mc/[Gmail].Sent Mail) and not (maildir:/mc/[Gmail].Trash or flag:trashed)"
;;              :key ?m)
;;             (make-mu4e-bookmark
;;              :name "Mastodonc (all)"
;;              :query "maildir:/mc/* and not maildir:/mc/[Gmail].Trash"
;;              :key ?M)
;;             (make-mu4e-bookmark
;;              :name "Dancingfrog (current)"
;;              :query "(maildir:/df/INBOX or maildir:/df/SentMail) and not maildir:/df/Trash"
;;              :key ?d)
;;             (make-mu4e-bookmark
;;              :name "Dancingfrog (all)"
;;              :query "maildir:/df/* and not maildir:/df/Trash"
;;              :key ?D)))

;; (add-hook 'mu4e-view-mode-hook 'visual-line-mode)

;; (setq message-send-mail-function 'smtpmail-send-it)
;; (setq smtpmail-queue-mail nil  ;; start in normal mode
;;       smtpmail-queue-dir   "~/Maildir/queue/cur")

;; (setq mu4e-update-interval 300)
;; (setq mu4e-use-fancy-chars nil)

;; ;; Pivotal
;; (setq pivotal-credentials (auth-source-user-and-password "pivotal"))
;; (setq pivotal-api-token (cadr pivotal-credentials))

;;Twitter
(setq twittering-connection-type-order '(wget curl urllib-http native urllib-https))
(setq twittering-use-master-password t)
(setq twittering-icon-mode t)
(setq twittering-convert-fix-size 64)
(setq twittering-use-icon-storage t)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
