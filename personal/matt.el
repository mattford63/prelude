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
                            ;;all-the-icons
                            ;;all-the-icons-dired
                            ;;all-the-icons-ivy
                            doom-themes
                            leuven-theme
                            pivotal-tracker
                            ;;org-gcal
                            ;;unicode-fonts
                            browse-at-remote
                            ;;calfw
                            ;;calfw-org
                            smart-mode-line
                            neotree
                            pinentry
                            ;;twittering-mode
                            mustache-mode
                            ipcalc
                            ))

;; GUI
(server-start)
(setq whitespace-style '(face tabs empty))
(disable-theme 'zenburn)
(load-theme 'doom-one)
(doom-themes-org-config)
(doom-themes-neotree-config)
;;(all-the-icons-install-fonts) ; run-once
;;(all-the-icons-ivy-setup)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq neo-theme 'icons)
(load-theme 'smart-mode-line-light)
(smart-mode-line-enable)

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
;;(nyan-mode)

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


;; (require 'org-gcal)
;; (setq org-gcal-credentials (auth-source-user-and-password "org-gcal"))
;; (setq org-gcal-client-id (car org-gcal-credentials)
;;       org-gcal-client-secret (cadr org-gcal-credentials)
;;       org-gcal-file-alist '(("matt.ford@mastodonc.com" .  "~/schedule.org")))

;; (require 'calfw)
;; (require 'calfw-org)
;; (require 'org-drill)

;; EPA Encryption
(setq epg-gpg-program "gpg2")

;; Email
;; (require 'mu4e)
;; (setq mu4e-maildir "~/mail")
;; (setq mu4e-get-mail-command "offlineimap")
;; (setq message-kill-buffer-on-exit t)
;; (setq mu4e-use-fancy-chars nil)
;; (setq mu4e-mastodonc-bookmarks `(("\\\\Inbox AND maildir:\"/mastodonc/[Gmail].All Mail\"" "Mastodonc Inbox" ?i)
;;                                  ("flag:flagged AND maildir:\"/mastodonc/[Gmail].All Mail\"" "Mastodonc Flagged messages" ?f)
;;                                  (,(concat "flag:unread AND "
;;                                            "maildir:\"/mastodonc/[Gmail].All Mail\" AND "
;;                                            "NOT flag:trashed AND "
;;                                            "NOT maildir:/mastodonc/[Gmail].Spam AND "
;;                                            "NOT maildir:/mastodonc/[Gmail].Trash")
;;                                   "Mastodonc Unread messages" ?u)
;;                                  ("maildir:\"/mastodonc/[Gmail].All Mail\"" "Mastodonc All Mail" ?a)))

;; (setq mu4e-dancingfrog-bookmarks `(("\\\\Inbox AND maildir:\"/dancingfrog/[Google Mail].All Mail\"" "Dancingfrog Inbox" ?i)
;;                                    ("flag:flagged AND maildir:\"/dancingfrog/[Google Mail].All Mail\"" "Dancingfrog Flagged messages" ?f)
;;                                    (,(concat "flag:unread AND "
;;                                              "maildir:\"/dancingfrog/[Google Mail].All Mail\" AND "
;;                                              "NOT flag:trashed AND "
;;                                              "NOT maildir:/dancingfrog/[Google Mail].Spam AND "
;;            https://gist.githubusercontent.com/mattford63/5a5a36aeb75e1dbde32e36d79f885a87/raw/4736f7b63e1e51cf39d631d9109d89e98abcd1f6/proxy.pachttps://gist.githubusercontent.com/mattford63/5a5a36aeb75e1dbde32e36d79f885a87/raw/4736f7b63e1e51cf39d631d9109d89e98abcd1f6/proxy.pachttps://gist.githubusercontent.com/mattford63/5a5a36aeb75e1dbde32e36d79f885a87/raw/4736f7b63e1e51cf39d631d9109d89e98abcd1f6/proxy.pachttps://gist.githubusercontent.com/mattford63/5a5a36aeb75e1dbde32e36d79f885a87/raw/4736f7b63e1e51cf39d631d9109d89e98abcd1f6/pr                                  "NOT maildir:/dancingfrog/[Google Mail].Bin")
;;                                     "Dancingfrog Unread messages" ?u)
;;                                    ("maildir:\"/dancingfrog/[Google Mail].All Mail\"" "Dancingfrog All Mail" ?a)))

;; (setq mu4e-maildir "~/mail")
(setq smtpmail-smtp-default-server "smtp.gmail.com")
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
(setq smtpmail-smtp-user "mattford63@gmail.com")
(setq smtpmail-smtp-service 587)

;; (setq mu4e-contexts
;;       `( ,(make-mu4e-context
;;            :name "Mastodonc"
;;            :enter-func (lambda () (mu4e-message "Entering Mastodonc context"))
;;            :leave-func (lambda () (mu4e-message "Leaving Mastodonc context"))
;;            ;; we match based on the contact-fields of the message
;;            :match-func (lambda (msg)
;;                          (when msg
;;                            (string-match-p "^/mastodonc" (mu4e-message-field msg :maildir))))
;;            :vars `((user-mail-address      . "matt@mastodonc.com"  )
;;                    (mu4e-user-mail-address-list . ("matt@mastodonc.com" "matt.ford@mastodonc.com"))
;;                    (user-full-name         . "Matt Ford" )
;;                    (mu4e-compose-signature . "Matt\n")
;;                    (mu4e-sent-folder . "/mastodonc/[Gmail].All Mail")
;;                    (mu4e-drafts-folder . "/mastodonc/[Gmail].Drafts")
;;                    (mu4e-trash-folder . "/mastodonc/[Gmail].Trash")
;;                    (mu4e-refile-folder . "/mastodonc/[Gmail].All Mail")
;;                    (mu4e-bookmarks . ,mu4e-mastodonc-bookmarks)
;;                    (smtpmail-smtp-user . "matt.ford@mastodonc.com")
;;                    (smtpmail-smtp-service . 587)))
;;          ,(make-mu4e-context
;;            :name "Dancingfrog"
;;            :enter-func (lambda () (mu4e-message "Switch to the Dancingfrog context"))
;;            ;; no leave-func
;;            ;; we match based on the maildir of the message
;;            ;; this matches maildir /Arkham and its sub-directories
;;            :match-func (lambda (msg)
;;                          (when msg
;;                            (string-match-p "^/dancingfrog" (mu4e-message-field msg :maildir))))
;;            :vars `((user-mail-address       . "matt@dancingfrog.co.uk" )
;;                    (user-full-name          . "Matt Ford" )
;;                    (mu4e-compose-signature . "Matt\n")
;;                    (mu4e-user-mail-address-list  . ("matt@dancingfrog.co.uk" "mattford63@gmail.com"))
;;                    (mu4e-sent-folder . "/dancingfrog/[Google Mail].All Mail")
;;                    (mu4e-drafts-folder . "/dancingfrog/[Google Mail].Drafts")
;;                    (mu4e-trash-folder . "/dancingfrog/[Google Mail].Bin")
;;                    (mu4e-refile-folder . "/dancingfrog/[Google Mail].All Mail")
;;                    (mu4e-bookmarks . ,mu4e-dancingfrog-bookmarks)
;;                    (smtpmail-smtp-user . "mattford63@gmail.com")
;;                    (smtpmail-smtp-service . 587)))))

;; (setq mu4e-context-policy 'pick-first)
;; (setq mu4e-compose-context-policy nil)

;; (add-hook 'mu4e-mark-execute-pre-hook
;;           (lambda (mark msg)
;;             (cond ((equal mark 'refile) (mu4e-action-retag-message msg "-\\Inbox"))
;;                   ((equal mark 'trash) (mu4e-action-retag-message msg "-\\Inbox,-\\Starred"))
;;                   ((equal mark 'flag) (mu4e-action-retag-message msg "-\\Inbox,\\Starred"))
;;                   ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\Starred")))))

;; (setq mu4e-headers-fields '((:human-date . 9)
;;                             (:flags . 6)
;;                             (:mailing-list . 12)
;;                             (:tags . 20)
;;                             (:from . 22)
;;                             (:subject)))

;; ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
;; (setq mu4e-sent-messages-behavior 'delete)

;; ;; Try to display images in mu4e
;; (setq
;;  mu4e-view-show-images t
;;  mu4e-view-image-max-width 800)

;; (setq mu4e-confirm-quit nil
;;       mu4e-headers-date-format "%d/%b/%Y %H:%M")

;; (add-to-list 'mu4e-view-actions
;;              '("ViewBrowser" . mu4e-action-view-in-browser) t)

;; ;;(unicode-fonts-setup) ; run once only?

;; (setq mu4e-update-interval 600)

;; ;; Dired mail attachments
;; (require 'gnus-dired)
;; ;; make the `gnus-dired-mail-buffers' function also work on
;; ;; message-mode derived modes, such as mu4e-compose-mode
;; (defun gnus-dired-mail-buffers ()
;;   "Return a list of active message buffers."
;;   (let (buffers)
;;     (save-current-buffer
;;       (dolist (buffer (buffer-list t))
;;         (set-buffer buffer)
;;         (when (and (derived-mode-p 'message-mode)
;;                 (null message-sent-message-via))
;;           (push (buffer-name buffer) buffers))))
;;     (nreverse buffers)))

;; (setq gnus-dired-mail-mode 'mu4e-user-agent)
;; (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; Pivotal
(setq pivotal-credentials (auth-source-user-and-password "pivotal"))
(setq pivotal-api-token (cadr pivotal-credentials))

;;Twitter
;;(setq twittering-connection-type-order '(wget curl urllib-http native urllib-https))
;;(setq twittering-use-master-password t)
;;(setq twittering-icon-mode t)
;;(setq twittering-convert-fix-size 64)
;;(setq twittering-use-icon-storage t)

;; eshell
(setq terraboot-witan-repo "~/src/github/mastodonc/terraboot-witan/terraform/")

(defun eshell/witan (env cmd &rest args)
  "Wrapper around mach - supply the ENV, the CMD and optionally for the ARGS the MFA."
  (let ((default-directory (concat terraboot-witan-repo env))
        (mfa (pop args)))
    (if mfa
        (shell-command (concat "MFA=" (number-to-string mfa) " mach " cmd ))
      (shell-command (concat "mach " cmd)))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
