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
                            fold-dwim-org
                            org2blog
                            deft
                            all-the-icons
                            all-the-icons-dired
                            all-the-icons-ivy
                            doom-themes
                            leuven-theme
                            pivotal-tracker
                            org-gcal
                            unicode-fonts
                            browse-at-remote
                            calfw
                            calfw-org
                            smart-mode-line
                            neotree
                            twittering-mode))

;; GUI
(setq whitespace-style '(face tabs empty))
(disable-theme 'zenburn)
;;(load-theme 'doom-nord)
;;(doom-themes-org-config)
;;(doom-themes-neotree-config)
;;(all-the-icons-install-fonts) ; run-once
(all-the-icons-ivy-setup)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq neo-theme 'icons)
(load-theme 'smart-mode-line-light)
(smart-mode-line-enable)

(let ((font-size (if (string-equal system-type "darwin")
                     "14"
                   "10")))
  (setq default-frame-alist (list (cons 'font (concat "Monospace-" font-size)))))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(global-set-key [f9] 'neotree-toggle)
(global-set-key [f8] 'mu4e)
(global-set-key [f7] 'cfw:open-org-calendar)
(global-set-key [f6] 'deft)
(global-set-key [f5] 'eshell)

(nyan-mode)

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


(require 'org-gcal)
(setq org-gcal-credentials (auth-source-user-and-password "org-gcal"))
(setq org-gcal-client-id (car org-gcal-credentials)
      org-gcal-client-secret (cadr org-gcal-credentials)
      org-gcal-file-alist '(("matt.ford@mastodonc.com" .  "~/schedule.org")))

(require 'calfw)
(require 'calfw-org)
(require 'org-drill)

;; EPA Encryption
(setq epg-gpg-program "gpg2")

;; Email
(require 'mu4e)
(setq mu4e-maildir "~/mail")
(setq mu4e-get-mail-command "offlineimap")
(setq mu4e-drafts-folder "/mail/drafts")
(setq mu4e-user-mail-address-list (list "matt@mastodonc.com"
                                        "matt.ford@mastodonc.com"
                                        "matt@dancingfrog.co.uk"
                                        "mattford63@gmail.com"))
(setq message-kill-buffer-on-exit t)
(setq mu4e-use-fancy-chars nil)
(setq mu4e-maildir-shortcuts
      '(("/mastodonc/INBOX" . ?m)
        ("/dancingfrog/INBOX" . ?d)))

(setq mu4e-headers-fields '((:human-date . 12)
                            (:flags . 6)
                            (:mailing-list . 15)
                            (:tags . 15)
                            (:from . 22)
                            (:subject)))

;; Smart refile locations
;; (setq mu4e-refile-folder
;;       (lambda (msg)
;;         (cond
;;          ;; messages sent directly to me go to /archive
;;          ;; also `mu4e-user-mail-address-regexp' can be used
;;          ((mu4e-message-contact-field-matches msg :to "marius@gitorious")
;;           "/Gitorious/archive")
;;          ((mu4e-message-contact-field-matches msg :to "marius.mathiesen@gmail.com")
;;           "/Gmail/archive")
;;          ((mu4e-message-contact-field-matches msg :to "zmalltalker@zmalltalker.com")
;;           "/Gmail/archive")
;;          ((mu4e-message-contact-field-matches msg :to "marius@shortcut.no")
;;           "/Shortcut/archive")
;;          ;; everything else goes to /archive
;;          ;; important to have a catch-all at the end!
;;          (t  "/Gmail/archive"))))

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; Try to display images in mu4e
(setq
 mu4e-view-show-images t
 mu4e-view-image-max-width 800)

;; sending mail
;;(setq message-send-mail-function 'message-send-mail-with-sendmail
;;      sendmail-program "/usr/bin/msmtp"
;;      user-full-name "Matt Ford")


(setq mu4e-confirm-quit nil
      mu4e-headers-date-format "%d/%b/%Y %H:%M" ; date format
      ;;mu4e-html2text-command "html2text -utf8 -width 72"
      )

(add-to-list 'mu4e-view-actions 
             '("ViewBrowser" . mu4e-action-view-in-browser) t)

(unicode-fonts-setup) ; run once only?

;; Borrowed from http://ionrock.org/emacs-email-and-mu.html
;; Choose account label to feed msmtp -a option based on From header
;; in Message buffer; This function must be added to
;; message-send-mail-hook for on-the-fly change of From address before
;; sending message since message-send-mail-hook is processed right
;; before sending message.
;; (defun choose-msmtp-account ()
;;   (if (message-mail-p)
;;       (save-excursion
;;         (let*
;;             ((from (save-restriction
;;                      (message-narrow-to-headers)
;;                      (message-fetch-field "from")))
;;              (account
;;               (cond
;;                ((string-match "marius.mathiesen@gmail.com" from) "gmail")
;;                ((string-match "zmalltalker@zmalltalker.com" from) "gmail")
;;                ((string-match "marius@shortcut.no" from) "shortcut")
;;                ((string-match "marius@gitorious.com" from) "gitorious")
;;                ((string-match "marius@gitorious.org" from) "gitorious"))))
;;           (setq message-sendmail-extra-arguments (list '"-a" account))))))
;; (setq message-sendmail-envelope-from 'header)
;; (add-hook 'message-send-mail-hook 'choose-msmtp-account)
;; (add-to-list 'mu4e-bookmarks
;;              '("maildir:/Gitorious/inbox OR maildir:/Shortcut/inbox OR maildir:/Gmail/inbox flag:unread" "Today's news" ?z))
;; (add-to-list 'mu4e-bookmarks
;;              '("maildir:/Gmail/gitorious-ml flag:unread" "Unread on the mailing list" ?m))

(setq mu4e-update-interval 600)

;; Pivotal
(setq pivotal-credentials (auth-source-user-and-password "pivotal"))
(setq pivotal-api-token (cadr pivotal-credentials))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
