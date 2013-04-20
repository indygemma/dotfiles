;; delete menu/tool/scroll bar
(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; set font
(set-default-font "Monaco-12")

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
    "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
    (lambda (s)
      (end-of-buffer)
      (eval-print-last-sexp))))

(setq
 el-get-sources
 '(el-get
   evil))

(el-get 'sync)

;; initialize evil mode
(require 'evil)
(evil-mode 1)

;; no need to go to "emacs-mode", just use "insert mode" to use emacs stuff
;(setcdr evil-insert-state-map nil)
;(define-key evil-insert-state-map
;  (read-kbd-macro evil-toggle-key) 'evil-normal-state)
;(define-key evil-insert-state-map "jj" 'evil-normal-state)

; easier window movement
;(define-key key-translation-map (kbd "C-h") (kbd "C-w h"))
;(define-key key-translation-map (kbd "C-j") (kbd "C-w j"))
;(define-key key-translation-map (kbd "C-k") (kbd "C-w k"))
;(define-key key-translation-map (kbd "C-l") (kbd "C-w l"))

;; load color theme
(load "~/.emacs.d/colorthemes/color-theme-molokai.el")
(color-theme-molokai)

;; set line number
(setq linum-format "%4d ")
(global-linum-mode 1)

;; setup org-mode
(autoload 'org-mode "org" "Org mode" t)
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-program-name "cabal-dev ghci")
 '(org-agenda-files (quote ("~/Dropbox/org")))
 '(safe-local-variable-values (quote ((encoding . utf-8)))))

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/refile.org"))

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Dropbox/org/refile.org")
	       "* TODO %? :Inbox:\n%U")
	      )))

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
				 (org-agenda-files :maxlevel . 9))))
; use full outline paths for refile targets
(setq org-refile-use-outline-path t)

(setq org-refile-path-complete-in-steps t)

;; do not show in agenda if the task is done (view in log mode if required)
(setq org-agenda-skip-scheduled-if-done t)

; (require 'utf-8m)
; (set-file-name-coding-system 'utf-8m)

;; to fix umlaute in "emacs -nw" on mac os x
(set-default-coding-systems 'iso-latin-1)
(set-keyboard-coding-system 'iso-latin-1)
(set-terminal-coding-system 'iso-latin-1)
(prefer-coding-system 'iso-latin-1)

(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(ac-config-default)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
