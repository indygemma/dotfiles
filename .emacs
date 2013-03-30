;; delete menu/tool/scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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

;; custom variables
(custom-set-variables
 '(haskell-program-name "cabal-dev ghci"))

;; load color theme
(load "~/.emacs.d/colorthemes/color-theme-molokai.el")
(color-theme-molokai)

;; set line number
(setq linum-format "%4d ")
(global-linum-mode 1)

;; setup org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)