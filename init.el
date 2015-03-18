;;; Commentary:

;;; Code:
(require 'cl)

;; Disable the splash screen
(setq inhibit-splash-screen t)

;; Add "~/.emacs.d/lisp/" to the load path
(add-to-list 'load-path (concat
                         user-emacs-directory
                         (convert-standard-filename "lisp/")
                         ))

;; packages
(setq package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")
                         ("marmalade"    . "http://marmalade-repo.org/packages/")
                         ("melpa"        . "http://melpa.org/packages/")))
(package-initialize)

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(setq-default indent-tabs-mode nil)

;; Turn on visual line-wrapping mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'tex-mode-hook 'turn-on-visual-line-mode)

(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro"
                :foundry "adobe"
                :slant normal
                :weight normal
                :height 82
                :width normal)))))


(load-theme 'adwaita)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Rainbow Delimiters
(require 'rainbow-delimiters)

;;; Setup Fill-Mode
(require 'fill-column-indicator)

;; Visual fci config
(setq fci-rule-width 1)
(setq fci-rule-color "darkgrey")

;; Turn on fci mode by default
(add-hook 'after-init-hook 'fci-mode)
(add-hook 'cmake-mode 'rainbow-delimiters-mode)

(defun soft-wrap-config (&optional width)
  (unless width (setq width 80))
  (set-fill-column width)
  (fci-mode 1)
  (auto-fill-mode -1)
  (turn-on-visual-line-mode)
  (window-margin-mode))

;; mode specific configs

(defun default-programming-config ()
  (auto-fill-mode 1)
  (auto-complete-mode 1)
  (rainbow-delimiters-mode 1)
  (fci-mode 1)
  (set-fill-column 80))

;; emacs lisp mode configuration

(defun elisp-config ()
  (default-programming-config)
  )

(add-hook 'emacs-lisp-mode-hook 'elisp-config)

(defun json-mode-config ()
  (rainbow-delimiters-mode)
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  (window-margin-mode)
  )
  

(add-hook 'json-mode-hook 'json-mode-config)

;; Haskell Mode

;; Set the haskell-mode default indentation mode, this is required for
;; haskell-mode to work

(defun haskell-config ()
  (default-programming-config)
  (haskell-indentation 1))

(add-hook 'haskell-mode-hook 'haskell-config)

;; Speedbar Configuration
(require 'speedbar)
;; Enable speedbar for haskell files
(speedbar-add-supported-extension ".hs")

;; AUCTeX-mode
(setq TeX-parse-self t); Enable automatic parsing
(setq TeX-auto-save t); Enable parse on save

;; Cc Mode
;; Set the indentation to 4 spaces
(setq-default c-basic-offset 4
              c-default-style "bsd")

;; Enable 80-column fill indicator for C files
(add-hook 'cc-mode-hook 'turn-on-auto-fill)

;; set up auto-complete-mode for C files
(add-hook 'c-mode-hook 'auto-complete-mode)
(add-hook 'cc-mode-hook 'auto-complete-mode)

;; Require ox-confluence
(require 'ox-confluence)

;; Enhanced Ruby Mode
(add-hook 'ruby-mode    'rainbow-delimiters-mode)
(add-hook 'enh-ruby-mode-hook 'fci-mode)
(add-hook 'enh-ruby-mode-hook 'turn-on-auto-fill)
(add-hook 'enh-ruby-mode-hook 'rainbow-delimiters-mode)
(add-hook 'enh-ruby-mode-hook 'auto-complete-mode)

(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "extern/enhanced-ruby-mode/")))
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(require 'enh-ruby-mode)

;; Inf-Ruby
(setq inf-ruby-default-implementation "pry") ; Use pry by default instead of irb for inf-ruby mode

(provide 'init)
;;; init.el ends here
