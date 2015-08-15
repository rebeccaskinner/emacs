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

(defun add-to-paths (path)
  (setenv "PATH" (concat path (concat ":" (getenv "PATH"))))
  (add-to-list 'exec-path path)
;  (setq exec-path (append exec-path (path)))
  )

(defun make-home-path (path)
  (concat (getenv "HOME") (concat "/" path))
  )

(defun cabal-path-cfg ()
  (add-to-paths (make-home-path ".cabal/bin"))
  )

(defun gem-path-cfg ()
  (add-to-paths (make-home-path ".gem/ruby/2.1.0/bin"))
  )

(defun update-path ()
  (cabal-path-cfg)
  (gem-path-cfg)
  )

(update-path)

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

(defun enable-expand-region ()
  (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))

;; mode specific configs

(defun default-programming-config ()
  (auto-fill-mode 1)
  (auto-complete-mode 1)
  (rainbow-delimiters-mode 1)
  (fci-mode 1)
  (set-fill-column 80)
  (enable-expand-region))

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


(defun load-enh-ruby-mode ()
  (defvar ruby-mode-path (convert-standard-filename
                          "extern/enhanced-ruby-mode/"))
  (add-to-list 'load-path (concat user-emacs-directory ruby-mode-path))
  (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
  (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
  (require 'enh-ruby-mode)
  )

(defun turn-on-enhanced-ruby-mode ()
  (load-enh-ruby-mode)
  (add-hook 'enh-ruby-mode-hook 'fci-mode)
  (add-hook 'enh-ruby-mode-hook 'turn-on-auto-fill)
  (add-hook 'enh-ruby-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'enh-ruby-mode-hook 'auto-complete-mode)
  )

(defun configure-inf-ruby ()
  ;; Use Pry instead of irb as the REPL for inferior ruby mode
  (setq inf-ruby-default-implementation "pry")
  )

(defun ruby-config ()
  (default-programming-config)
  (configure-inf-ruby)
  (turn-on-enhanced-ruby-mode)
  )

(add-hook 'json-mode-hook 'json-mode-config)
(add-hook 'markdown-mode-hook 'default-programming-config)

;; Haskell Mode

;; Set the haskell-mode default indentation mode, this is required for
;; haskell-mode to work

(require 'haskell-interactive-mode)
(require 'haskell-process)
(load "haskell-mode-autoloads")

(custom-set-variables
 '(haskell-notify-p t)
 '(haskell-tags-on-save t)
 '(haskell-stylish-on-save t)
 )

(defun haskell-config ()
  (default-programming-config)
  (cabal-path-cfg)
  ;; Use simple indentation.
  (turn-on-haskell-simple-indent)
  (define-key haskell-mode-map (kbd "<return>") 'haskell-simple-indent-newline-same-col)
  (define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)

  ;; Load the current file (and make a session if not already made).
  (define-key haskell-mode-map [?\C-c ?\C-l] 'haskell-process-load-file)
  (define-key haskell-mode-map [f5] 'haskell-process-load-file)

  ;; Switch to the REPL.
  (define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
  ;; “Bring” the REPL, hiding all other windows apart from the source
  ;; and the REPL.
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

  ;; Build the Cabal project.
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  ;; Interactively choose the Cabal command to run.
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

  ;; Get the type and info of the symbol at point, print it in the
  ;; message buffer.
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)

  ;; Contextually do clever things on the space key, in particular:
  ;;   1. Complete imports, letting you choose the module name.
  ;;   2. Show the type of the symbol after the space.
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

  ;; Jump to the imports. Keep tapping to jump between import
  ;; groups. C-u f8 to jump back again.
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

  ;; Jump to the definition of the current symbol.
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

  ;; Indent the below lines on columns after the current column.
  (define-key haskell-mode-map (kbd "C-<right>")
    (lambda ()
      (interactive)
      (haskell-move-nested 1)))
  ;; Same as above but backwards.
  (define-key haskell-mode-map (kbd "C-<left>")
    (lambda ()
      (interactive)
      (haskell-move-nested -1)))
  )

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-config)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c M-.") nil)
     (define-key haskell-mode-map (kbd "C-c C-d") nil)))

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

(ruby-config)

(provide 'init)
;;; init.el ends here
