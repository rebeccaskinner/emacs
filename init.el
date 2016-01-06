;;; init --- Emacs configuration
;;; provide (init)
;;; Commentary:

;;; Code:
(require 'cl)

;; Disable the splash screen
(setq inhibit-splash-screen t)

(defun emacs-cfg-dir ()
  (if (boundp 'user-emacs-directory)
      user-emacs-directory
    (concat (getenv "HOME") "/.emacs.d/")
    )
  )

 
;; Add "~/.emacs.d/lisp/" to the load path
(add-to-list 'load-path (concat
                         (emacs-cfg-dir)
                         (convert-standard-filename "lisp/")
                         ))
(add-to-list 'load-path "/Users/rebecca/repos/emacs/distel/elisp")

;; packages
(setq package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")
                         ("marmalade"    . "http://marmalade-repo.org/packages/")
                         ("melpa"        . "http://melpa.org/packages/")))
(package-initialize)


(defun require-package (package)
  "Install given PACKAGE."
  (setq-default highlight-tabs t)
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(setq-default indent-tabs-mode nil)

(defun add-to-paths (path)
  "Add PATH to the current Emacs search path."
  (setenv "PATH" (concat path (concat ":" (getenv "PATH"))))
  (add-to-list 'exec-path path)
  )

(defun make-home-path (path)
  "Make a PATH string relative to the current users home directory."
  (concat (getenv "HOME") (concat "/" path))
  )

(defun update-path ()
  "Add several useful paths to the default searchpath."
  (add-to-paths (make-home-path ".cabal/bin"))
  (add-to-paths (make-home-path ".gem/ruby/2.1.0/bin"))
  (add-to-paths "/usr/local/bin")
  (add-to-paths "/usr/local/texlive/2015/bin/x86_64-darwin/")
  )

(update-path)

;; Setup a backup directory so that working directories aren't polluted with
;; backup files
;; (defun set-backups-to-tempdir ()
;;   "Configure Emacs to store backup files in /tmp/emacs.$UID."
;;   (defconst emacs-tmp-dir (format "%s%s%s"
;;                                   temporary-file-directory
;;                                   "emacs."
;;                                   (user-id)))
;;   (setq backup-directory-alist
;;         `((".*" ., emacs-tmp-dir)))
;;   (setq auto-save-file-name-transforms
;;         `((".*", emacs-tmp-dir t)))
;;   (setq auto-save-list-file-prefix
;;         emacs-tmp-dir)
;;  )

;; (set-backups-to-tempdir)

;; Turn on visual line-wrapping mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'tex-mode-hook 'turn-on-visual-line-mode)

(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight medium :height 100 :width normal)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 2))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(load-theme 'adwaita)

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
  "Configure soft-wrap to WIDTH columns of text, and set a visual fill column at the boundry."
  (unless width (setq width 80))
  (set-fill-column width)
  (fci-mode 1)
  (auto-fill-mode -1)
  (turn-on-visual-line-mode)
  (window-margin-mode))

(defun enable-expand-region ()
  "Configures the 'expand-region' command for development modes."
  (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))

(defun line-nums ()
  "Enable global line numbers for programming modes."
  (global-linum-mode 1)
  (setq linum-format "%4d \u2502 ")
  )

;(defun configure-whitespace-visualizations()
;  "Configures the default whitespace visualization style for development modes."
;  (require 'whitespace)
;  ()
;  )

;; mode specific configs
(defun default-programming-config ()
  "Configure some sane defaults shared across various programming-related major modes."
  (auto-fill-mode 1)
  (auto-complete-mode 1)
  (rainbow-delimiters-mode 1)
  (fci-mode 1)
  (set-fill-column 80)
  (enable-expand-region)
  (line-nums)
  )

;; emacs lisp mode configuration
(defun elisp-config ()
  "Configuration for elisp-mode."
  (default-programming-config)
  )

(add-hook 'emacs-lisp-mode-hook 'elisp-config)

(defun json-mode-config ()
  "Configuration for JSON-mode."
  (rainbow-delimiters-mode)
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  (window-margin-mode)
  )

;; Erlang Mode
(defun configure-distel ()
  "Setup distel for erlang projects."
  (let ((distel-dir "/usr/local/share/distel/elisp")) ; Add distel-dir to the end of load-path
    (unless (member distel-dir load-path)
      (setq load-path (append load-path (list distel-dir)))))
  ;; (add-to-list 'load-path "/usr/local/share/distel/elisp")
  (require 'distel)
  (distel-setup)
  )

(defun erlang-config ()
  "Configure some defaults for erlang-mode."

  (defun erl-shell-with-flags (flags)
  "Start an erlang shell with flags"
  (interactive (list (read-string "Flags: ")))
  (set 'inferior-erlang-machine-options (split-string flags))
  (erlang-shell))

  (default-programming-config)
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  (setq erlang-indent-level 2)
  (configure-distel)
  (flycheck-mode 0)
  (flymake-mode 1)
  (require 'erlang-flymake)                           ; Automatic compilation of Erlang code
  (erlang-flymake-only-on-save)                       ; Only run flymake after
                                        ; saving a file

  ;; To integrate Flymake with rebar; these functions look for th topmost rebar.config
(defun ebm-find-rebar-top-recr (dirname)
  (let* ((project-dir (locate-dominating-file dirname "rebar.config")))
    (if project-dir
        (let* ((parent-dir (file-name-directory (directory-file-name project-dir)))
               (top-project-dir (if (and parent-dir (not (string= parent-dir "/")))
                                    (ebm-find-rebar-top-recr parent-dir)
                                  nil)))
          (if top-project-dir
              top-project-dir
            project-dir))
      project-dir)))

(defun ebm-find-rebar-top ()
  (interactive)
  (let* ((dirname (file-name-directory (buffer-file-name)))
         (project-dir (ebm-find-rebar-top-recr dirname)))
    (if project-dir
        project-dir
      (erlang-flymake-get-app-dir))))

(defun ebm-directory-dirs (dir name)
  "Find all directories in DIR."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir (directory-file-name dir))
        (dirs '())
        (files (directory-files dir nil nil t)))
    (dolist (file files)
      (unless (member file '("." ".."))
        (let ((absolute-path (expand-file-name (concat dir "/" file))))
          (when (file-directory-p absolute-path)
            (if (string= file name)
                (setq dirs (append (cons absolute-path
                                         (ebm-directory-dirs absolute-path name))
                                   dirs))
              (setq dirs (append
                          (ebm-directory-dirs absolute-path name)
                          dirs)))))))
    dirs))

(defun ebm-get-deps-code-path-dirs ()
  (ebm-directory-dirs (ebm-find-rebar-top) "ebin"))

(defun ebm-get-deps-include-dirs ()
  (ebm-directory-dirs (ebm-find-rebar-top) "include"))

(fset 'erlang-flymake-get-code-path-dirs 'ebm-get-deps-code-path-dirs)
(fset 'erlang-flymake-get-include-dirs-function 'ebm-get-deps-include-dirs)
  )

(add-hook 'erlang-mode-hook 'erlang-config)
; (add-hook 'erlang-mode-hook 'rebar-mode)

;; Ruby Mode

(defun turn-on-enhanced-ruby-mode ()
  "Enable enh-ruby-mode and add some configuration options."
  (require 'enh-ruby-mode)
  (add-hook 'enh-ruby-mode-hook 'fci-mode)
  (add-hook 'enh-ruby-mode-hook 'turn-on-auto-fill)
  (add-hook 'enh-ruby-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'enh-ruby-mode-hook 'auto-complete-mode)
  )

(defun configure-inf-ruby ()
  "Use Pry instead of irb as the REPL for inferior ruby mode."
  (setq inf-ruby-default-implementation "pry")
  )

(defun ruby-config ()
  "Setup 'ruby-mode` and enh-ruby-mode parameters for ruby editing."
  (default-programming-config)
  (configure-inf-ruby)
  (turn-on-enhanced-ruby-mode)
  )

(add-hook 'ruby-mode-hook 'ruby-config)

(add-hook 'json-mode-hook 'json-mode-config)
(add-hook 'markdown-mode-hook 'default-programming-config)

(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo `find-tag'."              
  "If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run `etags' on all peer files in current dir and reload them silentlyf, \
if EXTENSION is specified, use it for refreshing etags, or default to .el."
  
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently          
    (visit-tags-table default-directory nil)))

(defun create-tags(format)
  (eshell-command
   (format "find %s -type f -name \"%s\" | etags -" (pwd) format)
   )
  )

;; Erlang Mode

;; Haskell Mode

;; Set the haskell-mode default indentation mode, this is required for
;; haskell-mode to work

(require 'haskell-interactive-mode)
(require 'haskell-process)
(load "haskell-mode-autoloads")


(defun haskell-config ()
  "Setup a sane haskell development environment."
  (default-programming-config)
  (cabal-path-cfg)
  (custom-set-variables
   '(haskell-notify-p t)
   '(haskell-tags-on-save t)
   '(haskell-stylish-on-save t)
   )
  ;; Use haskell indentation.
  (haskell-indent-mode)
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
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-looad-file)
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

(defun extra-cc-keybindings()
  (global-set-key (kbd "C-?") (kbd "M-x manual-entry RET"))
  )

;; Cc Mode
;; Set the indentation to 4 spaces
(setq-default c-basic-offset 4
              c-default-style "bsd")

;; Enable 80-column fill indicator for C files
(add-hook 'cc-mode-hook 'turn-on-auto-fill)

;; set up auto-complete-mode for C files
(add-hook 'cc-mode-hook 'auto-complete-mode)
(add-hook 'cc-mode-hook 'etags-c-tags)
(add-hook 'cc-mode-hook 'auto-complete-mode)
(add-hook 'cc-mode-hook 'extra-cc-keybindings)

;; Require ox-confluence
(require 'ox-confluence)

(ruby-config)

;;; init.el ends here
