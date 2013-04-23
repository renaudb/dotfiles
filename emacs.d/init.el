;; -----------------------------------------------
;; STARTUP OPTIONS
;; -----------------------------------------------

;; Set Font Size
(set-face-attribute 'default nil :height 110)

;; No Startup Message
(setq inhibit-startup-message t)

;; Disable the menubar and toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Show column number
(column-number-mode t)

;; Save session on exit
(desktop-save-mode t)

;; Show matching parenthesis
(show-paren-mode t)

;; Reload etags file on change
(setq tags-revert-without-query t)

;; Set backup file folder
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; -----------------------------------------------
;; USER DEFINED SHORTCUTS
;; -----------------------------------------------

;; Compile shortcut
(global-set-key (kbd "C-c c") 'recompile)

;; Buffer menu shortcut
(global-set-key (kbd "<C-tab>") 'buffer-menu)

;; -----------------------------------------------
;; USER DEFINED FUNCTIONS
;; -----------------------------------------------

;; Special prints
(defun ps-spool-buffer-no-header ()
  "Postscript print, but without a header."
  (interactive)
  (let ((ps-print-header nil))
    (ps-spool-buffer)))

(defun ps-spool-buffer-with-faces-no-header ()
  "Postscript print, but without a header."
  (interactive)
  (let ((ps-print-header nil))
    (ps-print-buffer-with-faces)))

;; -----------------------------------------------
;; PACKAGES
;; -----------------------------------------------

;; Load built-in emacs modes
(require 'cl)
(require 'uniquify)
(require 'whitespace)

;; Load el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; Set local sources
(setq el-get-sources
      '(
	(:name emacs-clang-complete-async
	       :type github
	       :pkgname "Golevka/emacs-clang-complete-async"
	       :description "An emacs plugin to complete C and C++ code using libclang."
	       :build ("make")
	       :post-init (setq ac-clang-complete-executable (expand-file-name "clang-complete"))
	       )
	))

;; Set package list
(setq my-packages
      (append
       '(
	 ;; Language modes
	 ess
	 js2-mode
	 lua-mode
	 ;matlab-mode
	 php-mode
	 python-mode
	 ruby-mode
	 scala-mode

	 ;; Google style
	 google-c-style

	 ;; Autocomplete
	 auto-complete
	 ;auto-complete-clang
	 auto-complete-latex

	 ;; Syntax checking.
	 flymake

	 ;; Super modes
	 jedi
	 eclim
	 auctex

	 ;; Random modes
	 rainbow-mode

	 ;; Color themes
	 color-theme
	 color-theme-solarized
	 )
       (mapcar 'el-get-source-name el-get-sources)))

;; Sync el-get packages.
(el-get 'sync my-packages)

;; -----------------------------------------------
;; COLOR THEME
;; -----------------------------------------------

;; Set the color theme.
(color-theme-solarized-dark)

;; -----------------------------------------------
;; AUTO-COMPLETE
;; -----------------------------------------------

;; Load auto-complete.
(require 'auto-complete-config)
(ac-config-default)

;; Use auto-complete for everything.
(global-auto-complete-mode t)

;; -----------------------------------------------
;; C/C++ MODE
;; -----------------------------------------------

;; Set indent to 2 spaces
(setq c-basic-offset 2)

;; Set C-mode to Google.
(add-hook 'c-mode-common-hook 'google-set-c-style)

;; Setup C/C++ clang auto-complete.
(require 'auto-complete-clang-async)
(defun ac-cc-mode-setup ()
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process)
)
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)

;; Add standard library to clang completion.
(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               "
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.0/../../../../include/c++/4.8.0
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.0/../../../../include/c++/4.8.0/x86_64-unknown-linux-gnu
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.0/../../../../include/c++/4.8.0/backward
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.0/include
 /usr/local/include
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.0/include-fixed
 /usr/include
"
               )))

;; -----------------------------------------------
;; JAVA MODE
;; -----------------------------------------------

;; Start eclim.
(require 'eclim)
(require 'eclimd)
(global-eclim-mode)

;; Setup Java eclim auto-complete.
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

;; -----------------------------------------------
;; PYTHON MODE
;; -----------------------------------------------

;; Set Python-mode to use Jedi.
(add-hook 'python-mode-hook 'jedi:setup)

;; -----------------------------------------------
;; LATEX MODE
;; -----------------------------------------------

;; Set auctex to PDF.
(setq TeX-PDF-mode t)

;; Load flyspell for text correction.
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)

;; -----------------------------------------------
;; WHITESPACE MODE
;; -----------------------------------------------

;; Highlight trailing characters
(setq whitespace-line-column 80
      whitespace-style '(face trailing lines-tail))

;; Set invalid whitespace colors
(set-face-attribute 'whitespace-line nil
                    :background "red"
                    :foreground "yellow"
                    :weight 'bold)

(set-face-attribute 'whitespace-tab nil
                    :background "red"
                    :foreground "yellow"
                    :weight 'bold)

;; Enable whitespace mode on everything
(global-whitespace-mode t)

;; Remove trailing whitespaces on save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
