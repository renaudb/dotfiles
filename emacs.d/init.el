;; -----------------------------------------------
;; INITIALIZE STARTUP OPTIONS
;; -----------------------------------------------

;; Set Font Size
(set-face-attribute 'default nil :height 90)

;; No Startup Message
(setq inhibit-startup-message t)

;; -----------------------------------------------
;; SETUP UP BUILT IN MODES
;; -----------------------------------------------

;; Disable the menubar and toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Show column number
(column-number-mode t)

;; Save session on exit
(desktop-save-mode t)

;; Show matching parenthesis
(show-paren-mode t)

;; Set C/C++ indent to 2 spaces
(setq c-basic-offset 2)

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
;; INITIALIZE USER DEFINED SHORTCUTS
;; -----------------------------------------------

;; Compile shortcut
(global-set-key (kbd "C-c c") 'recompile)

;; Buffer menu shortcut
(global-set-key (kbd "<C-tab>") 'buffer-menu)

;; -----------------------------------------------
;; INITIALIZE USER DEFINED FUNCTIONS
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
;; LOAD EMACS MODES
;; -----------------------------------------------

;; Load emacs mode
(require 'cl)
(require 'uniquify)
(require 'whitespace)

;; Load el-get modes
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)

;; Set mode list
(setq el-get-sources
      '(package

	;; Language modes
	js2-mode
	lua-mode
	php-mode
	python-mode
	ruby-mode
	scala-mode

	;; Color theme modes
	color-theme
	color-theme-tango-2
	))
(el-get)

;; Set the color theme.
(color-theme-tango)

;; Autoload octave mode on MATLAB files.
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; -----------------------------------------------
;; INITIALIZE MODES AND OTHER OPTIONS
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
