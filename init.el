;; -----------------------------------------------
;; INITIALIZE STARTUP OPTIONS
;; -----------------------------------------------

;; Set Font Size
(set-face-attribute 'default nil :height 90)

;; No Startup Message
(setq inhibit-startup-message t)

;; Show column number
(column-number-mode t)

;; Save session on exit
(desktop-save-mode t)

;; -----------------------------------------------
;; INITIALIZE USER DEFINED SHORTCUTS
;; -----------------------------------------------

;; Compile shortcut
(global-set-key (kbd "C-c c") 'recompile)


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
	php-mode
	python-mode
	ruby-mode

	;; Color theme modes
	color-theme
	color-theme-tango
	))

(el-get)

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
