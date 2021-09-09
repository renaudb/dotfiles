;; Set name and email.
(setq user-full-name "Renaud Bourassa")
(setq user-mail-address "to.rhino@gmail.com")

;; Load package.
(load "package")
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Load use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Force installation if missing.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Install packages.
(use-package solarized-theme)

;; Disable UI options.
(setq inhibit-splash-screen t
      initial-scratch-message nil)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Set esc-up/esc-down.
(global-set-key (kbd "ESC <up>") 'backward-paragraph)
(global-set-key (kbd "ESC <down>") 'forward-paragraph)

;; Show column number.
(column-number-mode 1)

;; Y/n instead of Yes/no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Autoreload on file change.
(global-auto-revert-mode t)

;: No backup files.
(setq make-backup-files nil)

;; Default indent.
(setq tab-width 2
      c-basic-offset 2
      js-indent-level 2
      indent-tabs-mode nil)

;; Remove trailing whitespaces.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Load theme.
(load-theme 'solarized-dark t)
