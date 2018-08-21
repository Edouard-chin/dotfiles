(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load-theme 'zenburn t)

;; Disable arrow keys
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))

;; Enable line number and add margin
(global-linum-mode 1)

(custom-set-variables
 '(package-selected-packages
   (quote
    (flx-ido projectile auto-complete expand-region magit zenburn-theme))))

(custom-set-faces
 '(cursor ((t (:background "systemYellowColor" :foreground "#DCDCCC"))))
 '(region ((t (:background "Orange")))))

;; Run ruby test at point, this requires the rails test runner and `dev` (Shopify)
(defun run-test-at-point()
 (interactive)
 (setq default-directory (locate-dominating-file buffer-file-name "Gemfile"))

 (setq shell-command-switch "-ic")
 (async-shell-command (format "dev test %s:%d" (buffer-file-name) (line-number-at-pos)))
)
(global-set-key (kbd "C-x t") 'run-test-at-point)

;; Comment line
(global-set-key (kbd "C-x C-_") 'comment-line)

;; Use the expand region package
(require 'expand-region)
(global-set-key (kbd "C-x C-t") 'er/expand-region)

;; Enable ido mode everywhere with flex matching
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Rebing C-x C-b for ibuffer instead of the vanilla buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Enable winner mode (which allows to changes the window configuration)
(winner-mode 1)

;; Find file
(global-set-key (kbd "C-x p") 'find-name-dired)

;; Kill current buffer without having to confirm
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Disable the bell ding
(setq ring-bell-function 'ignore)

;; Enable whitespace visualization
(setq whitespace-style '(face tabs spaces empty lines-tail trailing))
(global-whitespace-mode t)
(setq whitespace-line-column 120)

;; Disable ruby mode to add the encoding utf8 comment when saving
(setq ruby-insert-encoding-magic-comment nil)

;; Enable autocompletion
(global-auto-complete-mode t)

;; Font and text customization
(set-face-attribute 'default nil
                   :font "Monaco"
                   :height 135
                   :weight 'normal)

;; Disable that toolbar when using X
(tool-bar-mode -1)

;; Turn off splash screen
(setq inhibit-splash-screen t)

;; Remove the scrollbar (window mode)
(scroll-bar-mode -1)

;; Start emacs maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Add a hook so that `compile` executes ruby when in ruby-mode
(add-hook 'ruby-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "ruby " buffer-file-name))))

;; Compile shortcut
(global-set-key (kbd "C-c e") 'compile)

;; Adds a newline at EOF when file is saved
(setq require-final-newline t)

;; Automatically closes pair like "''" or "{}"
(electric-pair-mode t)

;; Enable CUA selection mode for rectangle selection improvement
(cua-selection-mode t)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
