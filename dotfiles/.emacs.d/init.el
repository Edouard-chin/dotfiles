(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Start emacs maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Cursor and region customization
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right
 '(mc/cursor-face ((((class color)) (:background "Orange"))))
 '(cursor ((t (:background "systemYellowColor" :foreground "#DCDCCC"))))
 '(highlight ((t (:background "Orange"))))
 '(region ((t (:background "Orange")))))

;; Font and text customization
(set-face-attribute 'default nil
                   :font "Monaco"
                   :height 135
                   :weight 'normal)

;; Disable that toolbar when using X
(tool-bar-mode -1)

;; Turn off splash screen
(setq inhibit-splash-screen t)

;; Disable tabs
(setq-default indent-tabs-mode nil)

;; 2 spaces by default
(setq-default tab-width 2)

;; 2 spaces by default on JS files
(setq-default js-indent-level 2)

;; 2 spaces by default on typscript files
(setq-default typescript-indent-level 2)

;; Do What I Mean when asking for destination directory.
(setq dired-dwim-target t)

;; Remove the scrollbar (window mode)
(scroll-bar-mode -1)

;; Disable the bell ding
(setq ring-bell-function 'ignore)

;; Disable arrow keys
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))

;; Enable line number number mode
(global-linum-mode 1)
(column-number-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Enable winner mode (which allows to changes the window configuration)
(winner-mode 1)

;; highlight the current line
(global-hl-line-mode +1)

;; Automatically closes pair like "''" or "{}"
(electric-pair-mode t)

;; Rebing C-x C-b for ibuffer instead of the vanilla buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Find file
(global-set-key (kbd "C-x p") 'find-name-dired)

;; Interactive yank menu
(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))

;; Kill current buffer without having to confirm
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Automatically balance windows after creating one
(advice-add 'split-window-right :after #'balance-windows)

;; No "yes" or "no", only "y" or "n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Add a hook so that `compile` executes ruby when in ruby-mode
(add-hook 'ruby-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "ruby " buffer-file-name))))

;; Adds a newline at EOF when file is saved
(setq require-final-newline t)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package ace-window
  :ensure t
  :bind (([remap other-window] . ace-window))
  :config
  (setq aw-scope 'frame))

(use-package paren
  :config
  (show-paren-mode +1))

(use-package yaml-mode
  :ensure t)

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package counsel
  :ensure t
  :bind (("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c r" . counsel-rg)
         ("C-x l" . counsel-locate)))

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package avy
  :ensure t
  :bind (("C-'" . avy-goto-char-timer))
  :config
  (setq avy-background t))

(use-package ruby-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (setq ruby-align-to-stmt-keywords t)
  (add-hook 'ruby-mode-hook #'subword-mode))

(use-package expand-region
  :ensure t
  :bind ("C-x C-t" . er/expand-region))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this))
)

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-style '(face tabs spaces empty lines-tail trailing))
  (setq whitespace-line-column 120))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(web-mode typescript-mode wgrep multiple-cursors ripgrep company zenburn-theme yaml-mode use-package expand-region counsel auto-complete ace-window))
 '(require-final-newline nil))
