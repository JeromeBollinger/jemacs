(setq inhibit-startup-message t)
(setq visible-bell nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)

(setq history-length 25)
(savehist-mode 1)
(save-place-mode 1)

;; Move customization variables to a separate file and load it
;;(setq custom-file (locate-user-emacs-file "custom-vars.el"))
;;(load custom-file 'noerror 'nomessage)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Using a single aspect
(setq modus-themes-mode-line '(borderless))
(setq modus-themes-mode-line '(accented borderless padded))

;; Load the dark theme by default
(load-theme 'tango-dark t)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(require 'package)

(use-package magit
  :bind ("ŋ" . magit)
  :ensure t)

(defun new_vterm ()
  (interactive)
  (vterm 'N)
 )


(add-hook 'vterm-mode-hook (lambda () (text-scale-decrease 2)))
(use-package vterm
  :bind (("ß" . new_vterm)
	 ("æ" . vterm-copy-mode))
  :hook (vterm-mode . (lambda () (text-scale-decrease 2)))
  :ensure t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :ensure t)

(use-package which-key
  :init (which-key-mode)
  :diminish (which-key-mode)
  :config (setq which-key-idle-delay 0.3)
  :ensure t)

(use-package helm
  :config (helm-mode 1)
  :bind (("M-x" . helm-M-x)
  ;;("C-k" . helm-next-line)
	 ;;	 ("C-l" . helm-previous-line)
	 )
  :ensure t)

(use-package projectile
  :ensure t)

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode)
  :config (setq idle-highlight-visible-buffers t)
  :ensure t)


;; navigation
(global-set-key (kbd "C-j") 'backward-char)
(global-set-key (kbd "C-k") 'next-line)
(global-set-key (kbd "C-l") 'previous-line)
(global-set-key (kbd "C-ö") 'forward-char)

(global-set-key (kbd "M-j") 'backward-word)
(global-set-key (kbd "M-k") 'move-beginning-of-line)
(global-set-key (kbd "M-l") 'move-end-of-line)
(global-set-key (kbd "M-ö") 'forward-word)

(add-to-list 'default-frame-alist '(alpha-background . 90))

;; Buffer management
(global-set-key (kbd "ĸ") 'kill-this-buffer)
(global-set-key (kbd "ł") (lambda () (interactive) (switch-to-buffer nil))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(idle-highlight-mode projectile helm which-key rainbow-delimiters vterm magit use-package popup async)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
