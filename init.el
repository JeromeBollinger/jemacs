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
  :config (setq vterm-shell "/usr/bin/zsh")
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
  :bind (("M-x" . helm-M-x)
	 (:map helm-map
	      ("C-k" . helm-next-line)
	      ("C-l" . helm-previous-line)))
  :config (helm-mode 1)
  :ensure t)

(use-package projectile
  :ensure t)

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode)
  :config (setq idle-highlight-visible-buffers t)
  :ensure t)

(use-package lsp-mode
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :config((setq lsp-signature-auto-activate nil)
	  (setq lsp-rust-server 'rust-analyzer))
  :bind ("M-," . lsp-find-references)
  :commands lsp
  :ensure t)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol
  :ensure t)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :ensure t)

(use-package rust-mode
  :ensure t)

;; theme
(set-frame-parameter (selected-frame) 'alpha '(96 100))

(use-package spacemacs-theme
  :config (load-theme 'spacemacs-dark t)
  :ensure t)

(use-package dired-git-info
  :config (add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable)
  :ensure t)

(use-package justl
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

(global-set-key (kbd "C-i") 'recenter-top-bottom)

;; Buffer management
(global-set-key (kbd "ĸ") 'kill-this-buffer)
(global-set-key (kbd "ł") (lambda () (interactive) (switch-to-buffer nil)))
(setq backup-directory-alist `(("." . "~/.saves")))

;; window management
(global-set-key (kbd "C-(") '(lambda () (interactive) (split-window-right) (other-window 1)))
(global-set-key (kbd "C-)") '(lambda () (interactive) (split-window-below) (other-window 1)))
(global-set-key (kbd "ð") 'delete-window)
(global-set-key (kbd "C-)") 'split-window-below-and-focus)
(global-set-key (kbd "ŧ") 'tab-new)
(global-set-key (kbd "¢") 'tab-close)

;; hotkeys
(global-set-key (kbd "←") 'undo)
(global-set-key (kbd "«") 'undo-redo)
(global-set-key (kbd "ħ") 'replace-string)
(global-set-key (kbd "C-x C-r") 'rectangle-mark-mode)
(global-set-key (kbd "C-x C-l") 'string-rectangle)
(global-set-key (kbd "ſ") 'whitespace-cleanup)
;; (global-set-key (kbd "M-p") 'drag-stuff-up)
;; (global-set-key (kbd "M-n") 'drag-stuff-down)
(global-set-key (kbd "TAB") 'self-insert-command)


;; my functions
(defun jeb/e-init()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun jeb/e-spacemacs()
  (interactive)
  (find-file "~/.emacs.d/custom/.spacemacs"))

(defun jeb/e-zs()
  (interactive)
  (find-file "~/.zshrc"))

(defun jeb/e-todo()
  (interactive)
  (find-file "~/gdrive/notes/Todo/TODO.md"))

(defun jeb/d-home()
  (interactive)
  (dired "/home/jeb/"))

(defun jeb/d-projects()
  (interactive)
  (dired "/home/jeb/Documents/projects"))

(defun jeb/localhost (port &optional secure-answer)
  (interactive "sPort: \nsSecure? (y/n)")
  (setq my-secure nil)
  (setq my-secure (cl-equalp secure-answer "y"))
  (browse-url (concat"http" (if my-secure "s") "://localhost:" port)))

(defun jeb/ask-openai (prompt)
  "Establish a connection with OpenAI."
  (interactive "sAsk OpenAI:")
  (setq response (shell-command-to-string (concat "print $(curl https://api.openai.com/v1/completions \
		 -H 'Content-Type: application/json' \
		 -H 'Authorization: Bearer ' \
		 -d '{
		 \"model\": \"text-davinci-003\",
		 \"prompt\": \"" prompt "\",
		 \"max_tokens\": 500,
		 \"temperature\": 0
		 }' 2>/dev/null" "| jq '.choices[0].text' )"))
  )

  (cond ((get-buffer-window "AI"))
	(t (split-window-right) (switch-to-buffer "AI"))
  )
  (with-current-buffer (get-buffer-create "AI") (end-of-buffer) (insert (concat prompt ":" response "
"))))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(helm-lsp justl dired-git-info git-info rust-mode lsp-treemacs lsp-mode spacemacs-theme idle-highlight-mode projectile helm which-key rainbow-delimiters vterm magit use-package popup async))
 '(projectile-globally-ignored-directories
   '("^\\.idea$" "^\\.vscode$" "^\\.ensime_cache$" "^\\.eunit$" "^\\.git$" "^\\.hg$" "^\\.fslckout$" "^_FOSSIL_$" "^\\.bzr$" "^_darcs$" "^\\.pijul$" "^\\.tox$" "^\\.svn$" "^\\.stack-work$" "^\\.ccls-cache$" "^\\.cache$" "^\\.clangd$" "target")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;; navigation
(global-set-key (kbd "C-j") 'backward-char)
(global-set-key (kbd "C-k") 'next-line)
(global-set-key (kbd "C-l") 'previous-line)
(global-set-key (kbd "C-ö") 'forward-char)

(global-set-key (kbd "M-j") 'backward-word)
(global-set-key (kbd "M-k") 'move-beginning-of-line)
(global-set-key (kbd "M-l") 'move-end-of-line)
(global-set-key (kbd "M-ö") 'forward-word)

(global-set-key (kbd "C-i") 'recenter-top-bottom)

;; Buffer management
(global-set-key (kbd "ĸ") 'kill-this-buffer)
(global-set-key (kbd "ł") (lambda () (interactive) (switch-to-buffer nil)))

;; window management
(global-set-key (kbd "C-(") '(lambda () (interactive) (split-window-right) (other-window 1)))
(global-set-key (kbd "C-)") '(lambda () (interactive) (split-window-below) (other-window 1)))
(global-set-key (kbd "ð") 'delete-window)
(global-set-key (kbd "C-)") 'split-window-below-and-focus)
(global-set-key (kbd "ŧ") 'tab-new)
(global-set-key (kbd "¢") 'tab-close)

;; hotkeys
(global-set-key (kbd "←") 'undo)
(global-set-key (kbd "«") 'undo-redo)
(global-set-key (kbd "ħ") 'replace-string)
(global-set-key (kbd "C-x C-r") 'rectangle-mark-mode)
(global-set-key (kbd "C-x C-l") 'string-rectangle)
(global-set-key (kbd "ſ") 'whitespace-cleanup)
;; (global-set-key (kbd "M-p") 'drag-stuff-up)
;; (global-set-key (kbd "M-n") 'drag-stuff-down)



;; my functions
(defun jeb/e-init()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun jeb/e-spacemacs()
  (interactive)
  (find-file "~/.emacs.d/custom/.spacemacs"))

(defun jeb/e-zs()
  (interactive)
  (find-file "~/.zshrc"))

(defun jeb/e-todo()
  (interactive)
  (find-file "~/gdrive/notes/Todo/TODO.md"))

(defun jeb/d-home()
  (interactive)
  (dired "/home/jeb/"))

(defun jeb/d-projects()
  (interactive)
  (dired "/home/jeb/Documents/projects"))

(defun jeb/localhost (port &optional secure-answer)
  (interactive "sPort: \nsSecure? (y/n)")
  (setq my-secure nil)
  (setq my-secure (cl-equalp secure-answer "y"))
  (browse-url (concat"http" (if my-secure "s") "://localhost:" port)))

(defun jeb/ask-openai (prompt)
  "Establish a connection with OpenAI."
  (interactive "sAsk OpenAI:")
  (setq response (shell-command-to-string (concat "print $(curl https://api.openai.com/v1/completions \
		 -H 'Content-Type: application/json' \
		 -H 'Authorization: Bearer ' \
		 -d '{
		 \"model\": \"text-davinci-003\",
		 \"prompt\": \"" prompt "\",
		 \"max_tokens\": 500,
		 \"temperature\": 0
		 }' 2>/dev/null" "| jq '.choices[0].text' )"))
  )

  (cond ((get-buffer-window "AI"))
	(t (split-window-right) (switch-to-buffer "AI"))
  )
  (with-current-buffer (get-buffer-create "AI") (end-of-buffer) (insert (concat prompt ":" response "
"))))





