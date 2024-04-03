(setq inhibit-startup-message t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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

(use-package vterm
  :bind (("ß" . new_vterm)
	 ("æ" . vterm-copy-mode)
         :map vterm-mode-map
         ("C-j" . vterm-send-C-b)
         ("C-k" . vterm-send-C-n)
         ("C-l" . vterm-send-C-p)
         ("C-ö" . vterm-send-C-f)
         ("M-ö" . vterm-send-M-f)
         ("M-j" . vterm-send-M-b)
         ("M-k" . vterm-send-C-a)
         ("M-l" . vterm-send-C-e)
         ("C-ä" . vterm-send-backspace)
         ("M-ä" . vterm-send-meta-backspace)
         )
  :hook (vterm-mode . (lambda () (setq show-trailing-whitespace nil)))
  :config (setq vterm-shell "/usr/bin/zsh")
  :ensure t)

(defun new_vterm ()
  (interactive)
  (vterm 'N)
 )

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
      ("C-l" . helm-previous-line)
      :map helm-find-files-map
      ("C-l" . helm-previous-line)
      ("<tab>" . helm-execute-persistent-action)
      ("C-j" . helm-find-files-up-one-level)
      ("C-ö" . helm-execute-persistent-action)
      ("C-;" . helm-execute-persistent-action)
      ("t" . self-insert-command)))
  :config (helm-mode 1)
  (setq helm-move-to-line-cycle-in-source nil)
  :ensure t)


(use-package projectile
  :ensure t)

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode)
  (yaml-mode . idle-highlight-mode)
  :config (setq idle-highlight-visible-buffers t)
  (setq idle-highlight-exceptions-face '(font-lock-keyword-face))
  (setq idle-highlight-global-mode t)
  :ensure t)

(use-package lsp-mode
  :hook ((rust-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config(setq lsp-signature-auto-activate nil)
  (setq lsp-rust-server 'rust-analyzer)
  :bind ("M-," . lsp-find-references)
  :commands lsp
  :ensure t)

(use-package lsp-ui
  :config(setq lsp-ui-doc-show-with-cursor t)
  :ensure t)

(use-package drag-stuff
  :config(setq drag-stuff-global-mode t)
  :bind(("M-p" . drag-stuff-up)
         ("M-n" . drag-stuff-down))
  :ensure t)

(setq lsp-ansible-add-on? t)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol
  :ensure t)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-character-face-perc 80)
  :ensure t)

;; frame options
(add-to-list 'default-frame-alist '(alpha-background . 96))

(use-package spacemacs-theme
  :config (load-theme 'spacemacs-dark t)
  :ensure t)

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  :bind
  ("«" . undo-fu-only-redo)
  :ensure t)

(use-package dired-git-info
  :config (add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable)
  :ensure t)

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-preselect 'directory)
  (corfu-on-exact-match nil)
  (corfu-scroll-margin 5)
  (completion-styles '(basic))
  (corfu-auto-prefix 1)
  :bind
  (:map corfu-map
        ("RET" . nil))
  :init (use-package cape
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :ensure t)

(use-package justl
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package neotree
  :ensure t)

(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :ensure t)
(use-package all-the-icons-nerd-fonts
  :ensure t)

;; NeoTree can be opened (toggled) at projectile project root
(defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

;; need another one for python stuff, since this gets re-bound
(global-set-key (kbd "C-b") 'neotree-project-dir)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Run sudo pamac install dotnet-sdk
;; Clone git@github.com:christiaan-janssen/bicep-mode.git
;; follow instructions
;; run lsp server
(use-package bicep-mode
  :mode ("\\.bicep\\'"
         "\\.bicepparam\\'")
  :hook (bicep-mode . lsp-deferred)
  :bind (:map bicep-mode-map
         ("C-j" . backward-char))
  :load-path "~/Documents/bicep-mode")

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/projects/roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         (:map org-mode-map
               ("C-j" . backward-char)
               ("C-l" . previous-line)))
  :config
  (org-roam-db-autosync-mode)
  :hook (helm-mode-org-roam-node-find . (lambda () (setq show-trailing-whitespace nil)))
  :ensure t)

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :ensure t)

(use-package yasnippet
  :init (yas-global-mode)
  :ensure t)

(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :ensure t)

(use-package yascroll
  :config(setq yascroll:delay-to-hide nil)
  :ensure t)

(setq tramp-default-method "ssh")

(defun jeb/ssh-tramp (arg)
  "Easier ssh tramp access by reading ssh config"
  (interactive
   (list
    (completing-read "Select host: "
                     (list (shell-command-to-string "cat ~/.ssh/config | grep Host\ | cut -d ' ' -f 2")))))
  (find-file (concat "/ssh:" arg ":~/" ))
  )

(defun jeb/docker-tramp (arg)
  "Easier ssh tramp access by reading ssh config"
  (interactive
   (list
    (completing-read "Select container: "
                     (list (shell-command-to-string "docker ps --format '{{.Names}}'")))))
  (find-file (concat "/docker:root@" arg ":~/" ))
)

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

(delete-selection-mode t)
(setq initial-buffer-choice 'vterm)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; dired
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "j") 'helm-find-files)))
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; QOL
(fset 'yes-or-no-p 'y-or-n-p)  ;; Ask for y/n instead of yes/no

(set-face-attribute 'default nil :height 120)

;; Ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; markdown
(setq-default fill-column 120)
(add-hook 'markdown-mode-hook #'auto-fill-mode)

(delete-selection-mode 1)

(add-hook 'js-json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq tab-width 2)
            (setq js-indent-level 2)))

(setq css-indent-offset 2)

(setq exec-path (append exec-path '("~/.nvm/versions/node/v21.7.1/bin")))
(executable-find "npm")

(exec-path-from-shell-initialize)
(global-yascroll-bar-mode 1)
;; navigation
(global-set-key (kbd "C-j") 'backward-char)
(global-set-key (kbd "C-k") 'next-line)
(global-set-key (kbd "C-l") 'previous-line)
(global-set-key (kbd "C-ö") 'forward-char)
(global-set-key (kbd "C-;") 'forward-char)

(global-set-key (kbd "M-j") 'backward-word)
(global-set-key (kbd "M-k") 'move-beginning-of-line)
(global-set-key (kbd "M-l") 'move-end-of-line)
(global-set-key (kbd "M-ö") 'forward-word)
(global-set-key (kbd "C-ä") 'delete-backward-char)
(global-set-key (kbd "M-ä") 'backward-kill-word)

(define-key input-decode-map "\C-i" [C-i])

(add-to-list 'after-make-frame-functions 'jeb/ci)
(global-set-key (kbd "<C-i>") 'recenter-top-bottom)
(global-set-key (kbd "C-d") 'delete-forward-char)

;; Buffer management
(global-set-key (kbd "ĸ") 'kill-this-buffer)
(global-set-key (kbd "ł") (lambda () (interactive) (switch-to-buffer nil)))
(setq auto-save-timeout 1)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq make-backup-files nil)

;; window management
(global-set-key (kbd "C-(") (lambda () (interactive) (split-window-right) (other-window 1)))
(global-set-key (kbd "C-)") (lambda () (interactive) (split-window-below) (other-window 1)))
(global-set-key (kbd "ð") 'delete-window)
(global-set-key (kbd "ŧ") 'tab-new)
(global-set-key (kbd "¢") 'tab-close)

;; hotkeys
(global-set-key (kbd "←") 'undo)
(global-set-key (kbd "«") 'undo-fu-only-redo)
(global-set-key (kbd "ħ") 'replace-string)
(global-set-key (kbd "C-x C-r") 'rectangle-mark-mode)
(global-set-key (kbd "C-x C-l") 'string-rectangle)
(global-set-key (kbd "»") 'whitespace-cleanup)
(global-set-key (kbd "TAB") 'self-insert-command)
(electric-pair-mode t)
(global-set-key (kbd "C-d") 'delete-forward-char)

;; indentation and tabs
(add-hook 'yaml-mode-hook (lambda () (local-set-key (kbd "TAB") 'indent-for-tab-command)))
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "TAB") 'indent-for-tab-command)))
(add-hook 'yaml-mode-hook (lambda () (local-set-key (kbd "TAB") 'indent-for-tab-command)))
(add-hook 'lsp-mode-hook (lambda () (local-set-key (kbd "TAB") 'indent-for-tab-command)))
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)

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
  (dired "/home/jeb/projects"))

(defun jeb/write-date()
  (interactive)
  (insert (shell-command-to-string "date +\"%Y %M %d\"")))

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

(put 'upcase-region 'disabled nil)
