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

;; (setq package-check-signature nil) ;; org-roam org-mode for some reason fail to download with signature checking on

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(require 'package)

(use-package magit
  :bind ("ŋ" . magit)
  :config (setq magit-list-refs-sortby "-creatordate")
  :ensure t)
(setq magit-diff-refine-hunk 'all)

(use-package ace-window
  :bind ("C-o" . ace-window)
  :config (setq aw-keys '(?j ?k ?l ?ö ?a ?s ?d ?f ?g ?h))
  :ensure t)

(use-package vterm
  :bind (("ß" . new_vterm)
         ("æ" . vterm-copy-mode)
         :map vterm-mode-map
         ("C-o" . ace-window)
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
  :config (setq vterm-shell "/usr/bin/zsh" vterm-timer-delay 0.01)
  :ensure t)

;; Emacs has not the feature to change cursor color per buffer. Therefore I would need
;; to check every time the buffer mode when I want to have this.
;; (add-hook 'vterm-copy-mode-hook (lambda () (set-cursor-color "#ffaf00")))
;; (add-hook 'vterm-mode-hook (lambda () (set-cursor-color "#ffffff")))

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
      ("C-ö" . helm-execute-persistent-action)
      ("C-;" . helm-execute-persistent-action)
      ("t" . self-insert-command)))
  :config (helm-mode 1)
  (setq helm-move-to-line-cycle-in-source nil)
  :ensure t)
(helm-mode 1)

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
         (go-mode . lsp)
         (bicep-mode . lsp)
         (js-mode . lsp)
         (sh-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config(setq lsp-signature-auto-activate nil)
          (setq sh-basic-offset 2)
  (setq lsp-rust-server 'rust-analyzer)
  :bind (("M-," . lsp-find-references)
  ("C-." . lsp-execute-code-action))
  :commands lsp
  :ensure t)

(use-package lsp-ui
  :config(setq lsp-ui-doc-show-with-cursor t)
  :ensure t)

(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))

(add-hook 'go-mode-hook
  (lambda ()
    (setq-default)
    (setq tab-width 2)
    (setq standard-indent 2)
    (setq indent-tabs-mode nil)))


(use-package drag-stuff
  :config(setq drag-stuff-global-mode t)
  :bind(("M-p" . drag-stuff-up)
         ("M-n" . drag-stuff-down))
  :ensure t)

(setq lsp-ansible-add-on? t)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package just-mode
  :config (setq just-indent-offset 2)
  :ensure t)

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-character-face-perc 80)
  :ensure t)

;; frame options
(add-to-list 'default-frame-alist '(alpha-background . 91))

(use-package spacemacs-theme
  :config (load-theme 'spacemacs-dark t)
  :ensure t)

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  :bind
  ("«" . undo-fu-only-redo)
  :ensure t)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  (:map company-active-map
        ("C-k" . company-select-next)
        ("C-l" . company-select-previous))
  :ensure t)


;;   :custom
;;   (corfu-cycle t)
;;   (corfu-auto t)
;;   (corfu-separator ?\s)
;;   (corfu-quit-at-boundary t)
;;   (corfu-quit-no-match t)
;;   (corfu-preselect 'directory)
;;   (corfu-on-exact-match nil)
;;   (corfu-scroll-margin 5)
;;   (completion-styles '(basic))
;;   (corfu-auto-prefix 1)
;;   :bind
;;   (:map corfu-map
;;         ("RET" . nil))
;;   :init (global-corfu-mode)
;;   (use-package cape
;;     :init
;;     ;; Add to the global default value of `completion-at-point-functions' which is
;;     ;; used by `completion-at-point'.  The order of the functions matters, the
;;     ;; first function returning a result wins.  Note that the list of buffer-local
;;     ;; completion functions takes precedence over the global list.
;;     (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;     (add-to-list 'completion-at-point-functions #'cape-file)
;;     (add-to-list 'completion-at-point-functions #'cape-elisp-block)
;;     ;;(add-to-list 'completion-at-point-functions #'cape-history)
;;     ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
;;     ;;(add-to-list 'completion-at-point-functions #'cape-tex)
;;     ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
;;     ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
;;     ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
;;     ;;(add-to-list 'completion-at-point-functions #'cape-dict)
;;     ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
;;     ;;(add-to-list 'completion-at-point-functions #'cape-line)
;;     :ensure t)
;;   :ensure t)

(use-package justl
  :ensure t)

(use-package visual-regexp
  :bind (("ħ" . vr/replace)
         )
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package nerd-icons
  :ensure t)


(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :bind (
         (:map org-mode-map
               ("C-j" . backward-char)
               ("C-l" . previous-line)))
  :ensure t)

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/projects/roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n o" . org-open-at-point)
         (:map org-mode-map
               ("C-j" . backward-char)
               ("C-l" . previous-line)))
  :config
  (org-roam-db-autosync-mode)
  :hook (helm-mode-org-roam-node-find . (lambda () (setq show-trailing-whitespace nil)))
  :ensure t)

(setq org-emphasis-alist
  '(("*" (bold :foreground "Orange" ))
    ("/" italic)
    ("_" underline)
    ("=" (:background "maroon" :foreground "white"))
    ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
    ("+" (:strike-through t))))


(use-package yasnippet
  :init (yas-global-mode)
  :ensure t)

(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :ensure t)
(setq ispell-program-name "/usr/bin/aspell")

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package adoc-mode
  :hook (adoc-mode . flyspell-mode)
  :ensure t)

(use-package exec-path-from-shell
  :ensure t)

(use-package origami
  :hook ((org-mode . origami-mode)
         (lsp-mode . origami-mode))
  :bind (:map origami-mode-map
              ("ſ" . origami-forward-toggle-node))
  :ensure t)

(use-package kotlin-mode
  :ensure t)

(defun jeb/forge-pull-dispatch()
  (interactive)
  (forge-pull)
  (forge-dispatch)
)

(use-package forge
  :bind(:map magit-mode-map
         ("N" . jeb/forge-pull-dispatch))
  :config (setq auth-sources '("~/.authinfo"))
  :ensure t)

(add-hook 'kotlin-mode-hook
          (lambda ()
            (c-set-offset 'arglist-intro '+)
            (c-set-offset 'arglist-close 0)))

(setq tramp-default-method "ssh")

(defun jeb/ssh-tramp (arg)
  "Easier ssh tramp access by reading ssh config"
  (interactive
   (list
    (completing-read "Select host: "
                     (list (shell-command-to-string "cat ~/.ssh/config | grep Host\ | cut -d ' ' -f 2")))))
  (find-file (concat "/ssh:" arg ":~/" ))
  )

(defun jeb/ssh-tramp-to-sudo (arg)
  "Easier ssh tramp access by reading ssh config and switch user to root"
  (interactive
   (list
    (completing-read "Select host: "
                     (list (shell-command-to-string "cat ~/.ssh/config | grep Host\ | cut -d ' ' -f 2")))))
  (find-file (concat "/ssh:" arg "|sudo::~/" ))
  )

(defun jeb/docker-tramp (arg)
  "Easier ssh tramp access by reading ssh config"
  (interactive
   (list
    (completing-read "Select container: "
                     (list (shell-command-to-string "docker ps --format '{{.Names}}'")))))
  (find-file (concat "/docker:root@" arg ":~/" ))
)

(defun jeb/open-projects-tab-in-background ()
  "Open a new tab in the '~/projects' directory without focusing on it."
  (interactive)
  (let ((current-tab (tab-bar--current-tab-index)))
    (tab-new)
    (find-file "~/projects/")
    (tab-bar-select-tab (1+ current-tab))))

(defun jeb/open-link-in-edge (input)
  "Open INPUT in Microsoft Edge on Windows with focus.
- If INPUT is a URL, open it directly.
- If INPUT looks like a domain or domain + path, prepend https://
- Otherwise, perform a Google search."
  (interactive (list (read-string "Enter URL or search: " (thing-at-point 'url))))
  (let* ((input (string-trim input))
         (url
          (cond
           ;; Already a proper URL with scheme
           ((string-match-p "\\`https?://" input)
            input)
           ;; Looks like domain or domain + path (e.g. github.com/azure)
           ((string-match-p
             "\\`[a-zA-Z0-9.-]+\\.[a-zA-Z]+\\(/[^\s]*\\)?\\'" input)
            (concat "https://" input))
           ;; Otherwise, treat as search
           (t
            (concat "https://www.google.com/search?q=" (url-hexify-string input)))))
         (powershell-cmd (format "Start-Process 'msedge.exe' '%s'" url)))
    (message "Opening in Edge: %s" url)
    (call-process "powershell.exe" nil 0 nil "-Command" powershell-cmd)))

(setq browse-url-browser-function #'jeb/open-link-in-edge)

(defun jeb/open-link-browser ()
  "Open a browser with the selected link, or prompt if nothing is selected."
  (interactive)
  (let ((url
         (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Enter URL: " (thing-at-point 'url)))))
    (message "Opening: %s" url)
    (jeb/open-link-in-edge url)))

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

(delete-selection-mode t)
(setq initial-buffer-choice (find-file "~/projects/roam/20231208144318-todo.org"))

(jeb/open-projects-tab-in-background)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; dired
(use-package dired
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-Alh --group-directories-first"))
  :bind(("C-x C-j" . dired-jump)
        :map dired-mode-map
        ("C-o" . ace-window)
        ("j" . helm-find-files))
  :ensure nil)

(use-package dired-git-info
  :load-path "~/projects/dired-git-info")

(add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable)



;; QOL
(fset 'yes-or-no-p 'y-or-n-p)  ;; Ask for y/n instead of yes/no

(set-face-attribute 'default nil :height 140)

;; Ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)



;; markdown
(setq-default fill-column 120)
(add-hook 'markdown-mode-hook #'auto-fill-mode)
(add-hook 'markdown-mode-hook #'flyspell-mode)

(delete-selection-mode 1)

(add-hook 'js-json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq tab-width 2)
            (setq js-indent-level 2)))
(add-hook 'js-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq tab-width 2)
            (setq js-indent-level 2)))

(setq css-indent-offset 2)

(setq exec-path (append exec-path '("~/.nvm/versions/node/v21.7.1/bin")))
(executable-find "npm")

(defun delete-word-backward (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun delete-word-forward (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))


(use-package swiper-helm
  :bind(("C-f" . swiper-helm))
  :ensure t)

(exec-path-from-shell-initialize)

;; navigation
(setq-default cursor-type 'bar)


(global-set-key (kbd "”") 'jeb/open-link-browser)
(global-set-key (kbd "¶") 'helm-show-kill-ring)
(global-set-key (kbd "đ") 'projectile-grep)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-j") 'backward-char)
(global-set-key (kbd "C-k") 'next-line)
(global-set-key (kbd "C-l") 'previous-line)
(global-set-key (kbd "C-ö") 'forward-char)
(global-set-key (kbd "C-;") 'forward-char)

(global-set-key (kbd "M-p") 'drag-stuff-up)
(global-set-key (kbd "M-n") 'drag-stuff-down)

(global-set-key (kbd "M-j") 'backward-word)
(global-set-key (kbd "M-k") 'move-beginning-of-line)
(global-set-key (kbd "M-l") 'move-end-of-line)
(global-set-key (kbd "M-ö") 'forward-word)
(global-set-key (kbd "C-ä") 'delete-backward-char)
(global-set-key (kbd "M-ä") 'backward-kill-word)
(global-set-key (kbd "M-d") 'delete-word-forward)
(global-set-key (kbd "C-<backspace>") 'delete-word-backward)

(define-key input-decode-map "\C-i" [C-i])

(add-to-list 'after-make-frame-functions 'jeb/ci)
(global-set-key (kbd "<C-i>") 'recenter-top-bottom)
(global-set-key (kbd "C-d") 'delete-forward-char)

;; Buffer management
(global-set-key (kbd "ĸ") 'kill-this-buffer)
(global-set-key (kbd "ł") (lambda () (interactive) (switch-to-buffer nil)))
(global-set-key (kbd "Ł") 'switch-to-buffer)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq create-lockfiles nil)
(setq auto-save-interval 0)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-visited-mode t)
 '(package-selected-packages
   '(company adoc-mode swiper-helm dired-preview helm-lsp justl dired-git-info git-info rust-mode lsp-treemacs lsp-mode spacemacs-theme idle-highlight-mode projectile helm which-key rainbow-delimiters vterm magit use-package popup async))
 '(projectile-globally-ignored-directories
   '("^\\.idea$" "^\\.vscode$" "^\\.ensime_cache$" "^\\.eunit$" "^\\.git$" "^\\.hg$" "^\\.fslckout$" "^_FOSSIL_$" "^\\.bzr$" "^_darcs$" "^\\.pijul$" "^\\.tox$" "^\\.svn$" "^\\.stack-work$" "^\\.ccls-cache$" "^\\.cache$" "^\\.clangd$" "target")))

(setq auto-save-visited-interval 1)


;; window management
(global-set-key (kbd "C-(") (lambda () (interactive) (split-window-right) (other-window 1)))
(global-set-key (kbd "C-)") (lambda () (interactive) (split-window-below) (other-window 1) (new_vterm)))
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

(defun jeb/print-date()
  (interactive)
  (insert (shell-command-to-string "date +\"%Y %m %d\"")))

(defun jeb/localhost (port &optional secure-answer)
  (interactive "sPort: \nsSecure? (y/n)")
  (setq my-secure nil)
  (setq my-secure (cl-equalp secure-answer "y"))
  (browse-url (concat"http" (if my-secure "s") "://localhost:" port)))

(setq browse-url-browser-function 'browse-url-chrome)
(setq browse-url-chrome-program "/mnt/c/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(set-face-bold-p 'bold nil)
