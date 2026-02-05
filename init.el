;; --- UI & Performance ---
(if (getenv "APPDIR") (setenv "LD_LIBRARY_PATH" nil))
(setq-default bidi-paragraph-direction 'left-to-right
              indent-tabs-mode nil
              fill-column 80)
(when (version<= "27.1" emacs-version) (setq bidi-inhibit-bpa t))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(global-visual-line-mode t)
(desktop-save-mode 1)

(setq display-buffer-base-action '(display-buffer-same-window)
      display-buffer-alist '(("\\`\\*e?shell" display-buffer-use-some-window))
      image-animate-loop t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      scroll-step 1)

;; --- Package Management ---
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; --- Core Packages ---
(use-package exec-path-from-shell
  :if (not (memq system-type '(ms-dos windows-nt cygwin)))
  :config (exec-path-from-shell-initialize))

(use-package magit :bind ("<f2>" . magit-status))

(use-package projectile
  :config (projectile-mode +1)
  :bind-keymap ("s-p" . projectile-command-map)
               ("C-c p" . projectile-command-map)
  :init
  (setq projectile-completion-system 'ivy
        projectile-use-git-grep t
        projectile-use-native-indexing t
        projectile-git-command "/usr/bin/git ls-files -zc"))

(use-package which-key :config (which-key-mode))
(use-package git-gutter :config (global-git-gutter-mode +1))
(use-package so-long :config (global-so-long-mode 1))
(use-package htmlize)
(use-package gnuplot)
(use-package csv-mode)
(use-package adoc-mode :mode "\\.adoc\\'")

;; --- Flycheck & clj-kondo ---
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config (set-face-attribute 'flycheck-error nil :underline '(:style wave :color "#E6005A")))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  ;; 1. Original Flycheck Integration
  (require 'flycheck-clj-kondo))

;;(use-package flycheck
;  :ensure t
;  :init (global-flycheck-mode)
;  :config
;  ;; 1. Define the checker with project-root awareness
;  (flycheck-define-checker clojure-kondo
;    "A Clojure syntax checker using clj-kondo."
;    :command ("clj-kondo" "--lint" source)
;    :error-patterns
;    ((warning line-start (file-name) ":" line ":" column ": warning: " (message) line-end)
;     (error line-start (file-name) ":" line ":" column ": error: " (message) line-end))
;    :modes (clojure-mode clojurescript-mode clojurec-mode)
;    ;; This line ensures clj-kondo is standing in the right place
;    :working-directory (lambda (checker) (clojure-project-dir)))
;
;  ;; 2. Force it into the list of active checkers
;  (add-to-list 'flycheck-checkers 'clojure-kondo)
;  
;  ;; 3. Make the errors look right on your OLED
;;  (set-face-attribute 'flycheck-error nil :underline '(:style wave :color "#E6005A")))

;; --- Semantic Linefeed Logic ---
;; not yet working
;; See https://sachachua.com/blog/2025/09/emacs-cycle-through-different-paragraph-formats-all-on-one-line-wrapped-max-one-sentence-per-line-one-sentence-per-line/
;; Will be available in later version of Emacs
(global-set-key (kbd "<C-tab>") 'fill-paragraph-semlf)

;; --- Org Mode ---
(use-package org
  :hook (org-mode . (lambda ()
                      (local-set-key (kbd "C-b") 'switch-to-buffer)
                      (local-set-key (kbd "C-k") 'kill-line)
                      (local-set-key (kbd "C-q") 'org-ctrl-c-ctrl-c)
                      (local-set-key (kbd "C-e") 'org-ctrl-c-ctrl-c)
                      (local-set-key (kbd "C-M-s") 'org-insert-structure-template)
                      (local-set-key (kbd "<f10>") 'org-babel-execute-buffer)
                      (local-set-key (kbd "<f11>") 'org-edit-special)))
  :config
  (require 'org-tempo)
  (setq org-confirm-babel-evaluate nil
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-use-sub-superscripts '{}
        org-src-fontify-natively t
        org-export-backends '(ascii html latex md odt)
        org-cycle-emulate-tab 'white
        org-export-with-timestamps nil
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
        org-src-window-setup 'current-window)
  (org-babel-do-load-languages
   'org-babel-load-languages '((gnuplot . t) (octave . t) (clojure . t))))

;; --- Clojure ---
(use-package cider
  :init (setq org-babel-clojure-backend 'cider)
  :config
  (setq cider-save-file-on-load t
        cider-show-error-buffer t
        clojure-indent-style 'align-arguments
        clojure-align-forms-automatically t
        ob-clojure-literate-auto-jackin-p t)
  :hook (clojure-mode . (lambda ()
                          (flycheck-mode 1)
                          (local-set-key (kbd "C-d") 'cider-clojuredocs)
                          (local-set-key (kbd "C-e") 'cider-eval-defun-at-point)
                          (local-set-key (kbd "C-q") 'cider-eval-defun-at-point)
                          (local-set-key (kbd "<f10>") 'cider-eval-buffer))))

;; --- Global Keybindings ---
(global-set-key (kbd "C-w") 'kill-current-buffer)
(global-set-key (kbd "<f3>") 'cider-browse-ns)
(global-set-key (kbd "<f4>") 'eshell)
(global-set-key (kbd "<f5>") 'calendar)
(global-set-key (kbd "<f8>") 'cider-inspect)
(global-set-key (kbd "<f9>") 'cider-jack-in)
(global-set-key (kbd "<f10>") 'cider-eval-buffer)
(global-set-key (kbd "M-k") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-q") 'eval-last-sexp)
(global-set-key (kbd "C-e") 'eval-last-sexp)
(global-set-key (kbd "C-d") 'cider-doc)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-l") 'comment-region)
(global-set-key (kbd "C-S-l") 'uncomment-region)
(global-set-key (kbd "C-S-w") 'delete-window)
(global-set-key (kbd "C-`") 'execute-extended-command)

(cua-mode 1)
(setq cua-prefix-override-inhibit-delay 0.01)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

;; --- Monochrome Syntax & OLED Red ---
(set-face-attribute 'default nil :background "white" :foreground "black" :font "Unifont 12")
(set-face-attribute 'region nil :background "lightgray")

;; If you want the specific Black and White Blobs
(let ((font (font-spec :font "Noto Emoji")))
  (set-fontset-font t 'emoji font nil 'prepend)
  (set-fontset-font t 'symbol font nil 'prepend))
(set-fontset-font t 'emoji (font-spec :family "Noto Emoji") nil 'prepend)

;; --- UI & Syntax Colors ---

(let ((my-red "#E6005A")
      (string-bg "#efefef"))

  ;; 1. Normal Strings: Black text with a light grey background
  (set-face-attribute 'font-lock-string-face nil 
                      :foreground "black" 
                      :background string-bg 
                      :extend t)

  ;; 2. Docstrings: Red text, NO background
  ;; We set :inherit nil to ensure it doesn't pick up the string-bg
  (set-face-attribute 'font-lock-doc-face nil 
                      :foreground my-red 
                      :background 'unspecified 
                      :inherit nil)

  ;; 3. Comments: Red text
  (set-face-attribute 'font-lock-comment-face nil :foreground my-red :background 'unspecified)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground my-red :background 'unspecified)

  ;; 4. Reset other syntax to black
  (dolist (face '(font-lock-builtin-face
                  font-lock-constant-face
                  font-lock-function-name-face
                  font-lock-keyword-face
                  font-lock-type-face
                  font-lock-variable-name-face))
    (set-face-foreground face "black"))
  (set-face-foreground 'font-lock-warning-face my-red))

(dolist (face '(cider-repl-stdout-face
                cider-repl-input-face
                cider-result-overlay-face))
  (set-face-attribute face nil :foreground "black"))
;; Specifically for the error/stderr output if you want that black too
;; (Default is usually red or bold)
(set-face-attribute 'cider-repl-stderr-face nil :foreground "#E6005A")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block-begin-line ((t (:overline "#999900" :foreground "#000000" :background "#FFFFcc" :extend t))))
 '(org-block-end-line ((t (:underline "#999900" :foreground "#000000" :background "#FFFFcc" :extend t))))
 '(org-level-1 ((t (:height 1.0 :foreground "white" :background "#777777" :extend t :weight bold))))
 '(org-level-2 ((t (:height 1.0 :weight bold :box t))))
 '(org-level-3 ((t (:height 1.0 :weight bold :underline t))))
 '(org-todo ((t (:height 1.0 :foreground "#ff9999" :background "#777777" :extend t :weight bold)))))

;; --- Backup & Misc ---
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.saves/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

(defun asciidoctor-on-current-buffers-file ()
  (interactive)
  (shell-command (format "asciidoctor %s" (shell-quote-argument (buffer-file-name))))
  (revert-buffer t t t))
(global-set-key (kbd "<f6>") 'asciidoctor-on-current-buffers-file)

(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
