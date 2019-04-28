(if (getenv "APPDIR") (setenv "LD_LIBRARY_PATH" nil))

(require 'package)
(package-initialize)

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(setq
 load-path
 (cons
  (concat
   (file-name-directory load-file-name)
  "use-package")
  load-path))
(require 'use-package)

;; (use-package auto-package-update
;;    :ensure t
;;    :config
;;    (setq auto-package-update-delete-old-versions t
;;          auto-package-update-interval 4)
;;    (auto-package-update-maybe))

(use-package exec-path-from-shell
  :ensure t
  :config
  (unless (memq system-type '(ms-dos windos-nt cygwin))
    (exec-path-from-shell-initialize)))

;  (setq c-basic-offset 4)

;; (use-package indent-guide
;;    :ensure t
;;    :config
;;   (indent-guide-global-mode))

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             evil           ; If you use Evil.
             lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(use-package moe-theme
  :ensure t
  :config
  (moe-light)
  (set-face-attribute 'default nil :background "#ffffff" :foreground "#5f5f5f"))

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2)

(use-package magit
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(setq image-animate-loop t)

;; (use-package symon
;;   :ensure t
;;   :config
;;   (add-to-list 'symon-monitors 'symon-linux-battery-monitor)
;;   (symon-mode))

(use-package which-key
  :ensure t)

(setq org-confirm-babel-evaluate nil ;; don't prompt for confirmation about executing a block
      org-src-tab-acts-natively t
      org-use-sub-superscripts '{}
      org-src-fontify-natively t
      org-clock-into-drawer nil
      org-export-backends (quote (ascii html latex md odt))
      org-cycle-emulate-tab 'white)
(use-package htmlize
  :ensure t)

(use-package gnuplot
    :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)))

(gnuplot-inline-display-mode)

;; (use-package lsp-mode
;;   :ensure t)

;; (use-package emacs-cquery
;;   :commands lsp-cquery-enable
;;   :init (setq cquery-executable "~/Programs/cquery/bin/cquery")
;;   (add-hook 'c-mode-hook #'cquery//enable)
;;   (add-hook 'c++-mode-hook #'cquery//enable)
;;   :ensure t)

;; (use-package ob-clojure-literate
;;   :ensure t
;;   :after org
;;   :init
;;   (setq ob-clojure-literate-auto-jackin-p t)
;;   (add-hook 'org-mode-hook #'ob-clojure-literate-mode))

(use-package cider
  :ensure t
  :init (setq org-babel-clojure-backend 'cider))
