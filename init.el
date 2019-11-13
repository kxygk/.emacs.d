
(if (getenv "APPDIR") (setenv "LD_LIBRARY_PATH" nil))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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

;; (use-package moe-theme
;;   :ensure t
;;   :config
;;   (moe-light)
;;   (set-face-attribute 'default nil :background "#ffffff" :foreground "#5f5f5f"))
(use-package leuven-theme
  :init (setq leuven-scale-outline-headlines nil)
        (setq leuven-scale-org-agenda-structure nil)
  :ensure t)

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2)

(use-package magit
  :ensure t
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1))

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

;(setq mouse-wheel-scroll-amount '(0.07))
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) 
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1)

;(setq browse-url-browser-function 'eww-browse-url)

(delete-selection-mode 1)

;  (cua-mode t)

(setq org-confirm-babel-evaluate nil ;; don't prompt for confirmation about executing a block
      org-src-tab-acts-natively t
      org-use-sub-superscripts '{}
      org-src-fontify-natively t
      org-clock-into-drawer nil
      org-export-backends (quote (ascii html latex md odt))
      org-cycle-emulate-tab 'white)
(use-package htmlize
  :ensure t)
;; (setq org-babel-default-header-args:octave '((:results . "org")
;;                                        (:eval . "never-export")
;;                                        (:exports . "both")))

(use-package gnuplot
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)
   (octave . t)
   (clojure . t)))

 (gnuplot-inline-display-mode)

;; (use-package lsp-mode
;;   :ensure t)

;; (use-package emacs-cquery
;;   :commands lsp-cquery-enable
;;   :init (setq cquery-executable "~/Programs/cquery/bin/cquery")
;;   (add-hook 'c-mode-hook #'cquery//enable)
;;   (add-hook 'c++-mode-hook #'cquery//enable)
;;   :ensure t)

(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)
(setq ob-clojure-literate-auto-jackin-p t)

(use-package cider
  :ensure t
  :init (setq org-babel-clojure-backend 'cider))

;; (set-face-attribute 'default nil :font "tewi:pixelsize=11:foundry=lucy:weight=normal:slant=normal:width=normal:spacing=110:scalable=false")
;; (set-frame-font "tewi:pixelsize=11:foundry=lucy:weight=normal:slant=normal:width=normal:spacing=110:scalable=false" nil t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (cider which-key projectile parinfer magit leuven-theme htmlize gnuplot exec-path-from-shell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
