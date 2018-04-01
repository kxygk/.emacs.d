
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(setq
 load-path
 (cons
  (concat
   (file-name-directory load-file-name)
  "use-package")
  load-path))
(require 'use-package)

(setq c-basic-offset 4)

(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(use-package moe-theme
  :ensure t
  :config
  (moe-light)
  (set-face-attribute 'default nil :background "#ffffff" :foreground "#5f5f5f"))

(use-package magit
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(setq org-confirm-babel-evaluate nil ;; don't prompt for confirmation about executing a block
      org-src-tab-acts-natively t
      org-use-sub-superscripts '{}
      org-src-fontify-natively t)
(use-package htmlize
  :ensure t)

(use-package gnuplot
    :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)))

(gnuplot-inline-display-mode)

(use-package rtags
  :config
  (rtags-enable-standard-keybindings)
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (rtags-start-process-unless-running))

(use-package company
  :config
  (push 'company-rtags company-backends)
  (global-company-mode)
  (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete)))

(use-package cmake-ide
  :config
  (cmake-ide-setup))

(use-package cider)
