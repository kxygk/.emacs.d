
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(setq c-basic-offset 4)

(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(require 'moe-theme)
(moe-light)

(setq org-confirm-babel-evaluate nil ;; don't prompt for confirmation about executing a block
      org-src-tab-acts-natively t
      org-use-sub-superscripts '{}
      org-src-fontify-natively t)
(require 'htmlize)

(require 'rtags)
(rtags-enable-standard-keybindings)
(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(rtags-start-process-unless-running)

(require 'company)
(push 'company-rtags company-backends)
(global-company-mode)
(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

(cmake-ide-setup)
