(if (getenv "APPDIR") (setenv "LD_LIBRARY_PATH" nil))

(if 'x
    (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(require 'package)
;;(package-initialize)

;; (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
;;                           ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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
;;     :ensure t
;;     :config
;;    (indent-guide-global-mode))

;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'bitmap)
  (add-hook 'clojure-mode-hook 'highlight-indent-guides-mode)
  (highlight-indent-guides-mode))

;; ;; (use-package parinfer
;; ;;   :ensure t
;; ;;   :bind
;; ;;   (("C-," . parinfer-toggle-mode))
;; ;;   :init
;; ;;   (progn
;; ;;     (setq parinfer-extensions
;; ;;           '(defaults       ; should be included.
;; ;;              pretty-parens  ; different paren styles for different modes.
;; ;;              evil           ; If you use Evil.
;; ;;              lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
;; ;;              paredit        ; Introduce some paredit commands.
;; ;;              smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;; ;;              smart-yank))   ; Yank behavior depend on mode.
;; ;;     (add-hook 'clojure-mode-hook #'parinfer-mode)
;; ;;     (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;; ;;     (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;; ;;     (add-hook 'scheme-mode-hook #'parinfer-mode)
;; ;;     (add-hook 'lisp-mode-hook #'parinfer-mode)))

;; (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

 ;; (use-package moe-theme
 ;;    :ensure t
 ;;    :config
 ;;   (moe-light)
 ;;   (set-face-attribute 'default nil :background "#ffffff" :foreground "#5f5f5f"))


;; The scale adjustment is a complete hack..
;; I don't understand how the numbers work here :)
;; (use-package tao-theme
;;   :ensure t
;;   :init
;;   (setq tao-theme-use-boxes nil)
;;   (setq tao-theme-scale-fn (lambda () '(0 0
;;                                           0 0
;;                                           1 1
;;                                           2 3
;;                                           5 9
;;                                           14 23
;;                                           137 128
;;                                           128 139 ;;97 97
;;                                           158 158
;;                                           195 218
;;                                           232 241
;;                                           246 250
;;                                           252 253
;;                                           254 254
;;                                           255 255
;;                                           255 255)))
;;   (setq tao-theme-use-sepia t)
;;   (setq tao-theme-sepia-depth 0)
;;   (setq tao-theme-sepia-saturation 1.1)
;;   :config
;;   (load-theme 'tao-yang t))

;; Just using these font-lock setting without `tao` gives a similar looks..
;; .. but not as good ..

(set-face-foreground 'font-lock-function-name-face "black")
(set-face-background 'font-lock-function-name-face "whitesmoke")
(set-face-foreground 'font-lock-variable-name-face "black")
(set-face-foreground 'font-lock-keyword-face "black")
(set-face-foreground 'font-lock-type-face "black")
(set-face-foreground 'font-lock-constant-face "dimgray") ;;blue
(set-face-foreground 'font-lock-builtin-face "black")
(set-face-foreground 'font-lock-string-face "black")
(setq my-red "darkred") ;;#ff6666
(set-face-foreground 'font-lock-comment-face my-red) ;; doesn't seem to work on the console
(set-face-foreground 'font-lock-comment-delimiter-face my-red)
(set-face-foreground 'font-lock-warning-face my-red)
(set-face-foreground 'font-lock-doc-face my-red)

(set-face-attribute 'region nil :background "gray")

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(fixed-pitch ((t (:family "unifont"))))
;;  '(hl-line ((t (:background "#476047604760" :height 1.0))))
;;  '(org-block ((t (:inherit fixed-pitch :background "#FFFFEA" :extend t))))
;;  '(org-fontify-quote-and-verse-blocks ((t (:inherit fixed-pitch :background "blue" :extend t))))
;;  '(org-level-1 ((t (:inherit header-line :height 1.0))))
;;  '(org-level-2 ((t (:inherit header-line :height 1.0))))
;;  '(org-level-3 ((t (:inherit header-line :height 1.0))))
;;  '(variable-pitch ((t (:family "unifont")))))



;; Original "golden ratio" colors:
;; '(0 0 0 0 1 1 2 3 5 9 14 23 37 60 97 97 158 158 195 218 232 241 246 250 252 253 254 254 255 255 255 255)
      
;; (use-package leuven-theme
;;   :init (setq leuven-scale-outline-headlines nil)
;;  	(setq leuven-scale-org-agenda-structure nil)
;;   :ensure t)


(setq org-fontify-whole-heading-line t)

(set-face-attribute 'default nil :font "unifont 12" )
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "unifont"))))
 '(fixed-pitch ((t ( :family "unifont"))))
 '(org-fontify-whole-heading-line t)
 '(org-block-begin-line
   ((t (:overline "#999900" :foreground "#000000" :background "#FFFFcc" :extend t))))
 '(org-block-end-line
   ((t (:underline "#999900" :foreground "#000000" :background "#FFFFcc" :extend t))))
 '(org-src-block-faces '(("clojure" (:background "#FFFFEA"))
                            ("org" (:background "#E5FFB8"))))
;; '(org-block ((t (:inherit fixed-pitch :background "#FFFFEA" :extend t))))
 '(org-level-1 ((t (:height 1.0 :foreground "white" :background "#777777" :extend t :weight bold))))
 '(org-level-2 ((t (:height 1.0 :weight bold :box t))))
 '(org-level-3 ((t (:height 1.0 :weight bold :underline t))))
 '(org-todo ((t (:height 1.0 :foreground "#ff9999" :background "#777777" :extend t :weight bold))))
 )

(setq org-fontify-quote-and-verse-blocks t) ;; quote blocks are styled as source-code blocks
;; not ideal.. but better than unstyles completely

;; ;; (set-fontset-font t 'han "Zpix")
;; ;; (add-hook 'org-mode-hook 'variable-pitch-mode)
;; ;; (add-hook 'org-mode-hook 'visual-line-mode)

(delete-selection-mode 1)


(setq org-confirm-babel-evaluate nil ;; don't prompt for confirmation about executing a block
      org-src-tab-acts-natively t
      org-use-sub-superscripts '{}
      org-src-fontify-natively t
      org-clock-into-drawer nil
      org-export-backends (quote (ascii html latex md odt))
      org-cycle-emulate-tab 'white
      org-export-with-timestamps nil)
(use-package htmlize
  :ensure t)
(setq org-babel-default-header-args:octave '((:results . "org")
                                             (:session . "*Inferior Octave*")
                                             (:eval . "never-export")
                                             (:exports . "both")))

(setq org-src-window-setup 'current-window)

(setq org-babel-default-header-args:org '((:eval . "never")))

(use-package gnuplot
  :ensure t)


;;(load-file "~/.emacs.d/ob-clj.el")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)
;;   (clj . t)
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
;;(require 'ob-clj)
(setq org-babel-clojure-backend 'cider)
(setq ob-clojure-literate-auto-jackin-p t)
(setq cider-save-file-on-load 't)

(use-package cider
  :ensure t
  :init (setq org-babel-clojure-backend 'cider))

(setq-default cider-show-error-buffer nil)

;; https://docs.cider.mx/cider/config/indentation.html
(setq clojure-indent-style 'align-arguments)
(setq clojure-align-forms-automatically t)

(setq cider-clojure-cli-global-options "-A:reveal -J--add-opens=java.base/jdk.internal.ref=ALL-UNNAMED -J--add-opens=java.base/sun.nio.ch=ALL-UNNAMED -A:server:client:dev -J-XX:-OmitStackTraceInFastThrow")

;; (use-package company
;;   :ensure t
;;   :config
;;   (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
;;   (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
;;   (global-set-key (kbd "TAB") #'company-indent-or-complete-common))
;;   ;(push 'company-rtags company-backends) TODO: FIX this RTags related stuff!
;; (global-company-mode)


;; For Orgmode 9.5
;; directly from: https://github.com/xenodium/company-org-block
;; (use-package company-org-block
;;   :ensure t
;;   :custom
;;   (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
;;   :hook ((org-mode . (lambda ()
;;                        (setq-local company-backends '(company-org-block))
;;                        (company-mode +1)))))


(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2)

(use-package magit
  :ensure t
  :init)
;;  (setq magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'ivy)
(setq projectile-use-git-grep t)

(setq projectile-use-native-indexing t)
;;(setq projectile-enable-caching t)
(setq projectile-git-command "/usr/bin/git ls-files -zc")

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



;  (cua-mode t)
;;(use-package ergoemacs-mode
;;  :ensure t
;;  :config
;;  (setq ergoemacs-theme nil)
;;  (setq ergoemacs-keyboard-layout "us")
;;  (ergoemacs-mode 1))


;;(setq ergoemacs-ignore-prev-global nil)
;; (use-package ergoemacs-mode
;;  :ensure t
;;  :config
;;  (setq ergoemacs-theme nil)
;;  (setq ergoemacs-keyboard-layout "us")
;;  (ergoemacs-theme-option-off '(apps apps-apps apps-punctuation apps-swap more-line fixed-bold-italic fn-keys ))
;;  (setq ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut)
;;  (ergoemacs-mode 1))


;; (add-to-list 'load-path "~/.emacs.d/ergoemacs-mode/")

;; (require 'ergoemacs-mode)
;; (setq ergoemacs-theme "us")
;; (setq ergoemacs-keyboard-layout "us") ;
;; (ergoemacs-theme-option-off '(apps apps-apps apps-punctuation apps-swap more-line fixed-bold-italic fn-keys ))
;; (setq ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut)
;; (ergoemacs-mode 1)

;;(setq ergoemacs-ignore-prev-global nil)


(global-set-key (kbd "<f2>") 'magit-status)
(global-set-key (kbd "<f3>") 'cider-browse-ns)
(global-set-key (kbd "<f4>") 'eshell)
(setq display-buffer-alist '(("\\`\\*e?shell" display-buffer-use-some-window)))
(global-set-key (kbd "<f5>") 'calendar)
(global-set-key (kbd "<f6>") 'org-babel-tangle)
(global-set-key (kbd "<f7>") 'org-html-export-to-html)
(global-set-key (kbd "<f8>") 'cider-inspect)
(global-set-key (kbd "<f9>") 'cider-jack-in)
(global-set-key (kbd "<f10>") 'cider-eval-buffer)
;;(global-set-key (kbd "<f10>") 'cider-xref-fn-refs)
(global-set-key (kbd "<f11>") 'org-edit-special)
;;(define-key org-mode-map "<f11>" 'org-edit-special)

;;(global-unset-key (kbd "C-d"))q
(global-set-key (kbd "M-k") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-q") 'eval-last-sexp)
(global-set-key (kbd "C-e") 'eval-last-sexp)
(global-set-key (kbd "C-d") 'cider-doc)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "C-t") 'other-window)
;;(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "C-;") 'comment-region) ;; doesn't work
(global-set-key (kbd "C-l") 'comment-region)
(global-set-key (kbd "C-S-l") 'uncomment-region)
(global-set-key (kbd "C-S-w") 'delete-window)
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-`") 'execute-extended-command)

;; Pseudo Ergo...
(cua-mode 1)
(setq cua-prefix-override-inhibit-delay 0.01)
;; Emacs 28 will have
;; (global-set-key (kbd "C-z") 'undo-only)
;; (global-set-key (kbd "C-S-z") 'undo-redo)

(global-set-key (kbd "C-z") 'undo)
;; (global-set-key (kbd "C-r") 'redo) ;; Doesn't exist :((
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-s") 'save-buffer)
;; from : https://emacs.stackexchange.com/a/42534/15648
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map [(control f)] 'isearch-repeat-forward)

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

                           
(add-hook 'org-mode-hook
          (lambda ()
            ;; (local-set-key (kbd "S-M-h") 'end-of-line)
            (local-set-key (kbd "C-b") 'switch-to-buffer)
            (local-set-key (kbd "C-k") 'kill-line)
            (local-set-key (kbd "C-q") 'org-ctrl-c-ctrl-c)
            (local-set-key (kbd "C-e") 'org-ctrl-c-ctrl-c)
            (local-set-key (kbd "C-M-s") 'org-insert-structure-template)
	    (local-set-key (kbd "<f10>") 'org-babel-execute-buffer)
            (local-set-key (kbd "<f11>") 'org-edit-special)
;;            (local-set-key [end] 'end-of-line)
            ))

(add-hook 'clojure-mode-hook
          (lambda ()
            (local-set-key (kbd "C-d") 'cider-doc)
            (local-set-key (kbd "C-e") 'cider-eval-defun-at-point)
            (local-set-key
	     (kbd "C-S-e") (kbd "C-u 1 C-c C-v C-f d"))
	    ;;            (local-set-key (kbd "C-i") (kbd C-u C-c C-p))
            (local-set-key (kbd "C-q") 'cider-eval-defun-at-point)
            (local-set-key (kbd "<f10>") 'cider-eval-buffer)
            ;;(local-set-key (kbd "<f11>") 'org-edit-src-exit)
            ))
;; (global-set-key (kbd "<f12>") 'cider-enlighten-mode)

(add-hook 'org-src-mode-hook
          (lambda ()
            (local-set-key (kbd "<f11>") 'org-edit-src-exit)))



(put 'dired-find-alternate-file 'disabled nil)
;;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(cider-inject-dependencies-at-jack-in t)




;;Handling long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(if (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))
(use-package so-long
  :ensure t
  :config (global-so-long-mode 1))

;; Open Orgmode files with all blocks collapsed
;; (add-hook 'org-mode-hook 'org-hide-block-all)

;; (setq-default indent-tabs-mode nil) ;; parinfer doesn't work with tabs :(
;; (use-package parinfer-rust-mode
;;   :hook ((;;emacs-lisp-mode
;;           clojure-mode
;;           scheme-mode
;;           lisp-mode
;;           racket-mode
;;           hy-mode) . parinfer-rust-mode)
;;   :init
;;   (setq parinfer-rust-auto-download t)
;;   (setq parinfer-rust-check-before-enable "immediate")
;;   (setq parinfer-rust-preferred-mode "paren"))


;; (use-package paredit
;;   :ensure t
;;   :init
;;   (add-hook 'clojure-mode-hook 'paredit-mode))


;;(global-font-lock-mode 1)


(global-visual-line-mode t)


(desktop-save-mode 1)





;; MATRIX EFFECT
;; https://github.com/emacsmirror/zone-matrix
; (use-package zone-matrix)

; (add-to-list 'load-path 
;    (file-name-directory load-file-name) "zone-matrix")

;; (setq
;;  load-path
;;  (cons
;;   (concat
;;    (file-name-directory load-file-name)
;;    "zone-matrix")
;;   load-path))
;; (require 'zone-matrix)
;; (require 'zone-matrix-settings)
;; (require 'zone-settings)

;; (use-package zone-matrix)
;; (setq zone-programs [zone-matrix])


;;;;;;



;;
(require 'org-tempo)
(add-to-list 'org-modules 'org-tempo)


;; from: https://stackoverflow.com/a/53469018/7024671
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-preserve-indentation t)


;; (use-package solarized-theme
;;   :ensure t)


;; (setq solarized-high-contrast-mode-line t)
;; (setq solarized-use-variable-pitch nil)
;; (setq solarized-use-more-italic t)
;; (setq solarized-scale-org-headlines nil)
;; (setq solarized-height-minus-1 1.0)
;; (setq solarized-height-plus-1 1.0)
;; (setq solarized-height-plus-2 1.0)
;; (setq solarized-height-plus-3 1.0)
;; (setq solarized-height-plus-4 1.0)

;; (load-theme 'solarized-light t)


;; (use-package ox-reveal
;;   :ensure t)
