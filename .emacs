;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Package managment ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Auto-install packages by default on all machines
(defvar domargan-packages
  '(
    zenburn-theme
    auto-complete
    go-mode
    go-eldoc
    go-autocomplete)
  "Install all the packages!")

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
(dolist (package domargan-packages)
  (unless (package-installed-p package)
    (package-install package)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Themes, fonts, and layout ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Mono" :foundry "unknown" :slant normal :weight normal :height 158 :width normal)))))
 
(load-theme 'zenburn t)

;; Remove splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Add vertical buffer on startup
;;(split-window-vertically)

;; Indentation settings
(setq-default indent-tabs-mode t)
(setq-default tab-width 8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Other features ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Open recent files menu entry
(recentf-mode)

;; Restore last session on startup
(desktop-save-mode 1)

;; Enable fullscreen toggle on X11 with F11
(defun toggle-fullscreen ()
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))
     
(global-set-key [f11] 'toggle-fullscreen)

;; Enable visual line mode by default
(global-visual-line-mode t)
(global-linum-mode t)

;; Clean whitespaces on save
(add-hook 'before-save-hook 'whitespace-cleanup)

(require 'git)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Go programming essentials ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Credits: http://arenzana.org/2015/Emacs-for-Go/

;; Load Go-specific language syntax
(defun go-mode-setup ()
  (go-eldoc-setup))
(add-hook 'go-mode-hook 'go-mode-setup)

;; Format with fmt before saving
(defun go-mode-setup ()
  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'go-mode-setup)

;; Take care of imports
(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'go-mode-setup)

;; Show function definition when calling godef-jump
(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'go-mode-setup)

;; Custom Compile Command
(defun go-mode-setup ()
  (setq compile-command "go build -v && go test -v && go vet && golint && errcheck")
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'go-mode-setup)

;; Load auto-complete
(ac-config-default)
(require 'auto-complete-config)
(require 'go-autocomplete)

;; Run golint with M-x golint
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)
