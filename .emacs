;; Theme and fonts
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("71c2935f813f96a0f3835759971d4562854338997b66c0cf1f2e56bb5296a48f" default)))
 '(show-paren-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify))))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Mono" :foundry "unknown" :slant normal :weight normal :height 158 :width normal)))))
 
(load-theme 'zenburn)


;; Remove splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)


;; Add vertical buffer on startup
(split-window-vertically)


;; Open recent files menu entry
(recentf-mode)


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


;; Package managment
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;; Enable global autocomplete
(require 'auto-complete-config)


;; Go programming essentials
(require 'go-autocomplete)
