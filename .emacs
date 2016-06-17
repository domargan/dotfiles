(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(custom-set-variables
 '(custom-safe-themes (quote ("71c2935f813f96a0f3835759971d4562854338997b66c0cf1f2e56bb5296a48f" default)))
 '(show-paren-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify))))

(custom-set-faces
 '(default ((t (:family "Mono" :foundry "unknown" :slant normal :weight normal :height 158 :width normal)))))

(load-theme 'zenburn)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(split-window-vertically)

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

; M-x package-list-packages RET

(global-visual-line-mode t)
(global-linum-mode t)

(require 'auto-complete-config)
(require 'go-autocomplete)
