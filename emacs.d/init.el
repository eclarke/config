;;-----------------------------------------------------------------------;;
;;  ecl general config file for emacs >=24
;;-----------------------------------------------------------------------;;

;;-----------------------------------------------------------------------;;
;;  General options
;;-----------------------------------------------------------------------;;
(setq-default fill-column 80)
(column-number-mode 1)
(setq make-backup-files t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))
(setq inhibit-startup-message t)

;; Unnecessary UI --> gone
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; No tabs
(setq tab-width 2
      indent-tabs-mode nil)
;; Key bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; Ido stuff
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers)

;; Temp files --> gone
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;;-----------------------------------------------------------------------;;
;;  package repos
;;-----------------------------------------------------------------------;;
(require 'package)
;; Add melpa repository to archives
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;; Initialize packages
(package-initialize)

;;-----------------------------------------------------------------------;;
;;  el-get
;;-----------------------------------------------------------------------;;
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;;-----------------------------------------------------------------------;;
;;  pycheckers
;;-----------------------------------------------------------------------;;
(require 'flymake)
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))
;; To avoid having to mouse hover for the error message, these functions 
;; make flymake error messages appear in the minibuffer
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (require 'cl)
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
      (let ((err (car (second elem))))
        (message "%s" (flymake-ler-text err)))))))
(add-hook 'post-command-hook 'show-fly-err-at-point)

;;-----------------------------------------------------------------------;;
;;  External dependencies
;;-----------------------------------------------------------------------;;
(ignore-errors
  (load "~/.config/emacs.d/howdoi")
  (load "~/.config/emacs.d/lab-notebook"))

;;-----------------------------------------------------------------------;;
;;  Company mode
;;-----------------------------------------------------------------------;;
;; Enable company globally for all mode
(global-company-mode)

;; Reduce the time after which the company auto completion popup opens
(setq company-idle-delay 0.2)

;; Reduce the number of characters before company kicks in
(setq company-minimum-prefix-length 1)

;;-----------------------------------------------------------------------;;
;;  Rust IDE stuff
;;-----------------------------------------------------------------------;;


;;-----------------------------------------------------------------------;;
;;  Emacs-provided stuff
;;-----------------------------------------------------------------------;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
