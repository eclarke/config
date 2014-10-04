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
(condition-case ()
    (load "howdoi")
  (error nil))
