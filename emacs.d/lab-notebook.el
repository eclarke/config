(defun ensure-in-vc-or-checkin ()
  (interactive)
  (if (file-exists-p (format "%s" (buffer-file-name)))
      (progn (vc-next-action nil) (message "Committed"))
    (ding) (message "File not checked in.")))

(defun export-bibtex ()
  "Exports Papers library using a custom applescript."
  (interactive)
  (message "Exporting papers library...")
  (shell-command "osascript ~/notebook/papers_bibtex_export.scpt"))

(define-minor-mode lab-notebook-mode
  "Toggle lab notebook mode. 
In this mode, org files that are saved are automatically committed by the VC
  system in Emacs. Additionally, Papers library export to bibtex is hooked to
  the <f5> key. 
Future additions may hook a confluence upload or Org export to this mode as well."
  ;; initial value
  nil
  ;; indicator
  " lab-notebook"
  ;; keybindings
  '(("<f5>" . export-bibtex))
  ;; body
  (add-hook
   'org-mode-hook
   (lambda ()
     (add-hook 'after-save-hook 'ensure-in-vc-or-checkin nil 'make-it-local))))
