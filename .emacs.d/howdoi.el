;;;###autoload
(defun howdoi (query)
  "Execute a howdoi query and return results in minibuffer"
  (interactive "sHow do I... ")
  (shell-command (format "howdoi -a %s" (shell-quote-argument query))))

