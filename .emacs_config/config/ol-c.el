
(require 'cc-cmds)
(require 'cc-mode)

(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))

(provide 'ol-c)
