(add-hook 'grok-mode-hook
          #'grok-bullets-mode)

(push (cons "\\.grok" #'grok-mode)
      auto-mode-alist)
