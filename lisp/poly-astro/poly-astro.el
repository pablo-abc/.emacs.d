;;; poly-astro.el --- a polymode for astro files

;;; commentary:

;;; Requires polymode
;;; Requires web-mode

;;; code:
(require 'polymode)
(require 'web-mode)

(define-hostmode poly-astro-hostmode :mode 'web-mode)
(define-innermode poly-astro-fm-innermode
  :mode 'js-mode
  :head-matcher "\\`[ \t\n]*---\n"
  :tail-matcher "^---\n"
  :head-mode 'host
  :tail-mode 'host)
(define-auto-innermode poly-astro-style-tag-lang-innermode
  :head-matcher "<[[:space:]]*style[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*[[:alpha:]]+[[:space:]]*[\"'][[:space:]]*>\n"
  :tail-matcher "</[[:space:]]*style[[:space:]]*[[:space:]]*>"
  :mode-matcher (cons  "<[[:space:]]*style[[:space:]]*lang=[[:space:]]*[\"'][[:space:]]*\\([[:alpha:]]+\\)[[:space:]]*[\"'][[:space:]]*>" 1)
  :head-mode 'host
  :tail-mode 'host
  :body-indent-offset 2)
(define-innermode poly-astro-style-innermode
  :mode 'css-mode
  :head-matcher "<[[:space:]]*style[[:space:]]*[[:space:]]*>\n"
  :tail-matcher "</[[:space:]]*style[[:space:]]*[[:space:]]*>"
  :head-mode 'host
  :tail-mode 'host
  :body-indent-offset 2)
(define-polymode poly-astro
  :hostmode 'poly-astro-hostmode
  :innermodes '(poly-astro-fm-innermode
                poly-astro-style-tag-lang-innermode
                poly-astro-style-innermode))

(provide 'poly-astro)
;;; poly-astro.el ends here
