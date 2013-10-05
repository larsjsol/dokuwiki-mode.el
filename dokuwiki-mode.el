;; -*- coding: utf-8 -*-
;;
;;
;; Lars Jørgen Solberg <supersolberg@gmail.com> 2013
;;

(defvar dokuwiki-mode-hook nil)

(defvar dokuwiki-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for dokuwiki-mode")

(defvar dokuwiki-mode-syntax-table
  (let ((dokuwiki-mode-syntax-table (make-syntax-table)))
    dokuwiki-mode-syntax-table)
  "Syntax table for dokuwiki-mode")

(defvar dokuwiki-mode-font-lock-keywords
  (let ((dokuwiki-mode-font-lock-keywords '()))
    dokuwiki-mode-font-lock-keywords)
  "Syntax highlighting expressions for dokuwiki-mode")

(defun dokuwiki-ident-line ()
  "Ident current line as DokuWiki markup"
  (interactive)
  (beginning-of-line))

(add-to-list 'auto-mode-alist '("\\.dokuwiki\\'" . dokuwiki-mode))

(defun dokuwiki-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map dokuwiki-mode-map)
  (set-syntax-table dokuwiki-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(dokuwiki-mode-font-lock-keywords))
  (set (make-local-variable 'ident-line-function) 'dokuwiki-ident-line)
  (setq major-mode 'dokuwiki-mode)
  (setq mode-name "DokuWiki")
  (run-hooks 'dokuwiki-mode-hook))

(provide 'dokuwiki-mode)
  
