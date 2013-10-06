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

(defconst dokuwiki-mode-font-lock-keywords-named-link '(("\\(\\[\\[\\)\\([^]]+\\)\\(|{?{?\\)\\(.*?\\)\\(}?}?\\]\\]\\)"
                                                   (1 'font-lock-keyword-face)
                                                   (2 'font-lock-string-face)
                                                   (3 'font-lock-keyword-face)
                                                   (4 'font-lock-string-face)
                                                   (5 'font-lock-keyword-face))))

(defconst dokuwiki-mode-font-lock-keywords-image-link '(("\\({{\\)\\(.*?\\)\\(}}\\)"
                                                         (1 'font-lock-keyword-face)
                                                         (2 'font-lock-string-face)
                                                         (3 'font-lock-keyword-face))))


(defconst dokuwiki-mode-font-lock-keywords-link '(("\\(\\[\\[\\)\\(.*?\\)\\(\\]\\]\\)"
                                                   (1 'font-lock-keyword-face)
                                                   (2 'font-lock-string-face)
                                                   (3 'font-lock-keyword-face))))
                                                      

(defconst dokuwiki-mode-font-lock-keywords-bare-link (list (cons "\\(http://\\)?\w+\\.[[:alpha:]]+\\.[^[:space:]]+"
                                                             font-lock-string-face)))

(defface dokuwiki-face-bold '((default (:bold t))) "Bold")
(defconst dokuwiki-mode-font-lock-keywords-bold '(("\\([*][*]\\)\\(.*?\\)\\([*][*]\\)" 
                                                   (1 'font-lock-keyword-face)
                                                   (2 'dokuwiki-face-bold)
                                                   (3 'font-lock-keyword-face))))

(defface dokuwiki-face-italic '((default (:italic t :slant italic))) "Italic")
(defconst dokuwiki-mode-font-lock-keywords-italic '(("\\(//\\)\\(.*?\\)\\(//\\)"
                                                   (1 'font-lock-keyword-face)
                                                   (2 'dokuwiki-face-italic)
                                                   (3 'font-lock-keyword-face))))

(defface dokuwiki-face-underlined '((default (:underline t))) "Underlined")
(defconst dokuwiki-mode-font-lock-keywords-underlined '(("\\(__\\)\\(.*?\\)\\(__\\)"
                                                   (1 'font-lock-keyword-face)
                                                   (2 'dokuwiki-face-underlined)
                                                   (3 'font-lock-keyword-face))))

(defface dokuwiki-face-strike-through '((default (:strike-through t))) "Strike-through")
(defconst dokuwiki-mode-font-lock-keywords-strike-through '(("\\(<del>\\)\\(.*?\\)\\(</del>\\)"
                                                   (1 'font-lock-keyword-face)
                                                   (2 'dokuwiki-face-strike-through)
                                                   (3 'font-lock-keyword-face))))


(defconst dokuwiki-mode-font-lock-keywords-other (list (cons (regexp-opt '(
                                                                           "<sub>" "</sub>"
                                                                           "<sup>" "</sup>"
                                                                           "((" "))"
                                                                           "''"
                                                                           " * "
                                                                           " - "
                                                                           ) nil)
                                                             font-lock-keyword-face)))

(defconst dokuwiki-mode-font-lock-keywords-line (list (cons "----+" font-lock-keyword-face)))

(defconst dokuwiki-mode-font-lock-keywords-smiley (list (cons (regexp-opt '("8-)"
                                                                            "8-O"
                                                                            ":-("
                                                                            ":-)"
                                                                            "=)"
                                                                            ":-/"
                                                                            ":-\""
                                                                            ":-?"
                                                                            ":-D"
                                                                            ":-P"
                                                                            ":-O"
                                                                            ":-X"
                                                                            ":-|"
                                                                            ";-)"
                                                                            "^_^"
                                                                            ":?:"
                                                                            ":!:"
                                                                            "LOL"
                                                                            "FIXME"
                                                                            "DELETEME") nil)
                                                              font-lock-constant-face)))

(defconst dokuwiki-mode-font-lock-keywords-macro (list (cons (regexp-opt '("~~NOTOC~~"
                                                                           "~~NOCACHE~~") nil)
                                                             font-lock-constant-face)))


(defvar dokuwiki-mode-font-lock-keywords (append 
                                          dokuwiki-mode-font-lock-keywords-named-link
                                          dokuwiki-mode-font-lock-keywords-image-link
                                          dokuwiki-mode-font-lock-keywords-link
                                          dokuwiki-mode-font-lock-keywords-bare-link
                                          dokuwiki-mode-font-lock-keywords-bold
                                          dokuwiki-mode-font-lock-keywords-italic
                                          dokuwiki-mode-font-lock-keywords-underlined
                                          dokuwiki-mode-font-lock-keywords-strike-through
                                          dokuwiki-mode-font-lock-keywords-other
                                          dokuwiki-mode-font-lock-keywords-line
                                          dokuwiki-mode-font-lock-keywords-smiley
                                          dokuwiki-mode-font-lock-keywords-macro
                                          )
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
  
