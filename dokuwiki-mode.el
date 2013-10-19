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


(defconst dokuwiki-mode-font-lock-verbatim
  (let (
        (ident "\\(^ +[^* -].*$\\)")
        (percent "\\(%%.*?%%\\)")
        (nowiki "\\(<nowiki>\\(\n\\|.\\)+?</nowiki>\\)")       
        (code "\\(<code.*?>\\(\n\\|.\\)+?</code>\\)")
        (file "\\(<file.*?>\\(\n\\|.\\)+?</file>\\)")
        (php "\\(<php>\\(\n\\|.\\)+?</php>\\)")
        (html "\\(<html>\\(\n\\|.\\)+?</html>\\)")
        (cphp "\\(<PHP>\\(\n\\|.\\)+?</PHP>\\)")
        (chtml "\\(<HTML>\\(\n\\|.\\)+?</HTML>\\)")
        )
    (list (cons (mapconcat 'identity (list ident percent nowiki code file 
                                           php html cphp chtml) "\\|") 
                font-lock-preprocessor-face))))

(defface dokuwiki-face-heading1 '((default (:height 200))) "Level 1 Heading")
(defconst dokuwiki-mode-font-lock-keywords-heading1 '(("^[[:space:]]?\\(=\\{6\\}\\)\\(.*?\\)\\(=\\{6\\}\\)"
                                                       (1 'font-lock-keyword-face)
                                                       (2 'dokuwiki-face-heading1)
                                                       (3 'font-lock-keyword-face))))

(defface dokuwiki-face-heading2 '((default (:height 180))) "Level 2 Heading")
(defconst dokuwiki-mode-font-lock-keywords-heading2 '(("^[[:space:]]?\\(=\\{5\\}\\)\\(.*?\\)\\(=\\{5\\}\\)"
                                                       (1 'font-lock-keyword-face)
                                                       (2 'dokuwiki-face-heading2)
                                                       (3 'font-lock-keyword-face))))

(defface dokuwiki-face-heading3 '((default (:height 150))) "Level 3 Heading")
(defconst dokuwiki-mode-font-lock-keywords-heading3 '(("^[[:space:]]?\\(=\\{4\\}\\)\\(.*?\\)\\(=\\{4\\}\\)"
                                                       (1 'font-lock-keyword-face)
                                                       (2 'dokuwiki-face-heading3)
                                                       (3 'font-lock-keyword-face))))

(defface dokuwiki-face-heading4 '((default (:weight ultra-bold :height 130))) "Level 4 Heading")
(defconst dokuwiki-mode-font-lock-keywords-heading4 '(("^[[:space:]]?\\(=\\{3\\}\\)\\(.*?\\)\\(=\\{3\\}\\)"
                                                       (1 'font-lock-keyword-face)
                                                       (2 'dokuwiki-face-heading4)
                                                       (3 'font-lock-keyword-face))))

(defface dokuwiki-face-heading5 '((default (:weight extra-bold))) "Level 5 Heading")
(defconst dokuwiki-mode-font-lock-keywords-heading5 '(("^[[:space:]]?\\(=\\{2\\}\\)\\(.*?\\)\\(=\\{2\\}\\)"
                                                       (1 'font-lock-keyword-face)
                                                       (2 'dokuwiki-face-heading5)
                                                       (3 'font-lock-keyword-face))))


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

(defconst dokuwiki-mode-font-lock-keywords-email-link '(("\\(<\\)\\(.+@.+\\)\\(>\\)"
                                                   (1 'font-lock-keyword-face)
                                                   (2 'font-lock-string-face)
                                                   (3 'font-lock-keyword-face))))
                                                         

(defface dokuwiki-face-bold '((default (:bold t))) "Bold")
(defconst dokuwiki-mode-font-lock-keywords-bold '(("\\([*][*]\\)\\(.*?\\)\\([*][*]\\)" 
                                                   (1 'font-lock-keyword-face)
                                                   (2 'dokuwiki-face-bold)
                                                   (3 'font-lock-keyword-face))))

(defface dokuwiki-face-italic '((default (:slant italic))) "Italic")
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

(defconst dokuwiki-mode-font-lock-horizontal-line (list (cons "----+" font-lock-keyword-face)))

(defconst dokuwiki-mode-font-lock-constants
  (let ((smilies '("8-)" "8-O" ":-(" ":-)" "=)" ":-/" ":-\\" ":-?" ":-D" ":-P" ":-O" 
                   ":-X" ":-|" ";-)" "^_^" ":?:" ":!:" "LOL" "FIXME" "DELETEME"))
        (entities '("<->" "->" "<-" "<=>" "=>" "<=" ">>" "<<" 
                    "---" "--" "(c)" "(tm)" "(r)" "..."))
        (abbrs '("ACL" "AFAICS" "AFAIK" "AFAIR" "API" "ASAP" "ASCII" "BTW" "CMS" 
                 "CSS" "DNS" "EOF" "EOL" "EOM" "EOT" "FAQ" "FTP" "FOSS" "FLOSS" "FUD" 
                 "GB" "GHz" "GPL" "GUI" "HTML" "IANAL" "IE" "IIRC" "IMHO" "IMO" "IOW" 
                 "IRC" "IRL" "KISS" "LAN" "LGPL" "LOL" "MathML" "MB" "MHz" "MSIE" 
                 "OMG" "OS" "OSS" "OTOH" "PITA" "RFC" "ROTFL" "RTFM" "spec " "TIA" 
                 "TL;DR" "TOC" "URI" "URL" "W3C" "WTF?" "WYSIWYG" "YMMV"))
        (macros '("~~NOTOC~~" "~~NOCACHE~~")))
    (list (cons (regexp-opt (append smilies entities abbrs macros) nil) font-lock-constant-face)))
  "DokuWiki 'constants'")



(defconst dokuwiki-mode-font-lock-quote (list (cons "^>+" font-lock-keyword-face)))

(defconst dokuwiki-mode-font-lock-keywords-other (list (cons (regexp-opt '(
                                                                           "<sub>" "</sub>"
                                                                           "<sup>" "</sup>"
                                                                           "((" "))"
                                                                           "''"
                                                                           " * "
                                                                           " - "
                                                                           "|" "^"
                                                                           ) nil)
                                                             font-lock-keyword-face)))

(defface dokuwiki-face-linebreak '((default (:foreground "red1" :weight bold))) "Forced line break")
(defconst dokuwiki-font-lock-linebreak '(("[\\]\\{2\\}$" 
                                          (0 'dokuwiki-face-linebreak))))

(defvar dokuwiki-mode-font-lock (append
                                 dokuwiki-mode-font-lock-verbatim
                                 dokuwiki-mode-font-lock-keywords-heading1
                                 dokuwiki-mode-font-lock-keywords-heading2
                                 dokuwiki-mode-font-lock-keywords-heading3
                                 dokuwiki-mode-font-lock-keywords-heading4
                                 dokuwiki-mode-font-lock-keywords-heading5
                                 dokuwiki-mode-font-lock-horizontal-line
                                 dokuwiki-mode-font-lock-quote
                                 dokuwiki-mode-font-lock-constants
                                 dokuwiki-mode-font-lock-keywords-named-link
                                 dokuwiki-mode-font-lock-keywords-image-link
                                 dokuwiki-mode-font-lock-keywords-link
                                 dokuwiki-mode-font-lock-keywords-bare-link
                                 dokuwiki-mode-font-lock-keywords-email-link
                                 dokuwiki-mode-font-lock-keywords-bold
                                 dokuwiki-mode-font-lock-keywords-italic
                                 dokuwiki-mode-font-lock-keywords-underlined
                                 dokuwiki-mode-font-lock-keywords-strike-through
                                 dokuwiki-mode-font-lock-keywords-other
                                 dokuwiki-font-lock-linebreak
                                 )
  "Syntax highlighting expressions for dokuwiki-mode")

(defun dokuwiki-ident-line ()
  "Ident current line as DokuWiki markup"
  (interactive)
  (beginning-of-line))


;; taken from http://stackoverflow.com/a/15239704
(defun test-font-lock-extend-region ()
  "Extend the search region to include an entire block of text."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (or (re-search-backward "\n\n" nil t) (point-min))))
      (goto-char font-lock-end)
      (when (re-search-forward "\n\n" nil t)
        (beginning-of-line)
        (setq font-lock-end (point)))
      (setq font-lock-beg found))))


(define-derived-mode dokuwiki-mode fundamental-mode "DokuWiki"
  (interactive)
  (kill-all-local-variables)
  (use-local-map dokuwiki-mode-map)
  (set-syntax-table dokuwiki-mode-syntax-table)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'font-lock-defaults) '(dokuwiki-mode-font-lock t))
  (set (make-local-variable 'ident-line-function) 'dokuwiki-ident-line)
  (setq major-mode 'dokuwiki-mode)
  (run-hooks 'dokuwiki-mode-hook)
  (add-hook 'font-lock-extend-region-functions
            'test-font-lock-extend-region)
  )


(add-to-list 'auto-mode-alist '("\\.dokuwiki\\'" . dokuwiki-mode))
(provide 'dokuwiki-mode)
  
