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

(defconst dokuwiki-mode-font-lock-keywords-nowiki-tag '(("\\(<nowiki>\\)\\(\\([\n]\\|.\\)+?\\)\\(</nowiki>\\)"
                                                         (1 'font-lock-keyword-face)
                                                         (2 'font-lock-preprocessor-face)
                                                         (3 'font-lock-preprocessor-face)
                                                         (4 'font-lock-keyword-face))))

(defconst dokuwiki-mode-font-lock-keywords-nowiki-percent '(("\\(%%\\)\\(\\([\n]\\|.\\)+?\\)\\(%%\\)"
                                                             (1 'font-lock-keyword-face)
                                                             (2 'font-lock-preprocessor-face)
                                                             (3 'font-lock-preprocessor-face)
                                                             (4 'font-lock-keyword-face))))


(defconst dokuwiki-mode-font-lock-keywords-code-tag '(("\\(<code.*?>\\)\\(\\([\n]\\|.\\)+?\\)\\(</code>\\)"
                                                       (1 'font-lock-keyword-face)
                                                       (2 'font-lock-preprocessor-face)
                                                       (3 'font-lock-preprocessor-face)
                                                       (4 'font-lock-keyword-face))))

(defconst dokuwiki-mode-font-lock-keywords-file-tag '(("\\(<file.*?>\\)\\(\\([\n]\\|.\\)+?\\)\\(</file>\\)"
                                                       (1 'font-lock-keyword-face)
                                                       (2 'font-lock-preprocessor-face)
                                                       (3 'font-lock-preprocessor-face)
                                                       (4 'font-lock-keyword-face))))

(defconst dokuwiki-mode-font-lock-keywords-html-tag '(("\\(<html.*?>\\)\\(\\([\n]\\|.\\)+?\\)\\(</html>\\)"
                                                       (1 'font-lock-keyword-face)
                                                       (2 'font-lock-preprocessor-face)
                                                       (3 'font-lock-preprocessor-face)
                                                       (4 'font-lock-keyword-face))))

(defconst dokuwiki-mode-font-lock-keywords-php-tag '(("\\(<php.*?>\\)\\(\\([\n]\\|.\\)+?\\)\\(</php>\\)"
                                                       (1 'font-lock-keyword-face)
                                                       (2 'font-lock-preprocessor-face)
                                                       (3 'font-lock-preprocessor-face)
                                                       (4 'font-lock-keyword-face))))

(defconst dokuwiki-mode-font-lock-keywords-code-ident (list (cons "^ +[^* -].*$" font-lock-preprocessor-face)))

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

(defconst dokuwiki-mode-font-lock-keywords-email-link '(("\\(<\\)\\(.*\\)\\(>\\)"
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

(defconst dokuwiki-mode-font-lock-keywords-line (list (cons "----+" font-lock-keyword-face)))

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
    (list (cons (regexp-opt (append smilies entities abbrs) nil) font-lock-constant-face)))
  "DokuWiki 'constants'")



(defconst dokuwiki-mode-font-lock-keywords-quote (list (cons "^>+" font-lock-keyword-face)))

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

(defvar dokuwiki-mode-font-lock-keywords (append
                                          dokuwiki-mode-font-lock-keywords-code-ident
                                          dokuwiki-mode-font-lock-keywords-nowiki-percent
                                          dokuwiki-mode-font-lock-keywords-nowiki-tag
                                          dokuwiki-mode-font-lock-keywords-code-tag
                                          dokuwiki-mode-font-lock-keywords-file-tag
                                          dokuwiki-mode-font-lock-keywords-php-tag
                                          dokuwiki-mode-font-lock-keywords-html-tag
                                          dokuwiki-mode-font-lock-keywords-heading1
                                          dokuwiki-mode-font-lock-keywords-heading2
                                          dokuwiki-mode-font-lock-keywords-heading3
                                          dokuwiki-mode-font-lock-keywords-heading4
                                          dokuwiki-mode-font-lock-keywords-heading5
                                          dokuwiki-mode-font-lock-keywords-named-link
                                          dokuwiki-mode-font-lock-keywords-image-link
                                          dokuwiki-mode-font-lock-keywords-link
                                          dokuwiki-mode-font-lock-keywords-bare-link
                                          dokuwiki-mode-font-lock-keywords-bold
                                          dokuwiki-mode-font-lock-keywords-italic
                                          dokuwiki-mode-font-lock-keywords-underlined
                                          dokuwiki-mode-font-lock-keywords-strike-through
                                          dokuwiki-mode-font-lock-keywords-line
                                          dokuwiki-mode-font-lock-keywords-quote
                                          dokuwiki-mode-font-lock-constants
                                          dokuwiki-mode-font-lock-keywords-other
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
  
