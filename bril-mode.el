;;; bril-mode.el --- Major mode for Bril text format -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/bril-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, bril

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Major mode for Bril (Big Red Intermediate Language) text format files.
;;
;; Supports syntax highlighting for core plus extensions: ssa, memory, floating
;; point, speculative execution, imports, and characters.
;;
;; See https://capra.cs.cornell.edu/bril/ for Bril documentation.
;;
;;; Code:

(require 'smie)

(defgroup bril nil
  "Major mode for editing bril source code."
  :group 'languages
  :prefix "bril-")

(defcustom bril-mode-indent-offset 2
  "Indentation column following opening braces."
  :group 'bril
  :type 'integer)

(defcustom bril-mode-label-offset 0
  "Indentation column for labels."
  :group 'bril
  :type 'integer)

(defface bril-mode-extension-face
  '((t (:inherit font-lock-keyword-face)))
  "Face to highlight extension keywords.")

;; -------------------------------------------------------------------
;;; Indentation

(defconst bril-mode-smie-grammar
  (smie-prec2->grammar
   (smie-precs->prec2
    '((assoc ":"))))
  "Smie grammar for `bril-mode'.")

;; Only return ":" on label lines
(defun bril-mode--smie-forward-token ()
  "Function for `smie-forward-token-function'."
  (let ((tok (smie-default-forward-token)))
    (cond
     ((string= ":" tok) (funcall #'smie-default-forward-token))
     ((string-prefix-p "." tok)
      (save-match-data
        (if (not (looking-at "[ \t]*:")) tok
          (goto-char (match-end 0))
          ":")))
     (t tok))))

;; Only return ":" on label lines
(defun bril-mode--smie-backward-token ()
  "Function for `smie-backward-token-function'."
  (let ((tok (smie-default-backward-token)))
    (if (string= ":" tok)
        (save-match-data
          (if (not (looking-back "\\_<\\.[a-zA-Z0-9.]+" (line-beginning-position)))
              "ignore"
            (goto-char (match-beginning 0))
            ":"))
      tok)))

(defun bril-mode-smie-rules (kind token)
  "Indentation rules for `bril-mode'.
See `smie-rules-function' for description of KIND and TOKEN."
  (pcase (cons kind token)
    (`(:elem       . basic) bril-mode-indent-offset)
    (`(:close-all  . ,_) t)
    (`(:after      . ":") (cons 'column bril-mode-indent-offset))
    (`(:before     . ":") (cons 'column bril-mode-label-offset))
    (`(:list-intro . ,(or ":" "")) t)))

;;; Syntax

(defvar bril-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?% "_" table)
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?# "< " table)
    (modify-syntax-entry ?\n "> " table)
    ;; ptr<type>
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?* "." table)
    table)
  "Syntax table used while in Bril mode.")

;;; Font-locking

(defconst bril-mode-keywords
  '("const"
    "add" "mul" "sub" "div"
    "eq" "lt" "gt" "le" "ge"
    "not" "and" "or"
    "jmp" "br" "ret" "call"
    "id" "print" "nop")
  "Bril keywords for font-locking.")

(defconst bril-mode-types
  '("int" "bool"
    ;; extension types
    "float" "nullptr" "ptr" "char")
  "Bril mode types for font-locking.")

(defconst bril-mode-extension-operations
  '(;; ssa
    "phi"
    ;; memory
    "alloc" "store" "load" "free" "ptradd"
    ;; floating point
    "fadd" "fmul" "fsub" "fdiv"
    "feq" "flt" "fle" "fgt" "fge"
    ;; speculative execution
    "speculate" "guard" "commit"
    ;; imports
    "from" "import" "as"
    ;; character ops
    "ceq" "clt" "cle" "cgt" "cge"
    "char2int" "int2char")
  "Bril memory extension operations for font-locking.")

(defconst bril-mode-font-lock-keywords
  (let ((ident "[_%a-zA-Z][._%a-zA-Z0-9]*"))
    `(;; functions
      (,(concat "@" ident "\\_>") . font-lock-function-name-face)
      ;; variables (including SSA)
      (,(concat "\\(\\_<" ident "\\)\\([ \t]*:\\)")
       (1 font-lock-variable-name-face)
       (2 'font-lock-delimiter-face))
      ;; label
      (,(concat "\\(\\_<\\." ident "\\_>\\)\\([ \t]*:\\)?")
       (1 font-lock-preprocessor-face)
       (2 'font-lock-delimiter-face nil t))
      ;; types
      (,(rx-to-string `(seq symbol-start (or ,@bril-mode-types) symbol-end))
       . font-lock-type-face)
      ;; constants
      (,(rx symbol-start (or "true" "false") symbol-end) . font-lock-constant-face)
      ;; keywords
      (,(rx-to-string `(seq symbol-start (or ,@bril-mode-keywords) symbol-end))
       . font-lock-keyword-face)
      ;; extension keywords
      (,(rx-to-string
         `(seq symbol-start (or ,@bril-mode-extension-operations) symbol-end))
       . 'bril-mode-extension-face)
      ;; operators
      (,(rx symbol-start (or "=") symbol-end) . 'font-lock-operator-face)
      ;; chars
      ("'.'" . font-lock-string-face)
      ("\\('\\)\\(\\\\[0abtnvfr]\\)\\('\\)"
       (1 font-lock-string-face) (2 'font-lock-escape-face)
       (3 font-lock-string-face))
      ;; numbers
      ("\\_<[+-]?\\([0-9]+\\(.[0-9]*\\)?\\|\\.[0-9]+\\)\\([eE][+-]?[0-9]+\\)?\\_>"
       . 'font-lock-number-face)
      ;; delimiters
      (,(rx (or "," ";" ":")) . 'font-lock-delimiter-face)
      ;; brackets
      (,(rx (or "<" ">" "{" "}" "(" ")")) . 'font-lock-bracket-face)
      ;; ARGS
      ("\\(ARGS\\):" (1 'font-lock-escape-face prepend))))
  "Syntax highlighting for Bril.")

;;;###autoload
(define-derived-mode bril-mode prog-mode "Bril"
  "Major mode for editing Bril text format files.

Commands:
\\{bril-mode-map}"
  (setq font-lock-defaults `(bril-mode-font-lock-keywords))
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (smie-setup bril-mode-smie-grammar #'bril-mode-smie-rules
              :forward-token #'bril-mode--smie-forward-token
              :backward-token #'bril-mode--smie-backward-token))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.bril\\'" 'bril-mode))

(provide 'bril-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; bril-mode.el ends here
