;;; zilch-mode.el --- Major mode for Zilch
;; SPDX-License-Identifier: BSD3 License

;;; Commentary:

;; A major mode providing some syntax highlighting for the functional programming language Zilch.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dependency

(require 'rx)
(require 'font-lock)

;;; Code:

(defgroup zilch nil
  "Major mode for editing Zilch code."
  :link '(custom-group-link :tag "Font lock faces group" font-lock-faces)
  :group 'languages)

(defconst zilch-keywords
  '("let" "rec" "in" "alias" "enum" "record" "import" "export" "case" "of" "forall" "∀"
    "class" "impl" "where" "as" "open" "effect" "if" "then" "else" "pattern" "_" "·")
  "The list of Zilch keywords.")

(defconst zilch-builtin-types
  '("u8" "u16" "u32" "u64" "s8" "s16" "s32" "s64" "char" "ptr" "ref"
    "f32" "f64")
  "The list of Zilch builtin types.")

(defconst zilch-font-lock-keywords
  `(
    (,(rx-to-string `(: bow (| ,@zilch-keywords) eow))
     . font-lock-keyword-face)
    ; keywords
    (,(rx-to-string '(: "#[" (*? nonl) "]"))
      0 font-lock-type-face)
    ; meta-specifiers
    (,(rx-to-string `(: bow (| ,@zilch-builtin-types) eow))
     . font-lock-builtin-face)
    ; builtin types
    (,(rx-to-string '(: bow
                       (| (: "0" (| "x" "X") (+ xdigit))
                          (: "0" (| "b" "B") (+ (any "0" "1")))
                          (: "0" (| "o" "O") (+ (any "0" "1" "2" "3" "4" "5" "6" "7" "8")))
                          (+ digit (? "." (+ digit))))))
     . font-lock-constant-face)
    ; numbers
    )
  "Additional expressions to highlight in Zilch.")

(defvar zilch-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?-  ". 12b" st)
    (modify-syntax-entry ?_  "w" st)
    (modify-syntax-entry ?\" "|" st)
    (modify-syntax-entry ?'  "|" st)
    (modify-syntax-entry ?#  "! 1" st)
    (modify-syntax-entry ?\[ "! 2c" st)
    (modify-syntax-entry ?\] "> c" st)
    st)
  "Syntax table used while in Zilch mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zc\\'" . zilch-mode))
;;;###autoload
(modify-coding-system-alist 'file "\\.zc\\'" 'utf-8)
;;;###autoload
(define-derived-mode zilch-mode prog-mode "Zilch"
  "Major mode for editing simple Zilch source files.
Only provides syntax highlighting."
  :syntax-table zilch-mode-syntax-table

  (setq-local font-lock-defaults '(zilch-font-lock-keywords))
;  (set-syntax-table (make-syntax-table zilch-mode-syntax-table))

  (setq-local comment-start "-- ")
  (setq-local comment-end "")
  )


(provide 'zilch-mode)

;;; zilch-mode.el ends here
