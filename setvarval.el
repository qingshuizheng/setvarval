;;; setvarval.el --- Extract variables from package -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Qingshui Zheng <qingshuizheng at outlook dot com>
;;
;; Author: Qingshui Zheng <qingshuizheng at outlook dot com>
;; Maintainer: Qingshui Zheng <qingshuizheng at outlook dot com>
;;
;; Created: 21 Dec 2022
;;
;; URL: https://github.com/qingshuizheng/setvarval
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defgroup setvarval nil
  "Variable extraction."
  :group 'utilities
  :prefix "setvarval-")

(defcustom setvarval-extract-type 'defcustom
  "Which variable to collect.
Could be: `defcustom', `defvar', `defface', or `defconst'."
  :group 'setvarval
  :type 'symbol)

(defcustom setvarval-group-setter 'setq
  "Which setter to use after collecting.
Could be: `setq', `setopt', `customize-set-variables' or nil."
  :group 'setvarval
  :type 'symbol)

(defcustom setvarval-include-doc nil
  "Whether include doc or not."
  :group 'setvarval
  :type 'boolean)

(defcustom setvarval-group-style 'vanilla-default
  "How to format the results.

Could be:
`vanilla-default'
`vanilla-flatten'
`use-package'
`leaf'
`setup'.

-- VANILLA-DEFAULT:
(setter var1 val1)
(setter var2 val2)
(setter var3 val3)

-- VANILLA-COMPACT:
(setter var1 val1
        var2 val2
        var3 val3)

- USE-PACKAGE:
:custom(-face)?
(var1 val1)
(var2 val2)
(var3 val3)

-- LEAF:
:custom(-face)?
(var1 . val1)
(var2 . val2)
(var3 . val3)"

  :group 'setvarval
  :type 'symbol)

(defun setvarval--collect-sexps-from-buffer (buf)
  "Collect S-expression from BUF."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (cl-loop with it
               while (setq it (condition-case _
                                  (read (current-buffer))
                                (error nil)))
               collect it))))

(defun setvarval--collect-variables-from-sexps (buf)
  "Collect variables from S-expression from BUF."
  (let ((sexps (setvarval--collect-sexps-from-buffer buf)))
    (cl-loop for sexp in sexps
             for func = (car sexp)
             for var = (nth 1 sexp)
             for val = (nth 2 sexp)
             for doc = (nth 3 sexp)
             when (eq func setvarval-extract-type)
             collect
             (if setvarval-include-doc
                 (list var val doc)
               (list var val))
             into options
             finally (return options))))

;;;###autoload
(defun setvarval-setting ()
  "Interactvely config settings."
  (interactive)
  (setq setvarval-extract-type
        (intern (completing-read
                 "Which type to collect: "
                 '(defcustom defvar defconst defface))))
  (setq setvarval-group-setter
        (intern (completing-read
                 "Which setter to use after collecting: "
                 '(setq setopt customize-set-variables nil)))))

;;;###autoload
(defun setvarval-extract (&optional arg)
  "Extract variables to kill-ring.
With C-u prefix, run `setvarval-setting' first."
  (interactive "p")
  (when current-prefix-arg (setvarval-setting))
  (kill-new
   (let* ((list (setvarval--collect-variables-from-sexps (current-buffer))))
     (mapconcat
      (pcase setvarval-group-setter
        ((or 'setq 'setopt) (lambda (x) (format "%S" x)))
        (`nil (lambda (x) (substring-no-properties (format "%S" x) 5 -1))))
      list "\n"))))


(provide 'setvarval)
;;; setvarval.el ends here
