;;; varext.el --- Extract variables from package -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Qingshui Zheng <qingshuizheng at outlook dot com>
;;
;; Author: Qingshui Zheng <qingshuizheng at outlook dot com>
;; Maintainer: Qingshui Zheng <qingshuizheng at outlook dot com>
;;
;; Created: 21 Dec 2022
;;
;; URL: https://github.com/qingshuizheng/varext
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

(defgroup varext nil
  "Variable extraction."
  :group 'utilities
  :prefix "varext-")

(defcustom varext-type 'defcustom
  "Which variable to collect.
Could be: `defcustom', `defvar', `defface', or `defconst'."
  :group 'varext
  :type 'symbol)

(defcustom varext-setter 'setq
  "Which setter to use after collecting.
Could be: `setq', `setopt' or nil."
  :group 'varext
  :type 'symbol)

(defun varext--extract-symbol (sexps)
  "Loop, scrape and form the constructs."
  (cl-loop with var
           for sexp in sexps
           for func = (car sexp)
           for var = (nth 1 sexp)
           for val = (or (nth 2 sexp) '())
           when (eq func varext-type)
           collect (list varext-setter var val) into options
           finally (return options)))

(defun varext--construct (buf)
  "Scan for command definitions in BUF and return data structure."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (varext--extract-symbol
       (cl-loop with it
                while (setq it (condition-case v
                                   (read (current-buffer))
                                 (error nil)))
                collect it)))))

;;;###autoload
(defun varext-setting ()
  "Interactvely config settings."
  (interactive)
  (setq varext-type
        (intern (completing-read
                 "Which type to collect: "
                 '(defcustom defvar defconst defface))))
  (setq varext-setter
        (intern (completing-read
                 "Which setter to use after collecting: "
                 '(setq setopt nil)))))

;;;###autoload
(defun varext-extract (&optional arg)
  "Extract variables to kill-ring.
With C-u prefix, run `varext-setting' first."
  (interactive "p")
  (when current-prefix-arg (varext-setting))
  (kill-new
   (let* ((list (varext--construct (current-buffer))))
     (mapconcat
      (pcase varext-setter
        ((or 'setq 'setopt) (lambda (x) (format "%S" x)))
        (`nil (lambda (x) (substring-no-properties (format "%S" x) 5 -1))))
      list "\n"))))


(provide 'varext)
;;; varext.el ends here
