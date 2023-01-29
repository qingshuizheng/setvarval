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

(defcustom varext-setter 'setq
  "Setter to use: `setq', `setopt' or nil."
  :group 'varext
  :type 'symbol)

(defcustom varext-type 'defcustom
  "What to extract: `defcustom', `defvar', `defface', or `defconst'."
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
(defun varext-extract ()
  "Extract variables to kill-ring."
  (interactive)
  (kill-new
   (let* ((list (varext--construct (current-buffer))))
     ;; SRC 2023-01-29: https://stackoverflow.com/questions/18979300
     (mapconcat (lambda (x) (format "%S" x)) list "\n"))))


(provide 'varext)
;;; varext.el ends here
