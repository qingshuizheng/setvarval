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


;;;; CUSTOMIZATIONS

(defcustom setvarval-extract-type 'defcustom
  "Which variable to collect.
Alternatives: `defvar', `defface', or `defconst'."
  :group 'setvarval
  :type 'symbol)

(defcustom setvarval-group-style 'simple
  "How to format the results.

Could be:
`simple'
`one-setter'
`use-package:custom'
`use-package:custom-face'
`leaf:custom'
`leaf:custom-face'
`setup:option'.

-- SIMPLE:
(setter var1 val1)
(setter var2 val2)
(setter var3 val3)

-- ONE-SETTER:
(setter var1 val1
        var2 val2
        var3 val3)

-- USE-PACKAGE:CUSTOM
:custom
(var1 val1)
(var2 val2)
(var3 val3)

-- USE-PACKAGE:CUSTOM-FACE
:custom
(var1 (substring-no-properties val1 1))
(var2 (substring-no-properties val2 1))
(var3 (substring-no-properties val3 1))

-- LEAF:CUSTOM(-FACE)
:custom(-face)?
(var1 . val1)
(var2 . val2)
(var3 . val3)

-- SETUP:OPTION
(:option
 var1 val1
 var2 val2
 var3 val3)"

  :group 'setvarval
  :type 'symbol)

(defcustom setvarval-group-setter 'setq
  "Which setter to use after collecting.
Alternatives: `setopt', `custom-set-variables', `defface',
`custom-set-faces', or any random text you like."
  :group 'setvarval
  :type 'symbol)



;;;; INTERNAL FUNCTIONALS - data retrieval

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

(defun setvarval--collect-args-from-sexps (buf)
  "Collect variables from S-expression from BUF."
  (let ((sexps (setvarval--collect-sexps-from-buffer buf)))
    (cl-loop for sexp in sexps
             for func = (car sexp)
             for var = (nth 1 sexp)
             for val = (nth 2 sexp)
             when (eq func setvarval-extract-type)
             collect (list var val) into args
             finally (return args))))

(defun setvarval--string-wrap (prefix suffix s)
  "String wrap."
  (declare (indent 1))
  (concat prefix s suffix))


;;;; COMMANDS

;;;###autoload
(defun setvarval-setting ()
  "Interactvely config settings."
  (interactive)
  (let* ((type (intern (completing-read
                        "Which type to collect: "
                        '(defcustom defvar defconst defface))))
         (style (pcase type
                  ('defcustom
                    (intern (completing-read
                             "Which style to set: "
                             '(simple
                               one-setter
                               use-package:custom
                               leaf:custom
                               leaf:custom*
                               setup:option))))
                  ((or 'defvar 'defconst)
                   (intern (completing-read
                            "Which Syle to set: "
                            '(simple
                              one-setter))))
                  ('deface
                   (intern (completing-read
                            "Which style to set: "
                            '(simple
                              use-package:custom-face
                              leaf:custom-face))))))
         (setter (pcase type
                   ('defcustom
                     (intern (completing-read
                              "Which setter to use: "
                              '(setq
                                setopt
                                setq-local
                                setq-default
                                custom-set-variables))))
                   ((or 'defvar 'defconst)
                    (intern (completing-read
                             "Which setter to use: "
                             '(setq
                               setq-local
                               setq-default
                               custom-set-variables))))
                   ('defface
                     (intern (completing-read
                              "Which setter to use: "
                              '( defface
                                 custom-set-faces
                                 setq-local
                                 setq-default)))))))
    (setq setvarval-extract-type type)
    (setq setvarval-group-style style)
    (setq setvarval-group-setter setter)))

;;;###autoload
(defun setvarval-extract (arg)
  "Extract variables to kill-ring.
With C-u prefix, run `setvarval-setting' first."
  (interactive "P")
  (when arg (setvarval-setting))
  (kill-new
   (let* ((list (setvarval--collect-args-from-sexps (current-buffer))))
     (pcase setvarval-group-style
       ;; -- VANILLA
       ('simple
        (mapconcat
         (lambda (x) (push setvarval-group-setter x) (format "%S" x))
         list "\n"))
       ('one-setter
        (setvarval--string-wrap
            (format "(%S " setvarval-group-setter) ")"
            (mapconcat
             (lambda (x) (substring-no-properties (format "%S" x) 1 -1))
             list "\n")))
       
       ;; -- USE-PACKAGE: https://github.com/jwiegley/use-package#package-customization
       ('use-package:custom
        (setvarval--string-wrap
            ":custom\n" nil
            (mapconcat (lambda (x) (format "%S" x)) list "\n")))
       ('use-package:custom-face
        (setvarval--string-wrap
            ":custom-face\n" nil
            (substring-no-properties
             (mapconcat
              (lambda (x) (format "%S" x)) list "\n")
             1)))
       
       ;; -- LEAF: https://github.com/conao3/leaf.el#custom-custom-custom-face-keywords
       ('leaf:custom*
        (setvarval--string-wrap
            ":custom*\n(" ")"
            (mapconcat (lambda (x) (format "%S" x)) list "\n")))
       ('leaf:custom
        (setvarval--string-wrap
            ":custom\n" nil
            (mapconcat
             (lambda (x)
               (concat "("
                       (format "%S" (car x))
                       " . "
                       (format "%S" (cadr x))
                       ")"))
             list "\n")))
       ('leaf:custom-face
        (setvarval--string-wrap
            ":custom-face\n" nil
            (mapconcat
             (lambda (x)
               (concat "("
                       (format "%S" (car x))
                       " . "
                       (format "%S" (cadr x))
                       ")"))
             list "\n")))
       
       ;; -- SETUP: https://www.emacswiki.org/emacs/SetupEl
       ('setup:option
        (setvarval--string-wrap
            "(:option\n" ")"
            (mapconcat
             (lambda (x)
               (substring-no-properties (format "%S" x) 1 -1))
             list "\n")))))))


(provide 'setvarval)
;;; setvarval.el ends here
