;;; setvarval.el --- Extract variables from package -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022-2023, Qingshui Zheng <qingshuizheng at outlook
;; dot com>
;;
;; Author: Qingshui Zheng <qingshuizheng at outlook dot com>
;; Maintainer: Qingshui Zheng <qingshuizheng at outlook dot com>
;;
;; Created: 21 Dec 2022
;; Updated: 31 Jan 2023
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
  :prefix "setvarval-"
  :link "https://github.com/qingshuizheng/setvarval")



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
`custom-set-*'
`use-package:custom'
`use-package:custom-face'
`leaf:custom*'
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

-- CUSTOM-SET-*
(custom-set-variables/faces
 '(var1 val1)
  (var2 val2)
  (var3 val3))

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

-- LEAF:CUSTOM*
:custom*
((var1 val1)
 (var2 val2)
 (var3 val3))

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

(defvar setvarval-pkgmgr-list '(use-package leaf setup)
  "Package Manager list.")


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

(defun setvarval--inside-pkgmgr-p ()
  "If cursor within package managers, return pkgmgr name."
  (save-excursion
    (when (thing-at-point 'defun)
      (beginning-of-defun)
      (when-let* ((sexp (read (current-buffer)))
                  (func (car sexp))
                  (pkgmgrp (member func setvarval-pkgmgr-list))
                  (feature (when pkgmgrp (cadr sexp))))
        (list func feature)))))



;;; style transformation

(defun setvarval--string-wrap (prefix suffix string)
  "String wrap."
  (declare (indent 1))
  (concat prefix string suffix))

(defun setvarval--style-simple (list)
  "Transform LIST into group style: simple."
  (mapconcat
   (lambda (x)
     (push setvarval-group-setter x) (format "%S" x))
   list "\n"))

(defun setvarval--style-one-setter (list)
  "Transform LIST into group style: one-setter."
  (setvarval--string-wrap
      (format "(%S " setvarval-group-setter) ")"
      (mapconcat
       (lambda (x)
         (substring-no-properties
          (format "%S" x) 1 -1))
       list "\n")))

(defun setvarval--style-custom-set-* (list)
  "Transform LIST into group style: one-setter."
  (setvarval--string-wrap
      (format "(%S\n" setvarval-group-setter) ")"
      (mapconcat (lambda (x) (concat "'" (format "%S" x))) list "\n")))

(defun setvarval--style-use-package:custom (list)
  "Transform LIST into group style: use-package:custom.
See (info \"(use-package) User options\")."
  (setvarval--string-wrap
      ":custom\n" nil
      (mapconcat (lambda (x) (format "%S" x)) list "\n")))

(defun setvarval--style-use-package:custom-face (list)
  "Transform LIST into group style: use-package:custom-face.
see (info \"(use-package) Faces\")."
  (setvarval--string-wrap
      ":custom-face\n" nil
      (substring-no-properties
       (mapconcat
        (lambda (x) (format "%S" x)) list "\n")
       1)))

(defun setvarval--style-leaf:custom* (list)
  "Transform LIST into group style: leaf:custom*.
See https://github.com/conao3/leaf.el#custom-custom-custom-face-keywords."
  (setvarval--string-wrap
      ":custom*\n(" ")"
      (mapconcat (lambda (x) (format "%S" x)) list "\n")))

(defun setvarval--style-leaf:custom (list)
  "Transform LIST into group style: leaf:custom.
See https://github.com/conao3/leaf.el#custom-custom-custom-face-keywords."
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

(defun setvarval--style-leaf:custom-face (list)
  "Transform LIST into group style: leaf:custom-face.
See https://github.com/conao3/leaf.el#custom-custom-custom-face-keywords."
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

(defun setvarval--style-setup:option (list)
  "Transform LIST into group style: use-package:custom.
See https://www.emacswiki.org/emacs/SetupEl for format."
  (setvarval--string-wrap
      "(:option\n" ")"
      (mapconcat
       (lambda (x)
         (substring-no-properties (format "%S" x) 1 -1))
       list "\n")))



;;;; COMMANDS


;;;###autoload
(defun setvarval-config ()
  "Interactvely config settings.
With prefix C-u, set them to default value."
  (interactive)
  (let* ((type (intern (completing-read
                        "Variable type to collect: "
                        '( defcustom
                           defvar
                           defconst
                           defface))))
         (style (pcase type
                  ('defcustom
                    (intern (completing-read
                             "Group style to organize variables: "
                             '(;; -- (setter var1 val1) (setter var2 val2) ... ...
                               simple
                               ;; -- (setter var1 val1 var2 val2 ... ...)
                               one-setter
                               ;; -- (custom-set-variables/faces '(var1 val1) '(var2 val2) ... ...)
                               custom-set-*
                               ;; -- :custom (var1 val2) (var2 val2) ... ...
                               use-package:custom
                               ;; -- :custom* ((var1 val2) (var2 val2) ... ...)
                               leaf:custom*
                               ;; -- :custom (var1 . val2) (var2 . val2) ... ...
                               leaf:custom
                               ;; -- :option var1 val1 var2 val2 ... ...
                               setup:option))))
                  ((or 'defvar 'defconst)
                   (intern (completing-read
                            "Group style to organize variables: "
                            '(simple
                              one-setter))))
                  ('defface
                    (intern (completing-read
                             "Group style to organize variables: "
                             '(simple
                               custom-set-*
                               ;; -- :custom-face (var1 unquoted-val1) (var2 unquoted-val2) ... ...
                               use-package:custom-face
                               ;; -- same as leaf:custom
                               leaf:custom-face))))))
         (setter (pcase type
                   ('defcustom
                     (cond
                      ((member style '(custom-set-*)) 'custom-set-variables)
                      ((member style '(simple one-setter))
                       (intern (completing-read
                                "Setter to use: "
                                '(setq
                                  setopt
                                  setq-local
                                  setq-default))))))
                   ((or 'defvar 'defconst)
                    (intern (completing-read
                             "Setter to use: "
                             '(setq
                               setq-local
                               setq-default))))
                   ('defface
                     (cond ((member style '(simple)) 'defface)
                           ((member style '(custom-set-*)) 'custom-set-faces))))))
    (setq setvarval-extract-type type)
    (setq setvarval-group-style style)
    (setq setvarval-group-setter setter)))

;;;###autoload
(defun setvarval-extract-buffer (&optional no-kill-ring)
  "Extract variables from current buffer and save to kill-ring.
With NO-KILL-RING set, don't save to kill-ring."
  (interactive)
  (let* ((list (setvarval--collect-args-from-sexps (current-buffer)))
         (result
          (pcase setvarval-group-style
            ('simple (setvarval--style-simple list))
            ('one-setter (setvarval--style-one-setter list))
            ('custom-set-* (setvarval--style-custom-set-* list))
            ('use-package:custom (setvarval--style-use-package:custom list))
            ('use-package:custom-face (setvarval--style-use-package:custom-face list))
            ('leaf:custom* (setvarval--style-leaf:custom* list))
            ('leaf:custom (setvarval--style-leaf:custom list))
            ('leaf:custom-face (setvarval--style-leaf:custom-face list))
            ('setup:option (setvarval--style-setup:option list)))))
    (if no-kill-ring result
      (kill-new result))))

;;;###autoload
(defun setvarval-extract-current-package ()
  "Extract variables from current package the cursor is in.
TODO: Sub-packages and dependancies is not supported currently."
  (interactive)
  (let* ((pkgmgr-feature (setvarval--inside-pkgmgr-p))
         (feature (format "%S" (cadr pkgmgr-feature))))
    (with-temp-buffer
      (insert-file-contents
       (find-library-name feature))
      (goto-char (point-min))
      (setvarval-extract-buffer nil))
    (message "%s variables extracted to kill-ring." (upcase feature))))

(defun setvarval-extract-package (feature)
  "Extract variables from current package the cursor is in.
TODO: Sub-packages and dependancies is not supported currently."

(defun setvarval-extract-from-name (feature)
  "Extract variables from selection, save to kill-ring.
TODO: Sub-packages and dependancies is not supported currently.
TODO: support packages that are not loaded yet."
  (interactive (list (completing-read "Choose package: " features)))
  (with-temp-buffer
    (insert-file-contents
     (find-library-name feature))
    (goto-char (point-min))
    (setvarval-extract-buffer nil))
  (message "%s variables extracted to kill-ring." (upcase feature)))



(provide 'setvarval)
;;; setvarval.el ends here
