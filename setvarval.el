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


;;;; REQUIREMENTS

;; Built in
(require 'seq)
(require 'cl-lib)


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
  :type 'symbol.
  :options '( defcustom
              defvar
              defface
              defconst))

(defcustom setvarval-group-style 'simple
  "How to format the results.

Styles:

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
(var1 unquoted-val1)
(var2 unquoted-val2)
(var3 unquoted-val3)

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
  :type 'symbol
  :options '(simple
             one-setter
             custom-set-*
             use-package:custom
             use-package:custom-face
             leaf:custom*
             leaf:custom
             leaf:custom-face
             setup:option))

(defcustom setvarval-group-setter 'setq
  "Which setter to use after collecting.
Alternatives: `setopt', `custom-set-variables', `defface',
`custom-set-faces', or any random text you like."
  :group 'setvarval
  :type 'symbol
  :options '(setq
             setopt
             defvar
             defconst
             custom-set-variables
             custom-set-faces))

(defvar setvarval-pkgmgr-list '(use-package leaf setup)
  "Package Manager list.")


;;;; COLLECT DATA


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
  "If cursor within package manager, return a list: '(pkgmgr name)."
  (save-excursion
    (when (thing-at-point 'defun)
      (beginning-of-defun)
      (when-let* ((sexp (read (current-buffer)))
                  (func (car sexp))
                  (pkgmgrp (member func setvarval-pkgmgr-list))
                  (library (when pkgmgrp (cadr sexp))))
        (list func library)))))



;;;; STYLE TRANSFORMATION


(defun setvarval--string-wrap (prefix suffix string)
  "String wrap."
  (declare (indent 1))
  (concat prefix string suffix))

(defun setvarval--group-style-simple (list)
  "Transform LIST into group style: simple."
  (mapconcat
   (lambda (x)
     (push setvarval-group-setter x) (format "%S" x))
   list "\n"))

(defun setvarval--group-style-one-setter (list)
  "Transform LIST into group style: one-setter."
  (setvarval--string-wrap
      (format "(%S " setvarval-group-setter) ")"
      (mapconcat
       (lambda (x)
         (substring-no-properties
          (format "%S" x) 1 -1))
       list "\n")))

(defun setvarval--group-style-custom-set-* (list)
  "Transform LIST into group style: one-setter."
  (setvarval--string-wrap
      (format "(%S\n" setvarval-group-setter) ")"
      (mapconcat (lambda (x) (concat "'" (format "%S" x))) list "\n")))

(defun setvarval--group-style-use-package:custom (list)
  "Transform LIST into group style: use-package:custom.
See (info \"(use-package) User options\")."
  (setvarval--string-wrap
      ":custom\n" nil
      (mapconcat (lambda (x) (format "%S" x)) list "\n")))

(defun setvarval--group-style-use-package:custom-face (list)
  "Transform LIST into group style: use-package:custom-face.
see (info \"(use-package) Faces\")."
  (setvarval--string-wrap
      ":custom-face\n" nil
      (substring-no-properties
       (mapconcat
        (lambda (x) (format "%S" x)) list "\n")
       1)))

(defun setvarval--group-style-leaf:custom* (list)
  "Transform LIST into group style: leaf:custom*.
See https://github.com/conao3/leaf.el#custom-custom-custom-face-keywords."
  (setvarval--string-wrap
      ":custom*\n(" ")"
      (mapconcat (lambda (x) (format "%S" x)) list "\n")))

(defun setvarval--group-style-leaf:custom (list)
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

(defun setvarval--group-style-leaf:custom-face (list)
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

(defun setvarval--group-style-setup:option (list)
  "Transform LIST into group style: use-package:custom.
See https://www.emacswiki.org/emacs/SetupEl for format."
  (setvarval--string-wrap
      "(:option\n" ")"
      (mapconcat
       (lambda (x)
         (substring-no-properties (format "%S" x) 1 -1))
       list "\n")))



;;;; CONFIG

(defun setvarval--extract-type-options ()
  "Get custom options for `setvarval-extract-type'."
  (get 'setvarval-extract-type 'custom-options))

(defun setvarval--get-pkgmgr-name ()
  "Get package manager name."
  (car-safe (setvarval--inside-pkgmgr-p)))

(defun setvarval--config-set-extract-type ()
  "Customize group type to extract variables from."
  (let ((types (get 'setvarval-extract-type 'custom-options)))
    (intern (completing-read
             "Variable type to collect: "
             types))))

(defun setvarval--config-set-group-style (type pkgmgr)
  "Customize group style based on results of group TYPE and PKGMGR name."
  (pcase type
    ('defcustom (pcase pkgmgr
                  (`nil
                   (intern (completing-read
                            "Group style to organize variables: "
                            '(simple
                              one-setter
                              custom-set-*
                              use-package:custom
                              leaf:custom*
                              leaf:custom
                              setup:option))))
                  (`use-package
                    (intern (completing-read
                             "Group style to organize variables: "
                             '(simple
                               one-setter
                               custom-set-*
                               use-package:custom))))
                  (`leaf
                   (intern (completing-read
                            "Group style to organize variables: "
                            '(simple
                              one-setter
                              custom-set-*
                              leaf:custom*
                              leaf:custom))))
                  (`setup
                      (intern (completing-read
                               "Group style to organize variables: "
                               '(simple
                                 one-setter
                                 custom-set-*
                                 setup:option))))))
    ((or 'defvar 'defconst)
     (intern (completing-read
              "Group style to organize variables: "
              '(simple
                one-setter))))
    ('defface
      (pcase pkgmgr
        (`nil
         (intern (completing-read
                  "Group style to organize variables: "
                  '(simple
                    custom-set-*
                    use-package:custom-face
                    leaf:custom-face))))
        (`use-package
          (intern (completing-read
                   "Group style to organize variables: "
                   '(simple
                     custom-set-*
                     use-package:custom-face))))
        (`leaf
         (intern (completing-read
                  "Group style to organize variables: "
                  '(simple
                    custom-set-*
                    leaf:custom-face))))
        (`setup
            (intern (completing-read
                     "Group style to organize variables: "
                     '(simple
                       custom-set-*))))))))

(defun setvarval--config-set-group-setter (type style)
  "Customize group setter based on group TYPE and group STYLE."
  (pcase type
    ('defcustom
      (cond
       ((member style '(custom-set-*)) 'custom-set-variables)
       ((member style '(simple one-setter))
        (intern (completing-read
                 "Setter to use (choose or type your own): "
                 '(setq
                   setopt
                   setq-default
                   setq-local))))))
    ((or 'defvar 'defconst)
     (intern (completing-read
              "Setter to use (choose or type your own): "
              '(setq
                setq-local
                setq-default))))
    ('defface
      (cond ((member style '(simple)) 'defface)
            ((member style '(custom-set-*)) 'custom-set-faces)))))

;;;###autoload
(defun setvarval-config ()
  "Interactvely config settings.
With prefix C-u, set them to default value."
  (interactive)
  (let* ((pkgmgr (setvarval--get-pkgmgr-name))
         (type   (setvarval--config-set-extract-type))
         (style  (setvarval--config-set-group-style type pkgmgr))
         (setter (setvarval--config-set-group-setter type style)))
    (setq setvarval-extract-type type)
    (setq setvarval-group-style style)
    (setq setvarval-group-setter setter)))



;;;; EXTRACT


;;;###autoload
(defun setvarval-extract-current-buffer (&optional arg no-kill-ring)
  "Extract variables from current buffer and save to kill-ring.
With prefix C-u, run `setvarval-config' before extraction.
With NO-KILL-RING set, don't save to kill-ring."
  (interactive "P")
  (when arg (setvarval-config))
  (when-let* ((list (setvarval--collect-args-from-sexps (current-buffer)))
              (result
               (pcase setvarval-group-style
                 ('simple (setvarval--group-style-simple list))
                 ('one-setter (setvarval--group-style-one-setter list))
                 ('custom-set-* (setvarval--group-style-custom-set-* list))
                 ('use-package:custom (setvarval--group-style-use-package:custom list))
                 ('use-package:custom-face (setvarval--group-style-use-package:custom-face list))
                 ('leaf:custom* (setvarval--group-style-leaf:custom* list))
                 ('leaf:custom (setvarval--group-style-leaf:custom list))
                 ('leaf:custom-face (setvarval--group-style-leaf:custom-face list))
                 ('setup:option (setvarval--group-style-setup:option list)))))
    (if no-kill-ring result
      (kill-new result))))

;;;###autoload
(defun setvarval-extract-current-package (arg)
  "Extract variables from current package the cursor is in.
With prefix C-u, run `setvarval-config' before extraction.
TODO: Sub-packages and dependancies is not supported currently."
  (interactive "P")
  (when arg (setvarval-config))
  (when-let* ((pkgmgr-library (setvarval--inside-pkgmgr-p))
              (library (format "%S" (cadr pkgmgr-library))))
    (with-temp-buffer
      (insert-file-contents (find-library-name library))
      (goto-char (point-min))
      (setvarval-extract-current-buffer nil nil))
    (message "%s variables extracted to kill-ring."
             (upcase library))))

;;;###autoload
(defun setvarval-extract-current-package-insert (arg)
  "Extract variables from current package where the cursor is in.
With prefix C-u, run `setvarval-config' before extraction.
TODO: Sub-packages and dependancies is not supported currently."
  (interactive "P")
  (when arg (setvarval-config))
  (when-let* ((pkgmgr-library (setvarval--inside-pkgmgr-p))
              (library (format "%S" (cadr pkgmgr-library)))
              (result
               (with-temp-buffer
                 (insert-file-contents (find-library-name library))
                 (goto-char (point-min))
                 (setvarval-extract-current-buffer nil :no-kill-ring))))
    (when (and pkgmgr-library
               (member setvarval-group-style
                       '(use-package:custom
                         use-package:custom-face
                         leaf:custom
                         leaf:custom*
                         setup:option)))
      ;; Goto point after package name
      (beginning-of-defun)
      (forward-symbol 1)
      (forward-sexp 1)) ; for `setup' form: (setup (:straight corfu))
    (let ((beg (point)))
      (insert "\n")
      (insert result)
      (indent-region beg (point)))))

;;;###autoload
(defun setvarval-extract-from-library (arg)
  "Extract variables from selected library, save to kill-ring.
With prefix C-u, run `setvarval-config' before extraction.
TODO: Sub-packages and dependancies is not supported currently."
  (interactive "P")
  (when arg (setvarval-config))
  (when-let* ((library (read-library-name)))
    (with-temp-buffer
      (insert-file-contents
       (find-library-name library))
      (goto-char (point-min))
      (setvarval-extract-current-buffer nil nil))
    (message "%s variables extracted to kill-ring." (upcase library))))

;;;###autoload
(defun setvarval-extract-im-feeling-lucky (arg)
  "I don't know the package/feature/file name, but I'm feeling lucky.
Guess from file where symbol is in. With prefix C-u, run
`setvarval-config' before extraction."
  (interactive "P")
  (when arg (setvarval-config))
  (when-let* ((sym (completing-read "Choose symbol: " obarray))
              (file
               (symbol-file ; fixme: it returns .elc file
                (intern-soft sym))))
    (with-temp-buffer
      (insert-file-contents (find-library-name file))
      (goto-char (point-min))
      (setvarval-extract-current-buffer nil nil))))


(provide 'setvarval)
;;; setvarval.el ends here

