;;; anything-c-javadoc.el --- anything-sources for opening javadocs.

;; Copyright (C) 2010 Takeshi Banse <takebi@laafc.net>
;; Author: Takeshi Banse <takebi@laafc.net>
;; Keywords: convenience, anything, javadoc, help, lookup

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Some anything onfigurations for quickly open javadocs.

;;; Installation:
;;
;; Put the anything-c-javadoc.el and anything.el to your load-path.

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `anything-c-javadoc-dirs'
;;    *Urls of the javadoc to be used. A url will be treated as the absolute path on the local machine, unless starts with `http`.
;;    default = (quote ("http://java.sun.com/javase/6/docs/api/" "http://joda-time.sourceforge.net/api-release/"))
;;  `anything-c-javadoc-classes-cache-filename'
;;    *Filename to be used as the cache of the javadocs' all-classes.html contents.
;;    default = (expand-file-name "~/.emacs.d/.anything-c-javadoc-classes.cache")
;;  `anything-c-javadoc-indexes-cache-filename'
;;    *Filename to be used as the cache of the javadocs' indexed contents.
;;    default = (expand-file-name "~/.emacs.d/.anything-c-javadoc-indexes.cache")
;;  `anything-c-javadoc-type-face'
;;    *Face used to highlight type and classes.
;;    default = (quote font-lock-type-face)
;;  `anything-c-javadoc-parameter-type-face'
;;    *Face used to highlight type and classes in the parameter list.
;;    default = (quote font-lock-type-face)
;;  `anything-c-javadoc-function-name-face'
;;    *Face used to highlight function names.
;;    default = (quote font-lock-function-name-face)
;;  `anything-c-javadoc-variable-name-face'
;;    *Face used to highlight variable names.
;;    default = (quote font-lock-variable-name-face)
;;  `anything-c-javadoc-constant-face'
;;    * Font Lock mode face used to highlight constants and labels.
;;    default = (quote font-lock-constant-face)
;;  `anything-c-javadoc-paren-face'
;;    *Face used to highlight parens.
;;    default = nil

;;; Code:

(require 'anything)
(require 'simple)
(require 'url)

(defcustom anything-c-javadoc-dirs
  '("http://java.sun.com/javase/6/docs/api/"
    "http://joda-time.sourceforge.net/api-release/")
  "*Urls of the javadoc to be used. A url will be treated as the absolute path on the local machine, unless starts with `http`."
  :type 'list
  :group 'anything-config)

(defcustom anything-c-javadoc-classes-cache-filename
  (expand-file-name "~/.emacs.d/.anything-c-javadoc-classes.cache")
  "*Filename to be used as the cache of the javadocs' all-classes.html contents."
  :type 'file
  :group 'anything-config)

(defcustom anything-c-javadoc-indexes-cache-filename
  (expand-file-name "~/.emacs.d/.anything-c-javadoc-indexes.cache")
  "*Filename to be used as the cache of the javadocs' indexed contents."
  :type 'file
  :group 'anything-config)

(defvar anything-c-javadoc-classes-candidate-buffer-name
  " *anything javadoc classes*")
(defvar anything-c-javadoc-indexes-candidate-buffer-name
  " *anything javadoc indexes*")

(defun acjd-source-base-classes (ass)
  (append
   ass
   '((candidates-in-buffer)
     (get-line . buffer-substring)
     (action
      . (("Browse"
          . (lambda (c)
              (browse-url (format "%s%s.html#skip-navbar_top"
                                  (get-text-property 0 'acjd-dirname c)
                                  (replace-regexp-in-string "\\." "/" c)))))
         ("Insert class name at point"
          . (lambda (c) (insert (substring-no-properties c))))
         ("Copy class name in kill-ring"
          . (lambda (c) (kill-new (substring-no-properties c)))))))))

(defvar anything-c-source-javadoc-classes
  (acjd-source-base-classes
   '((name . "Java docs (classes)")
     (init
      . (lambda ()
          (acjd-initialize-candidate-buffer-maybe
           anything-c-javadoc-dirs
           anything-c-javadoc-classes-candidate-buffer-name
           anything-c-javadoc-classes-cache-filename
           (lambda (d b)
             (acjd-allclasses->any-cand-buffer
              (format "%sallclasses-frame.html" d) b))))))))

;; (anything '(anything-c-source-javadoc-classes))

(defun acjd-source-base-indexes (ass)
  (append
   ass
   '((candidates-in-buffer)
     (get-line . buffer-substring)
     (action
      . (("Browse"
          . (lambda (c)
              (browse-url (get-text-property 0 'acjd-uri c))))
         ("Insert the name at point"
          . (lambda (c) (insert (get-text-property 0 'acjd-name c))))
         ("Copy the name in kill-ring"
          . (lambda (c) (kill-new (get-text-property 0 'acjd-name c)))))))))

(defvar anything-c-source-javadoc-indexes
  (acjd-source-base-indexes
   '((name . "Java docs (indexes)")
     (init
      . (lambda ()
          (acjd-initialize-candidate-buffer-maybe
           anything-c-javadoc-dirs
           anything-c-javadoc-indexes-candidate-buffer-name
           anything-c-javadoc-indexes-cache-filename
           'acjd-index->any-cand-buffer))))))

;; (anything '(anything-c-source-javadoc-indexes))

(defun acjd-initialize-candidate-buffer-maybe
    (javadoc-dirs buffer-name cache-filename create-cand-buffer)
  (when (or current-prefix-arg (not (get-buffer buffer-name)))
    (acjd-initialize-candidate-buffer
     javadoc-dirs buffer-name cache-filename
     (acjd-regenerate-cache-p cache-filename) create-cand-buffer))
  (anything-candidate-buffer (get-buffer buffer-name)))

(defun acjd-regenerate-cache-p (cache-filename)
  (or (not (file-exists-p cache-filename))
      current-prefix-arg))

(defun acjd-initialize-candidate-buffer
    (javadoc-dirs any-cand-buffer cache-file regeneratep create-cand-buffer)
  (flet ((cache (cache-file create-cand-buffer)
           (with-temp-buffer
             (acjd-cache-cand-buffer
              javadoc-dirs cache-file create-cand-buffer (current-buffer)
              (lambda (buf)
                (with-temp-file cache-file
                  (prog1 nil
                    (prin1 (with-current-buffer buf
                             (buffer-substring (point-min) (point-max)))
                           (current-buffer)))))))))
    (when regeneratep
      (message "Generating javadoc cache...(this may take a while)")
      (cache cache-file create-cand-buffer)
      (message "Generating javadoc cache...done."))
    (with-current-buffer (get-buffer-create any-cand-buffer)
      (erase-buffer)
      (let ((b (find-file-noselect cache-file t t)))
        (unwind-protect
             (insert (with-current-buffer b
                       (goto-char (point-min))
                       (read (current-buffer))))
          (kill-buffer b))))))

(defun acjd-cache-cand-buffer
    (javadoc-dirs cache-file create-cand-buffer buffer write)
  (flet ((replace-text (from to)
           (goto-char (point-min))
           (while (re-search-forward (regexp-quote from) nil t)
             (replace-match to))))
    (with-current-buffer buffer
      (loop for d in javadoc-dirs
            do (funcall create-cand-buffer d (current-buffer))
            finally do
            (sort-lines nil (point-min) (point-max))
            (replace-text "&lt;" "<")
            (replace-text "&gt;" ">")
            (anything-aif anything-c-javadoc-paren-face
                (acjd-anything-mp-highlight-region
                 (point-min) (point-max) (rx (or ?( ?) ?[ ?] ?< ?>)) it))
            (funcall write (current-buffer))))))

;;; borrowed from anything-match-plugin.el
(defun acjd-anything-mp-highlight-region (start end regexp face)
  (save-excursion
    (goto-char start)
    (let (me)
      (while (and (setq me (re-search-forward regexp nil t)) (< (point) end))
        (put-text-property (match-beginning 0) me 'face face)))))

(defun acjd-allclasses->any-cand-buffer (filename buf)
  (with-temp-buffer
    (loop initially (acjd-insert-contents filename (current-buffer))
          until (or (eobp) (not (re-search-forward "^<A HREF=\"" nil t)))
          when (looking-at (rx (group (+ nonl)) ".html"
                               (+ nonl) ">" (group (+ nonl)) "</A>" eol))
          do ((lambda (fullname name javadoc-dirname)
                (with-current-buffer buf
                  (insert (acjd-allclasses-propertize fullname))
                  (add-text-properties
                   (line-beginning-position) (1+ (line-beginning-position))
                   `(,@'()
                        acjd-simple-name ,name
                        acjd-dirname     ,javadoc-dirname))
                  (insert "\n")))
              (replace-regexp-in-string "/" "." (match-string 1))
              (match-string 2)
              (file-name-directory (acjd-fix-url-scheme filename))))))

(defun acjd-insert-contents (filename buf)
  (with-current-buffer buf
    (cond ((string-match "^http" filename)
           (let ((k (apply-partially (lambda (b s)
                                       (with-current-buffer b
                                         (insert s)))
                                     (current-buffer))))
             (with-current-buffer (url-retrieve-synchronously filename)
               (goto-char (point-min))
               (re-search-forward "^$" nil t)
               (funcall k (buffer-substring-no-properties
                           (1+ (point)) (point-max)))
               (kill-buffer))))
          (t (insert-file-contents filename)))
    (delete-trailing-whitespace)
    (goto-char (point-min))))

(defun acjd-fix-url-scheme (filename)
  (if (string-match "^http" filename) filename (concat "file://" filename)))

(defun acjd-contents-file-exists-p (filename)
  (cond ((string-match "^https" filename)
         (url-https-file-exists-p filename))
        ((string-match "^http" filename)
         (url-http-file-exists-p filename))
        (t (file-exists-p filename))))

(defun acjd-index->any-cand-buffer (dir buf)
  (let ((indexall (format "%sindex-all.html" dir)))
    (if (acjd-contents-file-exists-p indexall)
        (acjd-index->any-cand-buffer-1 indexall buf)
      (loop for i from 1 to 27
            for file = (format "%sindex-files/index-%s.html" dir i)
            when (acjd-contents-file-exists-p file)
            do (acjd-index->any-cand-buffer-1 file buf)))))

(defun acjd-index->any-cand-buffer-1 (filename buf)
  (with-temp-buffer
    (loop initially (acjd-insert-contents filename (current-buffer))
          until (or (eobp) (not (re-search-forward "^<DT><A HREF=\"" nil t)))
          when (looking-at (rx (+ nonl) "<DT><A HREF=\""))
          do (goto-char (match-end 0))
          when (looking-at
                (rx (group (+ nonl)) "\"><B>" (group (+ nonl)) "</B>"
                    "</A> -" eol))
          do (apply
              (lambda (relative name base-filename
                       classification +full-classname classname)
                (with-current-buffer buf
                  (insert (acjd-index-propertize classification classname
                                                 name))
                  (add-text-properties
                   (line-beginning-position) (1+ (line-beginning-position))
                   `(,@'()
                        acjd-uri ,(url-expand-file-name relative base-filename)
                        acjd-simple-name ,classname
                        acjd-full-name ,(with-temp-buffer
                                         (insert +full-classname)
                                         (goto-char (point-min))
                                         (when (looking-at (rx (1+ (not white))
                                                               (1+ white)))
                                           (goto-char (match-end 0)))
                                         (buffer-substring
                                          (point) (point-max)))))
                  (insert "\n")))
              (match-string 1)
              (match-string 2)
              (acjd-fix-url-scheme filename) ;; match-data safeness
              (progn
                (forward-line 1)
                (save-restriction
                  (narrow-to-region (line-beginning-position)
                                    (line-end-position))
                  (if (looking-at
                       (rx (group (1+ (not white))
                                  (1+ white)
                                  (* (1+ word) (1+ white)))
                           (group (1+ nonl))
                           "<A HREF=\"" (1+ (not (any ?>))) "\">"
                           (* (or "<B>" "<CODE>")) (group (+? nonl))
                           (* (or "</B>" "</CODE>")) "</A>"
                           (group (* nonl))))
                      (list (concat (match-string 4) (match-string 1))
                            (concat (match-string 2) (match-string 3))
                            (match-string 3))
                    (error "cannot find index data"))))))))

(defcustom anything-c-javadoc-type-face
  'font-lock-type-face
  "*Face used to highlight type and classes."
  :type 'face
  :group 'anything-config)
(defcustom anything-c-javadoc-parameter-type-face
  'font-lock-type-face
  "*Face used to highlight type and classes in the parameter list."
  :type 'face
  :group 'anything-config)
(defcustom anything-c-javadoc-function-name-face
  'font-lock-function-name-face
  "*Face used to highlight function names."
  :type 'face
  :group 'anything-config)
(defcustom anything-c-javadoc-variable-name-face
  'font-lock-variable-name-face
  "*Face used to highlight variable names."
  :type 'face
  :group 'anything-config)
(defcustom anything-c-javadoc-constant-face
  'font-lock-constant-face
  "* Font Lock mode face used to highlight constants and labels."
  :type 'face
  :group 'anything-config)
(defcustom anything-c-javadoc-paren-face
  nil
  "*Face used to highlight parens."
  :type 'face
  :group 'anything-config)

(defun acjd-allclasses-propertize (fullname)
  (with-temp-buffer
    (insert fullname)
    (goto-char (point-min))
    (let ((s (point)))
      (while (re-search-forward "\\." nil t)
        (put-text-property s (match-beginning 0) 'face
                           anything-c-javadoc-constant-face)
        (setq s (goto-char (point))))
      (put-text-property s (line-end-position) 'face
                         anything-c-javadoc-type-face))
    (buffer-substring (point-max) (point-min))))

(defun acjd-index-propertize (classification classname x)
  (macrolet ((with-propertize-temp-buffer (() &body body)
               `(with-temp-buffer
                  (insert (propertize
                           classname 'face anything-c-javadoc-type-face)
                          "#")
                  ,@body
                  (buffer-substring (point-min) (point-max)))))
    (flet ((method-or-ctor-p (c)
             (or (string-match "method\\|constructor" c)
                 (string-match "メソッド\\|コンストラクタ" c)))
           (variablep (c)
             (or (string-match "variable" c) (string-match "変数" c)))
           (staticp (c)
             (or (string-match "static" c) (string-match "静的" c))))
      (let ((case-fold-search t)
            (staticp (staticp classification))
            (static-indication "+"))
        (cond ((method-or-ctor-p classification)
               (with-propertize-temp-buffer ()
                 (acjd-index-propertize-method
                  x staticp static-indication
                  anything-c-javadoc-function-name-face
                  anything-c-javadoc-parameter-type-face)))
              ((variablep classification)
               (with-propertize-temp-buffer ()
                 (acjd-index-propertize-variable
                  x staticp static-indication
                  anything-c-javadoc-variable-name-face)))
              (t x))))))

(defun acjd-index-propertize-method
    (x staticp static-indication funcname-face paramname-face)
  (when staticp (insert static-indication))
  (insert x)
  (goto-char (- (point) (length x)))
  (looking-at (rx (group (+ print)) "("))
  (put-text-property (match-beginning 1) (match-end 1) 'face funcname-face)
  (put-text-property (point-min) (1+ (point-min))
                     'acjd-name (buffer-substring-no-properties
                                 (match-beginning 1) (match-end 1)))
  (when staticp (goto-char (match-end 1)) (insert static-indication))
  (goto-char (match-end 0))
  (loop while (looking-at (rx (group (+? print)) (*? "[]") (or "," ")"))) do
        (put-text-property (point) (match-end 1) 'face paramname-face)
        (goto-char (match-end 0))))

(defun acjd-index-propertize-variable
    (x staticp static-indication varname-face)
  (when staticp (insert static-indication))
  (insert x)
  (put-text-property (- (point) (length x)) (point) 'face varname-face)
  (put-text-property (point-min) (1+ (point-min))
                     'acjd-name (buffer-substring-no-properties
                                 (- (point) (length x)) (point)))
  (when staticp (insert static-indication)))

(provide 'anything-c-javadoc)
;;; anything-c-javadoc.el ends here
