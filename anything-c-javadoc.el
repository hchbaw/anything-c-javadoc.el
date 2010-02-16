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
;;  `anything-c-javadoc-allclasses-cache-filename'
;;    *Filename to be used as the cache of the javadocs' all-classes.html contents.
;;    default = (expand-file-name "~/.emacs.d/.anything-c-javadoc-allclasses-cache.el")

;;; Code:

(require 'anything)
(require 'simple)
(require 'w3m)

(defcustom anything-c-javadoc-dirs
  '("http://java.sun.com/javase/6/docs/api/"
    "http://joda-time.sourceforge.net/api-release/")
  "*Urls of the javadoc to be used. A url will be treated as the absolute path on the local machine, unless starts with `http`."
  :type 'list
  :group 'anything-config)

(defcustom anything-c-javadoc-classes-cache-filename
  (expand-file-name "~/.emacs.d/.anything-c-javadoc-classes-cache.el")
  "*Filename to be used as the cache of the javadocs' all-classes.html contents."
  :type 'file
  :group 'anything-config)

(defvar anything-c-javadoc-classes-candidate-buffer-name
  " *anything javadoc classes*")

(defvar anything-c-source-javadoc-classes
  '((name . "Java docs (classes)")
    (init
     . (lambda ()
         (acjd-initialize-candidate-buffer-maybe
          anything-c-javadoc-classes-candidate-buffer-name
          anything-c-javadoc-classes-cache-filename
          (lambda (d b)
            (acjd-allclasses->any-cand-buffer
             (format "%sallclasses-frame.html" d) b)))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action
     . (("Browse"
         . (lambda (c)
             (browse-url (format "%s%s.html#skip-navbar_top"
                                 (get-text-property 0 'dirname c)
                                 (replace-regexp-in-string "\\." "/" c)))))
        ("Copy class name in kill-ring"
         . (lambda (c) (kill-new (substring-no-properties c))))
        ("Insert class name at point"
         . (lambda (c) (insert (substring-no-properties c))))))))

;; (anything '(anything-c-source-javadoc-classes))

(defun acjd-initialize-candidate-buffer-maybe
    (buffer-name cache-filename make-cand-buffer)
  (when (or current-prefix-arg (not (get-buffer buffer-name)))
    (acjd-initialize-candidate-buffer
     buffer-name cache-filename (acjd-regenerate-cache-p cache-filename)
     make-cand-buffer))
  (anything-candidate-buffer (get-buffer buffer-name)))

(defun acjd-regenerate-cache-p (cache-filename)
  (or (not (file-exists-p cache-filename))
      current-prefix-arg))

(defun acjd-initialize-candidate-buffer
    (any-cand-buffer cache-file regeneratep make-cand-buffer)
  (flet ((cache (cache-file make-cand-buffer)
           (with-temp-buffer
             (loop for d in anything-c-javadoc-dirs
                   do (funcall make-cand-buffer d (current-buffer))
                   finally do
                   (sort-lines nil (point-min) (point-max))
                   ((lambda (buf)
                      (with-temp-buffer
                        (prin1 (with-current-buffer buf
                                 (buffer-substring (point-min) (point-max)))
                               (current-buffer))
                        (write-region (point-min) (point-max) cache-file)))
                    (current-buffer))))))
    (when regeneratep
      (message "Generating javadoc cache...")
      (cache cache-file make-cand-buffer)
      (message "Generating javadoc cache...done."))
    (with-current-buffer (get-buffer-create any-cand-buffer)
      (erase-buffer)
      (let ((b (find-file-noselect cache-file)))
        (unwind-protect
             (insert (with-current-buffer b
                       (goto-char (point-min))
                       (read (current-buffer))))
          (kill-buffer b))))))

(defun acjd-allclasses->any-cand-buffer (filename buf)
  (with-temp-buffer
    (loop initially (acjd-insert-contents filename (current-buffer))
          until (or (eobp) (not (re-search-forward "^<A HREF=\"" nil t)))
          when (looking-at (rx (group (+ nonl)) ".html"
                               (+ nonl) ">" (group (+ nonl)) "</A>" eol))
          do ((lambda (fullname name javadoc-dirname)
                (with-current-buffer buf
                  (insert fullname)
                  (add-text-properties
                   (line-beginning-position) (line-end-position)
                   `(,@'()
                        simple-name ,name
                        dirname     ,javadoc-dirname))
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
              (re-search-forward "^$" nil 'move)
              (funcall k (buffer-substring-no-properties
                          (1+ (point)) (point-max)))
              (kill-buffer))))
         (t (insert-file-contents-literally filename)))
   (delete-trailing-whitespace)
   (goto-char (point-min))))

(defun acjd-fix-url-scheme (filename)
  (if (string-match "^http" filename) filename (concat "file://" filename)))

(provide 'anything-c-javadoc)
;;; anything-c-javadoc.el ends here
