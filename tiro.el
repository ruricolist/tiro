;;; tiro.el --- Modal shorthand  -*- lexical-binding: t -*-

;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Paul M. Rodriguez

;; Author: Paul M. Rodriguez <pmr@ruricolist.com>
;; Version: 1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(provide 'tiro)

(require 'cl-lib)
(require 'cl-format)
(require 'thingatpt)
(require 'ispell)

(defgroup tiro nil
  "Modal shorthand"
  :prefix "tiro-"
  :group 'abbrev
  :tag "Tiro")

(defcustom tiro-dictionary
  (or ispell-complete-word-dict
      ispell-alternate-dictionary)
  "The dictionary Tiro should use to check for expansions."
  :group 'tiro
  :type 'file)

(defcustom tiro-spell-out-below 20
  "Spell out numbers less than this."
  :group 'tiro
  :type '(choice
          (const :tag "Don't spell out numbers" 0)
          integer))

(defcustom tiro-cursor 'hbar
  "Cursor style to use when Tiro is in effect."
  :group 'tiro
  :type '(choice
          (const box)
          (const hollow)
          (const :tag "No cursor" nil)
          (const :tag "Vertical bar" bar)
          (const :tag "Horizontal bar" hbar)))

(defcustom tiro-file (expand-file-name ".tiro" user-emacs-directory)
  "File that holds Tiro abbreviations."
  :type 'file
  :group 'tiro)

(defcustom tiro-ok-faces '(default font-lock-comment-face font-lock-doc-face font-lock-string-face)
  "Faces where it is OK to expand.

Note that expansion always takes place where no face is
specified, and that `default' is always ignored in modes that
derive from `prog-mode'."
  :type '(repeat face)
  :group 'tiro)

(defcustom tiro-suffixes
  '(("mt" "ment")
    ("mt" "ement")
    ("n" "ion")
    ("c" "ic")
    ("l" "al")
    ("n" "en")
    ("bl" "ible")
    ("bly" "ably")
    ("bly" "ibly")
    ("blty" "ibility")
    ("bl" "able")
    ("blty" "ability")
    ("z" "ize")
    ("nal" "ional")
    ("g" "ing")
    ("g" "eing")
    ("g" "ying")
    ("gly" "ingly")
    ("d" "ed")
    ("r" "er")
    ("er" "er")
    ("or" "or")
    ("u" "ual")
    ("u" "ually")
    ("ly" "ely")
    ("ly" "ly")
    ("dly" "edly")
    ("ys" "ies")
    ("nt" "ient")
    ("nt" "ant")
    ("ry" "ory")
    ("ry" "ary")
    ("ry" "iary")
    ("ry" "ery")
    ("a" "ay"))
  "List of suffixes and what they expand to."
  :group 'tiro
  :type '(repeat (list string string)))

(eval-and-compile
  (defconst tiro-syntax-table
    (let ((st (make-syntax-table text-mode-syntax-table)))
      (modify-syntax-entry ?\' "w" st)
      (modify-syntax-entry ?\& "w" st)
      (modify-syntax-entry ?\’ "w" st)
      st)))

(eval-and-compile
  (unless (fboundp 'when-let)
    (cl-defmacro if-let ((var expr) then &body else)
      `(let ((,var ,expr))
         (if ,var
             ,then
           ,@else)))
    (cl-defmacro when-let ((var expr) &body body)
      `(if-let (,var ,expr)
               (progn ,@body)))))

(cl-defmacro with-tiro-syntax (&body body)
  `(with-syntax-table tiro-syntax-table
     ,@body))

(defun tiro-cmd (next)
  (lambda ()
    (interactive)
    (let ((cont
           (catch 'tiro-cancel
             (unless (eq last-command 'undo)
               (when (tiro-face-ok?)
                 (let ((expansion (tiro-expand (tiro-last-word))))
                   (when expansion
                     (tiro-sub-last-word expansion)))))
             (when next
               (call-interactively next))
             nil)))
      (when (functionp cont)
        (funcall cont)))))

(defun tiro-cancel (cont)
  (throw 'tiro-cancel cont))

(defun tiro-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(cl-defun tiro-face-ok? (&optional (face (face-at-point)))
  (or (null face)
      (memq face
            (if (tiro-prog-mode-p)
                (remq 'default tiro-ok-faces)
              tiro-ok-faces))))

(defun tiro-last-word ()
  (unless (eq (char-before) ?\ )
    (save-excursion
      (let ((end (point)))
        (with-tiro-syntax
         (when (not (zerop (skip-syntax-backward "w")))
           (buffer-substring (point) end)))))))

(defun tiro-upper-case-p (c)
  (not (eql c (downcase c))))

(defun tiro-sub-last-word (sub)
  (save-excursion
    (when (with-tiro-syntax
           (re-search-backward "\\W\\(\\w+\\)" nil t))
      (let* ((orig (match-string 1))
             (cap (tiro-upper-case-p (aref orig 0)))
             (sub (if cap (capitalize sub) sub)))
        (replace-match sub t t nil 1)))))

(defun tiro-memoize (function)
  (let ((cache (make-hash-table :test 'equal))
        (no-value-marker (make-symbol "no-value")))
    (lambda (&rest args)
      (let ((value (gethash args cache no-value-marker)))
        (if (eq value no-value-marker)
            (setf (gethash args cache)
                  (apply function args))
          value)))))

(defun tiro-lookup-word (word)
  (let ((process-connection-type nil))
    (zerop (call-process "look" nil nil nil
                         ispell-look-options
                         word tiro-dictionary))))

(defalias 'tiro-real-word-p
  (let ((lookup-word
         (tiro-memoize #'tiro-lookup-word)))
    (lambda (word)
      (or (funcall lookup-word word)
          (tiro-word-in-buffer-p word)))))

(defun tiro-word-in-buffer-p (word)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      ;; At least twice, counting the word we're trying to expand.
      (word-search-forward word nil t 2)
      (not (= (point) (point-min))))))

(defconst tiro-letters-by-frequency
  (eval-when-compile
    (string-to-list "etaoinshrdlucmfwypvbgkqjxz"))
  "Letter frequency, Linotype style.")

(defun tiro-char< (c1 c2)
  (if (eq c1 c2)
      nil
    (memq c2 (cdr (memq c1 tiro-letters-by-frequency)))))

(defun tiro-str< (s1 s2)
  (cl-some #'tiro-char< s1 s2))

(defun tiro-snarf-file (file)
  (with-temp-buffer
    (let ((buffer-undo-list t))
      (when (file-exists-p file)
        (insert-file-contents file))
      (buffer-string))))

(defun tiro-lines (string)
  "Return a list of lines in STRING."
  (split-string string "\n" t))

(defun tiro-snarf-expansions ()
  (with-temp-buffer
    (insert-file-contents tiro-file)
    (cl-loop
     with ht = (make-hash-table :test #'equal)
     for beg = (line-beginning-position)
     for end = (line-end-position)
     for line = (buffer-substring beg end)
     for (abbr . expansion) = (split-string line)
     do (setf (gethash abbr ht)
              (mapconcat 'identity expansion " "))
     while (zerop (forward-line 1))
     finally (return ht))))

(defconst tiro-expansions (tiro-snarf-expansions))

(defun tiro-update ()
  "Update list of expansions from tiro file."
  (interactive)
  (setq tiro-expansions (tiro-snarf-expansions)))

(defun tiro-word-at-point ()
  (with-tiro-syntax
   (downcase (word-at-point))))

(defun tiro-add (abbr)
  (interactive (list (tiro-word-at-point)))
  (let* ((default (tiro-raw-expansion abbr))
         (expansion (read-string (format "Expansion for `%s': " abbr) default)))
    (unless (> (length expansion) 0)
      (setf expansion abbr))
    (with-temp-buffer
      (insert-file-contents tiro-file)
      (goto-char (point-min))
      (save-excursion
        (delete-matching-lines (format "^%s " abbr)
                               (point-min)
                               (point-max)))
      (insert (format "\n%s %s\n" abbr expansion))
      (sort-lines nil (point-min) (point-max))
      (goto-char (point-min))
      (delete-blank-lines)
      (let ((require-final-newline nil))
        (write-file tiro-file nil)))
    (tiro-update)))

(defun tiro-toggle-spell-out-numbers ()
  (interactive)
  (let ((n tiro-spell-out-below))
    (set (make-local-variable 'tiro-spell-out-below)
         (if (zerop n)
             100
           0))))

(defun tiro-spell-out-number (string)
  (when (featurep 'cl-format)
    (save-excursion
      (unless (string-match-p "^00+$" string) ;All zeroes.
        (let ((n (string-to-number string)))
          (if (< n tiro-spell-out-below)
              ;; TODO Check for the beginning of a sentence.
              (cl-format nil "~r" n)))))))

(defconst tiro-prefixes
  (sort '("un" "anti" "in" "re" "per" "trans" "epi" "super" "hyper" "non" "inter")
        (lambda (x y)
          (> (length x) (length y)))))

(defun tiro-split-word (word)
  (if (tiro-real-word-p word)
      (list nil word nil)
    (let (prefix root suffix)
      (cl-loop for p in tiro-prefixes
               when (string-prefix-p p word)
               do (cl-return
                   (setf prefix p
                         word (substring word (length p)))))
      (let* ((case-fold-search t)
             (suffixp (string-match "^\\(.+?\\)\\(e?d\\|e?s\\)$" word)))
        (setf root (if suffixp (match-string 1 word) word)
              suffix (and suffixp (match-string 2 word))))
      (list prefix root suffix))))

;;; cl-digit-char-p is a recent addition.
(defalias 'tiro-digit-char-p
  (if (fboundp 'cl-digit-char-p)
      #'cl-digit-char-p
    (lambda (char &optional _rax)
      (cl-case char
        (?0 0)
        (?1 1)
        (?2 2)
        (?3 3)
        (?4 4)
        (?5 5)
        (?6 6)
        (?7 7)
        (?8 8)
        (?9 9)
        (t nil)))))

(defun tiro-raw-expansion (word)
  (gethash word tiro-expansions))

(defun tiro-pick-expansion (word)
  (let ((exp (tiro-raw-expansion word)))
    (when exp
      (if (not (string-match-p "," exp))
          exp
        (tiro-choose (split-string exp "," t))))))

(defun tiro-backspace ()
  (throw 'tiro-cancel
         (lambda ()
           (delete-char -1))))

(cl-defun tiro-format-prompt (choices)
  (let ((i -1))
    (concat
     (mapconcat
      (lambda (choice)
        (format "%d=%s" (cl-incf i) choice))
      choices
      ",")
     ": ")))

(defun tiro-gcp (strings)
  "The greatest common prefix of STRINGS.
If there is no common prefix, return `nil'."
  (cl-flet ((gcp
             (x y)
             (let ((miss (cl-mismatch x y)))
               (cond ((not miss) x)
                     ((> miss 0) (substring x 0 miss))
                     (t nil)))))
    (cl-block nil
      (cl-reduce
       (lambda (x y)
         (or (gcp x y)
             (cl-return)))
       strings))))

(cl-defun tiro-choose (choices)
  (let ((len (length choices)))
    (when (<= len 1)
      (cl-return-from tiro-choose
        (first choices)))
    (when (> len 9)
      (error "Too many choices in %s" choices))
    (cl-flet ((short-choices
               (choices)
               "Return an alist of (unique-suffix . choice)."
               (let ((gcp (tiro-gcp choices)))
                 (if (null gcp) (cl-mapcar #'cons choices choices)
                   (cl-loop for choice in choices
                            for suffix = (substring choice (length gcp))
                            collect (cons suffix choice))))))
      (cl-loop for prompt = (tiro-format-prompt choices)
               for char = (read-char prompt)
               ;; If the char is a digit, use it as an index. If the
               ;; char is not a digit, try to use it as a filter,
               ;; leaving only the choices that include that character.
               ;; If there is only one such choice, just return it.
               do (if-let (digit (tiro-digit-char-p char 10))
                      (when-let (choice (nth digit choices))
                        (cl-return choice))
                    (case char
                      ;; Space means the first choice.
                      (32 (cl-return (first choices)))
                      ;; Clear on backspace.
                      (127 (tiro-backspace))
                      (t (let* ((short-choices (short-choices choices))
                                (filtered-choices
                                 ;; If every choice has the same
                                 ;; prefix, only filter by the
                                 ;; disjoint suffixes.
                                 (mapcar #'cdr
                                         (cl-remove-if-not
                                          (lambda (suffix.choice)
                                            (cl-find char (car suffix.choice)))
                                          short-choices))))
                           (cond ((null filtered-choices))
                                 ((null (cdr filtered-choices))
                                  (cl-return (car filtered-choices)))
                                 (t (setf choices filtered-choices)))))))))))

(defun tiro-fixup-suffix (expansion)
  (cond ((string-suffix-p "ys" expansion)
         (concat (substring expansion 0 -2) "ies"))
        (t expansion)))

(defun tiro-expand (word)
  (when word
    (let ((word (downcase word)))
      (or (tiro-pick-expansion word)
          (and (cl-every #'tiro-digit-char-p word)
               (tiro-spell-out-number word))
          (cl-destructuring-bind (prefix root suffix)
              (tiro-split-word word)
            (or (and (or prefix suffix)
                     (let ((expansion (tiro-pick-expansion root)))
                       (and expansion
                            ;; E.g. frequencys -> frequencies
                            (tiro-fixup-suffix
                             (concat prefix expansion suffix)))))
                (let ((expansion/suffix
                       (tiro-try-expand/suffixes
                        (if suffix root word))))
                  (and expansion/suffix
                       (if suffix
                           (concat expansion/suffix suffix)
                         expansion/suffix)))))))))

(defun tiro-expand-suffix (word suffix expansion)
  (let* ((len (length suffix))
         (root (substring word 0 (- len))))
    (tiro-fixup-suffix
     (concat (or (tiro-pick-expansion root)
                 root)
             expansion))))

(defun tiro-try-expand/suffixes (word)
  (unless (tiro-real-word-p word)
    (cl-loop for (suffix expansion) in tiro-suffixes
             for try = (and (string-suffix-p suffix word)
                            (tiro-expand-suffix word suffix expansion))
             if (and try (tiro-real-word-p try))
             do (cl-return try))))

(defun tiro-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map [t] nil)
    (with-syntax-table tiro-syntax-table
      (cl-loop for c from 32 below 127
               unless (eql (char-syntax c) ?\w)
               do (define-key map (vector c)
                    (tiro-cmd (key-binding (vector c))))))
    map))

;;;###autoload
(define-minor-mode tiro-mode
  "Modal shorthand."
  :lighter " ⁊"
  (if tiro-mode
      (progn
        (push (cons 'tiro-mode (tiro-keymap))
              minor-mode-overriding-map-alist))
    (progn
      (pop minor-mode-overriding-map-alist))))
