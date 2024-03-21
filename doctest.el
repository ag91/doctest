;;; doctest.el --- Doctests for Emacs Lisp -*- lexical-binding: t -*-

;; Authors: Chris Rayner (dchrisrayner@gmail.com), Andrea (andrea-dev@hotmail.com) -- current maintainer
;; Created: Apr 8 2020
;; Keywords: lisp maint docs help
;; URL: https://github.com/ag91/doctest
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "28.1"))
;; Version: 0.1.0

;;; Commentary:

;; These are like a Python "doctest", but for Emacs Lisp and with an Emacs
;; twist. A doctest is a test written inside a docstring.  They look like:
;;
;; >> (+ 1 1)
;; => 2
;;
;; Inline comments are fine, but quotation marks must be escaped:
;;
;; >> (concat nil \"Hello world\")  ; concat should ignore nils
;; => \"Hello world\"               ; ...and return a string
;;
;; Some benefits:
;; - It's a clean way to test elisp code without any heavy dependencies
;; - It encourages functions that are pure or at least side-effect-free
;; - Your unit tests turn into documentation that your users can read!
;;
;; Use ~M-x doctest~ to run doctests on an entire buffer.
;; Use ~M-x doctest-here~ to run the doctest on the current line.
;; Use ~M-x doctest-defun~ to run the current defun's doctests.

;;; Code:

(defvar doctest-input "^>> \\(.*\\)"
  "The regexp for doctest input.
>> (stringp doctest-input)
=> t")
(defvar doctest-output "=> "
  "The regexp for doctest output.
>> (string-suffix-p \" \" doctest-output)
=> t")

(defvar doctest--first-failure nil "Point of first failure, or nil.")
(defvar doctest--text nil "Eventual report to show the user.")
(defvar doctest--fail 0 "Number of tests that have failed.")
(defvar doctest--pass 0 "Number of tests that have passed.")

(defvar doctest-byte-compile-warn-shown-already nil "Show warn only once, if `doctest-byte-compile-warn' set.")

(defcustom doctest-byte-compile-warn t
  "Warn user that tests may cause byte compile warnings."
  :type 'symbol
  :group 'doctest)

(defcustom doctest-message-level nil
  "Print extra information.
Info shows test info while running.
Verbose shows also passing tests."
  :type 'symbol
  :group 'doctest
  :options '(info verbose))

(defvar doctest-after-every-test-functions nil
  "Functions run by Doctest after every test.
Each function is called after a test is executed, but before its
result is printed.  It's given one argument: alist (in no
particular order) with at least the following keys: `result'
(currently always either symbol `pass' or `failure'), `expected'
and `actual' (both strings).  The alist may be expanded with more
keys in future.")

(defvar doctest-after-all-tests-hook nil
  "Hook run by Doctest after all planned test.
Executed after the tests are finished, but before summary is
printed.")

(defun doctest (&optional filename)
  "Run doctest on current buffer, or FILENAME if given.
When run interactively, the point will move to the site of the
first test failure (or the first syntax error in a test).

A living example:
>> (cons (list 6 'quoted :symbol 12345 \"A string\") (+ 0 8310247))
=> ((6 quoted :symbol 12345 \"A string\") . 8310247)

This function sends its report using `send-string-to-terminal' if
`noninteractive' is non-nil, otherwise it simply uses `message'."
  (interactive)
  (when filename (set-buffer (find-file filename)))
  (doctest--reset-state)
  (save-excursion
    (goto-char (point-min))
    (while (ignore-errors (goto-char (doctest--next-test)))
      (doctest-here)))
  (run-hooks 'doctest-after-all-tests-hook)
  (let ((tally (format "%s passed, %s failed" doctest--pass doctest--fail)))
    (cond (doctest--first-failure
           (goto-char doctest--first-failure)
           (doctest--message (format "%s\nTest run summary:\n%s" tally doctest--text)))
          (t (doctest--message tally)))))

(defun doctest-byte-compile-warn ()
  "Alert user of doctest that test cases in comments can cause byte compiler warnings."
  (when (and
         doctest-byte-compile-warn
         (not doctest-byte-compile-warn-shown-already)
         (or byte-compile-warnings
             (not (seq-contains-p byte-compile-warnings '(not docstrings)))))
    (setq doctest-byte-compile-warn-shown-already t)
    (warn "Test cases in comments can cause byte-compile warnings, please add '(not docstrings) to `byte-compile-warnings'")))

(defun doctest-here (&optional interactively)
  "Run the test that the point is currently on.
If called INTERACTIVELY, let the user know the test passed and
move the point down two lines (possibly onto the next test).
Internally, the doctest input line is evaluated with `eval' and
normalized into its `princ' form, while the output line is
normalized into its `princ' form without being evaluated."
  (interactive "p")
  (doctest-byte-compile-warn)
  (cond
   ((looking-at doctest-input)
    (let* ((input (doctest-unescape
                   (string-trim
                    (buffer-substring-no-properties
                     (save-excursion (- (search-forward "(" nil t) 1))
                     (save-excursion (- (search-forward (concat "\n" doctest-output) nil t)
                                        (length doctest-output)))))))
           (evaluated-input (string-trim
                             (condition-case err
                                 (format "%S" (eval (car (read-from-string input))))
                               (t
                                (format "Failure in evaluating input caused:\n%s\n\n" err)))))

           (output (and
                    (progn
                      (search-forward doctest-output nil t)
                      (not (beginning-of-line)))
                    (doctest--target-output)))
           (output (string-trim (format "%S" (car (read-from-string output)))))
           (result (if (string= evaluated-input output) 'pass 'failure)))
      (when doctest-after-every-test-functions
        (run-hook-with-args 'doctest-after-every-test-functions
                            `((result   . ,result)
                              (expected . ,output)
                              (actual   . ,evaluated-input))))
      (if interactively
          (doctest--here-interactively input evaluated-input output result)
        (doctest--here-noninteractively input evaluated-input output result))))
   (t (message "No doctest here."))))

(defun doctest-defun ()
  "Run `doctest' on the current defun.
This defun is the one that contains point or follows point,
determined by calling `narrow-to-defun'."
  (interactive)
  (narrow-to-defun)
  (eval-buffer)
  (condition-case err
      (progn (doctest) (widen))
      (error (progn (widen) (signal (car err) (cdr err))))))

(defun doctest--message (str)
  "Display STR or send string to terminal if `noninteractive'.
`message' expects format strings and has to be accommodated;
`send-string-to-terminal' has a newline added to the end."
  (if (not noninteractive)
      (message "%s" str)
    (send-string-to-terminal (concat str "\n"))))

(defun doctest--target-output ()
  "Read and return the target output on the current line.
This is one or many lines beginning with `doctest-output'."
  (when (looking-at doctest-output)
    (doctest-unescape
     (let ((bound (save-excursion (or (ignore-errors (end-of-defun)) (end-of-line)) (point))))
       (buffer-substring-no-properties
        (+ (point) (length doctest-output))
        (or
         (when (string-prefix-p (concat doctest-output "(") (thing-at-point 'line 'no-prop))
           (save-excursion (search-forward ")\"" bound 'no-error)))
         (save-excursion (and (re-search-forward doctest-input bound 'no-error) (beginning-of-line) (point)))
         (save-excursion (ignore-errors (- (re-search-forward "\n\n" bound) (length "\n\n"))))
         (save-excursion (ignore-errors (- (search-forward "\"\n " bound) (length "\"\n "))))
         (save-excursion (ignore-errors (- (re-search-forward "\"" bound) 1)))))))))

(defun doctest--here-interactively (sexp actual-value target-value result)
  "Compare ACTUAL-VALUE (generated by SEXP) to TARGET-VALUE.
Let the user know the test passed and move to the next line."
  (cond ((eq result 'failure)
         (message "%s => %s but got %s" sexp target-value actual-value))
        (t (forward-line 1) (message "Pass!"))))

(defun doctest--here-noninteractively (sexp actual-value target-value result)
  "Compare ACTUAL-VALUE (generated by SEXP) to TARGET-VALUE.
Call `doctest--append' to append to the running test output."
  (cond ((eq result 'failure)
         (setq doctest--fail (1+ doctest--fail))
         (setq doctest--first-failure (or doctest--first-failure (point)))
         (let ((output (format "%s.el#%s: %s => %s but got %s"
                               (or
                                (ignore-errors (file-name-base (buffer-file-name)))
                                (buffer-name))
                               (line-number-at-pos)
                               sexp target-value actual-value)))
           (when (equal 'info doctest-message-level) (doctest--message output))
           (doctest--append output)))
        (t
         (let ((output (format "%s.el#%s: %s => %s passed"
                               (or
                                (ignore-errors (file-name-base (buffer-file-name)))
                                (buffer-name))
                               (line-number-at-pos)
                               sexp target-value)))
           (when (equal 'info doctest-message-level)
             (doctest--message output))
           (when (equal 'verbose doctest-message-level) (doctest--append (concat "\n" output))))
         (setq doctest--pass (1+ doctest--pass)))))

(defun doctest--reset-state ()
  "Reset doctest's current state."
  (when (eq major-mode 'emacs-lisp-mode) (eval-buffer))
  (setq doctest--text nil
        doctest--fail 0
        doctest--pass 0
        doctest--first-failure nil))

(defun doctest--next-test ()
  "Return the point where the next test begins -- else nil.
>> (eq (doctest--next-test) (point))
=> t"
  (declare (side-effect-free t))
  (let (doctest-point)
    (save-excursion
      (while (and (not doctest-point)
                  (re-search-forward doctest-input nil t))
        (and (nth 3 (syntax-ppss))       ; in a string
             (zerop (forward-line 1))    ; ...with a next line
             (nth 3 (syntax-ppss))       ; ...also in a string
             (save-excursion (search-forward doctest-output)) ; ...with doctest output
             (zerop (forward-line -1))
             (setq doctest-point (point)))))
    doctest-point))

(defun doctest--append (str)
  "Append STR to `doctest-text' with a newline if necessary."
  (setq doctest--text (concat doctest--text (unless doctest--text "\n") str)))

(defun doctest-unescape (str)
  "Remove all backslashes from STR.
It's open work to parse/handle backslashes cleanly, so ignore them.
>> (doctest-unescape \"back\\\\slash\")
=> \"backslash\"

>> (doctest-unescape \"let\nnewlines\nbe\")
=> \"let\nnewlines\nbe\""
  (declare (side-effect-free t) (pure t))
  (replace-regexp-in-string "\\\\\\(.\\|\n\\)" "\\1" str))

(defun doctest-escape-test-strings (string)
  "Escape any \" in STRING.
When called interactively that would be the active region.

>> (doctest-escape-test-strings \"\\\"hello\\\"\")
=> \"\\\"\\\\\\\"hello\\\\\\\"\\\"\""
  (interactive
   (list (when (region-active-p)
           (buffer-substring-no-properties
            (caar (region-bounds))
            (cdar (region-bounds))))))
  (if (region-active-p)
      (let* ((begin (caar (region-bounds)))
             (end (cdar (region-bounds)))
             (string (buffer-substring-no-properties
                      begin
                      end))
             (result (prin1-to-string string)))
        (save-excursion
          (goto-char begin)
          (delete-region begin end)
          (insert (substring result 1 -1)))
        result)
    (prin1-to-string string)))

(provide 'doctest)
;;; doctest.el ends here

;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; End:
