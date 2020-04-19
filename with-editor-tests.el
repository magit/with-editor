;;; with-editor-tests.el --- unit tests for with-editor.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file.  If not,
;; see https://github.com/magit/with-editor/blob/master/AUTHORS.md.

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Unit tests for with-editor.el.

;;; Code:

(require 'with-editor)

(require 'cl-lib)
(require 'ert)
(require 'macroexp)
(require 'map)
(require 'nadvice)
(require 'pcase)

(defmacro with-editor-tests--mock (mocks &rest body)
  "Install MOCKS within BODY.
MOCKS is a list of (SYMBOL FUNCTION) pairs."
  (declare (debug (listp body)) (indent 1))
  (if mocks
      (cl-destructuring-bind ((symbol function) &rest rest) mocks
        (macroexp-let2 nil function function
          `(progn
             (advice-add #',symbol :override ,function '((depth . 100)))
             (unwind-protect
                 (with-editor-tests--mock ,rest ,@body)
               (advice-remove #',symbol ,function)))))
    (macroexp-progn body)))

(ert-deftest with-editor/make-process ()
  (let* ((default-directory "/ssh:host:/dir/")
         (with-editor-sleeping-editor "sleeping-editor")
         (filter-args nil)
         (filter (lambda (&rest args) (push args filter-args)))
         (calls nil))
    (with-editor-tests--mock
        ((make-process
          (lambda (&rest args)
            ;; We only keep the “interesting” (non-nil) arguments.
            (push (cl-sort (rassq-delete-all nil (map-pairs args))
                           #'string-lessp :key #'car)
                  calls)
            :fake-process))
         (process-put #'put)
         (process-get #'get))
      (with-editor
        (ert-info ("no file handlers")
          (should (eq (make-process :name "name"
                                    :command '("true")
                                    :filter filter)
                      :fake-process))
          (should (equal calls `(((:command "true")
                                  (:filter . ,filter)
                                  (:name . "name")))))
          (should-not (symbol-plist :fake-process)))
        (setq calls nil)
        (ert-info ("default filter")
          (should (eq (make-process :name "name"
                                    :command '("true")
                                    :file-handler t)
                      :fake-process))
          (should (equal calls
                         '(((:command "env" "EDITOR=sleeping-editor" "true")
                            (:file-handler . t)
                            (:filter . with-editor-process-filter)
                            (:name . "name")))))
          (should (equal (symbol-plist :fake-process)
                         '(default-dir "/ssh:host:/dir/"))))
        (setq calls nil)
        (ert-info ("custom filter")
          (should (eq (make-process :name "name"
                                    :command '("true")
                                    :filter filter
                                    :file-handler t)
                      :fake-process))
          (pcase calls
            (`(((:command "env" "EDITOR=sleeping-editor" "true")
                (:file-handler . t)
                (:filter . ,filter)
                (:name . "name")))
             (funcall filter :fake-process "output"))
            (other (ert-fail (list "Unexpected calls" other))))
          (should (equal filter-args '((:fake-process "output"))))
          (should (equal (symbol-plist :fake-process)
                         '(default-dir "/ssh:host:/dir/"))))))))

;;; with-editor-tests.el ends here
