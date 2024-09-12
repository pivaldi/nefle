;;; test.el --- Unit tests nefle -*- lexical-binding: t; -*-

(load (concat (concat (file-name-directory (buffer-file-name)) "nefle.el")))

(ert-deftest test-nefle-auto-ok ()
  (let
      ((test
        (make-ert-test
         :body
         (lambda ()
           ;; Auto generated tests that should find a directory.
           (let* ((test-dir (concat (file-name-directory (buffer-file-name)) "tests/"))
                  (cur-dir test-dir)
                  (passed '()))
             (dolist (dir-depth (number-sequence 1 4))
               (let ((dir (make-string dir-depth ?a)) result)
                 (setq cur-dir (concat cur-dir dir "/"))
                 (setq result test-dir)
                 ;; (insert cur-dir  "\n")
                 (dolist (f-depth (number-sequence 1 dir-depth))
                   (let* ((fname (concat (make-string f-depth ?a) "-f"))
                          (result-name (make-string f-depth ?a))
                          (found (nefle-get-up-dir-containing-file fname cur-dir)))
                     (setq result (file-name-as-directory (concat result result-name)))
                     ;; (when nefle-rt ;; C-x C-e compatible :)
                     ;;   (debug "Search " fname " in " cur-dir "\n")
                     ;;   (debug (format " Result => %s" (nefle-get-up-dir-containing-file fname cur-dir)))
                     ;;   (debug " Should be ===> " result "\n"))
                     (if (string= result found) (setq passed (append passed '(t)))
                       (ert-fail (format "Unit test failed -- Result is '%s' -- Should be '%s'" found result)))))))
             (ert-pass))))))
    (let ((result (ert-run-test test)))
      (should (ert-test-failed-p result)))))

(ert-deftest test-nefle-manual-ko ()
  (let
      ((test
        (make-ert-test
         :body
         (lambda ()
           ;; Manual tests
           (let* ((passed '())
                  (tests '((nil . ("a" . "a"))
                           (nil . ("" . "a"))
                           (nil . ("none" . "a"))
                           (nil . ("" . ""))
                           (nil . ("../nefle.el" . ""))
                           (nil . ("a/aa" . "a/aa"))
                           (nil . ("." . "a"))
                           (nil . (".." . "a"))
                           (nil . ("af" . "a/ab"))))
                  (c-dir (file-name-directory (buffer-file-name)))
                  (test-dir (concat c-dir "tests/")))
             (dolist (test tests)
               (let* ((expected-root-dir (pop test))
                      (expected (when expected-root-dir
                                  (file-truename (file-name-as-directory (concat test-dir expected-root-dir)))))
                      (fname (car test))
                      (search-dir (concat test-dir (cdr test)))
                      (result (nefle-get-up-dir-containing-file fname search-dir c-dir)))
                 (if expected
                     (if (string= expected result)
                         (setq passed (append passed '(t)))
                       (ert-fail (format "Unit test failed -- Result is '%s' -- Should be '%s'" result expected)))
                   (if (null result)
                       (setq passed (append passed '(t)))
                     (ert-fail (format "Unit test failed -- %s -- Result is '%s' -- Should be nil'" passed result)))))))
           (ert-pass)))))
    (let ((result (ert-run-test test)))
      (should (ert-test-failed-p result)))))

;; (apropos-internal "" #'ert-test-boundp)
;; (unless (= 2 (ert--stats-passed-expected (ert-run-tests 't (lambda (&rest args)))))
;;   (error "Test failed"))

(unless (ert-test-passed-p (ert-run-test (ert-get-test 'test-nefle-auto-ok)))
  (error "Test test-nefle-auto-ok failed"))

(unless (ert-test-passed-p (ert-run-test (ert-get-test 'test-nefle-manual-ko)))
  (error "Test test-nefle-manual-ko failed"))

(provide 'test)
;;; test.el ends here
