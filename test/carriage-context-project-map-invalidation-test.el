;;; carriage-context-project-map-invalidation-test.el --- Project Map invalidation tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)
(require 'carriage-context)
(require 'carriage-op-file)

(ert-deftest carriage-project-map-invalidated-after-create ()
  "Project Map must reflect newly created files after a file-op create."
  (let* ((root (make-temp-file "carriage-map-create-" t))
         (default-directory root)
         ;; Long TTL to ensure we really depend on explicit invalidation/rebuild.
         (carriage-context-project-map-cache-ttl 3600)
         (carriage-context--project-map-allow-compute t))
    (unwind-protect
        (let ((carriage-context--project-map-allow-compute t))
          ;; Initialize a tiny git repo so git-based listing is allowed.
          (call-process "git" nil nil nil "init" "-q" root)
          (let ((file "new-file.txt"))
            ;; First build: there should be no mention of FILE yet.
            (let* ((before-plist (carriage-context-project-map-build root))
                   (before (or (plist-get before-plist :text) "")))
              (should (plist-get before-plist :ok))
              (should-not (string-match-p (regexp-quote file) before)))
            ;; Create FILE via file-ops API and force invalidation.
            (should (eq (plist-get (carriage-apply-create
                                    (list (cons :file file)
                                          (cons :content "hello\n"))
                                    root)
                                   :status)
                        'ok))
            (carriage-context-project-map-invalidate root)
            ;; After rebuild, the Project Map must contain FILE.
            (let* ((after-plist (carriage-context-project-map-build root))
                   (after (or (plist-get after-plist :text) "")))
              (should (plist-get after-plist :ok))
              (should (string-match-p (regexp-quote file) after)))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest carriage-project-map-invalidated-after-rename-and-delete ()
  "Project Map cache should reflect rename/delete immediately after successful file ops."
  (let* ((root (make-temp-file "carriage-map-rm-" t))
         (default-directory root)
         (carriage-context-project-map-cache-ttl 3600)
         (carriage-context--project-map-allow-compute t))
    (unwind-protect
        (let ((carriage-context--project-map-allow-compute t))
          (call-process "git" nil nil nil "init" "-q" root)
          ;; Create old.txt first.
          (with-temp-file (expand-file-name "old.txt" root)
            (insert "data\n"))
          ;; Initial map must mention old.txt.
          (let* ((before-plist (carriage-context-project-map-build root))
                 (before (or (plist-get before-plist :text) "")))
            (should (plist-get before-plist :ok))
            (should (string-match-p (regexp-quote "old.txt") before)))
          ;; Rename old.txt → new.txt via file-ops API; then invalidate and rebuild.
          (should (eq (plist-get (carriage-apply-rename
                                  (list (cons :from "old.txt")
                                        (cons :to "new.txt"))
                                  root)
                                 :status)
                      'ok))
          (carriage-context-project-map-invalidate root)
          (let* ((after-rename-plist (carriage-context-project-map-build root))
                 (after-rename (or (plist-get after-rename-plist :text) "")))
            (should (plist-get after-rename-plist :ok))
            (should-not (string-match-p (regexp-quote "old.txt") after-rename))
            (should (string-match-p (regexp-quote "new.txt") after-rename)))
          ;; Delete new.txt; invalidate and rebuild; map must no longer mention it.
          (should (eq (plist-get (carriage-apply-delete
                                  (list (cons :file "new.txt"))
                                  root)
                                 :status)
                      'ok))
          (carriage-context-project-map-invalidate root)
          (let* ((after-delete-plist (carriage-context-project-map-build root))
                 (after-delete (or (plist-get after-delete-plist :text) "")))
            (should (plist-get after-delete-plist :ok))
            (should-not (string-match-p (regexp-quote "new.txt") after-delete))))
      (ignore-errors (delete-directory root t)))))

(provide 'carriage-context-project-map-invalidation-test)
;;; carriage-context-project-map-invalidation-test.el ends here
