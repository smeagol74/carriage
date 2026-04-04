;;; carriage-intent-registry.el --- Intent fragments registry  -*- lexical-binding: t; -*-
;; Specifications:
;;   spec/code-style-v2.org
;;   spec/index.org
;;   spec/errors-v2.org
;;   spec/compliance-checklist-v2.org
;;   spec/prompt-profiles-v2.org
;;   spec/parser-registry-v2.org
;;   spec/project-overview-v2.org

(require 'cl-lib)
(require 'subr-x)

(defgroup carriage-prompts nil
  "Prompt composition: intent fragments and suite builder."
  :group 'applications)

(defcustom carriage-intent-fragment-overrides nil
  "Alist of overrides for intent prompt fragments.
Each entry is (INTENT . FRAG), where FRAG is either STRING or FUNCTION (CTX → STRING).
Override takes precedence over the registry defaults."
  :type '(alist :key-type symbol :value-type (choice string function))
  :group 'carriage-prompts)

(defcustom carriage-intent-order '(Ask Code Hybrid MultiPatch)
  "Preferred order of intents for UI selectors."
  :type '(repeat symbol)
  :group 'carriage-prompts)

(defcustom carriage-intent-icons
  '((Ask . "❓")
    (Code . "🧩")
    (Hybrid . "🔀")
    (MultiPatch . "⧉"))
  "Alist mapping intent symbol to an icon string for UI."
  :type '(alist :key-type symbol :value-type string)
  :group 'carriage-prompts)

(defvar carriage--intent-fragments (make-hash-table :test 'eq)
  "Registry of intent fragments: INTENT → FRAG (STRING or FUNCTION (CTX → STRING)).")

(defun carriage-intent-register (intent fragment)
  "Register FRAGMENT for INTENT in the intent registry.
FRAGMENT must be a STRING or a FUNCTION of one argument CTX returning STRING."
  (unless (memq (type-of fragment) '(string cons))
    ;; Allow lambdas/closures as functions; `functionp' covers both.
    (unless (functionp fragment)
      (error "Invalid fragment for intent %S: must be string or function" intent)))
  (puthash intent fragment carriage--intent-fragments)
  t)

(defun carriage-intent-get (intent)
  "Return fragment (STRING or FUNCTION) for INTENT with overrides applied.
The caller is responsible for FUNCALL if the result is a function."
  (let* ((ov (assoc-default intent carriage-intent-fragment-overrides)))
    (cond
     (ov ov)
     ((gethash intent carriage--intent-fragments))
     (t (error "Unknown intent: %S" intent)))))

(defun carriage-intent-known ()
  "Return list of known intent symbols in the registry."
  (let (res)
    (maphash (lambda (k _v) (push k res)) carriage--intent-fragments)
    (nreverse res)))

;; -------------------------------------------------------------------
;; Default fragments (English-only by spec; overrides can replace)

(defun carriage--intent-frag-code (ctx)
  "Default fragment for Intent=Code.
CTX MAY carry: :suite, :context-meta, :profile."
  (let* ((suite (and (listp ctx) (plist-get ctx :suite)))
         (meta (and (listp ctx) (plist-get ctx :context-meta)))
         (om (and (listp meta) (plist-get meta :omitted))))
    (concat
     "OUTPUT: #+begin_patch blocks ONLY. One block = one op. No prose outside blocks.\n"
     (pcase suite
       ((or 'sre 'aibo) "Format: begin_from/begin_to.\n")
       ('udiff "Format: unified diff.\n")
       (_ ""))
     (when (and (integerp om) (> om 0)) (format "Omitted: %s.\n" om)))))

(defun carriage--intent-frag-org-formatting (_ctx)
  "Base formatting rules for answers: Org-mode only (never Markdown)."
  (concat
   "Formatting (Org-mode required):\n"
   "- Answer in VALID Org-mode (not Markdown).\n"
   "- Headings: use '*' / '**' / '***' (never '#', '##', etc.).\n"
   "- Code blocks: use '#+begin_src <lang> :results output' ... '#+end_src' (never triple backticks).\n"
   "- Links: use Org links like [[URL-or-path][label]].\n"
   "- Do not emit Markdown/HTML fences or other markup.\n"))

(defun carriage--intent-frag-hybrid (ctx)
  "Default fragment for Intent=Hybrid."
  (let* ((typed-hint
          (when (plist-get ctx :typedblocks-structure-hint)
            "\nTyped Blocks (v1): MAY use begin_task/analysis/plan/verify/commands/question/answer/context.\n"))
         (org-note (and (listp ctx) (plist-get ctx :org-structure-note))))
    (concat
     (carriage--intent-frag-org-formatting ctx)
     "\n"
     (when (stringp org-note) (concat org-note "\n"))
     "MAY include prose. Tool applies ONLY #+begin_patch blocks.\n"
     "Output patches ONLY when user explicitly asks to modify files.\n"
     "Text missing for edit? Output begin_context block.\n"
     (or typed-hint ""))))

(defun carriage--intent-frag-ask (ctx)
  "Default fragment for Intent=Ask."
  (let* ((typed-on (and (listp ctx) (plist-get ctx :typedblocks-structure-hint))))
    (concat
     (carriage--intent-frag-org-formatting ctx)
     "\n"
     "Do NOT produce begin_patch blocks. Concise answer only.\n"
     (when typed-on
       "\nTyped Blocks (v1): MAY use begin_task/analysis/plan/verify/commands/question/answer/context.\n"))))

;; New intent: MultiPatch
(defun carriage--intent-frag-multipatch (_ctx)
  "Default fragment for Intent=MultiPatch."
  (concat
   (carriage--intent-frag-org-formatting nil)
   "Answer with ONLY one of two legal outputs:\n"
   "- begin_patch blocks for changes grounded in current file text, or\n"
   "- one begin_context block when any required file text is missing.\n"
   "Do NOT include any prose outside blocks.\n"
   "- Use exactly one block per operation.\n"
   "- Paths must be RELATIVE to project root; no absolute paths, no \"..\" segments.\n"
   "- Edit a file ONLY if begin_state_manifest says exists=true and has_text=true.\n"
   "- Delete/rename state-sensitive operations also require exists=true in begin_state_manifest.\n"
   "- If the target file's CURRENT TEXT is not present in this request context, output ONLY begin_context.\n"
   "- If a path is present in begin_map, or begin_state_manifest says exists=true, it MUST be treated as an existing file (so :op create is forbidden for it).\n"
   "- For SRE/AIBO edits, use begin_from/begin_to blocks; do NOT use :from/:to header keys (those are only for :op rename).\n"
   "- Allowed operations depend on Suite. Do not mention formats that are not allowed by the Suite.\n"
   "\n"
   "After printing ALL required begin_patch blocks, decide whether further work is needed:\n"
   "- If more iterations are required, append in this exact order:\n"
   "  1) A #+begin_task block describing the next iteration scope, concrete steps and acceptance criteria.\n"
   "  2) A #+begin_context block with a strict list of file paths (one per line; absolute or repo-root-relative; no comments/labels).\n"
   "- If the work is fully complete and no further iterations are needed, output a single word on a separate line: ГОТОВО\n"
   "\n"
   "Typed Blocks — strict format:\n"
   "- Always use explicit Org typed blocks with '#+begin_<type>' ... '#+end_<type>' markers. Do not emulate blocks with headings, links, drawers, or Markdown fences.\n"
   "- begin_context:\n"
   "  • Strict list of file paths — exactly one path per line.\n"
   "  • Paths are absolute or relative to the repository root.\n"
   "  • No comments, labels or annotations — only paths.\n"))

;; Register defaults
(carriage-intent-register 'Code   #'carriage--intent-frag-code)
(carriage-intent-register 'Hybrid #'carriage--intent-frag-hybrid)
(carriage-intent-register 'Ask    #'carriage--intent-frag-ask)
(carriage-intent-register 'MultiPatch #'carriage--intent-frag-multipatch)

(provide 'carriage-intent-registry)
;;; carriage-intent-registry.el ends here
