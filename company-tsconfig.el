;;; company-tsconfig.el --- Company-mode completion backend for tsconfig files -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/company-tsconfig
;; Version: 0.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "27.1") (company "0.9.13"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Company-mode completion backend and eldoc support for tsconfig files.

;;; Code:

(require 'company)
(require 'cl-lib)

(defcustom company-tsconfig-enable-eldoc t
  "Whether to setup eldoc functions."
  :group 'company-tsconfig
  :type 'boolean)

(eval-and-compile
  (require 'cc-mode))

(defvar company-tsconfig-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?`"\"" table)
    table)
  "Syntax table for command `company-tsconfig'.")

(defmacro company-tsconfig-with-syntax-table (&rest body)
  "Evaluate BODY with syntax table set to `company-tsconfig-syntax-table'."
  `(with-syntax-table company-tsconfig-syntax-table
     ,@body))

(defvar company-tsconfig-spec
  '("files"
    "extends"
    "include"
    "exclude"
    "references"
    ("compilerOptions"
     ("allowUnreachableCode"
      "allowUnusedLabels"
      "alwaysStrict"
      "exactOptionalPropertyTypes"
      "noFallthroughCasesInSwitch"
      "noImplicitAny"
      "noImplicitOverride"
      "noImplicitReturns"
      "noImplicitThis"
      "noPropertyAccessFromIndexSignature"
      "noUncheckedIndexedAccess"
      "noUnusedLocals"
      "noUnusedParameters"
      "strict"
      "strictBindCallApply"
      "strictFunctionTypes"
      "strictNullChecks"
      "strictPropertyInitialization"
      "useUnknownInCatchVariables"
      "allowUmdGlobalAccess"
      "baseUrl"
      "module"
      "moduleResolution"
      "moduleSuffixes"
      "noResolve"
      "paths"
      "resolveJsonModule"
      "rootDir"
      "rootDirs"
      "typeRoots"
      "types"
      "declaration"
      "declarationDir"
      "declarationMap"
      "downlevelIteration"
      "emitBOM"
      "emitDeclarationOnly"
      "importHelpers"
      "importsNotUsedAsValues"
      "inlineSourceMap"
      "inlineSources"
      "mapRoot"
      "newLine"
      "noEmit"
      "noEmitHelpers"
      "noEmitOnError"
      "outDir"
      "outFile"
      "preserveConstEnums"
      "preserveValueImports"
      "removeComments"
      "sourceMap"
      "sourceRoot"
      "stripInternal"
      "allowJs"
      "checkJs"
      "maxNodeModuleJsDepth"
      "disableSizeLimit"
      "plugins"
      "allowSyntheticDefaultImports"
      "esModuleInterop"
      "forceConsistentCasingInFileNames"
      "isolatedModules"
      "preserveSymlinks"
      "charset"
      "keyofStringsOnly"
      "noImplicitUseStrict"
      "noStrictGenericChecks"
      "out"
      "suppressExcessPropertyErrors"
      "suppressImplicitAnyIndexErrors"
      "emitDecoratorMetadata"
      "experimentalDecorators"
      "jsx"
      "jsxFactory"
      "jsxFragmentFactory"
      "jsxImportSource"
      "lib"
      "moduleDetection"
      "noLib"
      "reactNamespace"
      "target"
      "useDefineForClassFields"
      "diagnostics"
      "explainFiles"
      "extendedDiagnostics"
      "generateCpuProfile"
      "listEmittedFiles"
      "listFiles"
      "traceResolution"
      "composite"
      "disableReferencedProjectLoad"
      "disableSolutionSearching"
      "disableSourceOfProjectReferenceRedirect"
      "incremental"
      "tsBuildInfoFile"
      "noErrorTruncation"
      "preserveWatchOutput"
      "pretty"
      "skipDefaultLibCheck"
      "skipLibCheck"
      "assumeChangesOnlyAffectDirectDependencies"))
    ("watchOptions" ("watchFile"
                     "watchDirectory"
                     "fallbackPolling"
                     "synchronousWatchDirectory"
                     "excludeDirectories"
                     "excludeFiles"))
    ("typeAcquisition"
     ("enable"
      "include"
      "exclude"
      "disableFilenameBasedTypeAcquisition"))))

(defvar company-tsconfig-top-level-fields (mapcar (lambda (it)
                                                    (if (listp it)
                                                        (car it)
                                                      it))
                                                  company-tsconfig-spec))

(defvar company-tsconfig-all-keys (flatten-list company-tsconfig-spec))

(defvar-local company-tsconfig-tsc-help-string nil)
(defvar-local company-tsconfig-properties-alist nil)


(defvar company-tsconfig-property-re
  (concat "\\_<" (regexp-opt
                  (seq-filter #'stringp
                              company-tsconfig-all-keys)
                  t)
          "\\_>"))

(defvar company-tsconfig-property-value-regexp
  "\"\\([a-z]+\\)\":[\s\t\n]+\\([a-z.]*\\)"
  "A regular expression matching property.")

(defun company-tsconfig-grab-property ()
  "Return the first matching normalized property from the list of all keys."
  (when-let* ((prefix (company-grab company-tsconfig-property-value-regexp)))
    (car (member (company-tsconfig-normalize-option prefix)
                 company-tsconfig-all-keys))))

(defun company-tsconfig-bounds-by-chars (&optional chars)
  "Return bounds of thing at point if it is match CHARS.
CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)."
  (unless chars (setq chars "-'*\"_~$A-Za-z0-9:.#\\+"))
  (save-excursion
    (let* ((a (save-excursion
                (skip-chars-backward chars)
                (point)))
           (b (save-excursion
                (skip-chars-forward chars)
                (point))))
      (if (string-blank-p (buffer-substring-no-properties a b))
          nil
        (cons a b)))))


(defun company-tsconfig-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Return stdout output if command existed with zero status, nil otherwise."
  (let ((buff (generate-new-buffer command)))
    (with-current-buffer buff
      (let ((status (apply #'call-process command nil t nil
                           (flatten-list args))))
        (let ((result (string-trim (buffer-string))))
          (if (= 0 status)
              (prog1 result (kill-current-buffer))
            (message result) nil))))))

(defun company-tsconfig-parse-help ()
  "Parse output from tsc help string."
  (let ((lines (split-string company-tsconfig-tsc-help-string "\n" t))
        (result))
    (setq lines (seq-drop-while (lambda (it)
                                  (not (string-match-p "^--" it)))
                                lines))
    (dolist (line lines)
      (cond ((string-prefix-p "--" line)
             (let* ((parts (split-string line nil t))
                    (arg (replace-regexp-in-string "," ""
                                                   (substring-no-properties
                                                    (pop
                                                     parts)
                                                    2)))
                    (cell
                     (seq-remove #'string-empty-p
                                 (delq nil
                                       (list arg (string-join
                                                  (cdr
                                                   parts)
                                                  "\s"))))))
               (push cell result)))
            ((string-prefix-p "#" line)
             nil)
            (t (when-let* ((cell (pop result)))
                 (setcdr cell (append (cdr cell)
                                      (list line)))
                 (push cell result)))))
    (seq-remove
     (lambda (it)
       (member (car it) '("all" "build" "help" "init" "listFilesOnly" "locale"
                          "project" "showConfig" "version" "watch"
                          "generateTrace" "verbose" "dry" "force" "clean")))
     (reverse result))))

(defun company-tsconfig-find-description (word)
  "Find annotation for WORD in `company-tsconfig-properties-alist'."
  (when-let* ((description (cdr (assoc word company-tsconfig-properties-alist))))
    (string-join description "\s")))

(defun company-tsconfig-find-choices (word)
  "Find completions for WORD."
  (when-let* ((found (cdr (assoc word company-tsconfig-properties-alist))))
    (or (when (member "type: boolean" found)
          '("true" "false"))
        (when (member "type: number" found)
          '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))
        (when-let* ((info (seq-find
                           (apply-partially #'string-match-p
                                            "one[\s]\\([^:]+\\):")
                           (cdr found)))
                    (options-str (replace-regexp-in-string "one[\s]\\([^:]+\\):"
                                                           ""
                                                           info))
                    (options (split-string options-str "[,\s\t]" t)))
          (if (string-match-p "^one or more" info)
              (apply #'vector options)
            options))
        (when "type: string"
          '("string")))))

(defun company-tsconfig-goto-string-start ()
  "If point inside string move to the beginning of string."
  (company-tsconfig-with-syntax-table
   (when-let* ((str-start (nth 8 (syntax-ppss
                                 (point)))))
     (goto-char str-start))))

(defun company-tsconfig-move-with (fn &optional n)
  "Move by calling FN N times.
Return new position if changed, nil otherwise."
  (unless n (setq n 1))
  (company-tsconfig-with-syntax-table
   (when-let* ((str-start (nth 8 (syntax-ppss (point)))))
     (goto-char str-start))
   (let ((init-pos (point))
         (pos)
         (count n))
     (while (and (not (= count 0))
                 (when-let* ((end (ignore-errors
                                   (funcall fn)
                                   (point))))
                   (unless (= end (or pos init-pos))
                     (setq pos end))))
       (setq count (1- count)))
     (if (= count 0)
         pos
       (goto-char init-pos)
       nil))))

(defun company-tsconfig-normalize-option (word)
  "Normalize WORD to match `company-tsconfig-properties-alist'."
  (replace-regexp-in-string "[:'\"]+" "" (car (split-string word nil t)) t))

(defun company-tsconfig-find-context ()
  "Return closest context for completion."
  (save-excursion
    (company-tsconfig-goto-string-start)
    (or (company-tsconfig-grab-property)
        (catch 'found
          (while (company-tsconfig-move-with 'backward-up-list
                                             1)
            (when-let* ((found (company-tsconfig-grab-property)))
              (throw 'found found)))))))

(defun company-tsconfig-find-candidates ()
  "Search for tsconfig completions."
  (when-let* ((found (company-tsconfig-find-context)))
    (if (assoc found company-tsconfig-spec)
        (cadr (assoc found company-tsconfig-spec))
      (company-tsconfig-find-choices found))))

(defun company-tsconfig-exec-help ()
  "Execute tsc help command."
  (when-let* ((tsc-bin (or
                       (when-let* ((located (locate-dominating-file
                                            default-directory
                                            "node_modules/.bin/tsc")))
                         (expand-file-name "node_modules/.bin/tsc" located))
                       (executable-find "tsc"))))
    (setq company-tsconfig-tsc-help-string
          (company-tsconfig-call-process tsc-bin
                                         "--help" "--all"))
    (setq company-tsconfig-properties-alist (company-tsconfig-parse-help))))

(defun company-tsconfig-eldoc-fn ()
  "Return annotation for word at point."
  (or (when-let* ((bounds (company-tsconfig-bounds-by-chars))
                 (word (buffer-substring-no-properties (car bounds)
                                                       (cdr bounds))))
        (setq word (replace-regexp-in-string "[:'\"]+" "" word t))
        (company-tsconfig-find-description word))
      (company-tsconfig-find-description (company-tsconfig-find-context))))

(defun company-tsconfig-highlight-value (docstring word)
  "Highlight WORD in DOCSTRING."
  (mapconcat
   #'identity
   (split-string docstring (concat "\\_<" (regexp-opt
                                           (list
                                            word)
                                           t)
                                   "\\_>"))
   (propertize word 'face 'font-lock-variable-name-face)))

(defun company-tsconfig-eldoc-funcall (callback &rest _ignored)
  "Document function call at point by calling CALLBACK.
Intended for `eldoc-documentation-functions' (which see)."
  (let* ((bounds (company-tsconfig-bounds-by-chars))
         (word (when bounds (buffer-substring-no-properties (car bounds)
                                                            (cdr bounds))))
         (context (company-tsconfig-find-context))
         (docstring)
         (thing))
    (when word
      (setq word (replace-regexp-in-string "[:'\"]+" "" word t))
      (setq docstring (company-tsconfig-find-description word))
      (setq thing (when docstring word)))
    (unless docstring
      (setq docstring (company-tsconfig-find-description
                       context))
      (setq thing (when docstring context))
      (when (and docstring word)
        (setq docstring (company-tsconfig-highlight-value docstring word))))
    (when docstring
      (funcall callback docstring
               :thing thing
               :face 'font-lock-property-face))))

;;;###autoload
(defun company-tsconfig (command &optional arg &rest _ignored)
  "Begin `company-mode' completion backend for tsconfig files.
If COMMAND interactive, begin backend `company-tsconfig',
else return completions for ARG."
  (interactive (list 'interactive))
  (pcase command
    ('init
     (unless company-tsconfig-properties-alist
       (company-tsconfig-exec-help)))
    ('interactive (company-begin-backend 'company-tsconfig))
    ('prefix (or
              (company-grab-word)
              (when (looking-back ",\n[ 	]+" 0)
                "")))
    ('candidates
     (let ((cands
            (company-tsconfig-with-syntax-table
             (or
              (pcase (car (syntax-ppss (point)))
                (1 company-tsconfig-top-level-fields)
                (0 "{}")
                (_ (company-tsconfig-find-candidates)))))))
       (if (vectorp cands)
           (append cands nil)
         cands)))
    ('annotation (company-tsconfig-find-description arg))
    ('post-completion
     (company-tsconfig-with-syntax-table
      (cond ((looking-back company-tsconfig-property-re 0)
             (cond ((looking-at "\":")
                    (forward-char 2)
                    (insert "\s"))
                   ((looking-at "\"")
                    (forward-char 1)
                    (insert ":\s"))
                   (t
                    (when-let* ((bounds
                                 (company-tsconfig-bounds-by-chars))
                                (str (buffer-substring-no-properties
                                      (car bounds)
                                      (cdr bounds))))
                      (replace-region-contents (car bounds)
                                               (cdr bounds)
                                               (lambda ()
                                                 (concat "\"" str "\":\s")))
                      (skip-chars-forward "\":\s")
                      (when (and (member str company-tsconfig-top-level-fields)
                                 (not (looking-at "{")))
                        (insert "{}")
                        (forward-char -2)
                        (newline-and-indent))))))
            ((and (looking-back "[a-z][0-9]*" 0)
                  (not (looking-back "false\\|true" 0))
                  (not (nth 3 (syntax-ppss (point)))))
             (when-let* ((bounds (company-tsconfig-bounds-by-chars))
                         (str (buffer-substring-no-properties (car bounds)
                                                              (cdr bounds))))
               (replace-region-contents (car bounds)
                                        (cdr bounds)
                                        (lambda ()
                                          (concat "\"" str "\"")))
               (forward-char 1))))
      (when (and (looking-at "[\n\s\t]+[\"]")
                 (not (looking-back ":[\n\s\t]*" 0))
                 (not (looking-at ",")))
        (insert ","))))
    ('sorted t)))

;;;###autoload
(defun company-tsconfig-turn-on-eldoc-mode ()
  "Enable `eldoc-mode' with tsconfig annotations."
  (interactive)
  (unless company-tsconfig-properties-alist
    (company-tsconfig-exec-help))
  (add-hook 'eldoc-documentation-functions
            #'company-tsconfig-eldoc-funcall
            nil t)
  (turn-on-eldoc-mode))


(define-minor-mode company-tsconfig-mode
  "Add `company-tsconfig' to list of company backends.

If `company-tsconfig-enable-eldoc' is t, also add eldoc functions and
activate `eldoc-mode'."
  :lighter " TS-conf"
  :global nil
  (if company-tsconfig-mode
      (progn
        (set (make-local-variable 'company-backends)
             (append '(company-tsconfig) company-backends))
        (company-mode 1)
        (when company-tsconfig-enable-eldoc
          (company-tsconfig-turn-on-eldoc-mode)))
    (set (make-local-variable 'company-backends)
         (remq 'company-tsconfig company-backends))
    (remove-hook 'eldoc-documentation-functions #'company-tsconfig-eldoc-funcall
                 t)))

(provide 'company-tsconfig)
;;; company-tsconfig.el ends here