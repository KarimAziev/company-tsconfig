;;; company-tsconfig.el --- Company-mode completion backend for tsconfig.json  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/company-tsconfig
;; Version: 0.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "27.1"))

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

;; Company-mode completion backend for tsconfig.json

;;; Code:

(require 'company)
(require 'cl-lib)

(defvar company-tsconfig-property-alist
  '(("allowJs" boolean)
    ("allowSyntheticDefaultImports" boolean)
    ("allowUnreachableCode" boolean)
    ("allowUnusedLabels" boolean)
    ("alwaysStrict" boolean)
    ("baseUrl" . string)
    ("charset" . string)
    ("checkJs" boolean)
    ("declaration" boolean)
    ("declarationDir" . string)
    ("disableSizeLimit" boolean)
    ("downlevelIteration" boolean)
    ("emitBOM" boolean)
    ("emitDecoratorMetadata" boolean)
    ("experimentalDecorators" boolean)
    ("forceConsistentCasingInFileNames" boolean)
    ("importHelpers" boolean)
    ("inlineSourceMap" boolean)
    ("inlineSources" boolean)
    ("isolatedModules" boolean)
    ("esModuleInterop" boolean)
    ("jsx" . ("react"
              "react-jsx"
              "react-jsxdev"
              "preserve"
              "react-native"))
    ("lib" . lib)
    ("locale" . string)
    ("mapRoot" . string)
    ("maxNodeModuleJsDepth" . number)
    ("module"  ("none"
                "commonjs"
                "amd"
                "umd"
                "system"
                "es6"
                "es2015"
                "esnext"
                "nodenext"))
    ("moduleResolution" "classic" "node")
    ("newLine" . ("crlf" "lf"))
    ("noEmit" boolean)
    ("noEmitHelpers" boolean)
    ("noEmitOnError" boolean)
    ("noErrorTruncation" boolean)
    ("noFallthroughCasesInSwitch" boolean)
    ("noImplicitAny" boolean)
    ("noImplicitReturns" boolean)
    ("noImplicitThis" boolean)
    ("noUnusedLocals" boolean)
    ("noUnusedParameters" boolean)
    ("noImplicitUseStrict" boolean)
    ("noLib" boolean)
    ("noResolve" boolean)
    ("out" . string)
    ("outDir" . string)
    ("outFile" . string)
    ("paths" . "[]")
    ("plugins" . "[]")
    ("preserveConstEnums" boolean)
    ("preserveSymlinks" boolean)
    ("project" . string)
    ("reactNamespace" . string)
    ("removeComments" boolean)
    ("references" . "[  { \"path\": \"../src\" }]")
    ("rootDir" . string)
    ("rootDirs" . rootDirs)
    ("skipLibCheck" boolean)
    ("skipDefaultLibCheck" boolean)
    ("sourceMap" boolean)
    ("sourceRoot" . string)
    ("strict" boolean)
    ("strictNullChecks" boolean)
    ("suppressExcessPropertyErrors" boolean)
    ("suppressImplicitAnyIndexErrors" boolean)
    ("useDefineForClassFields" boolean)
    ("target" . ("es3"
                 "es5"
                 "es6"
                 "es2015"
                 "es2016"
                 "es2017"
                 "es2018"
                 "es2019"
                 "es2020"
                 "es2021"
                 "es2022"
                 "esnext"))
    ("traceResolution" boolean)
    ("resolveJsonModule" boolean)
    ("types" . "string[]")
    ("typeRoots" . "string[]"))
  "A list of tsconfig properties and their possible values.")

(defun company-tsconfig-directory-files ()
  "Return a list of names of files in DIRECTORY excluding \".\" and \"..\".

Names are that are relative to the specified directory.

If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 Otherwise, the list returned is sorted with string-lessp."
  (when-let ((dir
              (or (locate-dominating-file default-directory "tsconfig.json")
                  (locate-dominating-file default-directory "jsconfig.json"))))
    (mapcar
     (lambda (it) (prin1-to-string (concat "./" it)))
     (directory-files default-directory nil
                      directory-files-no-dot-files-regexp))))

(defvar company-tsconfig-value-classes
  '((boolean "true" "false")
    (string "\"./\"")
    (number "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10")
    (lib "es5"
         "es2015"
         "es6"
         "es2016"
         "es7"
         "es2017"
         "es2018"
         "es2019"
         "es2020"
         "es2021"
         "esnext"
         "dom"
         "webworker"
         "scripthost")))

(defvar company-tsconfig-property-re
  (concat "\\_<" (regexp-opt
                  (seq-filter #'stringp
                              (mapcar #'car
                                      company-tsconfig-property-alist))
                  t)
          "\\_>"))

(defun company-tsconfig-property-values (attribute)
  "Access the `company-tsconfig-property-alist' with ATTRIBUTE."
  (setq attribute (if (stringp attribute)
                      (replace-regexp-in-string "\s\t\n:\"" "" attribute)
                    attribute))
  (or (let ((results)
            (vals (cdr (assoc attribute company-tsconfig-property-alist))))
        (dolist (value (if (listp vals) vals `(,vals)))
          (when-let ((class-val (cdr
                                 (assoc value company-tsconfig-value-classes))))
            (setq value class-val))
          (if (listp value)
              (setq results (flatten-list
                             (mapcar (lambda (i)
                                       (if (stringp i)
                                           i
                                         (company-tsconfig-property-values i)))
                                     value)))
            (push value results)))
        (setq results (sort results 'string<))
        results)))

(defun company-tsconfig-grab-property ()
  "Inside string return the word before point, if any."
  (when (nth 3 (syntax-ppss (point)))
    (company-grab-word)))

(defvar company-tsconfig-property-value-regexp
  "\"\\([a-zZ-A]+\\)\":[\s\t\n]+\\([a-zZ-A.]?+\\)"
  "A regular expression matching property.")

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

;;;###autoload
(defun company-tsconfig (command &optional arg &rest _ignored)
  "Begin `company-mode' completion backend for tsconfig files.
If COMMAND interactive, begin backend `company-tsconfig',
else return completions for ARG."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tsconfig))
    (prefix (or (company-grab-word)
                (company-tsconfig-grab-property)
                (when (looking-back ",\n[\s\t]+" 0)
                  "")))
    (candidates
     (cond
      ((looking-back ",\n[\s\t]+" 0)
       (all-completions arg company-tsconfig-property-alist))
      ((company-grab company-tsconfig-property-value-regexp)
       (let* ((attr (car (split-string
                          (company-grab company-tsconfig-property-value-regexp)
                          "[:\s\t\"]" t)))
              (vals (company-tsconfig-property-values attr)))
         (all-completions (replace-regexp-in-string "[^:]+:" ""
                                                    (replace-regexp-in-string
                                                     "[\"\s\t\n]" "" arg))
                          vals)))
      ((company-tsconfig-grab-property)
       (if-let ((prop (when (looking-back
                             "\"\\([a-zZ-A]+\\)\":[\s\t]+[\"a-z0-9]*" 0)
                        (match-string-no-properties 1))))
           (all-completions arg
                            (company-tsconfig-property-values
                             prop))
         (all-completions arg company-tsconfig-property-alist)))))
    (post-completion
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
                                     (car bounds) (cdr bounds))))
                     (replace-region-contents (car bounds)
                                              (cdr bounds)
                                              (lambda () (concat "\"" str "\":\s")))
                     (skip-chars-forward "\":\s")))))
           ((and (looking-back "[a-z0-9]+" 0)
                 (not (looking-back "false\\|true" 0))
                 (not (nth 3 (syntax-ppss (point)))))
            (when-let* ((bounds (company-tsconfig-bounds-by-chars))
                        (str (buffer-substring-no-properties (car bounds)
                                                             (cdr bounds))))
              (replace-region-contents (car bounds) (cdr bounds)
                                       (lambda () (concat "\"" str "\"")))
              (forward-char 1))))
     (when (and (looking-at "[\n\s\t]+[\"]")
                (not (looking-back ":[\n\s\t]*" 0)))
       (insert ",")))
    (sorted t)))

(provide 'company-tsconfig)
;;; company-tsconfig.el ends here