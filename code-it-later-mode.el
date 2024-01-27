;;; code-it-later-mode.el --- The code-it-later's mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 ccQpein

;; Author: ccQpein
;; URL: https://github.com/ccqpein/code-it-later-mode
;; Version: 0.1.4
;; Package-Requires: ((emacs "25.1") (helm "3.0"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This is the interactive mode for Emacs with codeitlater.
;; https://github.com/ccqpein/code-it-later-rs

;; This mode powered by helm framwork.

;;; Code:

(require 'helm)
(require 'helm-mode)
(require 'helm-grep)

(defgroup code-it-later-mode nil
  "Code-it-later Emacs mode."
  :group 'helm)

(defcustom code-it-later-mode-keywords nil
  "The keywords options for `code-it-later-mode' -k/--keywords."
  :type '(repeat string)
  :group 'code-it-later-mode)

(defcustom code-it-later-mode-filetypes nil
  "The keywords options for `code-it-later-mode' -f/--filetypes."
  :type '(repeat string)
  :group 'code-it-later-mode)

(defcustom code-it-later-mode-ignore-dirs nil
  "The keywords options for `code-it-later-mode' -x/--ignore-dir."
  :type '(repeat string)
  :group 'code-it-later-mode)

(defcustom code-it-later-mode-config-file-directory nil
  "The options for `code-it-later-mode' -C/--config."
  :type 'directory
  :group 'code-it-later-mode)

(defcustom code-it-later-mode-show-ignored nil
  "The options for `code-it-later-mode' --show-ignored."
  :type 'boolean
  :group 'code-it-later-mode)

(defvar code-it-later-mode-version nil "Code-it-later version.")

(defun code-it-later-mode--version ()
  "Split and get the version of `code-it-later-mode'."
  (mapcar #'string-to-number
          (split-string (replace-regexp-in-string
                         "\n\\'" ""
                         (cadr
                          (split-string
                           (shell-command-to-string "codeitlater -V")
                           " ")))
                        "\\.")))

(defun code-it-later-mode-version-compare (first second)
  "Compare version.
Argument FIRST: the first version.
Argument SECOND the second version."
  (let ((max-len (max (length first)
                      (length second))))
    (cl-loop for i from 0 below max-len
             for x = (if (nth i first) (nth i first) 0)
             and y = (if (nth i second) (nth i second) 0)

             ;; first larger than second
             if (> x y) return 1

             ;; first smaller than second
             if (< x y) return -1

             ;; versions are equal
             finally return 0)))

;;:= TODO: keywords should give some color??
(defun code-it-later-mode--filter-one-by-one (candidate)
  "Async filter one by one function.
Argument CANDIDATE: single candidate from code-it-later."
  (let* ((split (helm-grep-split-line candidate))
         (file (nth 0 split))
         (lineno (nth 1 split))
         (str (nth 2 split)))
    (cons (concat (propertize file 'face 'helm-moccur-buffer)
                  ":"
                  (propertize lineno 'face 'helm-grep-lineno)
                  ":"
                  str)
          candidate)))

(defun code-it-later-mode--persistent-action (candidate)
  "Persistent-action of helm-async source.
Argument CANDIDATE: single candidate from code-it-later."
  (let* ((file-line (helm-grep-split-line candidate))
         (filename (or (cl-first file-line) candidate))
         (line (cl-second file-line)))
    (find-file filename)
    (goto-char (point-min))
    (when line
      (forward-line (1- (string-to-number line))))
    (helm-highlight-current-line)))

(defun code-it-later-mode--action-find-file (candidate)
  "Find-file action.
Argument CANDIDATE: single candidate from code-it-later."
  (code-it-later-mode--persistent-action candidate))

(defun code-it-later-mode--action-find-file-other-window (candidate)
  "Find-file-other-window action.
Argument CANDIDATE: single candidate from code-it-later."
  (let* ((file-line (helm-grep-split-line candidate))
         (filename (or (cl-first file-line) candidate))
         (line (cl-second file-line)))
    (find-file-other-window filename)
    (goto-char (point-min))
    (when line
      (forward-line (1- (string-to-number line))))
    (helm-highlight-current-line)))

(defvar code-it-later-mode--actions
  (helm-make-actions
   "Open file"              #'code-it-later-mode--action-find-file
   "Open file other window"
   #'code-it-later-mode--action-find-file-other-window)
  "Actions of `code-it-later-mode'.")

(defclass code-it-later-mode-class (helm-source-async)
  ((candidate-number-limit :initform 99999)
   (filter-one-by-one :initform 'code-it-later-mode--filter-one-by-one)
   (persistent-action :initform 'code-it-later-mode--persistent-action)
   (action :initform 'code-it-later-mode--actions))
  "Async helm source for `code-it-later-mode'.")

(cl-defun code-it-later-mode--make-arguments
    (&key keywords filetypes ignore-dirs config-file-directory
          show-ignored)
  "Making the arguments for code-it-later.
Optional argument KEYWORDS: -k arguments of code-it-later.
Optional argument FILETYPES: -f arguments of code-it-later.
Optional argument IGNORE-DIRS: -x arguments of code-it-later.
Optional argument CONFIG-FILE-DIRECTORY: -C argument of code-it-later.
Optional argument SHOW-IGNORED: --show-ignored argument of code-it-later."
  (let ((arguments ""))
    (when config-file-directory
      (setf arguments
            (concat arguments "-C " config-file-directory " ")))

    (when keywords
      (setf arguments
            (concat arguments
                    (mapconcat #'identity
                               (cl-loop for kw in keywords
                                        append (list "-k" kw))
                               " ")
                    " ")))

    (when filetypes
      (setf arguments
            (concat arguments
                    (mapconcat #'identity
                               (cl-loop for ft in filetypes
                                        append (list "-f" ft))
                               " ")
                    " ")))

    (when ignore-dirs
      (setf arguments
            (concat arguments
                    (mapconcat #'identity
                               (cl-loop for ig in ignore-dirs
                                        append (list "-x" ig))
                               " ")
                    " ")))

    (when show-ignored
      (setf arguments
            (concat arguments
                    "--show-ignored"
                    " ")))

    arguments))

(cl-defun code-it-later-mode--make-command
    (dirs &optional keywords filetypes ignore-dirs
          config-file-directory show-ignored)
  "Make codeitlater command.
Argument DIRS that run code-it-later.
Optional argument KEYWORDS: -k arguments of code-it-later.
Optional argument FILETYPES: -f arguments of code-it-later.
Optional argument IGNORE-DIRS: -x arguments of code-it-later.
Optional argument CONFIG-FILE-DIRECTORY: -C argument of code-it-later.
Optional argument SHOW-IGNORED: --show-ignored argument of code-it-later."
  (let ((comm "codeitlater -O list ")
        (argumests (code-it-later-mode--make-arguments :keywords
                                                       keywords
                                                       :filetypes
                                                       filetypes
                                                       :ignore-dirs
                                                       ignore-dirs
                                                       :config-file-directory
                                                       config-file-directory
                                                       :show-ignored
                                                       show-ignored)))

    (setf comm (concat comm argumests (mapconcat #'identity dirs " ")))

    comm))

(defun code-it-later-mode--do
    (dirs keywords filetypes ignore-dirs config-file-directory
          show-ignored)
  "Do the codeitlater as the shell command.
Argument DIRS that run code-it-later.
Argument KEYWORDS: -k arguments of code-it-later.
Argument FILETYPES: -f arguments of code-it-later.
Argument IGNORE-DIRS: -x arguments of code-it-later.
Argument CONFIG-FILE-DIRECTORY: -C argument of code-it-later.
Argument SHOW-IGNORED: --show-ignored argument of code-it-later."
  (let* ((comm (code-it-later-mode--make-command dirs
                                                 keywords
                                                 filetypes
                                                 ignore-dirs
                                                 config-file-directory
                                                 show-ignored))
         (proc (apply
                #'start-process-shell-command "code-it-later" nil
                (list comm))))
    (prog1 proc
      (set-process-sentinel
       proc
       (lambda (process event)
         (cond ((/= 0 (process-exit-status process))
                (message "error on %s" comm))
               (t (when (string= event "finished\n")
                    (with-helm-window
                      ;;
                      )))))))))

(defun code-it-later-mode--string-join (ss &optional join-str)
  "String join helper function.
Argument SS: the list of strings.
Optional argument JOIN-STR: join string."
  (let ((j (if join-str join-str " ")))
    (cl-loop with result = (car ss)
             for s in (cdr ss)
             do (setf result (concat result j s))
             finally (return result))))

(defvar code-it-later-mode-source nil
  "The helm source of `code-it-later'.")

(defun code-it-later-mode--set-source
    (dirs keywords filetypes ignore-dirs config-file-directory
          show-ignored)
  "Set source.
Argument DIRS that run code-it-later.
Argument KEYWORDS: -k arguments of code-it-later.
Argument FILETYPES: -f arguments of code-it-later.
Argument IGNORE-DIRS: -x arguments of code-it-later.
Argument CONFIG-FILE-DIRECTORY: -C argument of code-it-later.
Argument SHOW-IGNORED: --show-ignored argument of code-it-later."
  (setf code-it-later-mode-source
        (helm-make-source "code-it-later"
            'code-it-later-mode-class
          :candidates-process
          (lambda ()
            (let ((proc (code-it-later-mode--do dirs
                                                keywords
                                                filetypes
                                                ignore-dirs
                                                config-file-directory
                                                show-ignored)))
              proc))
          :header-name
          (lambda (_)
            (concat "code it later at: "
                    (code-it-later-mode--string-join dirs " and ")))
          :follow (and helm-follow-mode-persistent 1))))

(defun code-it-later-mode--prompt-keywords ()
  "Prompt for keywords."
  (split-string (read-string "input the keyword(s): ") "[ |, *]+" t))

(defun code-it-later-mode--prompt-filetypes ()
  "Prompt for filetypes."
  (split-string (read-string "input the filetype(s): ") "[ |, *]+" t))

(defun code-it-later-mode--prompt-ignore-dirs ()
  "Prompt for ignore dirs."
  (split-string (read-string "input the ignore-dir(s): ") "[ |, *]+" t))

(defun code-it-later-mode--prompt-config-file-directory ()
  "Prompt for config-file-directory."
  (read-string "input the config file directory: "))

(defun code-it-later-mode--prompt-show-ignored ()
  "Prompt for if show-ignored."
  (yes-or-no-p "Do you want to show ignore crumbs?"))

(cl-defun code-it-later-mode--prompt ()
  "Prompt for `code-it-later-mode'."
  (let ((keywords code-it-later-mode-keywords)
        (filetypes code-it-later-mode-filetypes)
        (ignore-dirs code-it-later-mode-ignore-dirs)
        (config-file-directory
         code-it-later-mode-config-file-directory)
        (show-ignored code-it-later-mode-show-ignored)

        (all-args (helm :sources (helm-build-sync-source
                                     "Pick the arguments:"
                                   :candidates '(keywords
                                                 filetypes
                                                 ignore-dirs
                                                 config-file-directory
                                                 show-ignored)
                                   :fuzzy-match t
                                   :action
                                   (lambda (_) (helm-marked-candidates))))))

    (cl-loop for a in all-args
             do (cond ((string= "keywords" a)
                       (setf keywords
                             (code-it-later-mode--prompt-keywords)))
                      ((string= "filetypes" a)
                       (setf filetypes
                             (code-it-later-mode--prompt-filetypes)))
                      ((string= "ignore-dirs" a)
                       (setf ignore-dirs
                             (code-it-later-mode--prompt-ignore-dirs)))
                      ((string= "config-file-directory" a)
                       (setf config-file-directory
                             (code-it-later-mode--prompt-config-file-directory)))
                      ((string= "show-ignored" a)
                       (setf show-ignored
                             (code-it-later-mode--prompt-show-ignored)))
                      (t nil)))

    ;; return the new argumes or default
    (cl-values keywords filetypes ignore-dirs config-file-directory
               show-ignored)))

;;;###autoload
(defun code-it-later (&optional arg)
  "Major function.
Optional argument ARG."
  (interactive "P")
  (if (not code-it-later-mode-version)
      (setf code-it-later-mode-version (code-it-later-mode--version)))

  ;; min version support is 0.7
  (when
      (<
       (code-it-later-mode-version-compare code-it-later-mode-version
                                           '(0 7))
       0)
    (error "Minimum code-it-later-mode version is 0.7"))
  
  (let ((dirs (helm-read-file-name
               "Code it later in dir: "
               :default default-directory
               :marked-candidates t :must-match t))

        ;; binding with default custom values
        (keywords code-it-later-mode-keywords)
        (filetypes code-it-later-mode-filetypes)
        (ignore-dirs code-it-later-mode-ignore-dirs)
        (config-file-directory
         code-it-later-mode-config-file-directory)
        (show-ignored code-it-later-mode-show-ignored))

    (if arg ;; C-u will rebinding these values
        (cl-multiple-value-setq
            (keywords filetypes ignore-dirs config-file-directory
                      show-ignored)
          (code-it-later-mode--prompt)))

    (code-it-later-mode--set-source dirs keywords filetypes
                                    ignore-dirs config-file-directory
                                    show-ignored)
    (helm :sources
          'code-it-later-mode-source
          :buffer "*code-it-later*")))

(provide 'code-it-later-mode)
;;; code-it-later-mode.el ends here
