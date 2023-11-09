;;; helm-code-it-later.el --- The code-it-later's mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2023 ccQpein

;; Author: ccQpein
;; URL: https://github.com/ccqpein/helm-code-it-later
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

(defgroup helm-code-it-later nil
  "Helm-code-it-later Emacs mode."
  :group 'helm)

(defcustom helm-code-it-later-keywords nil
  "The keywords options for `helm-code-it-later' -k/--keywords."
  :type '(repeat string)
  :group 'helm-code-it-later)

(defcustom helm-code-it-later-filetypes nil
  "The keywords options for `helm-code-it-later' -f/--filetypes."
  :type '(repeat string)
  :group 'helm-code-it-later)

(defcustom helm-code-it-later-ignore-dirs nil
  "The keywords options for `helm-code-it-later' -x/--ignore-dir."
  :type '(repeat string)
  :group 'helm-code-it-later)

(defcustom helm-code-it-later-config-file-directory nil
  "The options for `helm-code-it-later' -C/--config."
  :type 'directory
  :group 'helm-code-it-later)

(defcustom helm-code-it-later-show-ignored nil
  "The options for `helm-code-it-later' --show-ignored."
  :type 'boolean
  :group 'helm-code-it-later)

(defvar helm-code-it-later-version nil "Helm-code-it-later version.")

(defun helm-code-it-later--version ()
  "Split and get the version of `helm-code-it-later'."
  (mapcar #'string-to-number
          (split-string (replace-regexp-in-string
                         "\n\\'" ""
                         (cadr
                          (split-string
                           (shell-command-to-string "codeitlater -V")
                           " ")))
                        "\\.")))

(defun helm-code-it-later-version-compare (first second)
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
(defun helm-code-it-later--filter-one-by-one (candidate)
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

(defun helm-code-it-later--persistent-action (candidate)
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

(defun helm-code-it-later--action-find-file (candidate)
  "Find-file action.
Argument CANDIDATE: single candidate from code-it-later."
  (helm-code-it-later--persistent-action candidate))

(defun helm-code-it-later--action-find-file-other-window (candidate)
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

(defvar helm-code-it-later--actions
  (helm-make-actions
   "Open file"              #'helm-code-it-later--action-find-file
   "Open file other window"
   #'helm-code-it-later--action-find-file-other-window)
  "Actions of `helm-code-it-later'.")

(defclass helm-code-it-later-class (helm-source-async)
  ((candidate-number-limit :initform 99999)
   (filter-one-by-one :initform 'helm-code-it-later--filter-one-by-one)
   (persistent-action :initform 'helm-code-it-later--persistent-action)
   (action :initform 'helm-code-it-later--actions))
  "Async helm source for `helm-code-it-later'.")

(cl-defun helm-code-it-later--make-arguments
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

(cl-defun helm-code-it-later--make-command
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
        (argumests (helm-code-it-later--make-arguments :keywords
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

(defun helm-code-it-later--do
    (dirs keywords filetypes ignore-dirs config-file-directory
          show-ignored)
  "Do the codeitlater as the shell command.
Argument DIRS that run code-it-later.
Argument KEYWORDS: -k arguments of code-it-later.
Argument FILETYPES: -f arguments of code-it-later.
Argument IGNORE-DIRS: -x arguments of code-it-later.
Argument CONFIG-FILE-DIRECTORY: -C argument of code-it-later.
Argument SHOW-IGNORED: --show-ignored argument of code-it-later."
  (let* ((comm (helm-code-it-later--make-command dirs
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

(defun helm-code-it-later--string-join (ss &optional join-str)
  "String join helper function.
Argument SS: the list of strings.
Optional argument JOIN-STR: join string."
  (let ((j (if join-str join-str " ")))
    (cl-loop with result = (car ss)
             for s in (cdr ss)
             do (setf result (concat result j s))
             finally (return result))))

(defvar helm-code-it-later-source nil
  "The helm source of `code-it-later'.")

(defun helm-code-it-later--set-source
    (dirs keywords filetypes ignore-dirs config-file-directory
          show-ignored)
  "Set source.
Argument DIRS that run code-it-later.
Argument KEYWORDS: -k arguments of code-it-later.
Argument FILETYPES: -f arguments of code-it-later.
Argument IGNORE-DIRS: -x arguments of code-it-later.
Argument CONFIG-FILE-DIRECTORY: -C argument of code-it-later.
Argument SHOW-IGNORED: --show-ignored argument of code-it-later."
  (setf helm-code-it-later-source
        (helm-make-source "code-it-later"
            'helm-code-it-later-class
          :candidates-process
          (lambda ()
            (let ((proc (helm-code-it-later--do dirs
                                                keywords
                                                filetypes
                                                ignore-dirs
                                                config-file-directory
                                                show-ignored)))
              proc))
          :header-name
          (lambda (_)
            (concat "code it later at: "
                    (helm-code-it-later--string-join dirs " and ")))
          :follow (and helm-follow-mode-persistent 1))))

(defun helm-code-it-later--prompt-keywords ()
  "Prompt for keywords."
  (split-string (read-string "input the keyword(s): ") "[ |, *]+" t))

(defun helm-code-it-later--prompt-filetypes ()
  "Prompt for filetypes."
  (split-string (read-string "input the filetype(s): ") "[ |, *]+" t))

(defun helm-code-it-later--prompt-ignore-dirs ()
  "Prompt for ignore dirs."
  (split-string (read-string "input the ignore-dir(s): ") "[ |, *]+" t))

(defun helm-code-it-later--prompt-config-file-directory ()
  "Prompt for config-file-directory."
  (read-string "input the config file directory: "))

(defun helm-code-it-later--prompt-show-ignored ()
  "Prompt for if show-ignored."
  (yes-or-no-p "Do you want to show ignore crumbs?"))

(cl-defun helm-code-it-later--prompt ()
  "Prompt for `helm-code-it-later'."
  (let ((keywords helm-code-it-later-keywords)
        (filetypes helm-code-it-later-filetypes)
        (ignore-dirs helm-code-it-later-ignore-dirs)
        (config-file-directory
         helm-code-it-later-config-file-directory)
        (show-ignored helm-code-it-later-show-ignored)

        (all-args (helm :sources (helm-build-sync-source
                                     "Pick the arguments:"
                                   :candidates '(keywords
                                                 filetypes
                                                 ignore-dirs
                                                 config-file-directory
                                                 show-ignored)
                                   :fuzzy-match t
                                   :action
                                   (lambda () (helm-marked-candidates))))))

    (cl-loop for a in all-args
             do (cond ((string= "keywords" a)
                       (setf keywords
                             (helm-code-it-later--prompt-keywords)))
                      ((string= "filetypes" a)
                       (setf filetypes
                             (helm-code-it-later--prompt-filetypes)))
                      ((string= "ignore-dirs" a)
                       (setf ignore-dirs
                             (helm-code-it-later--prompt-ignore-dirs)))
                      ((string= "config-file-directory" a)
                       (setf config-file-directory
                             (helm-code-it-later--prompt-config-file-directory)))
                      ((string= "show-ignored" a)
                       (setf show-ignored
                             (helm-code-it-later--prompt-show-ignored)))
                      (t nil)))

    ;; return the new argumes or default
    (cl-values keywords filetypes ignore-dirs config-file-directory
               show-ignored)))

;;;###autoload
(defun helm-code-it-later (&optional arg)
  "Major function.
Optional argument ARG."
  (interactive "P")
  (if (not helm-code-it-later-version)
      (setf helm-code-it-later-version (helm-code-it-later--version)))

  ;; min version support is 0.7
  (when
      (<
       (helm-code-it-later-version-compare helm-code-it-later-version
                                           '(0 7))
       0)
    (error "Minimum helm-code-it-later version is 0.7"))
  
  (let ((dirs (helm-read-file-name
               "Code it later in dir: "
               :default default-directory
               :marked-candidates t :must-match t))

        ;; binding with default custom values
        (keywords helm-code-it-later-keywords)
        (filetypes helm-code-it-later-filetypes)
        (ignore-dirs helm-code-it-later-ignore-dirs)
        (config-file-directory
         helm-code-it-later-config-file-directory)
        (show-ignored helm-code-it-later-show-ignored))

    (if arg ;; C-u will rebinding these values
        (cl-multiple-value-setq
            (keywords filetypes ignore-dirs config-file-directory
                      show-ignored)
          (helm-code-it-later--prompt)))

    (helm-code-it-later--set-source dirs keywords filetypes
                                    ignore-dirs config-file-directory
                                    show-ignored)
    (helm :sources
          'helm-code-it-later-source
          :buffer "*code-it-later*")))

(provide 'helm-code-it-later)
;;; helm-code-it-later.el ends here
