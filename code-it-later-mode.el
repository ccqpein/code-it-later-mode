;;; code-it-later-mode.el --- [code it later](https://github.com/ccqpein/code-it-later-rs) emacs mode -*-lexical-binding:t-*-

;; This emacs mode works with [code-it-later](https://github.com/ccqpein/code-it-later-rs).
;; Use helm framwork as user interface

;; Author: ccQpein
;; URL: https://github.com/ccqpein/code-it-later-mode
;; Version: 0.1.2
;; Keywords: code-it-later, helm
;; Package-Requires: ((helm "3.0") (cl-lib "0.7"))

;;; Commentary:

;; This is the interactive mode for emacs with [codeitlater](https://github.com/ccqpein/code-it-later-rs)

;;; Code:

(require 'cl-lib)
(require 'helm)

(defgroup code-it-later nil
  "code-it-later emacs mode"
  :group 'helm)

(defcustom code-it-later-keywords nil
  "the keywords options for code-it-later -k/--keywords"
  :type '(repeat string)
  :group 'code-it-later)

(defcustom code-it-later-filetypes nil
  "the keywords options for code-it-later -f/--filetypes"
  :type '(repeat string)
  :group 'code-it-later)

(defcustom code-it-later-ignore-dirs nil
  "the keywords options for code-it-later -x/--ignore-dir"
  :type '(repeat string)
  :group 'code-it-later)

(defcustom code-it-later-config-file-directory nil
  "the options for code-it-later -C/--config"
  :type 'directory
  :group 'code-it-later)

(defcustom code-it-later-show-ignored nil
  "the options for code-it-later --show-ignored"
  :type 'boolean
  :group 'code-it-later)

(defvar code-it-later-version nil "code-it-later version")

(defun code-it-later--version ()
  "split and get the version of code-it-later"
  (string-to-number (cadr (split-string (shell-command-to-string "codeitlater -V") " "))))

;;:= TODO: keywords should give some color??
(defun code-it-later--filter-one-by-one (candidate)
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

(defun code-it-later--persistent-action (candidate)
  "persistent-action of helm-async source"
  (let* ((file-line (helm-grep-split-line candidate))
		 (filename (or (cl-first file-line) candidate))
		 (line (cl-second file-line)))
	(find-file filename)
	(goto-char (point-min))
	(when line
	  (forward-line (1- (string-to-number line))))
	(helm-highlight-current-line)
	))

(defun code-it-later--action-find-file (candidate)
  (code-it-later--persistent-action candidate)
  )

(defun code-it-later--action-find-file-other-window (candidate)
  (let* ((file-line (helm-grep-split-line candidate))
		 (filename (or (cl-first file-line) candidate))
		 (line (cl-second file-line)))
	(find-file-other-window filename)
	(goto-char (point-min))
	(when line
	  (forward-line (1- (string-to-number line))))
	(helm-highlight-current-line)))

(defvar code-it-later--actions
  (helm-make-actions
   "Open file"              #'code-it-later--action-find-file
   "Open file other window" #'code-it-later--action-find-file-other-window))

(defclass code-it-later-class (helm-source-async)
  ((candidate-number-limit :initform 99999)
   (filter-one-by-one :initform 'code-it-later--filter-one-by-one)
   (persistent-action :initform 'code-it-later--persistent-action)
   (action :initform 'code-it-later--actions)
   )
  "async helm source for code-it-later")

(cl-defun code-it-later--make-arguments (&key keywords filetypes ignore-dirs config-file-directory show-ignored)
  (let ((arguments ""))
	(when (and config-file-directory (>= code-it-later-version 0.7))
	  (setf arguments
			(concat arguments "-C " config-file-directory " ")))
   
	(when keywords
	  (setf arguments
			(concat arguments
					(mapconcat 'identity
							   (cl-loop for kw in keywords
										append (list "-k" kw))
							   " ")
					" ")))

	(when filetypes
	  (setf arguments
			(concat arguments
					(mapconcat 'identity
							   (cl-loop for ft in filetypes
										append (list "-f" ft))
							   " ")
					" ")))

	(when ignore-dirs
	  (setf arguments
			(concat arguments
					(mapconcat 'identity
							   (cl-loop for ig in ignore-dirs
										append (list "-x" ig))
							   " ")
					" ")))

	(when show-ignored
	  (setf arguments
			(concat arguments
					"--show-ignored"
					" ")))
   
	arguments
	))

(cl-defun code-it-later--make-command (dirs &optional keywords filetypes ignore-dirs config-file-directory show-ignored)
  "make codeitlater command"
  (let ((comm "codeitlater -O list ")
		(argumests (code-it-later--make-arguments :keywords keywords
								   :filetypes filetypes
								   :ignore-dirs ignore-dirs
								   :config-file-directory config-file-directory
								   :show-ignored show-ignored)))

	(setf comm (concat comm argumests (mapconcat 'identity dirs " ")))
	
	comm
	))

(defun code-it-later--do (dirs keywords filetypes ignore-dirs config-file-directory show-ignored)
  "do the codeitlater as the shell command"
  (let* ((comm (code-it-later--make-command dirs
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

(defun code-it-later--string-join (ss &optional join-str)
  (let ((j (if join-str join-str " ")))
	(cl-loop with result = (car ss)
			 for s in (cdr ss)
			 do (setf result (concat result j s))
			 finally (return result)
			 )))


(defvar code-it-later-source nil
  "the helm source of code-it-later")

(defun code-it-later--set-source (dirs keywords filetypes ignore-dirs config-file-directory show-ignored)
  "set source"
  (setf code-it-later-source
		(helm-make-source "code-it-later"
			'code-it-later-class
		  :candidates-process
		  (lambda ()
			(let ((proc (code-it-later--do dirs
										   keywords
										   filetypes
										   ignore-dirs
										   config-file-directory
										   show-ignored)
						))
			  proc))
		  :header-name
		  (lambda (_) (concat "code it later at: " (code-it-later--string-join dirs " and ")))
		  :follow (and helm-follow-mode-persistent 1)
		  )))

(defun code-it-later--prompt-keywords ()
  "prompt for keywords"
  (split-string (read-string "input the keyword(s): ") "[ |, *]+" t))

(defun code-it-later--prompt-filetypes ()
  "prompt for filetypes"
  (split-string (read-string "input the filetype(s): ") "[ |, *]+" t))

(defun code-it-later--prompt-ignore-dirs ()
  "prompt for ignore dirs"
  (split-string (read-string "input the ignore-dir(s): ") "[ |, *]+" t))

(defun code-it-later--prompt-config-file-directory ()
  "prompt for config-file-directory"
  (read-string "input the config file directory: "))

(defun code-it-later--prompt-show-ignored ()
  "prompt for if show-ignored"
  (yes-or-no-p "do you want to show ignore crumbs?"))

(cl-defun code-it-later--prompt ()
  (let ((keywords code-it-later-keywords)
		(filetypes code-it-later-filetypes)
		(ignore-dirs code-it-later-ignore-dirs)
		(config-file-directory code-it-later-config-file-directory)
		(show-ignored code-it-later-show-ignored)
		
		(all-args (helm :sources (helm-build-sync-source "Pick the arguments:"
								   :candidates '(keywords
												 filetypes
												 ignore-dirs
												 config-file-directory
												 show-ignored)
								   :fuzzy-match t
								   :action (lambda (candidate) (helm-marked-candidates))
								   ))))
	
	(cl-loop for a in all-args
			 do (cond ((string= "keywords" a)
					   (setf keywords (code-it-later--prompt-keywords)))
					  ((string= "filetypes" a)
					   (setf filetypes (code-it-later--prompt-filetypes)))
					  ((string= "ignore-dirs" a)
					   (setf ignore-dirs (code-it-later--prompt-ignore-dirs)))
					  ((string= "config-file-directory" a)
					   (setf config-file-directory (code-it-later--prompt-config-file-directory)))
					  ((string= "show-ignored" a)
					   (setf show-ignored (code-it-later--prompt-show-ignored)))
					  (t nil)))

	;; return the new argumes or default
	(cl-values keywords filetypes ignore-dirs config-file-directory show-ignored)
	))

;;;###autoload
(defun code-it-later (&optional arg)
  (interactive "P")
  (if (not code-it-later-version)
	  (setf code-it-later-version (code-it-later--version)))
  
  (let ((dirs (helm-read-file-name
			   "Code it later in dir: "
			   :default default-directory
			   :marked-candidates t :must-match t))

		;; binding with default custom values
		(keywords code-it-later-keywords)
		(filetypes code-it-later-filetypes)
		(ignore-dirs code-it-later-ignore-dirs)
		(config-file-directory code-it-later-config-file-directory)
		(show-ignored code-it-later-show-ignored))

	(if arg ;; C-u will rebinding these values
		(cl-multiple-value-setq (keywords filetypes ignore-dirs config-file-directory show-ignored)
		  (code-it-later--prompt)))
	
	(code-it-later--set-source dirs keywords filetypes ignore-dirs config-file-directory show-ignored)
	(helm :sources
		  'code-it-later-source
          :buffer "*code-it-later*")))

(provide 'code-it-later-mode)
;;; code-it-later-mode.el ends here
