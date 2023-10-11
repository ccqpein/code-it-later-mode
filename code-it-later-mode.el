;;; code-it-later-mode.el --- [code it later](https://github.com/ccqpein/code-it-later-rs) emacs mode -*-lexical-binding:t-*-

;; This emacs mode works with [code-it-later](https://github.com/ccqpein/code-it-later-rs).
;; Use helm framwork as user interface

;; Author: ccQpein
;; URL: https://github.com/ccqpein/code-it-later-mode
;; Version: 0.1.0
;; Keywords: code-it-later, helm
;; Package-Requires: ((helm "3") (cl-lib "0.7"))

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

;;:= NEXT: need give some version control? this option only >= 0.7
(defcustom code-it-later-config-file-directory nil
  "the options for code-it-later -C/--config"
  :type 'directory
  :group 'code-it-later)

(defun format-to-emacs-buffer (responses)
  "format the results from code-it-later"
  (cl-loop with results = '()
		   for bread in responses
		   for filename = (car bread)
		   for crumbs = (cdr bread)
		   
		   do (cl-loop for crumb in crumbs
					   do (add-to-list 'results
									   (format "%s:%s: %s" filename (car crumb) (nth 1 crumb))))
		   finally (return results)))

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

(cl-defun make-code-it-later-command (dirs &optional keywords filetypes ignore-dirs)
  "make codeitlater command"
  (let ((comm "codeitlater -O list "))
	(when keywords
	  (setf comm
			(concat comm
					(mapconcat 'identity
							   (cl-loop for kw in keywords
										append (list "-k" kw))
							   " ")
					" ")))

	(when filetypes
	  (setf comm
			(concat comm
					(mapconcat 'identity
							   (cl-loop for ft in filetypes
										append (list "-f" ft))
							   " ")
					" ")))

	(when ignore-dirs
	  (setf comm
			(concat comm
					(mapconcat 'identity
							   (cl-loop for ig in ignore-dirs
										append (list "-x" ig))
							   " ")
					" ")))

	(setf comm (concat comm (mapconcat 'identity dirs " ")))
	
	comm
	))

(defun do-code-it-later (dirs keywords filetypes ignore-dirs)
  "do the codeitlater as the shell command"
  (let* ((comm (make-code-it-later-command dirs keywords filetypes ignore-dirs))
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

(defun string-join (ss &optional join-str)
  (let ((j (if join-str join-str " ")))
	(cl-loop with result = (car ss)
			 for s in (cdr ss)
			 do (setf result (concat result j s))
			 finally (return result)
			 )))


(defvar code-it-later-source nil
  "the helm source of code-it-later")

(defun set-code-it-later-source (dirs keywords filetypes ignore-dirs)
  "set source"
  (setf code-it-later-source
		(helm-make-source "code-it-later"
			'code-it-later-class
		  :candidates-process
		  (lambda ()
			(let ((proc (do-code-it-later dirs keywords filetypes ignore-dirs)
						))
			  proc))
		  :header-name
		  (lambda (_) (concat "code it later at: " (string-join dirs " and ")))
		  :follow (and helm-follow-mode-persistent 1)
		  )))

;;:= TODO: write the document
(defun code-it-later--prompt-keywords ()
  (split-string (read-string "input the keyword(s): ") "[ |, *]+" t))

(defun code-it-later--prompt-filetypes ()
  (split-string (read-string "input the filetype(s): ") "[ |, *]+" t))

(defun code-it-later--prompt-ignore-dirs ()
  (split-string (read-string "input the ignore-dir(s): ") "[ |, *]+" t))

(cl-defun code-it-later--prompt ()
  (let ((keywords code-it-later-keywords)
		(filetypes code-it-later-filetypes)
		(ignore-dirs code-it-later-ignore-dirs)
		(all-args (helm :sources (helm-build-sync-source "Pick the arguments:"
								   :candidates '(keywords filetypes ignore-dirs)
								   :fuzzy-match t
								   :action (lambda (candidate) (helm-marked-candidates))
								   )
						)))
	
	(cl-loop for a in all-args
			 do (cond ((string= "keywords" a)
					   (setf keywords (code-it-later--prompt-keywords)))
					  ((string= "filetypes" a)
					   (setf filetypes (code-it-later--prompt-filetypes)))
					  ((string= "ignore-dirs" a)
					   (setf ignore-dirs (code-it-later--prompt-ignore-dirs)))
					  (t nil)))

	;; return the new argumes or default
	(cl-values keywords filetypes ignore-dirs)
	))

;;;###autoload
(defun code-it-later (&optional arg)
  (interactive "P")
  (let ((dirs (helm-read-file-name
			   "Code it later in dir: "
			   :default default-directory
			   :marked-candidates t :must-match t))

		;; binding with default custom values
		(keywords code-it-later-keywords)
		(filetypes code-it-later-filetypes)
		(ignore-dirs code-it-later-ignore-dirs))

	(if arg ;; C-u will rebinding these values
		(cl-multiple-value-setq (keywords filetypes ignore-dirs)
			(code-it-later--prompt)))
	
	(set-code-it-later-source dirs keywords filetypes ignore-dirs)
	(helm :sources
		  'code-it-later-source
          :buffer "*code-it-later*")))

(provide 'code-it-later-mode)
;;; code-it-later-mode.el ends here
