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

;;:= NEXT: need to use this inside do-code-it-later
(defcustom code-it-later-keywords nil
  "the keywords options for code-it-later -k/--keywords"
  :type '(repeat string)
  :group 'code-it-later)

;;:= TODO: need to use this inside do-code-it-later
(defcustom code-it-later-filetypes nil
  "the keywords options for code-it-later -f/--filetypes"
  :type '(repeat string)
  :group 'code-it-later)

;;:= TODO: need to use this inside do-code-it-later
(defcustom code-it-later-ignore-dirs nil
  "the keywords options for code-it-later -x/--ignore-dir"
  :type '(repeat string)
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

(defun do-code-it-later (dirs)
  "do the codeitlater as the shell command"
  (let ((proc (apply
			   #'start-process-shell-command "code-it-later" nil
			   (list (cl-loop with args = "codeitlater -O list "
						 for d in dirs
						 do (setf args (concat args d " "))
						 finally (return args)
						 )))))
	(prog1 proc
	  (set-process-sentinel
	   proc
	   (lambda (process event)
		 (when (string= event "finished\n")
		   (with-helm-window
			 ;; 
			 )))))))

(defun string-join (ss &optional join-str)
  (let ((j (if join-str join-str " ")))
	(cl-loop with result = (car ss)
			 for s in (cdr ss)
			 do (setf result (concat result j s))
			 finally (return result)
			 )))


(defvar code-it-later-source nil
  "the helm source of code-it-later")

(defun set-code-it-later-source (dirs)
  "set source"
  (setf code-it-later-source
		(helm-make-source "code-it-later"
			'code-it-later-class
		  :candidates-process
		  (lambda ()
			(let ((proc (do-code-it-later dirs)
						))
			  proc))
		  :header-name
		  (lambda (_) (concat "code it later at: " (string-join dirs " and ")))
		  :follow (and helm-follow-mode-persistent 1)
		  )))

;;;###autoload
(defun code-it-later ()
  (interactive)
  (let ((dirs (helm-read-file-name
			  "Code it later in dir: "
			  :default default-directory
			  :marked-candidates t :must-match t)))
	(set-code-it-later-source dirs)
	(helm :sources
		  'code-it-later-source
          :buffer "*code-it-later*")))

(provide 'code-it-later-mode)
;;; code-it-later-mode.el ends here
