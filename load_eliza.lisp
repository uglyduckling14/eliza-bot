;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-


;;; change this parameter as needed.
(defparameter *eliza-path* 
  "C:/Users/a02391616/Downloads/cs5600_6600_f24_hw07/hw07/")

;;; the files that comprise ELIZA.
(defparameter *eliza-files* 
  '("auxfuns.lisp" "eliza.lisp")) 

(defun load-eliza-aux (path files)
  (mapc #'(lambda (file)
	    (load (concatenate 'string path file)))
	files))

;;; load ELIZA
(defun load-eliza ()
  (load-eliza-aux *eliza-path* *eliza-files*))

	
