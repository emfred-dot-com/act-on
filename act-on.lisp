;;; act-on.lisp: declarative argument handling

(defun act-on/fail (prog-name msg)
  (format t "Error: ~A: ~A~%" prog-name msg))

(defun act-on/codex-extract-from-entry (codex action process-entry)
  (if codex
      (let ((this-entry (car codex))
	    (other-entries (cdr codex)))
	(if (or (find action (car this-entry) :test #'equal)
		(equal (car this-entry) action))
	    (funcall process-entry this-entry)
	    (act-on/codex-extract-from-entry other-entries action process-entry)))
      nil))

(defun act-on/codex-action-exists-p (codex action)
  (act-on/codex-extract-from-entry codex action
			    (lambda (&optional discarded-arg)
			      t)))

(defun act-on/codex-function-from-action (codex action)
  (act-on/codex-extract-from-entry codex action
			    (lambda (entry)
			      (cadr entry))))

(defun act-on/codex-minimum-number-of-args-from-action (codex action)
  (act-on/codex-extract-from-entry codex action
			    (lambda (entry)
			      (caddr entry))))

(defun act-on/codex-action-accepts-ge-min-args-p (codex action)
  (act-on/codex-extract-from-entry codex action
			    (lambda (entry)
			      (cadddr entry))))

(defun act-on (program-args codex)
  ;; deconstruct program-args into the useful pieces
  (let ((prog-name (car program-args))
	(action (cadr program-args))
	(action-args (cddr program-args)))
    ;; if no action given, consult codex for a nil entry, and fail if
    ;; no nil entry present
    (if (not action)
	(if (act-on/codex-action-exists-p codex nil)
	    (apply (act-on/codex-function-from-action codex nil) nil)
	    (act-on/fail prog-name "no command given"))
	;; else, consult codex for an entry matching action, failing
	;; if action is not found
	(if (act-on/codex-action-exists-p codex action)
	    ;; if the action is found and enough args are given to it,
	    ;; then execute it, failing otherwise
	    (let ((min-args (act-on/codex-minimum-number-of-args-from-action codex action))
		  (compare (if (act-on/codex-action-accepts-ge-min-args-p codex action)
			       #'>=
			       #'=)))
	      (if (funcall compare (length action-args) min-args)
		  (apply (act-on/codex-function-from-action codex action) action-args)
		  (act-on/fail
		   prog-name
		   (let ((arguments-maybe-plural
			   (if (= min-args 1)
			       "argument"
			       "arguments"))
			 (maybe-at-least
			   (if (eq compare #'>=)
			       "at least "
			       "")))
		     (format nil "command '~A' expects ~A~A ~A"
			     action maybe-at-least min-args arguments-maybe-plural)))))
	    (act-on/fail prog-name (format nil "unknown command '~A'" action))))))
