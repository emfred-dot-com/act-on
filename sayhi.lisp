;;; sayhi.lisp: example program showcasing the usage of act-on

(load "act-on.lisp")

(defun hello-world ()
  (format t "Hello, World!~%"))

(defun hello-to-someone (someone)
  (format t "Hello, ~A!~%" someone))

(defun hello-to-someone-from-someone-else (someone someone-else)
  (format t "Hello, ~A! from, ~A~%" someone someone-else))

(defun hello-to-people (someone &rest others)
  (mapcar #'hello-to-someone (cons someone others)))

(defun main ()
  (let ((codex '((nil hello-world)
		 (("to") hello-to-someone 1)
		 (("to-and-from" "to-from" "tf") hello-to-someone-from-someone-else 2)
		 (("to-many") hello-to-people 1 t))))
    (act-on *posix-argv* codex)))

;; RESULTING BEHAVIOR:

;; $ sayhi
;; > "Hello, World!"

;; $ sayhi to Name
;; > "Hello, Name!"
;; $ sayhi to
;; > "Error: sayhi: command 'to' expects 1 argument"
;; $ sayhi to Name1 Name2
;; > "Error: sayhi: command 'to' expects 1 argument"

;; $ sayhi to-and-from Name1 Name2
;; > "Hello, Name1! from, Name2"
;; $ sayhi tf Name1 Name2
;; > "Hello, Name1! from, Name2"

;; $ sayhi to-many Name1
;; > "Hello, Name1!"
;; $ sayhi to-many Name1 Name2 Name3
;; > "Hello, Name1!
;;    Hello, Name2!
;;    Hello, Name3!"
;; $ sayhi to-many
;; > "Error: sayhi: command 'to-many' expects at least 1 argument"

