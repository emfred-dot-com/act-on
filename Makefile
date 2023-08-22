all:
	sbcl --load sayhi.lisp \
	--eval "(sb-ext:save-lisp-and-die \"sayhi\" :toplevel #'main :executable t)";
