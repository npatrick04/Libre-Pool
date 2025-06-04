# libre-pool:
# 	ecl \
# 		--eval "(ql:quickload :libre-pool)" \
# 		--eval "(asdf:make-build :libre-pool :type :program :move-here #P\"./\" :epilogue-code '(libre-pool:main))"

libre-pool:
	sbcl --eval "(ql:quickload :libre-pool)" \
		--eval "(sb-ext:save-lisp-and-die #p\"libre-pool\" :toplevel #'libre-pool:main :executable t)"
