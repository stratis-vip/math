(defsystem :math
	   :description "Το βιβλίο άλγεβρας του λυκείου"

	   :author "Stratis Christodoulou"
	   :pathname "src"

  :depends-on ("lists" "strings")
  :components ((:file "package")
	       (:file "math")
	       (:file "sets")
	       (:file "utilities")
	       (:file "predicates")
	       (:file "possibilities")
	       (:file "parser")
	       (:file "exercises")
	       (:file "askiseis")
))

(defsystem :math/tests
  :depends-on ("lists" "strings" "review" "math")
  :pathname "tests"
  :components ((:file "package")
	       (:file "test-setup")
	       (:file "math-tests")
	       (:file "set-tests")
	       (:file "possibilities-tests")
	       (:file "parser-tests"))

   :perform (test-op (op c)
                     (uiop:symbol-call :review :run-tests
                                       ; :show-only-errors t 
                                       ; :color t
				       )))
