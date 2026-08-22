(defsystem :math
	   :description "Το βιβλίο άλγεβρας του λυκείου"

	   :author "Stratis Christodoulou"
	   :pathname "src"

  :components ((:file "package")
	       (:file "math")
	       (:file "lists")
	       (:file "sets")
	       (:file "utilities")
	       (:file "predicates")))

(defsystem :math/tests
  :pathname "tests"
  :components ((:file "package")
	       (:file "math-tests")
	       (:file "set-tests"))

   :perform (test-op (op c)
                     (uiop:symbol-call :review :run-tests
                                       ; :show-only-errors t 
                                       ; :color t
				       )))
