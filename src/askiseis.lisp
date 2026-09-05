(in-package :math)

;;; ============================================================
;;; ΚΕΦΑΛΑΙΟ 1
;;; ΕΦΑΡΜΟΓΗ 1
;;; ============================================================


(defparameter *possibilities-efarmogi-1*
  (make-exercise*
   1
   "Ρίχνουμε ένα νόμισμα τρεις διαδοχικές φορές."
   (make-sub-exercise*
    "i)"
    "Να γραφτεί ο δειγματικός χώρος Ω του πειράματος."  
    (make-question*
     "Να γραφτεί οδειγματικός χώρος Ω."
     (make-solution*
      "O δειγματικός χώρος Ω παράγετατι από την καρτεσιανή δύναμη του ενδεχόμενου {Κ, Γ} εις την τρίτη"
      (make-solution-step*
       "Αρα πρέπει να κληθεί η cartesian-power"
       '(list->set (cartesian-power '(Κ Γ) 3) :test #'equalp)
       :omega
       '(math-set-documentation :omega)
       ))))
   
   
   (make-sub-exercise* 
    "ii)"
    "Να παρασταθούν με αναγραφή τα ενδεχόμενα που προσδιορίζονται από την αντίστοιχη ιδιότητα:"

    (make-question* "Α₁: Ο αριθμός των Κ υπερβαίνει τον αριθμό των Γ"
		    (make-solution*
		     "Εχοντας υπόψη το δειγματικό χώρο Ω και την ιδιότητα Α₁"
		     (make-solution-step*
		      "Δημιουργώ το Α₁ "
		      '(list->set
			(remove-if-not (lambda (x) (>= (count 'Κ x) 2))	(set->list :omega)) :test #'equalp)
		      :a1
		      '(math-set-documentation :a1)))
		    )

    (make-question* "Α₂: Ο αριθμός των Κ είναι ακριβώς 2"
		    (make-solution*
		     "Εχοντας υπόψη το δειγματικό χώρο Ω και την ιδιότητα Α₂"
		     (make-solution-step*
		      "Δημιουργώ το Α₂"
		      '(list->set (remove-if-not (lambda (x) (= (count 'Κ x) 2)) (set->list :omega)):test #'equalp)
		      :a2
		      '(math-set-documentation :a2))
		     ))

    (make-question* "Α₃: Ο αριθμός των Κ είναι τουλάχιστον 2"
		    (make-solution*
		     "Εχοντας υπόψη το δειγματικό χώρο Ω και την ιδιότητα Α₃"
		     (make-solution-step*
		      "Δημιουργώ το Α₃"
		      '(list->set (remove-if-not (lambda (x) (>= (count 'Κ x) 2)) (set->list :omega)) :test #'equalp)
		      :a3
		      '(math-set-documentation :a3))
		     ))

    (make-question* "Α₄: Ίδια όψη και στις τρεις ρίψεις"
		    (make-solution*
		     "Εχοντας υπόψη το δειγματικό χώρο Ω και την ιδιότητα Α₄"
		     (make-solution-step*
		      "Δημιουργώ το Α₄"
		      '(list->set
			(remove-if-not
			 (lambda (x)
			   (or (= (count 'Κ x) 3)
			       (= (count 'Γ x) 3)))
			 (set->list :omega))
			:test #'equalp)
		      :a4
		      '(math-set-documentation :a4))
		     ))

    (make-question* "A₅: Στην πρώτη ρίψη φέρνουμε K"
		    (make-solution*
		     "Εχοντας υπόψη το δειγματικό χώρο Ω και την ιδιότητα A₅"
		     (make-solution-step*
		      "Δημιουργώ το A₅"
		      '(list->set
			(remove-if-not
			 (lambda (x)
			   (eq 'Κ (first x)))
			 (set->list :omega))
			:test #'equalp)
		      :a5
		      '(math-set-documentation :a5))
		     ))
    )

   (make-sub-exercise*
    "iii)"
    "Να βρεθούν τα ενδεχόμενα:"
    (make-question* "A'₃"
		    (make-solution*
		     "Εχοντας υπόψη το δειγματικό χώρο Ω και την ιδιότητα A₃"
		     (make-solution-step*
		      "Δημιουργώ το A'₃ -> Ω\\Α₃"
		      '(set-complement :omega :a3)
		      :a3c
		      '(math-set-documentation :a3c))
		     ))
    
    (make-question* "A₅⋂A₂"
		    (make-solution*
		     "Εχοντας υπόψη τις ιδιότητες A₅ και A₂"
		     (make-solution-step*
		      "Δημιουργώ το A₅⋂A₂"
		      '(set-intersection :a5 :a2)
		      :a5ia2
		      '(math-set-documentation :a5ia2))
		     ))
    (make-question* "A₅⋃A₄"
		    (make-solution*
		     "Εχοντας υπόψη τις ιδιότητες A₅ και A₄"
		     (make-solution-step*
		      "Δημιουργώ το A₅⋃A₄"
		      '(set-union :a5 :a4)
		      :a5ua4 
		      '(math-set-documentation :a5ua4))
		     )))))

(defparameter *askisi-a-1*
  (make-exercise*
   1
   "Ένα κουτί έχει τρεις μπάλες, μια άσπρη, μια μαύρη και μια κόκκινη. Κάνουμε το
εξής πείραμα: παίρνουμε από το κουτί μια μπάλα, καταγράφουμε το χρώμα της
και την ξαναβάζουμε στο κουτί. Στη συνέχεια παίρνουμε μια δεύτερη μπάλα και
καταγράφουμε επίσης το χρώμα της. (Όπως λέμε παίρνουμε διαδοχικά δύο μπάλες
με επανατοποθέτηση)."

   (make-question*
     "i) Ποιος είναι ο δειγματικός χώρος του πειράματος;"
     (make-solution*
     "O δειγματικός χώρος Ω θα προκύψει από τη καρτεσιανή δύναμη του {Α, Μ, Κ} στο τετράγωνο."
      (make-solution-step*
       "Υπολογίζω το Ω"
       '(list->set (cartesian-power '(Α Μ Κ) 2))
       :omega
       '(math-set-documentation :omega)
       ))
     )

   (make-question*
     "ii) Ποιο είναι το ενδεχόμενο \"η πρώτη μπάλα να είναι κόκκινη;\""
     (make-solution*
      "Tο ενδεχόμενο Α = \"η πρώτη μπάλα να είναι κόκκινη\" υπολογίζεται με
έλεγχο των στοιχείων του Ω, όπου για κάθε διατεταγμένη δυάδα (a,b)|a = Κ\" "
      (make-solution-step*
       "Υπολογίζω το Α"
       '(list->set
	 (remove-if-not
	  (lambda (x)
	    (eq 'Κ (first x)))
	  (set->list :omega))
	 :test #'equalp)
       :a
       '(math-set-documentation :a))))
   
   (make-question*
    "iii) Ποιο είναι το ενδεχόμενο \"να εξαχθεί και τις δυο φορές μπάλα με το ίδιο χρώμα\";"
     (make-solution*
      "Το ενδεχόμενο  B =  \"να εξαχθεί και τις δυο φορές μπάλα με το ίδιο χρώμα\",
προκύπτει αν αφαιρέσω τα μέλη του Ω όπου κάθε ενδεχόμενου δεν έχει ίδια μέλη! "
      (make-solution-step*
       "Υπολογίζω το Β"
       '(list->set
	 (remove-if-not
	  (lambda (x)
	    (eq (first x) (second x)))
	  (set->list :omega))
	 :test #'equalp)
       :b
       '(math-set-documentation :b))))
   ))

(defparameter *askisi-a-2*
  (make-exercise*
   2
   "Ένα κουτί έχει τρεις μπάλες, μια άσπρη, μια μαύρη και μια κόκκινη. Κάνουμε το
εξής πείραμα: παίρνουμε από το κουτί μια μπάλα, καταγράφουμε το χρώμα της
και ΔΕΝ την ξαναβάζουμε στο κουτί. Στη συνέχεια παίρνουμε μια δεύτερη μπάλα και
καταγράφουμε επίσης το χρώμα της. (Όπως λέμε παίρνουμε διαδοχικά δύο μπάλες
χωρίς επανατοποθέτηση)."

   (make-question*
     "i) Ποιος είναι ο δειγματικός χώρος του πειράματος;"
     (make-solution*
      "O δειγματικός χώρος Ω θα προκύψει από την εφαρμογή της συνάρτησης (permutations-without-replacement list n)
με λίστα εφαρμογής (list) την {Α, Μ, Κ} και εφρμόζοντας το βήμα (n) 2 φορές ."
      (make-solution-step*
       "Υπολογίζω το Ω"
       '(list->set (permutations-without-replacement '(Α Μ Κ) 2))
       :omega
       '(math-set-documentation :omega)
       ))
     )

   (make-question*
     "ii) Ποιο είναι το ενδεχόμενο \"η πρώτη μπάλα να είναι κόκκινη;\""
     (make-solution*
      "Tο ενδεχόμενο Α = \"η πρώτη μπάλα να είναι κόκκινη\" υπολογίζεται με
έλεγχο των στοιχείων του Ω, όπου για κάθε διατεταγμένη δυάδα (a,b)|a = Κ\" "
      (make-solution-step*
       "Υπολογίζω το Α"
       '(list->set
	 (remove-if-not
	  (lambda (x)
	    (eq 'Κ (first x)))
	  (set->list :omega))
	 :test #'equalp)
       :a
       '(math-set-documentation :a))))
   
   (make-question*
    "iii) Ποιο είναι το ενδεχόμενο \"να εξαχθεί και τις δυο φορές μπάλα με το ίδιο χρώμα\";"
     (make-solution*
      "Το ενδεχόμενο  B =  \"να εξαχθεί και τις δυο φορές μπάλα με το ίδιο χρώμα\",
προκύπτει αν αφαιρέσω τα μέλη του Ω όπου κάθε ενδεχόμενου δεν έχει ίδια μέλη!
Αυτό είναι το κενό σύνολο, αφού δεν υπάρχει τη δεύτερη φορά η μπάλα που έπεσε την πρώτη φορά!  "
      (make-solution-step*
       "Υπολογίζω το Β"
       '(list->set
	 (remove-if-not
	  (lambda (x)
	    (eq (first x) (second x)))
	  (set->list :omega))
	 :test #'equalp)
       :b
       '(math-set-documentation :b))))
   ))

(defparameter *askisi-a-3*
  (make-exercise*
   3
   "Μια οικογένεια από την Αθήνα αποφασίζει να κάνει τις επόμενες διακοπές της στην
Κύπρο ή στη Μακεδονία. Στην Κύπρο μπορεί να πάει με αεροπλάνο ή με πλοίο.
Στη Μακεδονία μπορεί να πάει με το αυτοκίνητό της, με τρένο ή με αεροπλάνο. Αν
ως αποτέλεσμα του πειράματος θεωρήσουμε τον τόπο διακοπών και το ταξιδιωτικό
μέσο, τότε:"
   (make-question*
    "i) Να γράψετε το δειγματικό χώρο Ω του πειράματος."
    (make-solution*
     "Για να βρω τον δειγματικό χώρο Ω πρέπει να υπολογίσω πρώτα τον
Ω₁= ΚΥΠΡΟΣ Χ {ΑΕΡΟΠΛΑΝΟ, ΠΛΟΙΟ} και
Ω₂= ΜΑΚΕΔΟΝΙΑ Χ {ΑΕΡΟΠΛΑΝΟ, ΤΡΕΝΟ, ΑΥΤΟΚΙΝΗΤΟ}"
     
     (make-solution-step*
      "Ω₁ = ΚΥΠΡΟΣ Χ {ΑΕΡΟΠΛΑΝΟ, ΠΛΟΙΟ}"
      '(list->set (cartesian-product '(ΚΥΠΡΟΣ) '(ΑΕΡΟΠΛΑΝΟ ΠΛΟΙΟ)))
      :omega1
      '(math-set-documentation :omega1))

     (make-solution-step*
      "Ω₂ = ΜΑΚΕΔΟΝΙΑ Χ {ΑΕΡΟΠΛΑΝΟ, ΤΡΕΝΟ, ΑΥΤΟΚΙΝΗΤΟ}"
      '(list->set (cartesian-product '(ΜΑΚΕΔΟΝΙΑ) '(ΑΕΡΟΠΛΑΝΟ ΤΡΕΝΟ ΑΥΤΟΚΙΝΗΤΟ)))
      :omega2
      '(math-set-documentation :omega2))

     (make-solution-step*
      "Ω = Ω₁ ⋃ Ω₂"
      '(set-union :omega1 :omega2)
      :omega
      '(math-set-documentation :omega))

     
      )
     )
   (make-question*
    "ii) Να βρείτε το ενδεχόμενο Α: \"Η οικογένεια θα πάει με αεροπλάνο στον τόπο των
διακοπών της\". "

    (make-solution*
     "Για να βρω το ενδεχόμενο Α θα φιλτράρω τον δειγματικό χώρο Ω, έτσι ώστε να έχω
μόνο τα ενδεχόμενα με μεσο το ΑΕΡΟΠΛΑΝΟ"
     (make-solution-step*
      "A= (remove-if-not (lambda (x) (eq 'ΑΕΡΟΠΛΑΝΟ (second x))) (set->list :omega))"
      '(list->set (remove-if-not (lambda (x) (eq 'ΑΕΡΟΠΛΑΝΟ (second x))) (set->list :omega)))
      :a
      '(math-set-documentation :a))
     )
    )))

(defparameter *askisi-a-4*
  (make-exercise*
   4
   "Ένα ξενοδοχείο προσφέρει γεύμα που αποτελείται από τρία πιάτα. Το κύριο πιάτο,
το συνοδευτικό και το γλυκό. Οι δυνατές επιλογές δίνονται στον παρακάτω πίνακα:
    Γεύμα          Επιλογές
    Κύριο πιάτο    Κοτόπουλο ή φιλέτο
    Συνοδευτικό    Μακαρόνια ή ρύζι ή χόρτα
    Γλυκό          Παγωτό ή τούρτα ή ζελέ
Ένα άτομο πρόκειται να διαλέξει ένα είδος από κάθε πιάτο,"
   (make-question*
    "i) Να γράψετε το δειγματικό χώρο Ω του πειράματος."
    (make-solution*
     "Ο δειγματικός χώρος είναι "
     
     (make-solution-step*
      "Ω={Κ, Φ} Χ {Μ, Ρ, Χ} Χ {Π, Τ, Ζ}"
      '(list->set (cartesian-product '(Κ Φ) '(Μ Ρ Χ) '(Π Τ Ζ)))
      :omega
      '(math-set-documentation :omega))))
   
   (make-question*
    "ii) Να βρείτε το ενδεχόμενο Α:\"το άτομο επιλέγει παγωτό\". "

    (make-solution*
     "Για να βρω το ενδεχόμενο Α θα φιλτράρω τον δειγματικό χώρο Ω, έτσι ώστε να έχω
μόνο τα ενδεχόμενα με παγωτό"
     (make-solution-step*
      "A = (remove-if-not (lambda (x) (member 'Π x)) (set->list :omega))"
      '(list->set (remove-if-not (lambda (x) (member 'Π x)) (set->list :omega)))
      :a
      '(math-set-documentation :a))))

   
   (make-question*
    "iii) Να βρείτε το ενδεχόμενο Β:\"το άτομο επιλέγει κοτόπουλο\". "

    (make-solution*
     "Για να βρω το ενδεχόμενο B θα φιλτράρω τον δειγματικό χώρο Ω, έτσι ώστε να έχω
μόνο τα ενδεχόμενα με κοτόπουλο"
     (make-solution-step*
      "B = (remove-if-not (lambda (x) (member 'Κ x)) (set->list :omega))"
      '(list->set (remove-if-not (lambda (x) (member 'Κ x)) (set->list :omega)))
      :b
      '(math-set-documentation :b))))


   (make-question*
    "iv) Να βρείτε το ενδεχόμενο: Α⋂B"

    (make-solution*
     "Για να βρω την τομή του Α και Β "
     (make-solution-step*
      "Α⋂B = (set-intersection :a :b)"
      '(set-intersection :a :b)
      :aib
      '(math-set-documentation :aib))))


   (make-question*
    "v) Αν Γ το ενδεχόμενο: \"το άτομο επιλέγει ρύζι\", να βρείτε το ενδεχόμενο Γ: (Α⋂B)⋂Γ"

    (make-solution*
     "Για να βρω το (Α⋂B)⋂Γ πρέπει να υπολογίσω το Γ (αφού το Α⋂B το βρήκα στο (iv), θα φιλτράρω
το Ω με ενδεχόμενο το ρύζι "
     (make-solution-step*
      "Γ =  (remove-if-not (lambda (x) (member 'Ρ x)) (set->list :omega))"
      '(list->set (remove-if-not (lambda (x) (member 'Ρ x)) (set->list :omega)))
      :g
      '(math-set-documentation :g))

     (make-solution-step*
      "(Α⋂B)⋂Γ = (set-itnersection  (Α⋂B) Γ)"
      '(set-intersection :aIb :g)
      :aIbIg 
      '(math-set-documentation :aIbIg))))))

(defparameter *askisi-a-5*
  (make-exercise*
   5
   "Η διεύθυνση ενός νοσοκομείου κωδικοποιεί τους ασθενείς σύμφωνα με το αν είναι
ασφαλισμένοι ή όχι και σύμφωνα με την κατάσταση της υγείας τους, η οποία χαρα-
κτηρίζεται ως καλή, μέτρια, σοβαρή ή κρίσιμη. Η διεύθυνση καταγράφει με 0 τον
ανασφάλιστο ασθενή και με 1 τον ασφαλισμένο, και στη συνέχεια δίπλα γράφει ένα
από τα γράμματα α, β, γ ή δ, ανάλογα με το αν η κατάστασή του είναι καλή, μέτρια,
σοβαρή ή κρίσιμη. Θεωρούμε το πείραμα της κωδικοποίησης ενός νέου ασθενούς.
Να βρείτε:"
   (make-question*
    "i) Το δειγματικό χώρο Ω του πειράματος."
    (make-solution*
     "Ο δειγματικός χώρος είναι "
     
     (make-solution-step*
      "Ω= {0, 1} Χ {Α, Β, Γ, Δ}"
      '(list->set (cartesian-product '(0 1) '(Α Β Γ Δ)))
      :omega
      '(math-set-documentation :omega))))
   
   (make-question*
    "ii) Το ενδεχόμενο Α:\"η κατάσταση του ασθενούς είναι σοβαρή ή κρίσιμη και είναι
ανασφάλιστος\". "

    (make-solution*
     "Για να βρω το ενδεχόμενο Α θα φιλτράρω τον δειγματικό χώρο Ω, έτσι ώστε να έχω
μόνο τα ενδεχόμενα με {0,Γ} ⋃ {0, Δ}"
     (make-solution-step*
      "A = (remove-if-not (lambda (x) (and (zerop (first x))
                                           (or (member 'Γ x) (member 'Δ x))) (set->list :omega))"
      '(list->set (remove-if-not (lambda (x) (and (zerop (first x))
						  (or (member 'Γ x)
						      (member 'Δ x))))
		   (set->list :omega)))
      :a
      '(math-set-documentation :a))))

   
   (make-question*
    "iii)Το ενδεχόμενο Β:\"η κατάσταση του ασθενούς είναι καλή ή μέτρια\". "

    (make-solution*
     "Για να βρω το ενδεχόμενο B θα φιλτράρω τον δειγματικό χώρο Ω, έτσι ώστε να έχω
μόνο τα ενδεχόμενα {{0, A} {1, A} {0, Β} {1, Β}}"
     (make-solution-step*
      "B = (remove-if-not (lambda (x) (or (member 'Α x) (member 'Β x)) (set->list :omega))"
      '(list->set (remove-if-not (lambda (x) (or (member 'Α x) (member 'Β x))) (set->list :omega)))
      :b
      '(math-set-documentation :b))))


   (make-question*
    "iv) Το ενδεχόμενο Γ: \"ο ασθενής είναι ασφαλισμένος\""

    (make-solution*
     "Για να βρω το ενδεχόμενο B θα φιλτράρω τον δειγματικό χώρο Ω, έτσι ώστε να έχω
μόνο τα ενδεχόμενα (a, b)|a=1 "
     (make-solution-step*
      "Γ = (list->set (remove-if-not (lambda (x) (= 1 (first x)) (set->list :omega))))"
      '(list->set (remove-if-not (lambda (x) (= 1 (first x))) (set->list :omega)))
      :g
      '(math-set-documentation :g))))))

(defparameter *askisi-b-1*
  (make-exercise*
   1
   "Δύο παίκτες θα παίξουν σκάκι και συμφωνούν νικητής να είναι εκείνος που πρώτος
θα κερδίσει δύο παιχνίδια. Αν α είναι το αποτέλεσμα να κερδίσει ο πρώτος παίκτης
ένα παιχνίδι και β είναι το αποτέλεσμα να κερδίσει ο δεύτερος παίκτης ένα παιχνίδι,
να γράψετε το δειγματικό χώρο του πειράματος."
   (make-question*
    ""
    (make-solution*
     "Ο δειγματικός χώρος είναι Ω= {Α, Β}³ - ({A, A, A } ⋃ {B, B, B} + {{A, A} ⋃ {B, B}} "
     
     (make-solution-step*
      "βρίσκω το C = {Α, Β}³"
      '(list->set (cartesian-power '(Α Β ) 3))
      :c
      '(math-set-documentation :c))

     (make-solution-step*
      "βρίσκω το D = C - {AAA, BBB}"
      '(list->set (remove-if (lambda (x)  (or (> (count 'Α x) 2) (> (count 'Β x) 2))) (set->list :c)))
      :d
      '(math-set-documentation :d))

     (make-solution-step*
      "βρίσκω το Ω = (D - {AAB, BBA}) ⋃ {AA BB}"
      '(list->set (mapcar (lambda (x) (if (eq (first x) (second x))
					  (list (first x) (second x))
					  x))
		   (set->list :d)))
      :omega
      '(math-set-documentation :omega))))))

(defparameter *askisi-b-2*
  (make-exercise*
   2
   "Ρίχνουμε ένα ζάρι δύο φορές. Να βρείτε τα ενδεχόμενα:"
   (make-question*
    "A: Το αποτέλεσμα της 1ης ρίψης είναι μεγαλύτερο από το αποτέλεσμα της 2ης ρίψης"
    (make-solution*
     "για να βρω το δεδομένο Α μου χρειάζεται ο δειγματικός χώρος Ω."
     
     (make-solution-step*
      "Ο δειγματικός χώρος είναι Ω= {1, 2 , 3, 4, 5, 6}²"
      '(list->set (cartesian-power '(1 2 3 4 5 6 ) 2) :test #'equalp)
      :omega
      '(math-set-documentation :omega))

     (make-solution-step*
      "βρίσκω το A = (remove-if-not (lambda (x) (> (first x) (second x) (set->list :omega))))"
      '(list->set (remove-if-not (lambda (x) (> (first x) (second x))) (set->list :omega)))
      :a
      '(math-set-documentation :a))))

   (make-question*
    "B: Το άθροισμα των ενδείξεων στις δύο ρίψεις είναι άρτιος αριθμός."
    (make-solution*
     "Έχω υπολογίσει ήδη το δειγματικό χώρο Ω. Αφαιρώ όσα ενδεχόμενα δεν εχουν άρτιο άθροισμα."
     
     (make-solution-step*
      "Β = (remove-if-not (lambda (x) (evenp (+ (first x) (second x)))) (Set->list :omega))"
      '(list->set (remove-if-not (lambda (x) (evenp (+ (first x) (second x)))) (Set->list :omega)))
      :b
      '(math-set-documentation :b))))

   
   (make-question*
    "Γ: Το γινόμενο των ενδείξεων στις δύο ρίψεις είναι μικρότερο του 5"
    (make-solution*
     "Έχω υπολογίσει ήδη το δειγματικό χώρο Ω. Κρατώ μόνο τα ενδεχόμενα των οποίων το γινόμενο είναι μικρότερο του 5."
     
     (make-solution-step*
      "Β = (remove-if-not (lambda (x) (< (* (first x) (second x)) 5)) (Set->list :omega))"
      '(list->set (remove-if-not (lambda (x) (< (* (first x) (second x)) 5)) (Set->list :omega)))
      :g
      '(math-set-documentation :g))))

   (make-question*
    "Στη συνέχεια να βρείτε τα ενδεχόμενα Α⋂Β, Α⋂Γ, Β⋂Γ, (A⋂B)⋂Γ"
    (make-solution*
     "Έχω υπολογίσει ήδη τα Α,Β,Γ. Οπότε:"
     
     (make-solution-step*
      "Α⋂Β = (set-intersection :a :b)"
      '(set-intersection :a :b)
      :aIb
      '(math-set-documentation :aIb))

     (make-solution-step*
      "Α⋂Γ = (set-intersection :a :g)"
      '(set-intersection :a :g)
      :aIg
      '(math-set-documentation :aIg))

     (make-solution-step*
      "B⋂Γ = (set-intersection :b :g)"
      '(set-intersection :b :g)
      :bIg
      '(math-set-documentation :bIg))
     
     (make-solution-step*
      "(A⋂B)⋂Γ = (set-intersection :aIb :g)"
      '(set-intersection :aIb :g)
      :aIbIg
      '(math-set-documentation :aIbIg))))))
