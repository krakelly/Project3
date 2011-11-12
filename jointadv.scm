
;BASIC OBJECT class ----------------------------------------------------------------------------------------------

(define-class (basic-object)
	(instance-vars (properties (make-table)))
	(method (put key value) (insert! key value properties))
	(default-method (lookup message properties)))

;THING class ----------------------------------------------------------------------------------------------------------

(define-class (thing name)
	(parent (basic-object))
	(instance-vars (possessor 'no-one))
	(initialize (ask self 'put 'thing? #t))
	(method (type) 'thing)
	(method (change-possessor new-possessor)   
		(set! possessor new-possessor)))
		 

		



;PERSON class-------------------------------------------------------------------------------------------------------

(define-class (person name place)
  (parent (basic-object))
  (instance-vars
   (possessions '())
   (saying ""))
  (initialize (ask place 'enter self)(ask self 'put 'person? #t)
  (ask self 'put 'strength 10))
  (method (type) 'person)
  (method (look-around)
    (map (lambda (obj) (ask obj 'name))
	 (filter (lambda (thing) (not (eq? thing self)))
		 (append (ask place 'things) (ask place 'people)))))
  (method (take thing)
    (cond ((not (thing? thing)) (error "Not a thing" thing))
	  ((not (memq thing (ask place 'things)))
	   (error "Thing taken not at this place"
		  (list (ask place 'name) thing)))
	  ((memq thing possessions) (error "You already have it!"))
	  (else
	   (announce-take name thing)
	   (set! possessions (cons thing possessions))
	       
	   ;; If somebody already has this object...
	   (for-each
	    (lambda (pers)
	      (if (and (not (eq? pers self)) ; ignore myself
		       (memq thing (ask pers 'possessions)))
		  (begin
		   (ask pers 'lose thing)
		   (have-fit pers))))
	    (ask place 'people))
	       
	   (ask thing 'change-possessor self)
	   'taken)))
  (method (take-all) 
	(map (lambda (x) 
			(if (and (equal? (ask x 'type) 'thing) 
					(equal? (ask x 'possessor) 'no-one))
				(ask self 'take x))) 
	(filter (lambda (thing) (not (eq? thing self)))
		 (append (ask place 'things) (ask place 'people)))))
	
  (method (lose thing)
    (set! possessions (delete thing possessions))
    (ask thing 'change-possessor 'no-one)
    'lost)
  (method (talk) (print saying))
  (method (set-talk string) (set! saying string))
  (method (exits) (ask place 'exits))
  (method (notice person) (ask self 'talk))
  (method (go direction)
	  (let ((new-place (ask place 'look-in direction)))
	    (cond ((null? new-place)
		   (error "Can't go" direction))
		  (else
		   (if (ask new-place 'may-enter? self)
		       (begin (ask place 'exit self)
			      (announce-move name place new-place)
			      (for-each
			       (lambda (p)
				 (ask place 'gone p)
				 (ask new-place 'appear p))
			       possessions)
			      (set! place new-place))
		       (error "This building is locked") )))))
   (method (eat food)
	 (lambda (single) 
		(if (edible? single) 
			(ask self 'put 'strength (+ (ask self 'strength) (ask single 'calories)))
			'not-edible))))





;PLACE class----------------------------------------------------------------------------------------------------------
(define-class (jail name)
  (parent (place name))
  (method (exit person)
	  '(You can't leave muahaha))
  (method (exits)
	  '(There are no exits)))
	  
(define-class (place name)
  (parent (basic-object))
  (initialize (ask self 'put 'place? #t))
  (instance-vars
   (directions-and-neighbors '())
   (things '())
   (people '())
   (entry-procs '())
   (exit-procs '()))
  (method (type) 'place)
  (method (neighbors) (map cdr directions-and-neighbors))
  (method (exits) (map car directions-and-neighbors))
  (method (look-in direction)
    (let ((pair (assoc direction directions-and-neighbors)))
      (if (not pair)
	  '()                     ;; nothing in that direction
	  (cdr pair))))           ;; return the place object
  (method (appear new-thing)
    (if (memq new-thing things)
	(error "Thing already in this place" (list name new-thing))
    (set! things (cons new-thing things)))
    'appeared)
  (method (enter new-person)
    (if (memq new-person people)
	(error "Person already in this place" (list name new-person)))
    (set! people (cons new-person people))
    (for-each (lambda (proc) (proc)) entry-procs)
    'appeared)
  (method (gone thing)
    (if (not (memq thing things))
	(error "Disappearing thing not here" (list name thing)))
    (set! things (delete thing things)) 
    'disappeared)
  (method (exit person)
    (for-each (lambda (proc) (proc)) exit-procs)
    (if (not (memq person people))
	(error "Disappearing person not here" (list name person)))
    (set! people (delete person people)) 
    'disappeared)
  (method (new-neighbor direction neighbor)
    (if (assoc direction directions-and-neighbors)
	(error "Direction already assigned a neighbor" (list name direction)))
    (set! directions-and-neighbors
	  (cons (cons direction neighbor) directions-and-neighbors))
    'connected)
 (method (may-enter? person)
	  '(You may enter))
  (method (add-entry-procedure proc)
    (set! entry-procs (cons proc entry-procs)))
  (method (add-exit-procedure proc)
    (set! exit-procs (cons proc exit-procs)))
  (method (remove-entry-procedure proc)
    (set! entry-procs (delete proc entry-procs)))
  (method (remove-exit-procedure proc)
    (set! exit-procs (delete proc exit-procs)))
  (method (clear-all-procs)
    (set! exit-procs '())
    (set! entry-procs '())
    'cleared) )

(define-class (locked-place name)
  (parent (place name))
  (instance-vars (locked #t))
  (method (may-enter? person)
	  (ask self 'locked))
  (method (unlock)
	  (set! locked #f)))

(define-class (garage name)
  (parent (place name))
  (class-vars (count 0))
  (method (park auto)
	  (set! count (+ 1 count))
	  (let ((num count))
	    (if (and (not (ask auto 'auto?)) (memq auto (ask self 'things)))
		(error "This object is either not a car or it's not here")
		(begin (define ticket (instantiate ticket num))
		       (ask self 'put num auto)
		       (ask (owner auto) 'lose auto)
		       (ask (owner auto) 'take ticket)))))
  (method (unpark ticket)
	  (let ((auto (ask self (ask ticket 'num))))
	    (if auto
		(begin (ask (owner ticket) 'lose ticket)
		       (ask (owner ticket) 'take auto)
		       (insert! (ask ticket 'num) #f table))
		(error "No car matches that ticket")))))

(define-class (ticket num)
  (parent (thing num)))

(define-class (auto name)
  (parent (thing name))
  (initialize (ask self 'put 'auto? #t))
  (method (auto? #t)))


		      
;HOTSPOT Class ----------------------------------------------------------------------------------------------------

(define-class (hotspot name pass)
	(parent (place name))
	(instance-vars (list-of-connected '()) (thepassword pass))
	(method (appear new-thing)
		(if (memq new-thing (ask self 'things))
			(error "Thing already in this place" (list name new-thing))
				(if (equal? (ask new-thing 'type) 'laptop)
					(ask new-thing 'change-loc self)))
			(usual 'appear new-thing)
			'appeared)
	(method (connect lap password) 
		(if (and (equal? password pass)
			 (memq lap (ask self 'things)))
			(begin 
			(set! list-of-connected (cons lap list-of-connected))
			'connected)
			'wrong-password))
	(method (surf lap url) 
		(if (memq lap list-of-connected)
		(system (string-append "lynx " url))))
	(method (gone thing)
		(if (not (memq thing (ask self 'things)))
			(error "Disappearing thing not here" (list name thing))
				(if (equal? (ask thing 'type) 'laptop)
					(set! list-of-connected (delete thing list-of-connected))))
			(usual 'gone thing)))
	
			



;LAPTOP Class--------------------------------------------------------------------------------------------------------

(define-class (laptop name)
	(instance-vars (type 'laptop) (location '()))
	(parent (thing name))
	(method (connect password) (ask (car location) 'connect self password))
	(method (surf url) (ask (car location) 'surf self url))
	(method (change-loc loc) (set! location (cons loc location))))

;PREDICATES---------------------------------------------------------------------------------------------------------

 (define (person? obj)
  (and (procedure? obj)
       (ask obj 'person?)))

(define (thing? obj)
  (and (procedure? obj)
       (ask obj 'thing?)))
	   
(define (place? obj)
  (and (procedure? obj)
	   (ask obj 'place?)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define *foods* '(pizza potstickers coffee))

(define (edible? thing)
  (member? (ask thing 'name) *foods*))

	  
(define-class (thief name initial-place)
  (parent (person name initial-place))
  (instance-vars
   (behavior 'steal))
  (method (type) 'thief)
  (method (go-directly-to new-place)
	  (begin (ask initial-place 'exit self)
		 (announce-move name initial-place new-place)
		 (for-each
		  (lambda (p)
		    (ask intitial-place 'gone p)
		    (ask new-place 'appear p))
		  possessions)
		 (set! initial-place new-place)))	  
  
  (method (notice person)
	  (if (eq? behavior 'run)
	      (if (ask place 'exits)
		  (ask self 'go (pick-random (ask (usual 'place) 'exits))))
		  
	      (let ((food-things
		     (filter (lambda (thing)
			       (and (edible? thing)
				    (not (eq? (ask thing 'possessor) self))))
			     (ask (usual 'place) 'things))))
		(if (not (null? food-things))
		    (begin
		      (ask self 'take (car food-things))
		      (set! behavior 'run)
		      (ask self 'notice person)) )))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this next procedure is useful for moving around

(define (move-loop who)
  (newline)
  (print (ask who 'exits))
  (display "?  > ")
  (let ((dir (read)))
    (if (equal? dir 'stop)
	(newline)
	(begin (print (ask who 'go dir))
	       (move-loop who)))))


;; One-way paths connect individual places.

(define (can-go from direction to)
  (ask from 'new-neighbor direction to))


(define (announce-take name thing)
  (newline)
  (display name)
  (display " took ")
  (display (ask thing 'name))
  (newline))

(define (announce-move name old-place new-place)
  (newline)
  (newline)
  (display name)
  (display " moved from ")
  (display (ask old-place 'name))
  (display " to ")
  (display (ask new-place 'name))
  (newline))

(define (have-fit p)
  (newline)
  (display "Yaaah! ")
  (display (ask p 'name))
  (display " is upset!")
  (newline))


(define (pick-random set)
  (nth (random (length set)) set))

(define (delete thing stuff)
  (cond ((null? stuff) '())
	((eq? thing (car stuff)) (cdr stuff))
	(else (cons (car stuff) (delete thing (cdr stuff)))) ))

(define (person? obj)
  (and (procedure? obj)
       (member? (ask obj 'type) '(person police thief))))

(define (thing? obj)
  (and (procedure? obj)
       (eq? (ask obj 'type) 'thing)))

