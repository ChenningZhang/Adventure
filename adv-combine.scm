;; ADV.SCM
;; This file contains the definitions for the objects in the adventure
;; game and some utility procedures.
;;PersonA: Reia Cho (cs61as-bu)
;;PersonB: Chenning Zhang(cs61as-ed)

;MODIFICATION-personB basic-object
(define-class (basic-object)
  (instance-vars (properties (make-table)))
  (method (put key value)
    (insert! key value properties))
  (default-method (lookup message properties)))

(define-class (place name)
  ;personB
  (parent (basic-object))
  (instance-vars
   (directions-and-neighbors '())
   (things '())
   (people '())
   (entry-procs '())
   (exit-procs '())
   ;personB
   (place? #t))
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
	(error "Thing already in this place" (list name new-thing)))
    (set! things (cons new-thing things))
    'appeared)
  (method (enter new-person)
    (if (memq new-person people)
	(error "Person already in this place" (list name new-person)))
    (for-each
     (lambda (person)
       (ask person 'notice new-person))
     people)
    (set! people (cons new-person people))
    (for-each (lambda (proc) (proc)) entry-procs)
    'appeared)
  (method (may-enter? person) #t)
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

(define-class (person name place)
  ;personB
  (parent (basic-object))
  (instance-vars
   (possessions '())
   (saying "")
   ;personB
   (person? #t))
  (initialize
    ;personB
    (ask self 'put 'strength 100)
    (ask place 'enter self)
    ;personA
    (ask self 'put 'money 100))
  (method (type) 'person)
  (method (look-around)
    (map (lambda (obj) (ask obj 'name))
	 (filter (lambda (thing) (not (eq? thing self)))
		 (append (ask place 'things) (ask place 'people)))))
  ;personB
  (method (eat)
    (let ((things-edible
	   (filter (lambda (thing) (ask thing 'edible?))
		   (ask self 'possessions))))
      (ask self 'put 'strength
	   (accumulate + (ask self 'strength) (map (lambda (obj) (ask obj 'calories)) things-edible)))
      (map (lambda (obj) (ask self 'lose obj)) things-edible)
      (map (lambda (obj) (ask (ask self 'place) 'gone obj)) things-edible)
      "I've eaten all that I can eat!"
      ))

  ;personA
  (method (get-money amnt)
    (ask self 'put 'money
	 (+ (ask self 'money) amnt)))
  (method (pay-money amnt)
    (if (<= amnt (ask self 'money))
	(begin (ask self 'put 'money (- (ask self 'money) amnt))
	       #t)
	#f))
  ;personB
  (method (take-all)
    (let ((things-available
	   (filter (lambda (thing) (eq? (ask thing 'possessor) 'no-one))
		   (ask (ask self 'place) 'things))))
      (map (lambda (thing) (ask self 'take thing)) things-available)))
  ;personB
  (method (take thing)
    (cond ((equal? (ask thing 'possessor) 'no-one)
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
	  ((ask thing 'may-take? self)
	   (begin (announce-take name thing)
		  (set! possessions (cons thing possessions))
		  (ask (ask thing 'possessor) 'lose thing)
		  (ask thing 'change-possessor self)
		  'taken))
	  (else (error "The receiver is so weak"))))
  
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
	    ;;personA 
	    ((not (ask new-place 'may-enter? name))
	     (error "locked room"))
	    (else
	     (ask place 'exit self)
	     (announce-move name place new-place)
	     (for-each
	      (lambda (p)
		(ask place 'gone p)
		(ask new-place 'appear p))
	      possessions)
	     (set! place new-place)
	     (ask new-place 'enter self)))))
  ;personA
  (method (park vehical)
    (if (ask place 'garage?)
	(ask place 'park vehical)
	(error "not in a garage")))
  (method (unpark ticket)
    (cond ((not (eq? (ask ticket 'name) 'ticket))
	   (error "not a ticket"))
	  ((not (ask place 'garage?)) (error "not in a garage"))
	  (else (ask place 'unpark ticket))))
  (method (change-place new-place)
    (set! place new-place))
  ;personA
  (method (go-directly-to new-place)
    (if (null? new-place)
	(error "Can't go" new-place)
	(begin
	  (ask (ask self 'place) 'exit self)
	  (announce-move name (ask self 'place) new-place)
	  (for-each
	   (lambda (p)
	     (ask place 'gone p)
	     (ask new-place 'appear p))
	   (ask self 'possessions))
	  (ask self 'change-place new-place)
	  (ask new-place 'enter self))))
  ;personA
  (method (buy food)
    (let ((thing (ask place 'sell self food)))
      (if thing
	  (begin (announce-take name thing)
		 (set! possessions (cons thing possessions))
		 (ask place 'appear thing))))))

(define thing
  ;personB
  (let ((my-thing (instantiate basic-object)))
    (let ()
      (lambda (class-message)
	(cond
	 ((eq? class-message 'instantiate)
	  (lambda (name)
	    (let ((self '()) (possessor 'no-one) (thing? #t));personB
	      (define (dispatch message)
		(cond
		 ((eq? message 'initialize)
		  (lambda (value-for-self)
		    (set! self value-for-self)))
		 ((eq? message 'send-usual-to-parent)
		  (error "Can't use USUAL without a parent." 'thing))
		 ((eq? message 'name) (lambda () name))
		 ((eq? message 'possessor) (lambda () possessor))
		 ((eq? message 'type) (lambda () 'thing))
		 ((eq? message 'thing?) (lambda () #t))
		 ((eq? message 'change-possessor)
		  (lambda (new-possessor)
		    (set! possessor new-possessor)))
		 ;personB
		 ((eq? message 'may-take?)
		  (lambda (receiver)
		    (if (< (ask receiver 'strength) (ask (ask self 'possessor) 'strength))
			#f
			self)))
		 (else (my-thing message))))
	      dispatch)))
	 (else (error "Bad message to class" class-message)))))))
;personB
(define-class (hotspot name password)
  (parent (place name))
  (instance-vars (connected-laptops nil))
  (method (hotspot?) #t)
  (method (connect laptop pw)
    (if (and (equal? password pw)
	     (memq laptop (ask self 'things))
	     (not (equal? 'no-one (ask laptop 'possessor))))
	(set! connected-laptops (cons laptop connected-laptops))
	"cannot connect"))
  (method (surf laptop text)
    (let ((url text))
      (if (and (memq laptop connected-laptops) (not (equal? 'no-one (ask laptop 'possesor))))
	  (system (string-append "lynx " url))
	  "laptop is not connected")))
  (method (gone laptop)
    (if (not (memq laptop (ask self 'things)))
	(error "Disappearing laptop not here")
	(begin (usual 'gone laptop)
	       (set! connected-laptops (delete laptop connected-laptops))
	       'disappeared))))
;personB
(define-class (laptop name)
  (parent (thing name))
  (method (connect pw)
    (if (and (not (equal? 'no-one (ask self 'possessor))) (ask (ask (ask self 'possessor) 'place) 'hotspot?))
	(ask (ask (ask self 'possessor) 'place) 'connect self pw)
	"Cannot connect"))
  (method (surf text)
    (if (and (not (equal? 'no-one (ask self 'possessor))) (ask (ask (ask self 'possessor) 'place) 'hotspot?))
	(ask (ask (ask self 'possessor) 'place) 'surf self text)
	"Cannot Surf"))
  (method (leave)
    (ask (ask (ask self 'possessor) 'place) 'gone self)))
;personA
(define-class (locked-place place-name)
  (parent (place place-name))
  (instance-vars (lock-status #f))
  (method (may-enter? person) lock-status)
  (method (unlock)
    (set! lock-status #t)))


;personA
(define-class (garage garage-name)
  (parent (place garage-name))
  (instance-vars (car-holder (make-table)) (garage? #t))
  (class-vars (ticket-value 0))
  (method (park thing)
    (set! ticket-value (+ ticket-value 1))
    ;;create ticket
    (ask self 'appear (instantiate ticket ticket-value))
    ;;vehical holder takes ticket
    (ask (find-obj-holder thing (ask self 'people))
	 'take (car (ask self 'things)))
    ;;remove vehical from person
    (ask (find-obj-holder thing (ask self 'people))
	 'lose thing)
    ;;put vehical into table
    (insert! ticket-value thing car-holder))
  (method (unpark ticket)
    (let ((ticket-value (ask ticket 'tic-num)) (ticket-holder (find-obj-holder ticket (ask self 'people)))(vehical (lookup ticket-value car-holder)))
      (if vehical
	  (begin (ask ticket-holder 'take vehical) ;;get car
		 (ask ticket-holder 'lose ticket) ;;get rid of ticket
		 (ask self 'gone ticket) ;;remove from place
		 (insert! ticket-value #f car-holder)) ;; dont allow retake car
	  (error "that car is not here")))))
(define (find-obj-holder obj list-persons) ;;figures out who to return the item to
  (if (memq obj (ask (car list-persons) 'possessions))
      (car list-persons)
      (find-obj-holder obj (cdr list-persons))))
;personA
(define-class (restaurant restaurant-name food price)
  (parent (place restaurant-name))
  (method (menu)
    (list (ask food 'name) price))
  (method (sell buyer buy-food)
    (cond ((not (equal? buy-food (ask food 'name))) (display "you can't buy that here"))
	  ;;
	  ((ask buyer 'police?) (instantiate food))
	  ((ask buyer 'pay-money price) (instantiate food))
	  (else #f))))
;personA
(define-class (ticket tic-num)
  (parent (thing 'ticket)))
;personB
(define-class (food name calories)
  (parent (thing name))
  (instance-vars (edible? #t)))
(define-class (bagel)
  (class-vars (name 'bagel)
	      (calories 200))
  (parent (food 'bagel calories)))
(define-class (coffee)
  (class-vars (name 'coffee)
	      (calories 100))
  (parent (food 'coffee calories)))
;personB
(define-class (police name place)
  (parent (person name place))
  (instance-vars (police? #t))
  (initialize (ask self 'put 'strength 500))
  (method (notice person) (if (thief? person)
			      (begin (ask self 'set-talk "Crime Does Not Pay")
				     (ask self 'talk)
				     (map (lambda (obj) (ask self 'take obj))
					  (ask person 'possessions))
				     (ask person 'go-directly-to jail))
			      (ask self 'talk)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *foods* '(pizza potstickers coffee))
;personB
(define (edible? obj)
  (ask obj 'edible?))

;;personA
(define-class (thief name initial-place)
  (parent (person name initial-place))
  (instance-vars
   (behavior 'steal)
   (thief? #t))
  (initialize (ask self 'put 'strength 300))
  (method (notice person)
    (if (and (eq? behavior 'run) (not (null? (ask self 'exits))))
	(ask self 'go (pick-random (ask (usual 'place) 'exits)))
	(let ((food-things
	       (filter (lambda (thing)
			 (and (edible? thing)
			      (not (eq? (ask thing 'possessor) self))))
		       (ask (usual 'place) 'things))))
	  (if (not (null? food-things))
	      (begin
		(map (lambda (obj) (ask self 'take obj)) food-things)
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
;personB
(define (person? obj)
  (ask obj 'person?))

(define (thing? obj)
  (ask obj 'thing?))

(define (place? obj)
  (ask obj 'place?))

(define (thief? obj)
  (ask obj 'thief?))

(define (police? obj)
  (ask obj 'police?))
