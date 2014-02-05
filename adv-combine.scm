;;;  Data for adventure game.  This file is adv-world.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting up the world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PersonA: Reia Cho (cs61as-bu)
;;PersonB: Chenning Zhang(cs61as-ed)

(load "obj.scm")
(load "tables.scm")
(load "adv-combine.scm")

(define Soda (instantiate place 'Soda))
(define BH-Office (instantiate place 'BH-Office))
(define MJC-Office (instantiate place 'MJC-Office))
(define art-gallery (instantiate place 'art-gallery))
(define Pimentel (instantiate place 'Pimentel))
(define 61A-Lab (instantiate place '61A-Lab))
(define Sproul-Plaza (instantiate place 'Sproul-Plaza))
(define Telegraph-Ave (instantiate place 'Telegraph-Ave))
(define Haas (instantiate place 'Haas-Business-School))
(define s-h (instantiate place 'sproul-hall))
(define Chenning-home (instantiate place 'Chenning-home))
(define Unit1 (instantiate place 'Unit1))
(define Intermezzo (instantiate place 'Intermezzo))



(can-go Haas 'south Unit1)
(can-go Unit1 'north Haas)
(can-go Soda 'up art-gallery)
(can-go art-gallery 'down Soda)
(can-go art-gallery 'west BH-Office)
(can-go BH-Office 'east art-gallery)
(can-go art-gallery 'east MJC-Office)
(can-go MJC-office 'west art-gallery)
(can-go Soda 'down 61A-Lab)
(can-go 61A-Lab 'up Soda)
(can-go Soda 'south Pimentel)
(can-go Pimentel 'north Soda)
(can-go Pimentel 'south Haas)
(can-go Haas 'north Pimentel)
(can-go Haas 'west s-h)
(can-go s-h 'east Haas)
(can-go Sproul-Plaza 'east s-h)
(can-go s-h 'west Sproul-Plaza)
(can-go Sproul-Plaza 'north Pimentel)
(can-go Sproul-Plaza 'south Telegraph-Ave)
(can-go Telegraph-Ave 'north Sproul-Plaza)
(can-go Chenning-home 'east Soda)
(can-go Soda 'west Chenning-home)

;; somepeople.
; MOVED above the add-entry-procedure stuff, to avoid the "The compuers
; seem to be down" message that would occur when hacker enters 61a-lab
; -- Ryan Stejskal

(define Brian (instantiate person 'Brian BH-Office))
(define hacker (instantiate person 'hacker 61A-lab))
(define nasty (instantiate thief 'nasty sproul-plaza))
(define Chenning (instantiate person 'chenning Chenning-home))
(define Reia (instantiate person 'Reia Unit1))
(define policeman (instantiate police 'policeman s-h))


(define (bh-office-exit)
  (print "What's your favorite programming language?")
  (let ((answer (read)))
    (if (eq? answer 'scheme)
	(print "Good answer, but my favorite is Logo!")
	(begin (newline) (bh-office-exit)))))
    

(ask s-h 'add-entry-procedure
 (lambda () (print "Miles and miles of students are waiting in line...")))

(ask BH-Office 'add-exit-procedure bh-office-exit)
(ask Telegraph-Ave 'add-entry-procedure
 (lambda () (print "There are tie-dyed shirts as far as you can see...")))
(ask 61A-Lab 'add-entry-procedure
 (lambda () (print "The computers seem to be down")))
(ask 61A-Lab 'add-exit-procedure
 (lambda () (print "The workstations come back to life just in time.")))


;;Question 1
(define Kirin (instantiate place 'Kirin))
(can-go Soda 'north Kirin)
(can-go Kirin 'south Soda)
(define potstickers (instantiate thing 'Potstickers))
(ask Kirin 'appear Potstickers)

;;Question A3:
(define sproul-hall-exit
  (let ((counter 0))
    (lambda () 
    (if (= counter 3)
	(ask s-h 'remove-exit-procedure sproul-hall-exit)
	(begin (set! counter (+ counter 1))
	  (error "You can check out any time you'd like, but you can never leave"))))))
(ask s-h 'add-exit-procedure sproul-hall-exit)

;;A4 P1:
(define singer (instantiate person 'rick sproul-plaza))
(ask singer 'set-talk "My funny valentine, sweet comic valentine")
(define preacher (instantiate person 'preacher sproul-plaza))
(ask preacher 'set-talk "Praise the Lord")
(define street-person (instantiate person 'harry telegraph-ave))
(ask street-person 'set-talk "Brother, can you spare a buck")

;;A4 P2:
(define office (instantiate locked-place 'office))
(can-go Unit1 'east office)
(can-go office 'west Unit1)

;;A5
(define Underhill (instantiate garage 'underhill))
(can-go underhill 'north Unit1)
(can-go Unit1 'south underhill)
(define vehical (instantiate thing 'vehical))
(ask underhill 'appear vehical)

;;A6
(define jail (instantiate place 'jail))
(define nasty2 (instantiate thief 'nasty2 jail))

;;A7
(define Noahs (instantiate restaurant 'Noahs bagel 5))
(can-go Unit1 'west Noahs)
(can-go Noahs 'east Unit1)
(can-go Telegraph-Ave 'south Noahs)
(can-go Noahs 'north Telegraph-Ave)
(can-go Noahs 'south Intermezzo)
(can-go Intermezzo 'north Noahs)
(ask Noahs 'add-entry-procedure
 (lambda () (print "Would you like lox with it?")))
(ask Noahs 'add-exit-procedure
 (lambda () (print "How about a cinnamon raisin bagel for dessert?")))
