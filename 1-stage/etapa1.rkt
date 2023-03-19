#lang racket

(provide (all-defined-out))

; TODO 1
; Implementați o funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosiți recursivitate pe stivă.
(define (get-men mpref)
  (cond
    ((null? mpref) null)
    (else (cons (car (car mpref)) (get-men (cdr mpref))))))


; TODO 2
; Implementați o funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosiți recursivitate pe coadă.
(define (get-women wpref [acc null])
  (cond
    ((null? wpref) (reverse acc))
    (else (get-women (cdr wpref) (cons (car (car wpref)) acc)))))


; TODO 3
; Implementați o funcție recursivă care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Se garantează că persoana apare în listă.
; Exemplu: dacă în lista wpref apare linia (ana bobo adi cos),
; (get-pref-list wpref 'ana) => '(bobo adi cos)
; Observație: de fiecare dată când ne referim la lista
; preferințelor unei persoane p, ne referim la o listă care conține
; doar persoanele de sex opus, nu și pe p pe prima poziție.
(define (get-pref-list pref person)
  (cond
    ((equal? (car (car pref)) person) (cdr (car pref)))
    ((get-pref-list (cdr pref) person))))


; TODO 4
; Implementați o funcție recursivă care primește o listă de tipul
; întors la exercițiul precedent (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Nu folosiți operatori condiționali, folosiți în schimb operatori
; logici pentru a obține același efect.
(define (preferable? pref-list x y)
  (or (and (equal? (car pref-list) x) #t)
      (and (not (equal? (car pref-list) x)) (equal? (car pref-list) y) #f)
      (and (not (equal? (car pref-list) x)) (not (equal? (car pref-list) y)) (preferable? (cdr pref-list) x y))))


; TODO 5
; Implementați o funcție recursivă care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosiți cond.
(define (get-partner engagements person)
  (cond
    ((null? engagements) #f)
    ((equal? (car (car engagements)) person) (cdr (car engagements)))
    (else (get-partner (cdr engagements) person))))


; TODO 6
; Implementați o funcție care primește 2 persoane logodite, p1 și p2,
; lista preferințelor lui p1, lista preferințelor tuturor persoanelor
; de același gen cu p2, respectiv lista tuturor logodnelor, și întoarce
; true dacă există un partener mai potrivit pentru p1, și false altfel.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - lista logodnelor este completă, este un posibil rezultat al problemei
; - logodnele din listă au pe prima poziție persoana de același gen cu p2
; - un partener p' este mai potrivit decât p2 dacă îndeplinește 2 condiții:
;   - p1 îl preferă pe p' în raport cu p2
;   - p' îl preferă pe p1 în raport cu persoana cu care este logodit
(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (cond
    ((equal? (car p1-list) p2) #f)
    (else (or (helper (car p1-list) p1 engagements pref2) (better-match-exists? p1 p2 (cdr p1-list) pref2 engagements)))))

(define (helper name1 name2 engagements pref2)
  (preferable? (get-pref-list pref2 name1) name2 (get-partner engagements name1)))