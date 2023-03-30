#lang racket

(require "./etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.
(define (get-unstable-couples engagements mpref wpref)
  (let unstable
    ([man-engagements (map (λ (x) (cons (cdr x) (car x))) engagements)]
     [women-engagements engagements]
     [engagements engagements]
     [result null])
    (if (null? engagements)
        (reverse result)
        (let* ([pair (car engagements)]
               [rest (cdr engagements)]
               [woman (car pair)]
               [man (cdr pair)])
          (if (or (better-match-exists? man woman (get-pref-list mpref man) wpref women-engagements)
                  (better-match-exists? woman man (get-pref-list wpref woman) mpref man-engagements))
              (unstable man-engagements women-engagements rest (cons pair result))
              (unstable man-engagements women-engagements rest result))))))


; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.
(define (engage free-men engagements mpref wpref)
  (let engage-helper0
    ([free-men free-men]
     [engagements engagements])
    (cond
      ((null? free-men) engagements)
      (else (let ([man (car free-men)])
              (let engage-helper1 ([man-pref (get-pref-list mpref man)])
                (let* ([woman (car man-pref)] [woman-partner (get-partner engagements woman)])
                  (cond
                    ((not woman-partner) (engage-helper0 (cdr free-men) (cons (cons woman man) engagements)))
                    ((preferable? (get-pref-list wpref woman) man woman-partner) (engage-helper0 (cons woman-partner (cdr free-men)) (update-engagements engagements woman man)))
                    (else (engage-helper1 (cdr man-pref)))))))))))


; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (let* ([free-men (map car mpref)])
    (engage free-men null mpref wpref)))


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (foldr (λ (pair acc) (cons (car pair) (cons (cdr pair) acc))) null pair-list))

