#lang racket

(require test-engine/racket-tests)
(include "decision-tree-test.rkt")

;; pentru frunze:

;; primește un nod; întoarce #t dacă acesta este frunză, #f altfel
(define is-leaf?
  (λ (node) (not (list? node)))
  )

;; primște un nod frunză; întoarce clasa exemplelor din frunză
(define get-leaf-class
  (λ (node) node)
  )


;; pentru frunze speciale (BONUS):

;; primște un nod; întoarce #t dacă nodul este frunză specială (frunză fără exemple sau cu exemple de clase diferite), #f altfel
;; dacă nu implementați bonus, trebuie ca funcția să întoarcă #f

(define is-special-leaf?
  ; TODO
  (λ (node) #f)
  )

;; primște un nod frunză specială; întoarce tipul frunzei speciale (trebuie să fie unul dintre membrii 2 și 3 ai strings
;; clasa exemplelor din frunza specială va fi verificată tot cu get-leaf-class

(define get-special-leaf-type
  ; TODO
  (λ (node) #f)
  )

;; pentru noduri care nu sunt frunze:

;; primește un nod; întoarce numele atributului verificat în nod
(define get-attribute-name
  (λ (node) (car node)) 
  )

;; primește un nod și o valoare a atributului verificat în nod
;; întoarce nodul copil (nod intern sau frunză) corespunzător valorii date

(define get-child
  (λ (node value) 
    (cdr (assoc value (filter (lambda (x) (pair? x) ) node)))) )

;; opțional: verificare nod
;; primește un argument; întoarce #t dacă argumentul este o reprezentare corectă a unui nod (frunză sau nu) din arbore; #f altfel

(define is-node?
  (λ (node) (if (list? node) #t node)))

; asamblare funcții de acces arbore
(define functions (list is-leaf? get-leaf-class is-special-leaf? get-special-leaf-type get-attribute-name get-child is-node?))


;; TASK (pregătitor):
;; scrieți (manual) în formatul ales un arbore de decizie pentru exemple conținând 1 atribut - shape, care are două valori - round și square
;; un exemplu va fi în clasa "yes" dacă este rotund, și în "no" altfel
;; arborele trebuie să fie astfel:
;;    shape
;;     / \
;; round square
;;   /     \
;; yes     no

(define tree-1 '(shape (round . yes) (square . no)))

(check-expect (is-node? tree-1) #t)
(check-expect (is-leaf? tree-1) #f)
(check-expect (get-attribute-name tree-1) 'shape)
(check-expect (not (get-child tree-1 'round)) #f)
(check-expect (not (get-child tree-1 'square)) #f)
(check-expect (is-leaf? (get-child tree-1 'round)) #t)
(check-expect (is-leaf? (get-child tree-1 'square)) #t)
(check-expect (is-special-leaf? (get-child tree-1 'round)) #f)
(check-expect (get-leaf-class (get-child tree-1 'round)) 'yes)
(check-expect (get-leaf-class (get-child tree-1 'square)) 'no)


;; TASK
;; scrieți funcția de mai jos pentru a calcula entropia unui set de exemple, fiecare exemplu conținând informație despre clasa sa
;; funcția log2 este implementată în decision-tree-test

;; examples: o listă de exemple (setul S), nevidă, în care fiecare exemplu este o listă de perechi, una dintre ele fiind (<nume-atribut-clasă> <clasă>)
;; class-attribute: o listă de forma (<nume-atribut-clasă> <clasă-1> <clasă-2> <clasă-3>)
;; întoarce: entropia setului de exemple în raport cu clasa, H(S) = - sumă-peste-clase p(clasă)*log2 p(clasă)
;;   unde p(clasă) este numărul de exemple cu clasa <clasă> împărțit la numărul de exemple din setul S

;parcurgere lista de exemple si extradere perechile care incep cu atibutul cerut
(define (parcurgere lista atribut)
  (map car (map (lambda (x ) (append (list (assoc atribut x)) '())) lista)))

;-> calculeaza de cate ori apare un atribut in lista noua
(define (sum nou-lista atr)
  (length (filter (lambda (lista)
                    (equal? (cdr lista) atr)) nou-lista )))

; calculeaza enthropy
(define (enth examples class lista)
  (apply
   + (map (lambda (c) (let* ((s1 (sum lista c)) 
                             (s2 (length examples)) 
                             (p (if (= 0 s1) 0 (/ s1 s2)))) ;atribuie lui p = p(clasa) / suma (p (clasa))
                        (if (= 0 p) 0 (* p (log2 p))))) class)))


(define compute-enthropy
  (λ (examples class-attribute)
    (let ((lista (parcurgere examples (car class-attribute)))) ;lista -> noua lista ce contine numai perechi de atribute
      ( - (enth  examples (cdr class-attribute) lista))       
      )))

(define tolerance 0.001)
(check-within (compute-enthropy '() '(classname yes no)) 0 tolerance) ; expect error
(check-within (compute-enthropy '(((shape . round) (classname . yes)) ((shape . square) (classname . yes))) '(classname yes no)) 0 tolerance)
(check-within (compute-enthropy '(((shape . round) (classname . no)) ((shape . square) (classname . no))) '(classname yes no)) 0 tolerance)
(check-within (compute-enthropy '(((shape . round) (classname . yes)) ((shape . square) (classname . no))) '(classname yes no)) 1 tolerance)
(check-within (compute-enthropy '(((shape . round) (classname . yes)) ((shape . square) (classname . no)) ((shape . square) (classname . no))) '(classname yes no maybe)) 0.918 tolerance)
(check-within (compute-enthropy '(((shape . round) (classname . yes)) ((shape . square) (classname . no)) ((shape . square) (classname . maybe))) '(classname yes no maybe)) 1.584 tolerance)

;; TASK
;; scrieți funcția de mai jos pentru a calcula câștigul informațional al unui atribut în raport cu clasa, pentru un set de exemple

;; examples: o listă de exemple, nevidă, în care fiecare exemplu este o listă de perechi, una dintre ele fiind (<nume-atribut-clasă> <clasă>)
;; attribute: o listă de forma (<nume-atribut> <valore-1> <valoare-2> <valoare-3>)
;; class-attribute: o listă de forma (<nume-atribut-clasă> <clasă-1> <clasă-2> <clasă-3>)
;; întoarce: câștigul informațional al atributului, G(S, A) = H(S) - sumă-peste-valori-A p(v)*H(Sv)
;;   unde p(v) este numărul de exemple cu valoarea v pentru A împărțit la numărul de exemple din S
;;   iar Sv este mulțimea exemplelor din S care au valoarea v pentru A

;verifica daca in lista e atribut
(define (verifica-exista L atr)
  (if (null? L)
      #f
      (if (equal?  (caar L) atr) 
          #t
          (if (equal? (cdr (car L)) atr)
              #t
              (verifica-exista (cdr L) atr)))))

;parcurgere-atribute si returneaza lista de elemente care contin atributul
(define (par-at lista atr)
  (filter (lambda (l)
            (equal? (verifica-exista l atr) #t)) lista))

;calcul suma dintre p(v) si H(Sv)
(define (suma-valori examples attribute class x)
  (apply 
   + (map (lambda (a) (let* ((p (/ (sum (parcurgere examples x) a) (length examples)))
                             (Sv (par-at examples a))) ;Sv => lista care contine doar atributul cautat
                        (* p (compute-enthropy Sv class)))) attribute)))


(define compute-gain
  (λ (examples attribute class-attribute)
    (let ((H (compute-enthropy examples class-attribute)) ;calculeaza H
          (A (car attribute))) ; A -> <nume-atribut> 
      (- H (suma-valori examples (cdr attribute) class-attribute A) )))) ;rezultat final

(check-within (compute-gain 
               '(((shape . round) (classname . yes)) ((shape . square) (classname . yes)))
               '(shape round square)
               '(classname yes no)
               ) 0 tolerance)
(check-within (compute-gain 
               '(((shape . round) (classname . no)) ((shape . square) (classname . yes)))
               '(shape round square)
               '(classname yes no)
               ) 1 tolerance)
(check-within (compute-gain 
               '(((shape . round) (classname . no)) ((shape . round) (classname . yes)))
               '(shape round square)
               '(classname yes no)
               ) 0 tolerance)
(check-within (compute-gain 
               '(((shape . round) (size . 1) (classname . yes))
                 ((shape . round) (size . 2) (classname . no))
                 ((shape . square) (size . 1) (classname . yes))
                 ((shape . square) (size . 2) (classname . yes)))
               '(shape round square)
               '(classname yes no)
               ) 0.311 tolerance)
(check-within (compute-gain 
               '(((shape . round) (size . 1) (classname . yes))
                 ((shape . round) (size . 2) (classname . no))
                 ((shape . square) (size . 1) (classname . yes))
                 ((shape . square) (size . 2) (classname . no))
                 ((shape . square) (size . 2) (classname . yes)))
               '(size 1 2)
               '(classname yes no)
               ) 0.419 tolerance)



;; TASK
;; creați un arbore de decizie pentru mulțimea dată de exemple, pentru muțimea dată de atribute, și pentru clasa dată

;; examples: o listă de exemple, nevidă, în care fiecare exemplu este o listă de perechi, una dintre ele fiind (<nume-atribut-clasă> <clasă>)
;; attributes: o listă de liste de forma (<nume-atribut> <valore-1> <valoare-2> <valoare-3>)
;; class-attribute: o listă de forma (<nume-atribut-clasă> <clasă-1> <clasă-2> <clasă-3>)
;; întoarce: un arbore de decizie, în formatul ales, corespunzător cu argumentele date

;functie care face o lista de perechi (valoare ,castigul international) 
(define (perechi attributes examples class)
  (map (lambda (x) (cons (compute-gain examples x class) (car x))) attributes)) 

;cea mai frecventa clasa 
(define (most-frecvent lista class-at class)
  (if (null? class-at)
      null
      (let* ((cauta (filter (lambda(a) (equal? (car class-at) (cdr a))) (map (lambda (x) (assoc class x )) lista))))
        (if (>= (length cauta) (/ (length lista) 2))
            (car class-at)
            (atribute-la-fel lista (cdr class-at) class)))))

;functie de aflat maxim
(define (maxim attribute examples class)
  (let ((p (perechi attribute examples class)))
    (cdar (sort (perechi attribute examples class) (lambda (paira pairb) (> (car paira) (car pairb)))))))  

;functie de verificare daca exemplele au aceeasi clasa
(define (atribute-la-fel lista class-at class)
  (if (null? class-at)
      null
      (let* ((cauta (filter (lambda(a) (equal? (car class-at) (cdr a))) (map (lambda (x) (assoc class x )) lista))))
        (if (equal? (length cauta) (length lista))
            (car class-at)
            (atribute-la-fel lista (cdr class-at) class)))))

;elimina din lista atributul si valorile lui
(define (eliminare lista value)
  (filter (lambda (x) (not (equal? (car x) value))) lista))

;functie care returneaza <valori de atribute> pentru un atribut anume
(define (valori A val)
  (if (equal? #f (assoc val A))
      null
      (cdr (assoc val A))))

(define create-tree
  (λ (examples attributes class-attribute) 
    (cond ((null? examples) null) 
          ;verifica daca exemplele au aceeasi clasa se intoarce frunza cu clasa respectiva
          ((not (null? (atribute-la-fel examples (cdr class-attribute) (car class-attribute))) ) 
           (atribute-la-fel examples (cdr class-attribute) (car class-attribute)))
          ;daca lista de atribute e null intoarce clasa cea mai frecventa in exemplu
          ((if (null? attributes)
               (cons (most-frecvent examples (cdr class-attribute) (car class-attribute )) '())
               (let* ((M (maxim attributes examples class-attribute)) ;M -> maximul intre atribute
                      (A (eliminare attributes M))) ;A -> noua lista de atribute 
                 (cons M (map (lambda (x) (cons x (create-tree (par-at examples x) A class-attribute )))
                              (valori attributes M)))))))))


(define I-DID-THE-BONUS #f)
(check-expect (perform-test functions 'food-small create-tree) #t)
(check-expect (perform-test functions 'food-big create-tree) #t)
(check-expect (perform-test functions 'objects create-tree) #t)
(check-expect (perform-test functions 'weather create-tree) #t)

(check-expect (and (perform-test functions 'food-small create-tree) (get-tree-height functions (get-tree 'food-small create-tree) (get-test 'food-small))) 2)
(check-expect (and (perform-test functions 'food-big create-tree)   (get-tree-height functions (get-tree 'food-big create-tree) (get-test 'food-big)))   4)
(check-expect (and (perform-test functions 'weather create-tree)    (get-tree-height functions (get-tree 'weather create-tree) (get-test 'weather)))    3)
(check-expect (and (perform-test functions 'objects create-tree)    (get-tree-height functions (get-tree 'objects create-tree) (get-test 'objects)))    3)

;(if I-DID-THE-BONUS (display "BONUS DONE\n") (display "bonus not done\n"))
;(check-expect (if I-DID-THE-BONUS (perform-test functions 'bonus create-tree) #t) #t)
;(when I-DID-THE-BONUS (display (get-tree 'bonus create-tree)) (newline))



(test)
