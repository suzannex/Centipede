;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname CentipedeAbsoluteFinal) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; DATA DEFINITIONS

; A Health is an integer between 0 and 4, inclusive
; where 4 is a new healthy mushroom
; and 0 is a dead (not visible) mushroom

; A Mushroom is a (make-mushroom Posn Health)
(define-struct mushroom (posn health))

; A [Listof Mushroom] is one of
; - empty
; - (cons Mushroom [Listof Mushrooms])

; Template for [Listof Mushroom]
#;(define (lom-temp a-lom)
    (cond
      [(empty? a-lom) ...]
      [(cons? a-lom) ... (first a-lom) ... (lom-temp (rest a-lom)) ...]))

; A Bullet is a (make-bullet Posn Boolean)
; where the Posn is its position in grid coords
; and the Boolean is the status of the Bullet (active/inactive)
(define-struct bullet (posn status))

; A Player is a Posn

; A Segment is a Posn

; A Direction is one of
; - 'left
; - 'right
; Template for Directions
#;(define (dir-temp d)
    (cond
      [(symbol=? d 'left) ...]
      [(symbol=? d 'right) ...]))

; A CBody is a [Listof Segments], which is one of
; - empty
; - (cons Segment [Listof Segments]
; Template for CBody
#;(define (cbody-temp cb)
    (cond
      [(empty? cb) ...]
      [(cons? cb) ... (first cb) ... (cbody-temp (rest cb))]))

; A Centipede is a (make-centipede Direction CBody Descending?)
(define-struct centipede (dir body descend?))

; A [Listof Centipedes] is one of
; - empty
; - (cons Centipede [Listof Centipede])
; Template for [Listof Centipede]
#;(define (loc-temp a-loc)
    (cond
      [(empty? a-loc) ...]
      [(cons? a-loc) ... (first a-loc) ... (loc-temp (rest a-loc))]))

; A World is a
; (make-world [Listof Centipedes] [Listof Mushrooms] Player Bullet Number)
(define-struct world (loc lom player bullet tick))


(define GRID-WIDTH 25)
(define GRID-HEIGHT 40)
(define CELL-SIZE 15)
(define BG (empty-scene (* CELL-SIZE GRID-WIDTH) (* CELL-SIZE GRID-HEIGHT)))
 
(define PLAYER (square CELL-SIZE 'solid 'black))
(define BULLET (rectangle 3 8 'solid 'orange))
(define CENTIPEDE-CELL (square CELL-SIZE 'solid 'green))
(define TONGUE (triangle 5 'solid 'red))
(define LEFT-HEAD (overlay/align "left" "middle" (rotate 90 TONGUE) CENTIPEDE-CELL))
(define RIGHT-HEAD (overlay/align "right" "middle" (rotate 30 TONGUE) CENTIPEDE-CELL))

(define MUSHROOM-RADIUS (/ CELL-SIZE 2))
(define MUSHROOM-1-C 'LightSalmon)
(define MUSHROOM-2-C 'Salmon)
(define MUSHROOM-3-C 'OrangeRed)
(define MUSHROOM-4-C 'DarkRed)

(define CENT-SPEED 1)
 
(define WINNER (text "WINNER" 72 'black))
(define LOSER (text "LOSER" 72 'black))

;;--------------------------H E L P E R S---------------------------------------

; posn=? : Posn Posn -> Boolean
; Determine if two Posns are the same
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(check-expect (posn=? (make-posn 0 0) (make-posn 0 0)) true)
(check-expect (posn=? (make-posn 0 0) (make-posn 0 1)) false)
(check-expect (posn=? (make-posn 0 0) (make-posn 1 1)) false)

;;------Player Functions------ P L A Y E R -------------------------------------
;;Examples:
(define P1 (make-posn 0 0))
(define P2 (make-posn 5 0))
(define P3 (make-posn 24 0))
(define P4 (make-posn 25 0))

;;move-player?: Player Number -> Boolean
;;Checks if the player is in the boundry with the new location
(define (move-player? p x)
  (and (> GRID-WIDTH (+ (posn-x p) x))
       (<= 0 (+ (posn-x p) x))))

(check-expect (move-player? P1 -1) false)
(check-expect (move-player? P1 1) true)
(check-expect (move-player? P1 0) true)
(check-expect (move-player? P4 1) false)

; move-player : Player Direction -> Player
; Moves player to new location
(define (move-player p dir)
  (cond
    [(symbol=? dir 'left) (make-posn (- (posn-x p) 1)
                                      (posn-y p))]
    [(symbol=? dir 'right) (make-posn (+ (posn-x p) 1)
                                       (posn-y p))]))

(check-expect (move-player P1 'right) (make-posn 1 0))
(check-expect (move-player P2 'left) (make-posn 4 0))

; intersects-segment? : Posn [Listof Segment] -> Boolean
; Check if Posn is equal to any in the list
(define (intersects-segment? p los)
  (cond
    [(empty? los) false]
    [(cons? los) (or (posn=? p (first los))
                     (intersects-segment? p (rest los)))]))

(check-expect (intersects-segment? (make-posn 17 36) (list)) false)
(check-expect (intersects-segment? (make-posn 17 20) (centipede-body C1)) false)
(check-expect (intersects-segment? (make-posn 17 38) (centipede-body C1)) true)

; one-right : Posn -> Posn
; creates Posn located one to the right of input Posn
(define (one-right p)
  (make-posn (+ 1 (posn-x p)) (posn-y p)))

(check-expect (one-right (make-posn 0 0)) (make-posn 1 0))
(check-expect (one-right (make-posn 5 6)) (make-posn 6 6))

; one-left : Posn -> Posn
; creates Posn located one to the left of input Posn
(define (one-left p)
  (make-posn (- (posn-x p) 1) (posn-y p)))

(check-expect (one-left (make-posn 2 0)) (make-posn 1 0))
(check-expect (one-left (make-posn 5 6)) (make-posn 4 6))

; p-hits-centipede? : Player Centipede -> Boolean
; Does the centipede hit the player?
(define (p-hits-centipede? p cent)
  (or (intersects-segment? (one-right p) (centipede-body cent))
      (intersects-segment? (one-left p) (centipede-body cent))
      (intersects-segment? p (centipede-body cent))))

(check-expect (p-hits-centipede? (make-posn 16 38) C1) true)
(check-expect (p-hits-centipede? (make-posn 19 39) C1) true)
(check-expect (p-hits-centipede? (make-posn 17 38) C1) true)
(check-expect (p-hits-centipede? (make-posn 1 1) C1) false)

; player-lost? Player [Listof Centipede] -> Boolean
; Has the player hit any of the centipedes in the list and lost the game?
(define (player-lost? p loc)
  (cond
    [(empty? loc) false]
    [(cons? loc) (or (p-hits-centipede? p (first loc))
                     (player-lost? p (rest loc)))]))

(check-expect (player-lost? (make-posn 16 38) loc1) true)
(check-expect (player-lost? (make-posn 4 28) loc1) true)
(check-expect (player-lost? (make-posn 1 0) loc1) false)
(check-expect (player-lost? (make-posn 4 28) (list)) false)

; END OF PLAYER

;;------Centipede Functions------ C E N T I P E D E ----------------------------
;;Examples:
(define C0 (make-centipede 'right (list) true))
(define C1 (make-centipede 'right (list (make-posn 17 38)
                                       (make-posn 16 38)
                                       (make-posn 16 39)
                                       (make-posn 17 39)
                                       (make-posn 18 39))
                           true))
(define C2 (make-centipede 'left (list (make-posn 4 28)
                                        (make-posn 5 28)
                                        (make-posn 6 28))
                           true))
(define C3 (make-centipede 'left (list (make-posn 12 2)
                                       (make-posn 11 2)
                                       (make-posn 10 2))
                           true))
(define C4 (make-centipede 'left (list (make-posn 22 7)) true))

(define C5 (make-centipede 'left (list (make-posn 4 28)
                                        (make-posn 5 28)
                                        (make-posn 6 28)
                                        (make-posn 7 28)
                                        (make-posn 8 28)
                                        (make-posn 9 28)
                                        (make-posn 10 28)
                                        (make-posn 11 28)
                                        (make-posn 12 28)
                                        (make-posn 13 28)
                                        (make-posn 14 28)
                                        (make-posn 15 28)
                                        (make-posn 16 28)
                                        (make-posn 17 28)
                                        (make-posn 18 28)
                                        (make-posn 19 28))
                           true))
(define C6 (make-centipede 'left (list (make-posn 0 39)
                                       (make-posn 1 39)
                                       (make-posn 2 39))
                           true))
(define C00 (make-centipede 'right (list (make-posn 4 39)
                                        (make-posn 5 39)
                                        (make-posn 6 39)
                                        (make-posn 7 39)
                                        (make-posn 8 39)
                                        (make-posn 9 39)
                                        (make-posn 10 39)
                                        (make-posn 11 39)
                                        (make-posn 12 39)
                                        (make-posn 13 39)
                                        (make-posn 14 39)
                                        (make-posn 15 39)
                                        (make-posn 16 39)
                                        (make-posn 17 39)
                                        (make-posn 18 39)
                                        (make-posn 19 39))
                            true))
(define C02 (make-centipede 'right (list
                                    (make-posn 19 39)
                                    (make-posn 18 39)
                                    (make-posn 17 39)
                                    (make-posn 16 39)
                                    (make-posn 15 39)
                                    (make-posn 14 39)
                                    (make-posn 13 39)
                                    (make-posn 12 39)
                                    (make-posn 11 39)
                                    (make-posn 10 39)
                                    (make-posn 9 39)
                                    (make-posn 8 39)
                                    (make-posn 7 39)
                                    (make-posn 6 39)
                                    (make-posn 5 39)
                                    (make-posn 4 39))
                            true))
(define C01 (make-centipede 'right (list (make-posn 4 38)
                                        (make-posn 5 38)
                                        (make-posn 6 38)
                                        (make-posn 7 38)
                                        (make-posn 8 38)
                                        (make-posn 9 38)
                                        (make-posn 10 38)
                                        (make-posn 11 38)
                                        (make-posn 12 38)
                                        (make-posn 13 38)
                                        (make-posn 14 38)
                                        (make-posn 15 38)
                                        (make-posn 16 38)
                                        (make-posn 17 38)
                                        (make-posn 18 38)
                                        (make-posn 19 38))
                            true))

(define loc1 (list C1 C2 C3 C4))
(define loc2 (list C5))
(define LOC0 (list C00))
(define LOC1 (list C01))
(define LOC2 (list C02))

;;------------Getters & Setters:-------------------------
;;get-cent-head: Centipede -> Segment
;;Gets the first segment of the Centipede
(define (get-cent-head the-cent)
  (first (centipede-body the-cent)))

(check-expect (get-cent-head C2) (make-posn 4 28))

;;get-cent: CBody -> CBody
;;Creates an identical CBody except for the last element
(define (get-cent body)
  (cond
    [(empty? (rest body)) empty]
    [(cons? body) (cons (first body) (get-cent (rest body)))]))

(check-expect (get-cent (centipede-body C3)) (list (make-posn 12 2)
                                                  (make-posn 11 2)))

;;get-cent-speed:  Centipede -> Number
;;Gets the value to be added for the centipede's next move
(define (get-cent-speed the-cent)
  (cond
    [(symbol=? (centipede-dir the-cent) 'right) CENT-SPEED]
    [(symbol=? (centipede-dir the-cent) 'left) (* -1 CENT-SPEED)]))

(check-expect (get-cent-speed C1) CENT-SPEED)
(check-expect (get-cent-speed C2) (* -1 CENT-SPEED))

;;get-cent-state: Centipede -> Number
;;Gets the speed for descend/ascend
(define (get-cent-state state)
  (if state (* -1 CENT-SPEED) CENT-SPEED))

(check-expect (get-cent-state (centipede-descend? C1)) (* -1 CENT-SPEED))  

;;---Helpers--------------------------
;;change-state: Centipede -> Boolean
;;flips the boolean
(define (change-state the-cent)
  (not (centipede-descend? the-cent)))

(check-expect (change-state C1) false)

;;change-dir: Dir -> Dir
;;Changes the direction
(define (change-dir d)
  (if (symbol=? d 'right) 'left 'right))

(check-expect (change-dir 'right) 'left)
(check-expect (change-dir 'left) 'right)

;;move-segment-x: Number Segment -> Segment
;;Moves the segment's x value by adding the number
(define (move-segment-x dir seg)
  (make-posn (+ (posn-x seg) dir) (posn-y seg)))

(check-expect (move-segment-x 5 (make-posn 3 6))
              (make-posn 8 6))

;;move-segment-y: Number Segment -> Segment
;;Moves the segment's y value by adding the number
(define (move-segment-y dir seg)
  (make-posn (posn-x seg) (+ (posn-y seg) dir)))

(check-expect (move-segment-y 5 (make-posn 3 6))
              (make-posn 3 11))

;;move-centipede-x.v2: Centipede -> CBody
;;moves the centipede left and right based on his direction
(define (move-centipede-x.v2 the-cent)
  (cons (move-segment-x (get-cent-speed the-cent)
                        (first (centipede-body the-cent)))
        (get-cent (centipede-body the-cent))))

(check-expect (move-centipede-x.v2 C2)
              (list (make-posn 3 28)
                    (make-posn 4 28)
                    (make-posn 5 28)))

;;move-centipede-y.v2: Centipede -> CBody
;;moves the centipede up and down based on his direction
(define (move-centipede-y.v2 the-cent)
  (cons (move-segment-y (get-cent-state (centipede-descend? the-cent))
                        (first (centipede-body the-cent)))
        (get-cent (centipede-body the-cent))))

(check-expect (move-centipede-y.v2 C2)
              (list (make-posn 4 27)
                    (make-posn 4 28)
                    (make-posn 5 28)))

;;move-centipede-ycorner.v2: Centipede -> CBody
;;moves the centipede if it's in a corner
(define (move-centipede-ycorner.v2 the-cent)
  (cons (move-segment-y (get-cent-state (not(centipede-descend? the-cent)))
                        (first (centipede-body the-cent)))
        (get-cent (centipede-body the-cent))))

(check-expect (move-centipede-ycorner.v2 C2)
              (list (make-posn 4 29)
                    (make-posn 4 28)
                    (make-posn 5 28)))

; c-hit-corner : Segment Number State -> Boolean
; Checks if the centipede is in a corner
(define (c-hit-corner? head speed state)
  (and (or (and (= (posn-y head) 0) state)
           (and (= (posn-y head) (- GRID-HEIGHT 1)) (not state)))
       (c-hit-wall? head speed)))

(check-expect (c-hit-corner? (get-cent-head C2)
                             (get-cent-speed C2)
                             (centipede-descend? C2))
              false)

(check-expect (c-hit-corner? (make-posn 0 0)
                             (get-cent-speed C2)
                             (centipede-descend? C2))
              true)

;;c-hit-wall?: Segment Number -> Boolean
;;checks if the next move would be fatal
(define (c-hit-wall? head speed)
  (or (<= (+ (posn-x head) speed) -1)
       (>= (+ (posn-x head) speed) GRID-WIDTH)))

(check-expect (c-hit-wall? (make-posn 21 4) 1) false)
(check-expect (c-hit-wall? (make-posn 21 4) -1) false)
(check-expect (c-hit-wall? (make-posn 0 3) -1) true)
(check-expect (c-hit-wall? (make-posn 25 3) 1) true)
(check-expect (c-hit-wall? (make-posn 25 3) -1) false)

;;addx: Posn Number -> Posn
;;adds the number to the x value of posn
(define (addx posn x)
  (make-posn (+ x (posn-x posn)) (posn-y posn)))

(check-expect (addx (make-posn 10 8) 3)
              (make-posn 13 8))

;;centipede-trail: Centipede -> Centipede
;;moves the centipede according to the rules
(define (centipede-trail the-cent lom)
  (cond
    [(c-hit-corner? (get-cent-head the-cent)
                    (get-cent-state (centipede-descend? the-cent))
                    (centipede-descend? the-cent))
     (make-centipede (change-dir (centipede-dir the-cent))
                     (move-centipede-ycorner.v2 the-cent)
                     (change-state the-cent))]
    
    [(c-hit-wall? (get-cent-head the-cent)
                  (get-cent-speed the-cent))
     (make-centipede (change-dir (centipede-dir the-cent))
                     (move-centipede-y.v2 the-cent)
                     (centipede-descend? the-cent))]
    
    [(in-list? (addx (first (centipede-body the-cent)) 
                     (get-cent-speed the-cent))
               lom)
     (make-centipede (change-dir (centipede-dir the-cent))
                     (move-centipede-y.v2 the-cent)
                     (centipede-descend? the-cent))]
    [else (make-centipede (centipede-dir the-cent)
                          (move-centipede-x.v2 the-cent)
                          (centipede-descend? the-cent))]))

(check-expect (centipede-trail
               (make-centipede 'left (list (make-posn 0 0)
                                           (make-posn 1 0))
                               true)
               lom2)
              (make-centipede 'right (list (make-posn 0 1)
                                           (make-posn 0 0))
                              false))
(check-expect (centipede-trail C6 lom2)
              (make-centipede 'right (list (make-posn 0 38)
                                           (make-posn 0 39)
                                           (make-posn 1 39))
                              true))
(check-expect (centipede-trail C1 lom2)
              (make-centipede 'right (list (make-posn 18 38)
                                           (make-posn 17 38)
                                           (make-posn 16 38)
                                           (make-posn 16 39)
                                           (make-posn 17 39))
                              true))

;;loc-trail: LoC LoM -> LoC
;;creates an updated loc with cent movement
;;while checking if they hit the mushroom
;;moves all the centipedes according to the rules
(define (loc-trail loc lom)
  (cond
    [(empty? loc) empty]
    [(cons? loc) (cons (centipede-trail (first loc) lom)
                       (loc-trail (rest loc) lom))]))

;;loc-move-tracker: LoC LoM Tick -> LoC
;;checks if it's the third tick in order to move the centipedes
(define (loc-move-tracker loc lom tick)
  (if (= 0 (modulo tick 3)) (loc-trail loc lom) loc))


;; END OF CENTIPEDE

;;------Mushroom Functions----- M U S H R O O M --------------------------------

(define posn0 (make-posn 6 7))
(define posn1 (make-posn 13 20))
(define posn2 (make-posn 23 30))
(define posn3 (make-posn 20 5))

; Examples of Mushroom
(define m2 (make-mushroom posn0 4))
(define m3 (make-mushroom posn1 4))
(define m4 (make-mushroom posn2 4))
(define m5 (make-mushroom posn3 4))
(define m6 (make-mushroom posn3 1))

; decrement-health : Mushroom -> Mushroom
; Reduce the Health of a Mushroom by 1
(define (decrement-health m)
  (make-mushroom (mushroom-posn m) (- (mushroom-health m) 1)))

(check-expect (decrement-health (make-mushroom (make-posn 4 39) 4))
              (make-mushroom (make-posn 4 39) 3))
(check-expect (decrement-health (make-mushroom (make-posn 4 39) 3))
              (make-mushroom (make-posn 4 39) 2))
(check-expect (decrement-health (make-mushroom (make-posn 4 39) 1))
              (make-mushroom (make-posn 4 39) 0))

; health=1? : Mushroom -> Boolean
; Check if Mushroom's health is 1
(define (health=1? m)
  (= 1 (mushroom-health m)))

(check-expect (health=1? m2) false)
(check-expect (health=1? (make-mushroom posn0 1)) true)

; get-mx : Mushroom -> Number
; Get the x-coordinate of the Mushroom
(define (get-mx m)
  (posn-x (mushroom-posn m)))

(check-expect (get-mx (make-mushroom posn0 4)) 6)
(check-expect (get-mx (make-mushroom posn1 4)) 13)

; get-my : Mushroom -> Number
; Get the y-coordinate of the Mushroom
(define (get-my m)
  (posn-y (mushroom-posn m)))

(check-expect (get-my (make-mushroom posn0 4)) 7)
(check-expect (get-my (make-mushroom posn1 4)) 20)

; x-equal? : Number Mushroom -> Boolean
; Determine if the Number and the Mushroom's Posn's x-coord are equal
(define (x-equal? x m)
  (= x (get-mx m)))


(check-expect (x-equal? 6 m2) true)
(check-expect (x-equal? 7 m2) false)

; y-equal? : Number Mushroom -> Boolean
; Determine if the Number and the Mushroom's Posn's y-coord are equal
(define (y-equal? y m)
  (= y (get-my m)))

(check-expect (y-equal? 7 m2) true)
(check-expect (y-equal? 6 m2) false)

; in-list? : Posn [Listof Mushroom] -> Boolean
; Determines if the Posn is the same as any Mushroom's Posn in the list
(define (in-list? posn lom)
  (cond
    [(empty? lom) false]
    [(cons? lom) (or (and (x-equal? (posn-x posn) (first lom))
                          (y-equal? (posn-y posn) (first lom)))
                     (in-list? posn (rest lom)))]))

(define lom00 (list))
(define lom2 (list m3 m2))
(define lom3 (list m2 m3 m4 m5))
(define lom8 (list m2 m3 m4 m6))
(check-expect (in-list? (make-posn 3 3) lom00) false)
(check-expect (in-list? (make-posn 3 3) lom2) false)
(check-expect (in-list? (make-posn 13 20) lom2) true)
(check-expect (in-list? (make-posn 13 20) lom3) true)

; make-unique-mushroom : Number Number [Listof Mushroom] -> Posn
; Makes a Mushroom that is not identical to any in the given list.
(define (make-unique-mushroom x y lom)
  (cond
    [(not (in-list? (make-posn x y) lom)) (make-mushroom (make-posn x y) 4)]
    [else (make-unique-mushroom (random GRID-WIDTH)
                            (+ 1 (random (- GRID-HEIGHT 2))) lom)]))

(define lom4 (list
              (make-mushroom (make-posn 15 24) 4)
              (make-mushroom (make-posn 0 17) 4)
              (make-mushroom (make-posn 16 26) 4)
              (make-mushroom (make-posn 12 20) 4)
              (make-mushroom (make-posn 14 9) 4)
              (make-mushroom (make-posn 19 6) 4)
              (make-mushroom (make-posn 23 34) 4)
              (make-mushroom (make-posn 5 7) 4)
              (make-mushroom (make-posn 23 17) 4)
              (make-mushroom (make-posn 13 32) 4)))
; test: Does make-unique-mushroom produce a mushroom?
(check-expect (mushroom? (make-unique-mushroom (random 25)
                                               (+ 1 (random 38))
                                               lom4))
              true)
; test: Is the mushroom the function produces in the given list?
(check-expect (in-list? (mushroom-posn (make-unique-mushroom (random 25)
                                              (+ 1 (random 38))
                                              lom4))
                        lom4)
              false)

; create-lom : Number [Listof Mushroom] -> [Listof Mushroom]
; Create a list of unique Mushrooms of the given length, when
; 0 < length < (GRID-WIDTH * (GRID-HEIGHT-2)).
(define (create-lom n lom)
  (cond
    [(= n 1) (cons (make-unique-mushroom (random GRID-WIDTH)
                                         (+ 1 (random (- GRID-HEIGHT 2)))
                                         lom)
                   lom)]
    [else (create-lom (- n 1) (cons
                               (make-unique-mushroom
                                (random GRID-WIDTH)
                                (+ 1 (random (- GRID-HEIGHT 2)))
                                lom)
                                     lom))]))

(check-expect (length (create-lom 1 empty)) 1)
(check-expect (length (create-lom 5 empty)) 5)

;;EXAMPLES:
(define lom1 (create-lom 50 empty))

;;END OF MUSHROOM

;;------Bullet Functions------ B U L L E T -------------------------------------

; (define-struct bullet (posn status))
; examples:
(define B1 (make-bullet P1 false))
(define b0 (make-bullet (make-posn 0 2) false))
(define b1 (make-bullet (make-posn 2 3) true))
(define b2 (make-bullet (make-posn 0 1) true))

; move-up : Posn -> Posn
; Increases the given Posn's y-value by 1
(define (move-up p)
  (make-posn (posn-x p) (+ 1 (posn-y p)))) 

(check-expect (move-up (make-posn 0 0)) (make-posn 0 1))
(check-expect (move-up (make-posn 2 0)) (make-posn 2 1))

; active? : Bullet -> Boolean
; Check if the Bullet's status is active
(define (active? b)
  (bullet-status b))

(check-expect (active? (make-bullet (make-posn 4 4) true)) true)
(check-expect (active? (make-bullet (make-posn 4 4) false)) false)

; activate : Bullet -> Bullet
; Switches Bullet's status to true
(define (activate b)
  (make-bullet (bullet-posn b) true))

(check-expect (activate b0) (make-bullet (bullet-posn b0) true))
(check-expect (activate b1) (make-bullet (bullet-posn b1) true))

; deactivate : Bullet -> Bullet
; Switches Bullet's status to false
(define (deactivate b)
  (make-bullet (bullet-posn b) false))

(check-expect (deactivate b0) (make-bullet (bullet-posn b0) false))
(check-expect (deactivate b1) (make-bullet (bullet-posn b1) false))

; hits-mushroom? Bullet [Listof Mushroom] -> Boolean
; Check if the Bullet is hitting any Mushroom in the list
(define (hits-mushroom? b lom)
  (in-list? (one-higher (bullet-posn b)) lom))

(check-expect (hits-mushroom? (make-bullet (make-posn 3 4) true) lom00) false)
(check-expect (hits-mushroom? (make-bullet (make-posn 20 4) true) lom3) true)
(check-expect (hits-mushroom? (make-bullet (make-posn 3 4) true) lom3) false)

; one-higher : Posn -> Posn
; Returns the Posn that is one cell above the given Posn
(define (one-higher p)
  (make-posn (posn-x p) (+ 1 (posn-y p))))

(check-expect (one-higher (make-posn 3 4)) (make-posn 3 5))
(check-expect (one-higher (make-posn 0 0)) (make-posn 0 1))

; hits-centipede? : Bullet Centipede -> Boolean
; Check if the Bullet is hitting the Centipede
(define (hits-centipede? b c)
  (intersects-segment? (one-higher (bullet-posn b)) (centipede-body c)))

(check-expect (hits-centipede? (make-bullet (make-posn 16 38) true) C0) false)
(check-expect (hits-centipede? (make-bullet (make-posn 16 37) true) C1) true)
(check-expect (hits-centipede? (make-bullet (make-posn 16 1) true) C1) false)

; hits-any-centipede? : Bullet [Listof Centipede] -> Boolean
; Checks if bullet is hitting any centipede in the list
(define (hits-any-centipede? b loc)
  (cond
    [(empty? loc) false]
    [(cons? loc) (or (hits-centipede? b (first loc))
                     (hits-any-centipede? b (rest loc)))]))

(check-expect (hits-any-centipede?
               (make-bullet (make-posn 16 39) true) (list)) false)
(check-expect (hits-any-centipede?
               (make-bullet (make-posn 12 1) true) loc1) true)

; on-top-edge? Posn -> Boolean
; Determines if Posn is located on top edge of screen
(define (on-top-edge? p)
  (< GRID-HEIGHT (posn-y p)))

(check-expect (on-top-edge? (make-posn 9 39)) false)
(check-expect (on-top-edge? (make-posn 9 40)) false)
(check-expect (on-top-edge? (make-posn 9 41)) true)

; hits-wall? Bullet -> Boolean
; Check if bullet is hitting the wall
(define (hits-wall? b)
  (on-top-edge? (bullet-posn b)))

(check-expect (hits-wall? (make-bullet (make-posn 10 39) true)) false)
(check-expect (hits-wall? (make-bullet (make-posn 10 40) true)) false)
(check-expect (hits-wall? (make-bullet (make-posn 10 41) true)) true)

; Bullet Mushroom -> Boolean
; Is the Bullet hitting the Mushroom?
(define (b-hits-m? b m)
  (posn=? (one-higher (bullet-posn b)) (mushroom-posn m)))

(check-expect (b-hits-m? (make-bullet (make-posn 4 2) true)
                            (make-mushroom (make-posn 4 4) 4))
              false)
(check-expect (b-hits-m? (make-bullet (make-posn 4 2) true)
                            (make-mushroom (make-posn 4 3) 4))
              true)

; when-hits-m : Bullet [Listof Mushroom] -> [Listof Mushroom]
; Decrement the health of the Mushroom in the List that the Bullet hit.
(define (when-hits-m b lom)
  (cond
    [(b-hits-m? b (first lom)) (if (health=1? (first lom))
                                      (rest lom)
                                      (cons (decrement-health (first lom))
                                            (rest lom)))] 
    [else (cons (first lom) (when-hits-m b (rest lom)))]))

(check-expect (when-hits-m (make-bullet (make-posn 20 4) true) lom3)
              (list m2 m3 m4 (make-mushroom (mushroom-posn m5)
                                            (- (mushroom-health m5) 1))))
(check-expect (when-hits-m (make-bullet (make-posn 23 29) true) lom3)
              (list m2 m3 (make-mushroom (mushroom-posn m4)
                                         (- (mushroom-health m4) 1))
                    m5))
(check-expect (when-hits-m (make-bullet (make-posn 20 4) true) lom8)
              (list m2 m3 m4))

; new-mushroom : Posn -> Mushroom
; Create a new mushroom with the input Posn and full Health
(define (new-mushroom posn)
  (make-mushroom posn 4))

(check-expect (new-mushroom posn1) (make-mushroom posn1 4))
(check-expect (new-mushroom posn2) (make-mushroom posn2 4))

; add-mushroom : Bullet [Listof Mushroom] -> [Listof Mushroom]
; Create list with an added Mushroom located at where the Bullet is hitting.
(define (add-mushroom b lom)
  (cons (new-mushroom (one-higher (bullet-posn b))) lom))

(check-expect (add-mushroom b1 (list))
              (list (make-mushroom (make-posn 2 4) 4)))
(check-expect (add-mushroom b2 (list m2 m3 m4))
              (list (make-mushroom (make-posn 0 2) 4) m2 m3 m4))

; cent-length : Centipede -> Number
; Returns length of Centipede
(define (cent-length c)
  (length (centipede-body c)))

(check-expect (cent-length C1) 5)
(check-expect (cent-length C2) 3)

; body-length-front : CBody Bullet -> Number
; Find length of CBody in front of where the Bullet hits
(define (body-length-front bod bullet)
  (cond
    [(posn=? (first bod) (one-higher (bullet-posn bullet))) 0]
    [else (+ 1 (body-length-front (rest bod) bullet))]))

(check-expect (body-length-front (centipede-body C1)
                                 (make-bullet (make-posn 17 37) true)) 0)
(check-expect (body-length-front (centipede-body C1)
                                 (make-bullet (make-posn 16 37) true)) 1)
(check-expect (body-length-front (centipede-body C1)
                                 (make-bullet (make-posn 18 38) true)) 4)

; body-length-back : CBody Bullet -> Number
; Find length of CBody behind the Bullet
(define (body-length-back bod bullet)
  (cond
    [(empty? bod) 0]
    [(posn=? (first bod) (one-higher (bullet-posn bullet)))
     (length (rest bod))]
    [else (body-length-back (rest bod) bullet)]))

(check-expect (body-length-back (centipede-body C2)
                                (make-bullet (make-posn 4 27) true)) 2)
(check-expect (body-length-back (centipede-body C2)
                                (make-bullet (make-posn 5 27) true)) 1)
(check-expect (body-length-back (centipede-body C2)
                                (make-bullet (make-posn 6 27) true)) 0)
(check-expect (body-length-back (list)
                                (make-bullet (make-posn 3 0) true)) 0)

; cent-length-front : Centipede Bullet -> Number
; Returns length of front half of centipede (before the Bullet)
(define (cent-length-front c b)
  (body-length-front (centipede-body c) b))

(check-expect (cent-length-front C1 (make-bullet (make-posn 17 37) true)) 0)
(check-expect (cent-length-front C1 (make-bullet (make-posn 16 37) true)) 1)
(check-expect (cent-length-front C1 (make-bullet (make-posn 18 38) true)) 4)
 
; cent-length-back Centipede Bullet -> Number
; Returns length of back half of centipede (after the Bullet)
(define (cent-length-back c b)
  (body-length-back (centipede-body c) b))

(check-expect (cent-length-back C2 (make-bullet (make-posn 4 27) true)) 2)
(check-expect (cent-length-back C2 (make-bullet (make-posn 5 27) true)) 1)
(check-expect (cent-length-back C2 (make-bullet (make-posn 6 27) true)) 0)

; new-front-cent-body : CBody Number -> CBody
; Return new CBody of length Number starting from the front.
(define (new-front-cent-body cbody num)
  (cond
    [(= num 0) empty]
    [else (cons (first cbody) (new-front-cent-body (rest cbody) (- num 1)))]))

(check-expect (new-front-cent-body (centipede-body C1) 4)
              (list (make-posn 17 38) (make-posn 16 38)
                    (make-posn 16 39) (make-posn 17 39)))
(check-expect (new-front-cent-body (centipede-body C1) 2)
              (list (make-posn 17 38) (make-posn 16 38)))
(check-expect (new-front-cent-body (centipede-body C1) 0)
              (list))

; new-back-cent-body : CBody Number -> CBody
; Return new CBody of length Number starting from the back.
(define (new-back-cent-body cbody num)
  (cond
    [(= (length cbody) num) cbody]
    [else (new-back-cent-body (rest cbody) num)]))

(check-expect (new-back-cent-body (centipede-body C1) 4)
              (list (make-posn 16 38) (make-posn 16 39)
                    (make-posn 17 39) (make-posn 18 39)))
(check-expect (new-back-cent-body (centipede-body C1) 2)
              (list (make-posn 17 39) (make-posn 18 39)))
(check-expect (new-back-cent-body (centipede-body C1) 0)
              (list))

; new-front-cent : Centipede Number -> Centipede
; Returns new Centipede of length Number, starting from the front.
(define (new-front-cent c num)
  (make-centipede (centipede-dir c)
                  (new-front-cent-body (centipede-body c) num)
                  (centipede-descend? c)))

(check-expect (new-front-cent C1 4)
              (make-centipede 'right (list (make-posn 17 38)
                                           (make-posn 16 38)
                                           (make-posn 16 39)
                                           (make-posn 17 39)) #true))
(check-expect (new-front-cent C2 1)
              (make-centipede 'left (list (make-posn 4 28)) #true))

; new-back-cent : Centipede Number -> Centipede
; Returns new Centipede of length Number, starting from
; the back, and opposite Direction.
(define (new-back-cent c num)
  (make-centipede (centipede-dir c)
                  (new-back-cent-body (centipede-body c) num)
                  (centipede-descend? c)))

(check-expect (new-back-cent C1 4)
              (make-centipede 'right (list (make-posn 16 38)
                                           (make-posn 16 39)
                                           (make-posn 17 39)
                                           (make-posn 18 39)) #true))
(check-expect (new-back-cent C2 1)
              (make-centipede 'left (list (make-posn 6 28)) #true))

; when-hits-c : Bullet [Listof Centipede] -> [Listof Centipede]
; Create new list of Centipede with the centipede(s) split by the Bullet
(define (when-hits-c b loc)
  (cond
    [(empty? loc) empty]
    [(hits-centipede? b (first loc))
     (cond
       [(and (< 0 (cent-length-front (first loc) b))
             (< 0 (cent-length-back (first loc) b)))
        (cons (new-front-cent (first loc) (cent-length-front (first loc) b))
              (cons (new-back-cent (first loc) (cent-length-back (first loc) b))
                    (when-hits-c b (rest loc))))]

       [(= 0 (cent-length-front (first loc) b) (cent-length-back (first loc) b))
        (when-hits-c b (rest loc))]

       [(= 0 (cent-length-front (first loc) b))
        (cons (new-back-cent (first loc) (cent-length-back (first loc) b))
              (when-hits-c b (rest loc)))]
       
       [(= 0 (cent-length-back (first loc) b))
        (cons (new-front-cent (first loc) (cent-length-front (first loc) b))
              (when-hits-c b (rest loc)))])]
    
    [else (cons (first loc) (when-hits-c b (rest loc)))]))

(define b3 (make-bullet (make-posn 17 37) true))
(define b4 (make-bullet (make-posn 16 38) true))
(define b5 (make-bullet (make-posn 3 1) true))
(define b6 (make-bullet (make-posn 5 27) true))
(check-expect (when-hits-c b3 (list)) empty)
(check-expect (when-hits-c b6 loc1)
              (cons C1 (cons (make-centipede 'left (list (make-posn 4 28)) true)
                             (cons (make-centipede 'left (list (make-posn 6 28)) true)
                                   (cons C3 (cons C4 empty))))))
(check-expect (when-hits-c (make-bullet (make-posn 22 6) true) loc1)
              (cons C1 (cons C2 (cons C3 empty))))
(check-expect (when-hits-c b3 loc1)
              (cons (make-centipede 'right (list (make-posn 16 38)
                                                 (make-posn 16 39)
                                                 (make-posn 17 39)
                                                 (make-posn 18 39)) #true)
                    (rest loc1)))
(check-expect (when-hits-c (make-bullet (make-posn 10 1) true) loc1)
              (cons C1
                    (cons C2
                          (cons (make-centipede 'left (list (make-posn 12 2)
                                                            (make-posn 11 2))
                                                true)
                                (cons C4 empty)))))


; move-bullet : Bullet -> Bullet
; Moves bullet up one cell if Bullet is active
(define (move-bullet b)
  (if (active? b)
      (make-bullet (move-up (bullet-posn b)) true)
      b))

(check-expect (move-bullet b0) b0) 
(check-expect (move-bullet b1) (make-bullet (make-posn 2 4) true))
(check-expect (move-bullet (move-bullet b1)) (make-bullet (make-posn 2 5) true))

; bullet-with-player : Bullet Player -> Bullet
; Assigns Bullet's coordinates to those of Player if Bullet is inactive
(define (bullet-with-player b p)
  (if (not (active? b))
      (make-bullet (make-posn (posn-x p) (posn-y p)) false)
      b))

(check-expect (bullet-with-player b0 (make-posn 3 3))
              (make-bullet (make-posn 3 3) false))
(check-expect (bullet-with-player b0 (make-posn 3 4))
              (make-bullet (make-posn 3 4) false))
(check-expect (bullet-with-player b1 (make-posn 3 4)) b1)

; END OF BULLET


;;------W O R L D --------------------------------------------------------------
;;Examples:
(define WORLD0 (make-world loc2 lom1 P1 B1 0))

; next-world : World -> World
; Given a World, produces the next World
(define (next-world w)
  (if (active? (world-bullet w))
      (cond
        [(hits-mushroom? (world-bullet w) (world-lom w))
         (make-world (loc-move-tracker (world-loc w) (world-lom w) (world-tick w))
                     (when-hits-m (world-bullet w) (world-lom w))
                     (world-player w)
                     (bullet-with-player (deactivate (world-bullet w))
                                         (world-player w))
                     (+ 1 (world-tick w)))]
        
        [(hits-any-centipede? (world-bullet w) (world-loc w))
         (make-world (when-hits-c (world-bullet w) (world-loc w))
                     (add-mushroom (world-bullet w) (world-lom w))
                     (world-player w)
                     (bullet-with-player (deactivate (world-bullet w))
                                         (world-player w))
                     (+ 1 (world-tick w)))]
        
        [(hits-wall? (world-bullet w))
         (make-world (loc-move-tracker (world-loc w) (world-lom w) (world-tick w))
                     (world-lom w)
                     (world-player w)
                     (bullet-with-player (deactivate (world-bullet w))
                                         (world-player w))
                     (+ 1 (world-tick w)))]
        
        [else (make-world (loc-move-tracker (world-loc w) (world-lom w) (world-tick w))
                          (world-lom w)
                          (world-player w)
                          (move-bullet (world-bullet w))
                          (+ 1 (world-tick w)))])
      (make-world (loc-move-tracker (world-loc w) (world-lom w) (world-tick w))
                  (world-lom w)
                  (world-player w)
                  (world-bullet w)
                  (+ 1 (world-tick w)))))

;(define WORLD0 (make-world loc2 lom1 P1 B1 0))
(check-expect (next-world (make-world (list (make-centipede 'right
                                                      (list (make-posn 5 37)
                                                            (make-posn 4 37)
                                                            (make-posn 3 37)
                                                            (make-posn 2 37)
                                                            (make-posn 1 37)
                                                            (make-posn 0 37)
                                                            (make-posn 0 38)
                                                            (make-posn 1 38)
                                                            (make-posn 2 38)
                                                            (make-posn 3 38)
                                                            (make-posn 4 38)
                                                            (make-posn 5 38)
                                                            (make-posn 6 38)
                                                            (make-posn 7 38)
                                                            (make-posn 8 38)
                                                            (make-posn 9 38))
                                                      #true))
                                      (list (make-mushroom (make-posn 23 20) 4)
                                            (make-mushroom (make-posn 2 36) 3)
                                            (make-mushroom (make-posn 6 21) 4)
                                            (make-mushroom (make-posn 9 24) 4)
                                            (make-mushroom (make-posn 17 32) 4))
                                      (make-posn 2 0)
                                      (make-bullet (make-posn 23 19) #true) 108))
              (make-world (list (make-centipede 'right (list (make-posn 6 37)
                                                             (make-posn 5 37)
                                                             (make-posn 4 37)
                                                             (make-posn 3 37)
                                                             (make-posn 2 37)
                                                             (make-posn 1 37)
                                                             (make-posn 0 37)
                                                             (make-posn 0 38)
                                                             (make-posn 1 38)
                                                             (make-posn 2 38)
                                                             (make-posn 3 38)
                                                             (make-posn 4 38)
                                                             (make-posn 5 38)
                                                             (make-posn 6 38)
                                                             (make-posn 7 38)
                                                             (make-posn 8 38))
                                                #true))
                          (list
                           (make-mushroom (make-posn 23 20) 3)
                           (make-mushroom (make-posn 2 36) 3)
                           (make-mushroom (make-posn 6 21) 4)
                           (make-mushroom (make-posn 9 24) 4)
                           (make-mushroom (make-posn 17 32) 4))
                          (make-posn 2 0)
                          (make-bullet (make-posn 2 0) #false)
                          109))

(check-expect (next-world
               (make-world (list (make-centipede 'left (list (make-posn 8 36)
                                                             (make-posn 9 36)
                                                             (make-posn 10 36)
                                                             (make-posn 11 36)
                                                             (make-posn 12 36)
                                                             (make-posn 13 36)
                                                             (make-posn 14 36)
                                                             (make-posn 15 36)
                                                             (make-posn 16 36)
                                                             (make-posn 17 36)
                                                             (make-posn 18 36)
                                                             (make-posn 19 36)
                                                             (make-posn 20 36)
                                                             (make-posn 21 36)
                                                             (make-posn 22 36)
                                                             (make-posn 23 36))
                                                 #true))
                           (list (make-mushroom (make-posn 11 31) 4)
                                 (make-mushroom (make-posn 10 25) 4)
                                 (make-mushroom (make-posn 22 24) 4)
                                 (make-mushroom (make-posn 4 30) 4)
                                 (make-mushroom (make-posn 11 32) 4))
                           (make-posn 7 0)
                           (make-bullet (make-posn 9 35) #true)
                           216))
(make-world (list (make-centipede 'left (list (make-posn 8 36)) #true)
                  (make-centipede 'left (list (make-posn 10 36)
                                              (make-posn 11 36)
                                              (make-posn 12 36)
                                              (make-posn 13 36)
                                              (make-posn 14 36)
                                              (make-posn 15 36)
                                              (make-posn 16 36)
                                              (make-posn 17 36)
                                              (make-posn 18 36)
                                              (make-posn 19 36)
                                              (make-posn 20 36)
                                              (make-posn 21 36)
                                              (make-posn 22 36)
                                              (make-posn 23 36))
                                  #true))
            (list (make-mushroom (make-posn 9 36) 4)
                  (make-mushroom (make-posn 11 31) 4)
                  (make-mushroom (make-posn 10 25) 4)
                  (make-mushroom (make-posn 22 24) 4)
                  (make-mushroom (make-posn 4 30) 4)
                  (make-mushroom (make-posn 11 32) 4))
            (make-posn 7 0)
            (make-bullet (make-posn 7 0) #false) 217))

(check-expect (next-world (make-world
 (list  (make-centipede 'left (list (make-posn 24 38)
                                    (make-posn 24 39)
                                    (make-posn 23 39)
                                    (make-posn 22 39)
                                    (make-posn 21 39)
                                    (make-posn 20 39)
                                    (make-posn 19 39)
                                    (make-posn 18 39)
                                    (make-posn 17 39)
                                    (make-posn 16 39)
                                    (make-posn 15 39)
                                    (make-posn 14 39)
                                    (make-posn 13 39)
                                    (make-posn 12 39)
                                    (make-posn 11 39)
                                    (make-posn 10 39)) #true))
 (list  (make-mushroom (make-posn 0 37) 4)
        (make-mushroom (make-posn 3 18) 4)
        (make-mushroom (make-posn 1 28) 4)
        (make-mushroom (make-posn 4 32) 4)
        (make-mushroom (make-posn 17 34) 4))
 (make-posn 0 0)
 (make-bullet (make-posn 0 0) #false)
 16))
 (make-world (list (make-centipede 'left (list (make-posn 24 38)
                                               (make-posn 24 39)
                                               (make-posn 23 39)
                                               (make-posn 22 39)
                                               (make-posn 21 39)
                                               (make-posn 20 39)
                                               (make-posn 19 39)
                                               (make-posn 18 39)
                                               (make-posn 17 39)
                                               (make-posn 16 39)
                                               (make-posn 15 39)
                                               (make-posn 14 39)
                                               (make-posn 13 39)
                                               (make-posn 12 39)
                                               (make-posn 11 39)
                                               (make-posn 10 39)) #true))
 (list (make-mushroom (make-posn 0 37) 4)
       (make-mushroom (make-posn 3 18) 4)
       (make-mushroom (make-posn 1 28) 4)
       (make-mushroom (make-posn 4 32) 4)
       (make-mushroom (make-posn 17 34) 4))
 (make-posn 0 0)
 (make-bullet (make-posn 0 0) #false)
 17))

; end-game? : World -> Boolean
; Has the player won or lost the game?
(define (end-game? w)
  (or (player-lost? (world-player w) (world-loc w))
      (empty? (world-loc w))))

(define W1 (make-world loc1 lom2 (make-posn 16 38)
                       (make-bullet (make-posn 16 38) false) 20))
(define W2 (make-world loc1 lom2 (make-posn 10 38)
                       (make-bullet (make-posn 10 38) false) 30))
(define W3 (make-world (list) lom2 (make-posn 7 0)
                       (make-bullet (make-posn 7 0) false) 100))
;
(check-expect (end-game? W1) true)
(check-expect (end-game? W2) false)
(check-expect (end-game? W3) true)

; last-image : World -> Image
; Displays WINNER if player won or LOSER if player lost.
(define (last-image w)
  (place-image-grid 
   (if (player-lost? (world-player w) (world-loc w))
       LOSER
       WINNER)
   (/ GRID-WIDTH 2)
   (/ GRID-HEIGHT 2)
   BG))

(check-expect (last-image W1)
              (place-image-grid LOSER (/ GRID-WIDTH 2) (/ GRID-HEIGHT 2) BG))
(check-expect (last-image W3)
              (place-image-grid WINNER (/ GRID-WIDTH 2) (/ GRID-HEIGHT 2) BG))

;;------Key Handling Functions----- K E Y  H A N D L I N G ---------------------
;key-event: World KeyString -> World
;Moves the player if < > are pressed or shoots the bullet if space is pressed

(define (key-event w a-key)
  (cond
    [(string=? a-key "left")
      (if (move-player? (world-player w) -1)
          (make-world (world-loc w)
                      (world-lom w)
                      (move-player (world-player w) 'left)
                      (bullet-with-player (world-bullet w)
                                          (move-player (world-player w) 'left))
                      (world-tick w))
          w)]
    [(string=? a-key "right")
     (if (move-player? (world-player w) 1)
          (make-world (world-loc w)
                      (world-lom w)
                      (move-player (world-player w) 'right)
                      (bullet-with-player (world-bullet w)
                                          (move-player (world-player w) 'right))
                      (world-tick w))
          w)]
    [(string=? a-key " ")
     (if (boolean=? (active? (world-bullet w)) false) 
         (make-world (world-loc w)
                     (world-lom w)
                     (world-player w)
                     (activate (world-bullet w))
                     (world-tick w))
         w)]
    [else w]))

;TESTS:
(check-expect (key-event WORLD0 "w") WORLD0)
(check-expect (key-event WORLD0 "left") WORLD0)
(check-expect (key-event WORLD0 "right")
              (make-world (world-loc WORLD0)
                      (world-lom WORLD0)
                      (make-posn 1 0)
                      (bullet-with-player (world-bullet WORLD0) (make-posn 1 0))
                      (world-tick WORLD0)))
(check-expect (key-event WORLD0 " ")
              (make-world (world-loc WORLD0)
                          (world-lom WORLD0)
                          (world-player WORLD0)
                          (activate (world-bullet WORLD0))
                          (world-tick WORLD0)))


;;END OF KEY HANDLING

;;------Drawing Functions----- D R A W I N G -----------------------------------

;;place-image-grid: Image Number Number Image -> Image
;;Places an image in the input grid coordinates over the 2nd image
(define (place-image-grid img x y bkg)
  (place-image img
               (* (+ x 0.5) CELL-SIZE)
               (- (* GRID-HEIGHT CELL-SIZE) (* (+ y 0.5) CELL-SIZE))
               bkg))

(check-expect (place-image-grid PLAYER 0 10 BG)
              (place-image PLAYER
               (* (+ 0 0.5) CELL-SIZE)
               (- (* GRID-HEIGHT CELL-SIZE) (* (+ 10 0.5) CELL-SIZE))
               BG))

;;player+image: Player Image -> Image
;;Puts the Player on an image
(define (player+image player image)
  (place-image-grid PLAYER
                    (posn-x player)
                    (posn-y player)
                    image))

(check-expect (player+image (make-posn 5 5) BG)
              (place-image-grid PLAYER 5 5 BG))

; bullet+image : Bullet Image -> Image
; Puts the Bullet onto given image
(define (bullet+image b image)
  (place-image-grid BULLET
                    (posn-x (bullet-posn b))
                    (posn-y (bullet-posn b))
                    image))

(check-expect (bullet+image (make-bullet (make-posn 8 2) 0) BG)
              (place-image-grid BULLET 8 2 BG))

; MUSHROOM HANDLING_____________________________________
; mushroom-image : Mushroom -> Image
; Produces an image of a Mushroom
(define (mushroom-image m)
  (circle MUSHROOM-RADIUS 'solid
          (cond
            [(= (mushroom-health m) 4) MUSHROOM-4-C]
            [(= (mushroom-health m) 3) MUSHROOM-3-C]
            [(= (mushroom-health m) 2) MUSHROOM-2-C]
            [(= (mushroom-health m) 1) MUSHROOM-1-C])))
  
(check-expect (mushroom-image (make-mushroom posn0 4))
              (circle MUSHROOM-RADIUS 'solid MUSHROOM-4-C))
(check-expect (mushroom-image (make-mushroom posn0 3))
              (circle MUSHROOM-RADIUS 'solid MUSHROOM-3-C))
(check-expect (mushroom-image (make-mushroom posn0 2))
              (circle MUSHROOM-RADIUS 'solid MUSHROOM-2-C))
(check-expect (mushroom-image (make-mushroom posn0 1))
              (circle MUSHROOM-RADIUS 'solid MUSHROOM-1-C))
              
; mushroom+image : mushroom Image -> Image
; Draws a Mushroom onto an image
(define (mushroom+image m img)
  (place-image-grid (mushroom-image m)
                    (get-mx m)
                    (get-my m)
                    img))

(define m0 (make-mushroom posn0 4))
(define m1 (make-mushroom posn1 2))
(check-expect (mushroom+image m0 BG)
              (place-image-grid (mushroom-image m0)
                                (get-mx m0)
                                (get-my m0)
                                BG))
(check-expect (mushroom+image m1 BG)
              (place-image-grid (mushroom-image m1)
                                (get-mx m1)
                                (get-my m1)
                                BG))

; lom+image : Image [Listof Mushroom] -> Image
; Draws a List of Mushrooms onto an image
(define (lom+image lom image)
  (cond
    [(empty? lom) image]
    [(cons? lom)
     (mushroom+image (first lom)
                     (lom+image (rest lom) image))]))

(check-expect (lom+image (list) BG) BG)
(check-expect (lom+image (list (make-mushroom posn0 4)
                                  (make-mushroom posn1 3)) BG)
              (place-image-grid (circle MUSHROOM-RADIUS
                                        'solid
                                        MUSHROOM-4-C)
                                (posn-x posn0)
                                (posn-y posn0)
                                (place-image-grid (circle MUSHROOM-RADIUS
                                                          'solid
                                                          MUSHROOM-3-C)
                                                  (posn-x posn1)
                                                  (posn-y posn1)
                                                  BG)))

;;END OF MUSHROOM HANDLING
;;Centipede Handling____________________________________ C E N T I P E D E

;;segment+image: Segment Image -> Image
;;Draws a segment over an image
(define (segment+image segment image ) ;;CENTIPEDE-CELL
  (place-image-grid CENTIPEDE-CELL
                    (posn-x segment)
                    (posn-y segment)
                    image))

(check-expect (segment+image (make-posn 5 3) BG)
              (place-image-grid CENTIPEDE-CELL 5 3 BG))

;;cbody+image: CBody Image -> Image
;;Draws the CBody over an Image
(define (cbody+image body image)
  (cond
    [(empty? body) image]
    [(cons? body)
     (segment+image (first body)
                    (cbody+image (rest body) image))]))

(check-expect (cbody+image (list (make-posn 5 4)) BG)
              (place-image-grid CENTIPEDE-CELL 5 4 BG))

(check-expect (cbody+image empty BG)
              BG)

;;chead+image: Direction CBody Image -> Image
;;Draws the head of the Centipede then calls the cbody to draw the rest of it
(define (chead+image dir cent image)
   (place-image-grid
    (cond
     [(symbol=? dir 'left) LEFT-HEAD]
     [(symbol=? dir 'right) RIGHT-HEAD])
    (posn-x (first cent))
    (posn-y (first cent))
    (cbody+image (rest cent) image)))

(check-expect (chead+image 'left
                          (list (make-posn 5 5))
                           BG)
              (place-image-grid LEFT-HEAD 5 5 (cbody+image empty BG)))

;;centipede+image: Centipede Image -> Image
;;Draws the Centipede over the Image
(define (centipede+image cent image)
  (chead+image (centipede-dir cent) (centipede-body cent) image))

;;loc+image: Loc Image -> Image
;;Draws the Lists of Centipedes over the Image
(define (loc+image loc image)
  (cond
    [(empty? loc) image]
    [(cons? loc)
     (centipede+image (first loc)
                      (loc+image (rest loc) image))]))

;;World Handling________________________________________

;;draw-world: World -> World
;;Draws the world with the following overlay ranking:
;;Player>Centipede>Mushroom>Bullet
(define (draw-world w) ;player + loc + lom
  (player+image (world-player w)
                (loc+image (world-loc w)
                           (lom+image (world-lom w)
                                      (bullet+image (world-bullet w) BG)))))
;;END OF DRAWING

;;-------------- MAIN ------------------------------------------------ 
(define (main num-mushrooms)
  (big-bang (make-world LOC2
                        (create-lom num-mushrooms empty)
                        P1
                        B1
                        0)
            (on-tick next-world 1/10)
            (on-key key-event)
            (to-draw draw-world)
            (stop-when end-game? last-image)))
