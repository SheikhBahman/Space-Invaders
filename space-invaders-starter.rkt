;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


;; My Space Invaders (Bahman Sheikh)
;; Start Date: 9/27/2018
;; -------------------------------------
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (rectangle WIDTH HEIGHT "outline" "black"))
(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))            ;Half of the Tank Height
(define TANK-HEIGHT-POS (- HEIGHT TANK-HEIGHT/2))           ;Tank Height position

(define MISSILE (ellipse 5 15 "solid" "red"))               ;Missile
;; -------------------------------------
;; Data Definitions:

(define-struct game (invaders missiles tank time))
;; Game is (make-game  (listof Invader) (listof Missile) Tank Integer)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position.
;;         time track the number of tick from the start of the game.

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))   ;listof Invader
       (fn-for-lom (game-missiles s))         ;listof Missile
       (fn-for-tank (game-tank s))            ;Tank
       (fn-for-time (game-time s))))          ;Time

;; Templete rules used:
;; - compound: 4 fields


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

;; Templete rules used:
;; - compound: 2 fields

(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvaders is one of:
;; - empty
;; (cons Invader ListOfInvaders)
;; interp. list of invaders in the domain
(define LOI-0 empty)
(define LOI-1 (cons I1 (cons I2 empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]                      ;Base case 
        [else (...(fn-for-invader (first loi))    ;An invader
                  (fn-for-loi (rest loi)))]))     ;Natural recursion

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Invader ListOfInvaders)
;;  - refrence: (first loi) is Invader
;;  - self-refrence: (rest loi) is ListOfInvaders
  
(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissiles is one of:
;; - empty
;; (cons Missile ListOfMissiles)
;; interp. list of missiles in the domain
(define LOM-0 empty)
(define LOM-1 (cons M1 (cons M3 empty)))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]                      ;Base case 
        [else (...(fn-for-missile (first loi))    ;Missile
                  (fn-for-lom (rest lom)))]))     ;Natural recursion

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Missile ListOfMissiles)
;;  - refrence: (first lom) is Missile
;;  - self-refrence: (rest lom) is ListOfMissiles

;; /////////////////////////////////////////////////////////////////
;; /////////////////////////////////////////////////////////////////
;; /////////////////////////////////////////////////////////////////
;; /////////////////////////////////////////////////////////////////
;; /////////////////////////////////////////////////////////////////
;; /////////////////////////////////////////////////////////////////

(define G0 (make-game empty empty T0 0))
(define G1 (make-game empty empty T1 0))
(define G2 (make-game (list I1) (list M1) T1 0))
(define G3 (make-game (list I1 I2) (list M1 M2) T1 0))

;; /////////////////////////////////////////////////////////////////
;; /////////////////////////////////////////////////////////////////
;; /////////////////////////////////////////////////////////////////
;; Functions
;; /////////////////////////////////////////////////////////////////
;; /////////////////////////////////////////////////////////////////
;; /////////////////////////////////////////////////////////////////


;; Stat Function////////////////////////////////////////////////////
;; Game -> Game
;; start the game world

(define (main game)
  (big-bang game                          ;Game
  (on-tick   update-world )               ;Game -> Game
  (to-draw   render)                      ;Game -> Image
  (on-key    handle-keyboard)             ;Game keyEvents -> Game
  (stop-when reachedEnd? last-picture)))  ;Game -> if true -> last-picture, stop tick
;; End Function////////////////////////////////////////////////////


;; Stat Function////////////////////////////////////////////////////
;; Game -> Image
;; Draw image of all invaders and missiles and the tank on the scene
(check-expect (render (make-game empty empty (make-tank (/ WIDTH 2) 1) 0)) (underlay MTS BACKGROUND BACKGROUND (place-image TANK (/ WIDTH 2) TANK-HEIGHT-POS BACKGROUND)))
(check-expect (render (make-game (list I1 I2) (list M1 M2) T1 0))
              (underlay MTS (place-image INVADER (invader-x I1) (invader-y I1) (place-image INVADER (invader-x I2) (invader-y I2) BACKGROUND))
                            (place-image MISSILE (missile-x M1) (missile-y M1) (place-image MISSILE (missile-x M2) (missile-y M2) BACKGROUND))
                            (place-image TANK 50 TANK-HEIGHT-POS BACKGROUND)))

;(define (render game) BACKGROUND) ; stub

(define (render game)
  (underlay MTS (draw-invaders (game-invaders game)) (draw-missiles (game-missiles game)) (drawTank (game-tank game))))
;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; ListOfInvaders -> Image
;; Itirate over a given list of invaders and render onto background.
(check-expect (draw-invaders empty) BACKGROUND)
(check-expect (draw-invaders (list I1 I2)) (place-image INVADER (invader-x I1) (invader-y I1) (place-image INVADER (invader-x I2) (invader-y I2) BACKGROUND)))

;<tenplate from ListOfInvaders>

(define (draw-invaders loi)
  (cond [(empty? loi) BACKGROUND]                     
        [else (draw-one-invader (first loi) (draw-invaders (rest loi)))]))  ;Cascade image of the invaders
;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; Invader Image -> Image
;; Draw image of a given invader on a given image.
(check-expect (draw-one-invader I1 BACKGROUND) (place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND))

;(define (draw-one-invader i img) BACKGROUND) ;stub

(define (draw-one-invader invader img)
  (place-image INVADER (invader-x invader) (invader-y invader) img))
;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; ListOfMissiles -> Image
;; Itirate over a given list of missiles and render onto background.
(check-expect (draw-missiles empty) BACKGROUND)
(check-expect (draw-missiles (list M1 M2)) (place-image MISSILE (missile-x M1) (missile-y M1) (place-image MISSILE (missile-x M2) (missile-y M2) BACKGROUND)))

;<tenplate from ListOfMissiles>

(define (draw-missiles lom)
  (cond [(empty? lom) BACKGROUND]                     
        [else (draw-one-missile (first lom) (draw-missiles (rest lom)))]))  ;Cascade image of the missiles
;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; Missile Image -> Image
;; Draw image of a given missile on a given image.
(check-expect (draw-one-missile M1 BACKGROUND) (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND))
;(define (draw-one-missiles m img) BACKGROUND) ;stub

(define (draw-one-missile missile img)
  (place-image MISSILE (missile-x missile) (missile-y missile) img))
;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; Tank -> Image
;; Draw the tank at its position
(check-expect (drawTank T0) (place-image TANK (/ WIDTH 2) TANK-HEIGHT-POS BACKGROUND))
(check-expect (drawTank T1) (place-image TANK 50 TANK-HEIGHT-POS BACKGROUND))
              
;(define (drawTank t) BACKGROUND) ;stub

(define (drawTank t)
  (place-image TANK (tank-x t) TANK-HEIGHT-POS BACKGROUND))
;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; Game -> Game
;; produce a new Game after moving each invaders, missiles and the tank to their new position
;; Thereis no check-expect for this function because the invaders enter the domain at a random position and a random dirction of movement.
;; Therefore check-expect dosen't work.
;; (check-expect (update-world G0) (make-game empty empty (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1) 0))
;; (check-expect (update-world G3) (make-game empty empty (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1) 0))

;(define (update-world g) G0) ;stub

(define (update-world game)
  (cond [(integer? (/ (game-time game) INVADE-RATE)) (make-game (move-invaders-produceNewInvader (game-invaders game) (game-missiles game)) (move-missiles (game-missiles game)) (move-tank (game-tank game)) (add1 (game-time game)))]
        [else (make-game (move-invaders (game-invaders game) (game-missiles game)) (move-missiles (game-missiles game)) (move-tank (game-tank game)) (add1 (game-time game)))]))
;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; Randemely create a new invade with random horizontal position,
;; also move available invaders in the list.

;; As the position and the direction of movement of the new invader are random there is no check-expect 
;; for this function.

; (define (move-invaders-produceNewInvader LOI-1 LOM-1) LOI-1) ; stub

(define (move-invaders-produceNewInvader loi lom)
  (cons (make-invader (random WIDTH) 1 (- (random WIDTH) (random WIDTH))) (move-invaders loi lom)))
;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; NULL -> Natural
;; randomely return +1 or -1
;; As this function produce randomely +1 or -1 check-expect not exist

(define randomSign
  (if (> (random 11) 5)
      1.0
      -1.0))

;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; Check the collision between the invaders in the list of invaders and missiles if there is no collision
;; move the invader to a new possition if there is a collision omit the invader from the list of invaders.

(check-expect (move-invaders LOI-0 LOM-1) empty)
(check-expect (move-invaders (cons I2 (cons I1 empty)) LOM-1) (cons (make-invader (- 150 INVADER-X-SPEED) (+ HEIGHT INVADER-Y-SPEED) (- (invader-dx I2) INVADER-X-SPEED)) empty))

; (define (move-invaders LOI-1 LOM-1) LOI-1) ; stub

(define (move-invaders loi lom)
  (cond [(empty? loi) empty]                      
        [else (if (invader-missile-collision? (first loi) lom)
                  (move-invaders (rest loi) lom)
                  (cons (move-invader  (first loi))
                        (move-invaders (rest loi) lom)))]))
;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; Invader ListOfMissiles -> Boolean
;; Check the collision between an invader and missiles in the given list of missiles if there is a collision
;; return true if no return false.

(check-expect (invader-missile-collision? I1 LOM-0) false)
(check-expect (invader-missile-collision? I1 LOM-1) true)
(check-expect (invader-missile-collision? I2 LOM-1) false)

; (define (invader-missile-collision? I1) true) ; stub

(define (invader-missile-collision? inavder lom)
  (cond [(empty? lom) false]                      
        [else (if (and (> (+ (invader-x inavder) HIT-RANGE) (missile-x (first lom))) (< (- (invader-x inavder) HIT-RANGE) (missile-x (first lom))))
                  (if (and (> (+ (invader-y inavder) HIT-RANGE) (missile-y (first lom))) (< (- (invader-y inavder) HIT-RANGE) (missile-y (first lom))))
                      true
                      (invader-missile-collision? inavder (rest lom)))
                  (invader-missile-collision? inavder (rest lom)))]))

;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; Invader -> Invader
;; move an invader to its new position: check direction of movement and right/left walls.
(check-expect (move-invader I1) (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) (+ 12 INVADER-X-SPEED)))
(check-expect (move-invader I2) (make-invader (- 150 INVADER-X-SPEED) (+ HEIGHT INVADER-Y-SPEED) (- (invader-dx I2) INVADER-X-SPEED)))
(check-expect (move-invader (make-invader WIDTH 100 12)) (make-invader (- WIDTH INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) (* WIDTH -1)))
(check-expect (move-invader (make-invader 0 100 -10)) (make-invader INVADER-X-SPEED (+ 100 INVADER-Y-SPEED) 0))

; (define (move-invader i) I1) ; stub

(define (move-invader invader)  
   (make-invader (newX invader) (+ INVADER-Y-SPEED (invader-y invader)) (check-wall invader)))


;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; Invader -> Natural
;; Produces the new horizontal position of an invader depending on 1) the invender is
;; going to the right or the left 2) the invader is reached to the left or right sides of the domain.
(check-expect (newX I1) (+ 150 INVADER-X-SPEED))
(check-expect (newX I2) (- 150 INVADER-X-SPEED))
(check-expect (newX (make-invader WIDTH 100 12)) (- WIDTH INVADER-X-SPEED))
(check-expect (newX (make-invader 0 100 -10)) INVADER-X-SPEED)


; (define (invader-dx i) I1) ; stub

(define (newX invader)
  (cond [(>= (invader-dx invader) 0)(if (>= (+ INVADER-X-SPEED (invader-x invader)) WIDTH)
                                       (- (invader-x invader) INVADER-X-SPEED)
                                       (+ (invader-x invader) INVADER-X-SPEED))]
        [(< (invader-dx invader) 0)(if (>= (-(invader-x invader) INVADER-X-SPEED) 0)
                                       (- (invader-x invader) INVADER-X-SPEED)
                                       INVADER-X-SPEED)]))
;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; Invader -> Natural
;; Check the position of a given invader if it reached to the right wall calculate the new direction of movement to the left and vice versa.
;; If the invader is not at the left or right wall its dx modified according to the invader speed.
(check-expect (check-wall I1) (+ 12 INVADER-X-SPEED))
(check-expect (check-wall I2) (- (invader-dx I2) INVADER-X-SPEED))
(check-expect (check-wall(make-invader WIDTH 100 12)) (* WIDTH -1))
(check-expect (check-wall(make-invader 0 100 -10)) 0)


; (define (check-wall i) I1) ; stub

  
(define (check-wall invader)
  (cond [(>= (invader-dx invader) 0)(if (>= (+ INVADER-X-SPEED (invader-x invader)) WIDTH)
                                       (* WIDTH -1)
                                       (+ (invader-dx invader) INVADER-X-SPEED))]
        [(< (invader-dx invader) 0)(if (<= (-(invader-x invader) INVADER-X-SPEED) 0)
                                       0
                                       (- (invader-dx invader) INVADER-X-SPEED))]))


;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; ListOfMissiles -> ListOfMissiles
;; Move missiles toward the top according to the missile speed,
;; if a missile reaches the top it will be omited from the list of missiles.
(check-expect (move-missiles empty) empty)
(check-expect (move-missiles (list (make-missile 150 (- 0 MISSILE-SPEED )))) empty)
(check-expect (move-missiles (list M1)) (list (make-missile 150 (- 300 MISSILE-SPEED ))))
(check-expect (move-missiles (list M1 M2)) (list (make-missile 150 (- 300 MISSILE-SPEED )) (make-missile 150(- 110 MISSILE-SPEED ))))


;(define (move-missiles M) M1);stub

(define (move-missiles lom)
  (cond [(empty? lom) empty]                      
        [else (if (missile-reached-top? (first lom))
                  (move-missiles (rest lom))
                  (cons (move-missile  (first lom))    
                   (move-missiles (rest lom))))]))

;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; Missile -> Boolean
;; Produce true if the missile reached the top of the domain.
(check-expect (missile-reached-top? M1) false)
(check-expect (missile-reached-top? (make-missile 150 0)) true)
(check-expect (missile-reached-top? (make-missile 150 -5)) true)

; (define (missile-reached-top? M1) false) ; stub

(define (missile-reached-top? missile)
  (if (<= (- (missile-y missile) MISSILE-SPEED ) 0)
      true
      false))
;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; Missile -> Missile
;; Move the missile toward the top according to the missile speed 
(check-expect (move-missile M1) (make-missile 150 (- 300 MISSILE-SPEED )))
(check-expect (move-missile M2) (make-missile 150(- 110 MISSILE-SPEED )))
; (define (move-missile M) M1) ; stub 

(define (move-missile missile)
  (make-missile (missile-x missile) (- (missile-y missile) MISSILE-SPEED )))
;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; Tank -> Tank
;; Mave the tank according to the tank speed and direction also
;; Simulate periodic-boundary condition for the tank: If the tank reaches the right boundary it re-enter to the domain from the left side and vice-versa.
(check-expect (move-tank T0) (make-tank (+ TANK-SPEED (/ WIDTH 2)) +1))
(check-expect (move-tank T2) (make-tank (+ (* -1 TANK-SPEED) 50) -1))
(check-expect (move-tank (make-tank WIDTH +1)) (make-tank 0 +1))
(check-expect (move-tank (make-tank 0 -1)) (make-tank WIDTH -1))

;(define (move-tank T) T1) ;stub

(define (move-tank tank)
  (cond [(> (+ (* (tank-dir tank) TANK-SPEED) (tank-x tank)) WIDTH) (make-tank 0 (tank-dir tank))]
        [(< (+ (* (tank-dir tank) TANK-SPEED) (tank-x tank)) 0) (make-tank WIDTH (tank-dir tank))]
        [else (make-tank (+ (* (tank-dir tank) TANK-SPEED) (tank-x tank)) (tank-dir tank))]))
;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; Game Keyboard-Event -> Game
;; if press left-arrow key the tank should move to the left,
;; if press Right-arrow key the tank should move to the right,
;; if press space bar the tank should fire a missile.
(check-expect (handle-keyboard G0 "up") G0)
(check-expect (handle-keyboard G0 "left") (make-game empty empty (make-tank (tank-x (game-tank G0)) -1) (game-time G0)))
(check-expect (handle-keyboard G0 "right") (make-game empty empty (make-tank (tank-x (game-tank G0)) +1) (game-time G0)))
(check-expect (handle-keyboard G0 " ") (make-game empty (cons (make-missile (tank-x (game-tank G0)) (- HEIGHT (image-height TANK))) empty) (game-tank G0) (game-time G0)))

;<template from KeyboardEvent>

(define (handle-keyboard game a-key)
  (cond
    [(key=? a-key "left")  (make-game (game-invaders game) (game-missiles game) (make-tank (tank-x (game-tank game)) -1) (game-time game))]
    [(key=? a-key "right") (make-game (game-invaders game) (game-missiles game) (make-tank (tank-x (game-tank game)) +1) (game-time game))]
    [(key=? a-key " ") (make-game (game-invaders game) (cons (make-missile (tank-x (game-tank game)) (- HEIGHT (image-height TANK))) (game-missiles game)) (game-tank game) (game-time game))]
    [else game]))

;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; Game -> Boolean
;; produce true if at list one invader in the game world reaches the bottom of the screen.
(check-expect (reachedEnd? G0) false)
(check-expect (reachedEnd? G1) false)
(check-expect (reachedEnd? G2) false)
(check-expect (reachedEnd? G3) true)

;(define (reachedEnd? game) true)  ;stub

(define (reachedEnd? game)
  (invader-reachedEnd? (game-invaders game)))
;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; ListOfInvaders -> Boolean
;; produce true if at list one invader reaches the bottom of the screen.
(check-expect (invader-reachedEnd? empty) false)
(check-expect (invader-reachedEnd? (list I1)) false)
(check-expect (invader-reachedEnd? (list I1 I2)) true)

;<template from ListOfInvaders>

(define (invader-reachedEnd? loi)
  (cond [(empty? loi) false]                  
        [else (if (>= (invader-y (first loi)) HEIGHT)
                  true
                  (invader-reachedEnd? (rest loi)))]))
;; End Function////////////////////////////////////////////////////


;; Stat Function///////////////////////////////////////////////////
;; Game -> VOID
;; Draw a game over message at the center of the screen.
(check-expect (last-picture G2) (underlay MTS (text "GAME OVER" 36 "indigo")))

;(define (last-picture t)  (underlay MTS (text "GAME OVER" 36 "indigo"))) ;stub

(define (last-picture s)
  (underlay MTS (text "GAME OVER" 36 "indigo")))
;; End Function////////////////////////////////////////////////////

(main G2)