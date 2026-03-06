;;;; https://www.lispbm.com/cheatsheet.pdf
;;;; https://github.com/vedderb/bldc/blob/master/lispBM/README.md

(define *cruise-speed* 0.7) ; initial cruise speed from 0.0 to 1.0

(define +rsc-interval+ 0.04) ; seconds for RSC update
(define +rsc-step+ 0.008) ; plus or minus actual value by step
(define *rsc-target* 0.0) ; target value of RSC from 0.0 to 1.0
(define *rsc-actual* 0.0) ; actual value of RSC from 0.0 to 1.0

(define +button-step+ 0.05) ; plus or minus target value by step
(define +button-hold-secs+ 0.35) ; seconds to detect button hold
(define +button-click-secs+ 0.35) ; seconds to detect button click
(define +button-pin+ 0) ; index pin name
(define +button-up+ 1) ; index flag button is up
(define +button-changed+ 2) ; index flag button has changed
(define +button-clicked+ 3) ; index flag button was (multi) clicked
(define +button-last-press+ 4) ; index timestamp last button press
(define +button-last-release+ 5) ; index timestamp last button release
(define +button-click-count+ 6) ; index of (multi) click counter
(define *button-plus* (list 'pin-adc2 nil nil nil 0 0 0)) ; poor man's object
(define *button-minus* (list 'pin-adc3 nil nil nil 0 0 0)) ; poor man's object
(define *button-trigger* (list 'pin-adc1 nil nil nil 0 0 0)) ; poor man's object

(define +timer-interval+ 0) ; index time interval in seconds
(define +timer-fun+ 1) ; index function to eval
(define +timer-last-call+ 2) ; index timestamp last call of function
(define *timer* (list )) ; poor man's object

(defun utils-map(x in-min in-max out-min out-max)
  ;; re-maps X from in-range to out-range
  (+ (/ (* (- x in-min)
           (- out-max out-min))
        (- in-max in-min))
     out-min)
)

(defun utils-constrain(x out-min out-max)
  ;; constrains X to be within out-range
  (cond
    ((< x out-min) out-min)
    ((> x out-max) out-max)
    (t x)
  )
)

(defun utils-min(a b)
  ;; return the smaller of two numbers
  (if (> a b) b a)
)

(defun timer-every(interval function) {
  ;; call FUNCTION every INTERVAL seconds
  (var x (list interval function 0)) ; create new timer
  (setq *timer* (cons x *timer*)) ; prepend new timer to list
  (setix (ix *timer* 0) +timer-last-call+ (systime)) ; start new timer
})

(defun timer-tick() {
  ;; ticks the timer forward
  (loopfor i 0 (< i (length *timer*)) (+ i 1) {
    (var a (ix (ix *timer* i) +timer-interval+))
    (var b (ix (ix *timer* i) +timer-fun+))
    (var c (ix (ix *timer* i) +timer-last-call+))
    (if (< a (secs-since c)) {
      (eval b) ; call function
      (setix (ix *timer* i) +timer-last-call+ (systime)) ; restart timer
    })
  })
})

(defun button-update(x) {
  ;; update the button X
  ; reset button-changed
  (setix x +button-changed+ nil)
  ; set button-changed
  (if (eq 0 (gpio-read (ix x +button-pin+)))
    (if (ix x +button-up+) {
      (setix x +button-up+ nil)
      (setix x +button-changed+ t)
      (setix x +button-last-press+ (systime))
      (setix x +button-click-count+ (+ 1 (ix x +button-click-count+)))
    })
    (if (not (ix x +button-up+)) {
      (setix x +button-up+ t)
      (setix x +button-changed+ t)
      (setix x +button-last-release+ (systime))
    })
  )
  ; reset button-clicked
  (if (ix x +button-clicked+)
    (if (< 0 (ix x +button-click-count+)) {
      (setix x +button-clicked+ nil)
      (setix x +button-click-count+ 0)
    })
  )
  (if (ix x +button-up+)
    ; set button-clicked
    (if (< 0 (ix x +button-click-count+))
      (if (< +button-click-secs+ (secs-since (ix x +button-last-release+)))
        (setix x +button-clicked+ t)
      )
    )
    ; reset button-click-count on button-hold
    (if (< +button-hold-secs+ (secs-since (ix x +button-last-press+)))
      (setix x +button-click-count+ 0)
    )
  )
})

(defun button-press(x)
  ;; return T if button X is pressed
  (if (not (ix x +button-up+))
    (if (ix x +button-changed+) t nil)
  )
)

(defun button-click(x y) {
  ;; return T if button X is clicked Y times
  (if (ix x +button-clicked+)
    (if (eq y (ix x +button-click-count+)) t nil)
  )
})

(defun button-hold(x)
  ;; return T if button X is hold
  (if (not (ix x +button-up+))
    (if (< +button-hold-secs+ (secs-since (ix x +button-last-press+))) t nil)
  )
)

(defun button-minus-on-hold()
  ;; handler for button-minus on hold
  (if (button-hold *button-minus*) {
    (print "button-minus-on-hold")
    (setq *rsc-target* (utils-constrain (- *rsc-target* +button-step+) 0.0 1.0))
  })
)

(defun button-plus-on-hold()
  ;; handler for button-plus on hold
  (if (button-hold *button-plus*) {
    (print "button-plus-on-hold")
    (setq *rsc-target* (utils-constrain (+ *rsc-target* +button-step+) 0.0 1.0))
  })
)

(defun rsc-update() {
  ;; handler for ramp/soak controller
  (var error (abs (- *rsc-target* *rsc-actual*)))
  (var step +rsc-step+)
  ; stop motor on timeout
  (if (< 0.0 *rsc-actual*)
    (timeout-reset)
  )
  ; ramp motor
  (if (< 0.0 error) {
    ; bigger steps on bigger error
    (if (< +button-step+ error)
      (setq step (* 2 +rsc-step+))
    )
    (setq step (utils-min error step))
    (if (< *rsc-actual* *rsc-target*)
      (setq *rsc-actual* (+ *rsc-actual* step))
      (setq *rsc-actual* (- *rsc-actual* step))
    )
    (set-duty (utils-map *rsc-actual* 0.0 1.0 (conf-get 'l-min-duty) (conf-get 'l-max-duty)))
    (print (str-merge "actual: " (str-from-n *rsc-actual* "%.3f") " target: " (str-from-n *rsc-target* "%.2f")))
  })
})

(defun main-loop() {
  ;; main loop
  (button-update *button-plus*)
  (button-update *button-minus*)
  (button-update *button-trigger*)
  (timer-tick)
  (cond
    ((button-press *button-minus*) {
      (print "button-press *button-minus*")
      (setq *rsc-target* (utils-constrain (- *rsc-target* +button-step+) 0.0 1.0))
    })
    ((button-press *button-plus*) {
      (print "button-press *button-plus*")
      (setq *rsc-target* (utils-constrain (+ *rsc-target* +button-step+) 0.0 1.0))
    })
    ((button-click *button-trigger* 2) {
      (print "button-click *button-trigger* twice")
      (if (eq 1.0 *rsc-target*)
        (setq *rsc-target* *cruise-speed*)
        (setq *rsc-target* 1.0)
      )
    })
    ((button-click *button-trigger* 1) {
      (print "button-click *button-trigger* once")
      (if (eq 0.0 *rsc-target*)
        (setq *rsc-target* *cruise-speed*)
        {
          (if (not (eq 1.0 *rsc-target*))
            (setq *cruise-speed* *rsc-target*)
          )
          (setq *rsc-target* 0.0)
        }
      )
    })
  )
})

;;; button init
(gpio-configure (ix *button-plus* +button-pin+) 'pin-mode-in-pu)
(gpio-configure (ix *button-minus* +button-pin+) 'pin-mode-in-pu)
(gpio-configure (ix *button-trigger* +button-pin+) 'pin-mode-in-pu)

;;; timer init
(timer-every +button-hold-secs+ '(button-minus-on-hold))
(timer-every +button-hold-secs+ '(button-plus-on-hold))
(timer-every +rsc-interval+ '(rsc-update))

;;; low voltage
(if (< (get-vin) 21) {
  ;signal 4 beeps
  (foc-beep 1000 0.3 10)
  (foc-beep 1100 0.3 10)
  (foc-beep 1200 0.3 10)
  (foc-beep 1300 0.3 10)
})

;;; signal init
(foc-beep 800 0.5 10)

;;; main loop
(loopwhile t {
  (main-loop)
  (sleep +rsc-interval+) ; keep CPU low
})
