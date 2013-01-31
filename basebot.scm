;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

#lang scheme

(define square
  (lambda (x) (* x x)))

(define quadrature
  (lambda (x y)
    (sqrt (+ (square x) (square y)))))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
(define pi 3.14159)

;; Problem 1

(define position
  (lambda (a v u t)
    (+ u (* v t) (* .5 a (square t)))))

;; you need to complete this procedure, then show some test cases

; (= (position 0 0 0 0) 0)
; (= (position 0 0 20 0) 20)
; (= (position 0 5 10 10) 60)
; (= (position 2 2 2 2) 10)
; (= (position 5 5 5 5) 92.5)
; (= (position -10 100 -20 3) 235)

;; Problem 2

(define imaginary-root?
  (lambda (a b c)
    (if (> 0 (- (square b) (* 4 a c))) #t #f)))

(define order?
  (lambda (a b c)
    (cond ((= a b 0) 0)
          ((= a 0) 1)
          (else 2))))

(define root
  (lambda (a b c sign)
    (cond ((= (order? a b c) 0) (if (= c 0) 0 #f))
          ((= (order? a b c) 1) (/ (- c) b))
          ((imaginary-root? a b c) #f)
          (else 
           (/ ((if (= sign 1) + -) (- b) (sqrt (- (square b) (* 4 a c)))) (* 2 a))))))

(define root1
  (lambda (a b c)
    (root a b c 1)))

(define root2
  (lambda (a b c)
    (root a b c 2)))

;; complete these procedures and show some test cases

; (root1 5 3 6)
; (root2 5 3 6)
; (root1 0 0 0)
; (root2 0 0 0)
; (root1 1 2 -15)
; (root2 1 2 -15)

;; Problem 3

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (root2 (/ (- gravity) 2) vertical-velocity elevation)))

;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-velocity (- elevation target-elevation))))

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

(define velocity-x (lambda (velocity angle) (* velocity (cos (degree2radian angle)))))
(define velocity-y (lambda (velocity angle) (* velocity (sin (degree2radian angle)))))

(define travel-distance-simple
  (lambda (elevation velocity angle)
    (* (velocity-x velocity angle) (time-to-impact (velocity-y velocity angle) elevation))))

;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; at an angle of (/ pi 4) radians or 45 degrees

;; what is the distance traveled in each case?
;; record both in meters and in feet

; (travel-distance-simple 1 45 0)
; (travel-distance-simple 1 45 45)
; (travel-distance-simple 1 45 90)

; (meters-to-feet (travel-distance-simple 1 45 0))
; (meters-to-feet (travel-distance-simple 1 45 45))
; (meters-to-feet (travel-distance-simple 1 45 90))

;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define find-best-angle
  (lambda (elevation velocity)
    (find-best-angle-helper elevation velocity 0 90 1)))

(define find-better-angle
  (lambda (elevation velocity a b)
    (if (> (travel-distance-simple elevation velocity a) 
           (travel-distance-simple elevation velocity b))
           a b)))

(define find-best-angle-helper
  (lambda (elevation velocity min max step)
    (if (= min max) min
        (find-better-angle elevation velocity min (find-best-angle-helper elevation velocity (+ min step) max step)))))
    
;; find best angle
;; try for other velocities
;; try for other heights

; (find-best-angle 1 45)
; (find-best-angle 5 20)
; find-best-angle 100 0)
; (find-best-angle 1000 1)

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin 
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two 
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level 
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4 
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor 
;; that can be computed

;; We would like to again compute distance , but taking into account 
;; drag.
;; Basically we can rework the equations to get four coupled linear 
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the 
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speeed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one 
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

(define dx
  (lambda (u dt)
    (* u dt)))

(define dy
  (lambda (v dt)
    (* v dt)))

(define du
  (lambda (u v dt m beta)
    (- (* (/ (* beta u (quadrature u v)) m) dt))))

(define dv
  (lambda (u v dt m beta g)
    (- (* (+ (/ (* beta v (quadrature u v)) m) g) dt))))

(define integrate
  (lambda (x0 y0 u0 v0 dt g m beta)
    (if (< y0 0) x0
        (integrate (+ x0 (dx u0 dt))
                   (+ y0 (dy v0 dt))
                   (+ u0 (du u0 v0 dt m beta))
                   (+ v0 (dv u0 v0 dt m beta g))
                   dt g m beta))))
    
(define travel-distance
  (lambda (elevation velocity angle)
    (integrate 0 elevation (velocity-x velocity angle) (velocity-y velocity angle) 0.01 gravity mass beta)))

; (travel-distance 1 35 45)
; (travel-distance 1 40 45)
; (travel-distance 1 45 45)

; (travel-distance 1 45 28)
; (travel-distance 1 45 45)
; (travel-distance 1 45 50)

;; RUN SOME TEST CASES

;; what about Denver?

;; Problem 7
 
;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to 
;; use, given a velocity, in order to reach a given height (receiver) at a 
;; given distance

(define right-distance?
  (lambda (elevation velocity angle target-distance error)
    (if (< (abs (- (travel-distance elevation velocity angle) target-distance)) error) #t #f)))

(define min-angle
  (lambda (elevation velocity target-distance error min max step)
    (cond ((> min max) #f)
          ((right-distance? elevation velocity min target-distance error) min)
          (else (min-angle elevation velocity target-distance error (+ min step) max step)))))

(define max-angle
  (lambda (elevation velocity target-distance error min max step)
    (cond ((> min max) #f)
          ((right-distance? elevation velocity max target-distance error) max)
          (else (max-angle elevation velocity target-distance error min (- max step) step)))))

(define integrate-time
  (lambda (x0 y0 u0 v0 t0 dt g m beta)
    (if (< y0 0) t0
        (integrate-time (+ x0 (dx u0 dt))
                        (+ y0 (dy v0 dt))
                        (+ u0 (du u0 v0 dt m beta))
                        (+ v0 (dv u0 v0 dt m beta g))
                        (+ t0 dt)
                        dt g m beta))))

(define travel-time
  (lambda (elevation velocity angle)
    (if (not angle) #f
        (integrate-time 0 elevation (velocity-x velocity angle) (velocity-y velocity angle) 0 0.01 gravity mass beta))))

(define find-faster-angle
  (lambda (elevation velocity a b)
    (if (< (travel-time elevation velocity a) 
           (travel-time elevation velocity b))
           a b)))

(define find-fastest-angle
  (lambda (elevation velocity min max step)
    (if (= min max) min
        (find-faster-angle elevation velocity min (find-fastest-angle elevation velocity (+ min step) max step)))))

(define best-throwing-angle
  (lambda (elevation velocity target-distance error)
    (if (not (min-angle elevation velocity target-distance error -90 90 1)) #f
        (find-fastest-angle elevation velocity 
                            (min-angle elevation velocity target-distance error -90 90 1)
                            (max-angle elevation velocity target-distance error -90 90 1) 1))))

; (best-throwing-angle 1 45 50 2) -> 8
; (best-throwing-angle 1 45 40 2) -> 6
; (best-throwing-angle 1 45 1000 2) -> #f

(define best-throwing-time
  (lambda (elevation velocity target-distance error)
    (travel-time elevation velocity (best-throwing-angle elevation velocity target-distance error))))

;; a catcher trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft) 
;; using 45m/s

; INTERESTING BUG: take a look at these throwing times

; (best-throwing-time 1 45 34 1) -> 0.8800000000000006
; (best-throwing-time 1 45 35 1) -> #f
; (best-throwing-time 1 45 36 1) -> 6.7799999999999
; (best-throwing-time 1 45 37 1) -> 0.9900000000000007

; but if we increase the error...

; (best-throwing-time 1 45 36 2) -> 0.9900000000000007

;; Problem 8

;; Problem 9