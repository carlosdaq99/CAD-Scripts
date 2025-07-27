;; 5-Point Ellipse  -  Lee Mac
;; Args: p1,p2,p3,p4,p5 - UCS points defining Ellipse
;; Returns a list of: ((10 <WCS Center>) (11 <WCS Major Axis Endpoint from Center>) (40 . <Minor/Major Ratio>))
;; Version 1.1 - 2013-11-28

(defun LM:5P-Ellipse ( p1 p2 p3 p4 p5 / a av b c cf cx cy d e f i m1 m2 rl v x )
    (setq m1
        (trp
            (mapcar
                (function
                    (lambda ( p )
                        (list
                            (* (car  p) (car  p))
                            (* (car  p) (cadr p))
                            (* (cadr p) (cadr p))
                            (car  p)
                            (cadr p)
                            1.0
                        )
                    )
                )
                (list p1 p2 p3 p4 p5)
            )
        )
    )
    (setq i -1.0)
    (repeat 6
        (setq cf (cons (* (setq i (- i)) (detm (trp (append (reverse m2) (cdr m1))))) cf)
              m2 (cons (car m1) m2)
              m1 (cdr m1)
        )
    )
    (mapcar 'set '(f e d c b a) cf) ;; Coefficients of Conic equation ax^2 + bxy + cy^2 + dx + ey + f = 0
    (if (< 0 (setq x (- (* 4.0 a c) (* b b))))
        (progn
            (if (equal 0.0 b 1e-8) ;; Ellipse parallel to coordinate axes
                (setq av '((1.0 0.0) (0.0 1.0))) ;; Axis vectors
                (setq av
                    (mapcar
                        (function
                            (lambda ( v / d )
                                (setq v (list (/ b 2.0) (- v a)) ;; Eigenvectors
                                      d (distance '(0.0 0.0) v)
                                )
                                (mapcar '/ v (list d d))
                            )
                        )
                        (quad 1.0 (- (+ a c)) (- (* a c) (* 0.25 b b))) ;; Eigenvalues
                    )
                )
            )
            (setq cx (/ (- (* b e) (* 2.0 c d)) x) ;; Ellipse Center
                  cy (/ (- (* b d) (* 2.0 a e)) x)
            )
            ;; For radii, solve intersection of axis vectors with Conic Equation:
            ;; ax^2 + bxy + cy^2 + dx + ey + f = 0  }
            ;; x = cx + vx(t)                       }- solve for t
            ;; y = cy + vy(t)                       }
            (setq rl
                (mapcar
                    (function
                        (lambda ( v / vv vx vy )
                            (setq vv (mapcar '* v v)
                                  vx (car  v)
                                  vy (cadr v)
                            )
                            (apply 'max
                                (quad
                                    (+ (* a (car vv)) (* b vx vy) (* c (cadr vv)))
                                    (+ (* 2.0 a cx vx) (* b (+ (* cx vy) (* cy vx))) (* c 2.0 cy vy) (* d vx) (* e vy))
                                    (+ (* a cx cx) (* b cx cy) (* c cy cy) (* d cx) (* e cy) f)
                                )
                            )
                        )
                    )
                    av
                )
            )
            (if (apply '> rl)
                (setq rl (reverse rl)
                      av (reverse av)
                )
            )
            (list
                (cons 10 (trans (list cx cy) 1 0)) ;; WCS Ellipse Center
                (cons 11 (trans (mapcar '(lambda ( v ) (* v (cadr rl))) (cadr av)) 1 0)) ;; WCS Major Axis Endpoint from Center
                (cons 40 (apply '/ rl)) ;; minor/major ratio
            )
        )
    )
)

;; Matrix Determinant (Upper Triangular Form)  -  ElpanovEvgeniy
;; Args: m - nxn matrix

(defun detm ( m / d )
    (cond
        (   (null m) 1)
        (   (and (zerop (caar m))
                 (setq d (car (vl-member-if-not (function (lambda ( a ) (zerop (car a)))) (cdr m))))
            )
            (detm (cons (mapcar '+ (car m) d) (cdr m)))
        )
        (   (zerop (caar m)) 0)
        (   (*  (caar m)
                (detm
                    (mapcar
                        (function
                            (lambda ( a / d ) (setq d (/ (car a) (float (caar m))))
                                (mapcar
                                    (function
                                        (lambda ( b c ) (- b (* c d)))
                                    )
                                    (cdr a) (cdar m)
                                )
                            )
                        )
                        (cdr m)
                    )
                )
            )
        )
    )
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;; Quadratic Solution  -  Lee Mac
;; Args: a,b,c - coefficients of ax^2 + bx + c = 0

(defun quad ( a b c / d r )
    (if (<= 0 (setq d (- (* b b) (* 4.0 a c))))
        (progn
            (setq r (sqrt d))
            (list (/ (+ (- b) r) (* 2.0 a)) (/ (- (- b) r) (* 2.0 a)))
        )
    )
)