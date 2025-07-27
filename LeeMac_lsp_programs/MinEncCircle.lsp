;; Minimum Enclosing Circle  -  Lee Mac
;; Implements the algorithm by Pr.Chrystal (1885) using the Convex Hull
;; to determine the Minimum Enclosing Circle of a point set.

(defun LM:MinEncCircle ( lst / _sub )

    (defun _sub ( p1 p2 l1 / a1 a2 l2 p3 p4 )
        (setq l2 (LM:RemoveWithFuzz (list p1 p2) l1 1e-8)
              p3 (car l2)
              a1 (LM:GetInsideAngle p1 p3 p2)
        )
        (foreach p4 (cdr l2)
            (if (< (setq a2 (LM:GetInsideAngle p1 p4 p2)) a1)
                (setq p3 p4 a1 a2)
            )
        )
        (cond
            (   (<= (/ pi 2.0) a1)
                (list (mid p1 p2) (/ (distance p1 p2) 2.0))
            )
            (   (vl-some
                    (function
                        (lambda ( a b c )
                            (if (< (/ pi 2.0) (LM:GetInsideAngle a b c)) (_sub a c l1))
                        )
                    )
                    (list p1 p1 p2) (list p2 p3 p1) (list p3 p2 p3)
                )
            )
            (   (LM:3PCircle p1 p2 p3)   )
        )
    )

    (
        (lambda ( lst )
            (cond
                (   (< (length lst) 2)
                    nil
                )
                (   (< (length lst) 3)
                    (list (apply 'mid lst) (/ (apply 'distance lst) 2.0))
                )
                (   (_sub (car lst) (cadr lst) lst)   )
            )
        )
        (LM:ConvexHull lst)
    )
)

;; Remove With Fuzz  -  Lee Mac
;; Removes items from a list which are equal to a supplied tolerance

(defun LM:RemoveWithFuzz ( l1 l2 fz )
    (vl-remove-if
        (function
            (lambda ( a )
                (vl-some
                    (function (lambda ( b ) (equal a b fz)))
                    l1
                )
            )
        )
        l2
    )
)

;; Get Inside Angle  -  Lee Mac
;; Returns the smaller angle subtended by three points with vertex at p2

(defun LM:GetInsideAngle ( p1 p2 p3 )
    (   (lambda ( a ) (min a (- (+ pi pi) a)))
        (rem (+ pi pi (- (angle p2 p1) (angle p2 p3))) (+ pi pi))
    )
)

;; 3-Point Circle  -  Lee Mac
;; Returns the Center and Radius of the Circle defined by
;; the supplied three points.

(defun LM:3PCircle ( p1 p2 p3 / cn m1 m2 )
    (setq m1 (mid p1 p2)
          m2 (mid p2 p3)
    )
    (list
        (setq cn
            (inters
                m1 (polar m1 (+ (angle p1 p2) (/ pi 2.)) 1.0)
                m2 (polar m2 (+ (angle p2 p3) (/ pi 2.)) 1.0)
                nil
            )
        )
        (distance cn p1)
    )
)

;; Midpoint - Lee Mac
;; Returns the midpoint of two points

(defun mid ( a b )
    (mapcar (function (lambda ( a b ) (/ (+ a b) 2.0))) a b)
)

;; Convex Hull  -  Lee Mac
;; Implements the Graham Scan Algorithm to determine the
;; Convex Hull of a list of points.

(defun LM:ConvexHull ( lst / hul p0 )
    (cond
        (   (< (length lst) 4)
            lst
        )
        (   t
            (setq p0 (car lst))
            (foreach p1 (cdr lst)
                (if (or (< (cadr p1) (cadr p0))
                        (and (equal (cadr p1) (cadr p0) 1e-8) (< (car p1) (car p0)))
                    )
                    (setq p0 p1)
                )
            )
            (setq lst
                (vl-sort lst
                    (function
                        (lambda ( a b / c d )
                            (if (equal (setq c (angle p0 a)) (setq d (angle p0 b)) 1e-8)
                                (< (distance p0 a) (distance p0 b))
                                (< c d)
                            )
                        )
                    )
                )
            )
            (setq hul (list (caddr lst) (cadr lst) (car lst)))
            (foreach pt (cdddr lst)
                (setq hul (cons pt hul))
                (while (and (caddr hul) (LM:Clockwise-p (caddr hul) (cadr hul) pt))
                    (setq hul (cons pt (cddr hul)))
                )
            )
            hul
        )
    )
)

;; Clockwise-p  -  Lee Mac
;; Returns T if p1,p2,p3 are clockwise oriented or collinear
                 
(defun LM:Clockwise-p ( p1 p2 p3 )
    (<  (-  (* (- (car  p2) (car  p1)) (- (cadr p3) (cadr p1)))
            (* (- (cadr p2) (cadr p1)) (- (car  p3) (car  p1)))
        )
        1e-8
    )
)