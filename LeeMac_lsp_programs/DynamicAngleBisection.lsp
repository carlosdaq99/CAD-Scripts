;;--------------=={ Dynamic Angle Bisection }==---------------;;
;;                                                            ;;
;;  Allows the user to dynamically bisect the angle formed by ;;
;;  a selected line and the cursor, using a 'traditional'     ;;
;;  ruler & compass method.                                   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:bisect

    ( / _SelectIf _GroupByNum _inters _Circle _Line c1 c2 c3 cp en ep gr ip le li o1 o2 p1 p2 rd )

    (defun _SelectIf ( msg pred )
        (
            (lambda ( f / e )
                (while
                    (progn (setvar 'ERRNO 0) (setq e (car (entsel msg)))
                        (cond
                            (   (= 7 (getvar 'ERRNO))
                                (princ "\nMissed, try again.")
                            )
                            (   (eq 'ENAME (type e))
                                (if (and f (null (f e)))
                                    (princ "\nInvalid Object.")
                                )
                            )
                        )
                    )
                )
                e
            )
            (eval pred)
        )
    )
    
    (defun _GroupByNum ( l n / r)
        (if l
            (cons
                (reverse (repeat n (setq r (cons (car l) r) l (cdr l)) r))
                (_GroupByNum l n)
            )
        )
    )
  
    (defun _inters ( obj1 obj2 )
        (_GroupByNum (vlax-invoke obj1 'intersectwith obj2 acExtendNone) 3)
    )

    (defun _Circle ( c r )
        (entmakex (list '(0 . "CIRCLE") (cons 10 c) (cons 40 r) '(62 . 252)))
    )

    (defun _Line ( p q )
        (entmakex (list '(0 . "LINE") (cons 10 p) (cons 11 q) '(62 . 134)))
    )

    (if (setq en (_Selectif "\nSelect Line: " '(lambda ( x ) (eq "LINE" (cdr (assoc 0 (entget x)))))))
        (progn
            (setq o1 (vlax-ename->vla-object en)
                  o2 (vla-copy o1)
                  ip (vlax-get o1 'StartPoint)
                  le (vla-get-length o1)
                  rd (* 0.25 le)
                  c1 (vlax-ename->vla-object (_Circle ip rd))
                  p1 (polar ip (angle ip (vlax-get o1 'EndPoint)) rd)
                  c2 (vlax-ename->vla-object (_Circle p1 rd))
                  c3 (vla-copy c2)
                  li (entget (_Line ip p1))
            )
            (while (= 5 (car (setq gr (grread 't 13 0))))
                (setq cp (cadr gr)
                      p2 (polar ip (angle ip cp) rd)
                )
                (vlax-put-property c3 'Center   (vlax-3D-point p2))
                (vlax-put-property o2 'EndPoint (vlax-3D-point (polar ip (angle ip cp) le)))

                (if (setq ep (car (vl-remove-if (function (lambda ( x ) (equal x ip 1e-8))) (_inters c2 c3))))
                    (entmod
                        (subst
                            (cons  11 (polar ip (angle ip ep) le))
                            (assoc 11 li)
                            li
                        )
                    )
                )
            )
        )
    )
    (princ)
)
(vl-load-com) (princ)