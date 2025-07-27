;;-------------------=={ Lottery Numbers }==------------------;;
;;                                                            ;;
;;  Generates a sequence of random numbers in the form of     ;;
;;  lottery balls in the active drawing.                      ;;
;;                                                            ;;
;;                      Fingers crossed!                      ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2012 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:lotto ( / *error* app bco cir clr col hat hgt ins lst num qty rng spc txt )
    
    (defun *error* ( m )
        (if (and (= 'vla-object (type col)) (not (vlax-object-released-p col)))
            (vlax-release-object col)
        )
        (princ (strcat "\nError: " m))
        (princ)
    )

    (setq

;;------------------------------------------------------------;;
;;                        Adjustments                         ;;
;;------------------------------------------------------------;;

        qty 6       ;; Number of Balls
        rng '(1 49) ;; Number Range
        clr         ;; Ball Colours
       '(
            (10 (236 014 014) (050 000 000)) ;; Numbers less than 10
            (20 (050 236 014) (000 050 000)) ;; Numbers less than 20
            (30 (236 236 014) (050 050 000)) ;; Numbers less than 30
            (40 (050 014 236) (000 000 050)) ;; Numbers less than 40
            (50 (236 014 236) (050 000 050)) ;; Numbers less than 50
        )

;;------------------------------------------------------------;;
        
    )
    (cond
        (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" (getvar 'clayer))))))
            (princ "\nCurrent layer locked.")
        )
        (   t
            (setq ins (getvar 'viewctr)
                  hgt (getvar 'textsize)
                  app (vlax-get-acad-object)
                  spc (vlax-get-property (vla-get-activedocument app) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                  col (vla-getinterfaceobject app (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2)))
            )
            (while (< (length lst) qty)
                (if (not (member (setq num (apply 'LM:randrange rng)) lst))
                    (setq lst (cons num lst))
                )
            )
            (foreach num (vl-sort lst '<)
                (setq cir (vlax-invoke spc 'addcircle ins (* 1.2 hgt))
                      hat (vlax-invoke spc 'addhatch acpredefinedgradient "INVSPHERICAL" :vlax-false acgradientobject)
                )
                (vlax-invoke hat 'appendouterloop (list cir))
                (vla-put-gradientcentered hat :vlax-true)
                (vl-some '(lambda ( x ) (if (< num (car x)) (setq bco x))) clr)
                (apply 'vla-setrgb (cons col (cadr  bco)))
                (vla-put-gradientcolor1 hat col)
                (apply 'vla-setrgb (cons col (caddr bco)))
                (vla-put-gradientcolor2 hat col)
                (vla-delete cir)
                (setq txt (vlax-invoke spc 'addtext (itoa num) ins hgt))
                (vla-put-color txt acwhite)
                (vla-put-alignment txt acalignmentmiddlecenter)
                (vla-put-textalignmentpoint txt (vlax-3D-point ins))
                (setq ins (cons (+ (car ins) (* 3 hgt)) (cdr ins)))
            )
            (vlax-invoke app 'zoomcenter
                (cons (- (car ins) (* 1.5 (1+ qty) hgt)) (cdr ins))
                (* (1+ qty) hgt)
            )
            (vlax-release-object col)
        )
    )
    (princ)
)

;; Rand  -  Lee Mac
;; PRNG implementing a linear congruential generator with
;; parameters derived from the book 'Numerical Recipes'

(defun LM:rand ( / a c m )
    (setq m   4294967296.0
          a   1664525.0
          c   1013904223.0
          $xn (rem (+ c (* a (cond ($xn) ((getvar 'date))))) m)
    )
    (/ $xn m)
)

;; Random in Range  -  Lee Mac
;; Returns a pseudo-random number in a given range (inclusive)

(defun LM:randrange ( a b )
    (fix (+ a (* (LM:rand) (- b a -1))))
)

;;------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: Lotto.lsp | Version 1.0 | © Lee Mac "
        (menucmd "m=$(edtime,$(getvar,DATE),YYYY)")
        " www.lee-mac.com ::"
        "\n:: Type \"Lotto\" to Invoke ::"
    )
)
(princ)

;;------------------------------------------------------------;;
;;                        End of File                         ;;
;;------------------------------------------------------------;;