;;-----------------------------=={ XLine }==----------------------------;;
;;                                                                      ;;
;;  This program will create XLINE objects on a predetermined layer     ;;
;;  (created automatically if not present in the active drawing), and   ;;
;;  offers custom commands to directly create XLINEs of a given type.   ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2025  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2025-01-18                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;

;;----------------------------------------------------------------------;;
;;                          Program Parameters                          ;;
;;----------------------------------------------------------------------;;

(setq x:layerprops
   '(
        (000 . "LAYER")
        (100 . "AcDbSymbolTableRecord")
        (100 . "AcDbLayerTableRecord")
        (002 . "XLINE")      ;; Layer Name
        (070 .  0)           ;; Layer Status     (bit-coded)
        (006 . "Continuous") ;; Layer Linetype   (must be loaded)
        (062 .  96)          ;; Layer Colour     (1-255)
        (290 .  0)           ;; Non-Plot Flag    (0=Plot, 1=NoPlot)
        (370 . -3)           ;; Layer Lineweight (-3=Default)
    )
)

(setq x:params
   '(
        ("Point"      "Horizontal Vertical Angle Exit"       "\nPick first point for xline [Horizontal/Vertical/Angle/Exit] <Exit>: "           nil)
        ("Horizontal" "Vertical Point Angle Exit"            "\nPick point for horizontal xline [Vertical/Point/Angle/Exit] <Exit>: " (1.0 0.0 0.0))
        ("Vertical"   "Horizontal Point Angle Exit"          "\nPick point for vertical xline [Horizontal/Point/Angle/Exit] <Exit>: " (0.0 1.0 0.0))
        ("Angle"      "Horizontal Vertical Point Angle Exit" "\nPick point for angled xline [Horizontal/Vertical/Point/Angle/Exit] <Exit>: ")
    )
)

;;----------------------------------------------------------------------;;

(defun c:xx ( )
    (apply 'x:xline (cons (x:getlayer x:layerprops) (cdr (assoc "Point" x:params))))
)

;;----------------------------------------------------------------------;;

(defun c:xh ( )
    (apply 'x:xline (cons (x:getlayer x:layerprops) (cdr (assoc "Horizontal" x:params))))
)

;;----------------------------------------------------------------------;;

(defun c:xv ( )
    (apply 'x:xline (cons (x:getlayer x:layerprops) (cdr (assoc "Vertical" x:params))))
)

;;----------------------------------------------------------------------;;

(defun c:xa ( / ang )
    (if (setq ang (getorient "\nSpecify xline angle: "))
        (apply 'x:xline
            (append
                (list (x:getlayer x:layerprops))
                (cdr (assoc "Angle" x:params))
                (list (list (cos ang) (sin ang) 0.0))
            )
        )
        (princ)
    )
)

;;----------------------------------------------------------------------;;

(defun x:xline ( lay ini msg vec / ang pt1 pt2 )
    (while (progn (initget ini) (and (setq pt1 (getpoint msg)) (/= "Exit" pnt)))
        (cond
            (   (= "Angle" pt1)
                (if (setq ang (getorient "\nSpecify xline angle <back>: "))
                    (setq vec (list (cos ang) (sin ang) 0.0))
                    (setq vec nil pt1 "Point")
                )
                (mapcar 'set '(ini msg) (cdr (assoc pt1 x:params)))
            )       
            (   (= 'str (type pt1))
                (mapcar 'set '(ini msg vec) (cdr (assoc pt1 x:params)))
            )
            (   vec 
                (x:xatvec pt1 vec lay)
            )
            (   (setq pt2 (getpoint "\nPick second point: " pt1))
                (x:xatvec pt1 (mapcar '- pt2 pt1) lay)
            )
        )
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(defun x:xatvec ( bpt vec lay )
    (entmake
        (list
           '(000 . "XLINE")
           '(100 . "AcDbEntity")
           '(100 . "AcDbXline")
            (cons 008 lay)
            (cons 010 (trans bpt 1 0))
            (cons 011 (trans vec 1 0 t))
        )
    )
)

;;----------------------------------------------------------------------;;

(defun x:getlayer ( lst / lay )
    (if (and (setq lay (cdr (assoc 2 lst)))
             (or (tblsearch "layer" lay) (entmake lst))
        )
        lay
        (getvar 'clayer)
    )
)

;;----------------------------------------------------------------------;;

(princ
    (strcat
        "\n:: XLine.lsp | Version 1.0 | \\U+00A9 Lee Mac "
        ((lambda ( y ) (if (= y (menucmd "m=$(edtime,0,yyyy)")) y (strcat y "-" (menucmd "m=$(edtime,0,yyyy)")))) "2025")
        " www.lee-mac.com ::"
        "\n:: \"xx\" - 2-Point | \"xh\" - Horizontal | \"xv\" - Vertical | \"xa\" - Angle ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;