;;------------------------------=={ Star }==----------------------------;;
;;                                                                      ;;
;;  This program allows the user to create a customised star shape,     ;;
;;  constructed dynamically using a polyline object.                    ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'star' at the AutoCAD command-line  ;;
;;  the user is prompted to specify a center for the star shape.        ;;
;;                                                                      ;;
;;  Following a valid response to this prompt, the program will         ;;
;;  generate a real-time preview of the star, with the size & rotation  ;;
;;  updated dynamically based on the position of cursor.                ;;
;;                                                                      ;;
;;  During this prompt, the user may also control the number of points  ;;
;;  forming the star shape using the +/- keys, and the inside radius    ;;
;;  or length of each point or 'leg' using the </> keys.                ;;
;;                                                                      ;;
;;  The user may exit the program at any time during this prompt by     ;;
;;  pressing 'Enter' or right-clicking.                                 ;;
;;                                                                      ;;
;;  When satisfied with the star shape displayed, the user may          ;;
;;  left-click or manually enter an absolute or relative point for the  ;;
;;  star vertex. The program will then construct the star shape using   ;;
;;  an LWPolyline object.                                               ;;
;;                                                                      ;;
;;  This program utilises my GrSnap utility to enable full Object Snap  ;;
;;  functionality during the dynamic prompt. Full documentation for     ;;
;;  this function may be found at: http://www.lee-mac.com/grsnap.html   ;;
;;                                                                      ;;
;;  Finally, the program is designed to perform successfully under all  ;;
;;  UCS and View settings, and in any construction plane.               ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2013-12-27                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;

(defun c:star ( / *error* points vectors ang gr1 gr2 lst mat mod msg num ocs osf osm pt1 pt2 rad str tmp )
 
    (defun *error* ( msg )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (redraw) (princ)
    )
  
    (defun points ( num mod / ang inc lst rad )
        (setq inc (/ (+ pi pi) num)
              ang 0.0
              rad 1.0
        )
        (repeat num
            (setq lst (cons (polar '(0.0 0.0) ang rad) lst)
                  ang (+ ang inc)
                  rad (- mod rad)
            )
        )
        lst
    )

    (defun vectors ( lst )
        (apply 'append (mapcar 'list lst (cons (last lst) lst)))
    )
    (if (setq pt1 (getpoint "\nPick center: "))
        (progn
            (setq num 10
                  mod 1.3
                  mat (trp (mapcar '(lambda ( x ) (trans x 1 2 t)) '((1.0 0.0) (0.0 1.0) (0.0 0.0))))
                  lst (cons -1 (vectors (points num mod)))
                  pt2 (mapcar 'list (trans pt1 1 2))
                  osf (LM:grsnap:snapfunction)
                  osm (getvar 'osmode)
                  msg "\nPick point for radius & rotation [+/-/</>] <exit>: "
                  str ""
            )
            (princ msg)
            (while
                (progn
                    (setq gr1 (grread t 15 0)
                          gr2 (cadr gr1)
                          gr1 (car  gr1)
                    )
                    (cond
                        (   (or (= 5 gr1) (= 3 gr1))
                            (redraw)
                            (osf gr2 osm)
                            (if (< 0 (setq rad (distance pt1 gr2)))
                                (progn
                                    (setq ang (angle pt1 gr2))
                                    (grvecs lst
                                        (append
                                            (mapcar 'append
                                                (mxm mat
                                                    (list
                                                        (list (* rad (cos ang)) (* rad (sin (- ang))) 0.0)
                                                        (list (* rad (sin ang)) (* rad (cos ang))     0.0)
                                                       '(0.0 0.0 1.0)
                                                    )
                                                )
                                                pt2
                                            )
                                           '((0.0 0.0 0.0 1.0))
                                        )
                                    )
                                )
                            )
                            (= 5 gr1)
                        )
                        (   (= 2 gr1)
                            (cond
                                (   (= 6 gr2)
                                    (if (zerop (logand 16384 (setq osm (setvar 'osmode (boole 6 16384 (getvar 'osmode))))))
                                        (princ "\n<Osnap on>")
                                        (princ "\n<Osnap off>")
                                    )
                                    (princ msg)
                                )
                                (   (= 8 gr2)
                                    (if (< 0 (strlen str))
                                        (progn
                                            (princ "\010\040\010")
                                            (setq str (substr str 1 (1- (strlen str))))
                                        )
                                    )
                                    t
                                )
                                (   (= 45 gr2)
                                    (if (< 7 num)
                                        (setq num (- num 2)
                                              lst (cons -1 (vectors (points num mod)))
                                        )
                                        (princ "\nMinimum reached.")
                                    )
                                )
                                (   (member gr2 '(43 61))
                                    (setq num (+ 2 num)
                                          lst (cons -1 (vectors (points num mod)))
                                    )
                                )
                                (   (= 60 gr2) ;(member gr2 '(91 123))
                                    (if (< 1.0 mod)
                                        (setq mod (- mod 0.1)
                                              lst (cons -1 (vectors (points num mod)))
                                        )
                                        (princ "\nMinimum reached.")
                                    )
                                )
                                (   (= 62 gr2) ;(member gr2 '(93 125))
                                    (setq mod (+ mod 0.1)
                                          lst (cons -1 (vectors (points num mod)))
                                    )
                                )
                                (   (< 32 gr2 127)
                                    (setq str (strcat str (princ (chr gr2))))
                                )
                                (   (member gr2 '(13 32))
                                    (cond
                                        (   (= "" str) nil)
                                        (   (setq gr2 (LM:grsnap:parsepoint pt1 str))
                                            (setq osm 16384)
                                            nil
                                        )
                                        (   (setq tmp (LM:grsnap:snapmode str))
                                            (setq osm tmp
                                                  str ""
                                            )
                                        )
                                        (   (setq str "")
                                            (princ (strcat "\n2D / 3D Point Required." msg))
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (if (listp gr2)
                (progn
                    (setq gr2 (osf gr2 osm)
                          ang (angle pt1 gr2)
                          rad (distance pt1 gr2)
                          ocs (trans '(0.0 0.0 1.0) 1 0 t)
                          mat (list (list (* rad (cos ang)) (* rad (sin (- ang))))
                                    (list (* rad (sin ang)) (* rad (cos ang)))
                              )
                    )
                    (if (< 0.0 rad)
                        (entmake
                            (append
                                (list
                                   '(000 . "LWPOLYLINE")
                                   '(100 . "AcDbEntity")
                                   '(100 . "AcDbPolyline")
                                    (cons 90 num)
                                   '(070 . 1)
                                    (cons 38 (caddr (trans '(0.0 0.0) 1 0)))
                                )
                                (mapcar
                                    (function
                                        (lambda ( x )
                                            (cons 10 (trans (mapcar '+ (mxv mat x) pt1) 1 ocs))
                                        )
                                    )
                                    (points num mod)
                                )
                                (list (cons 210 ocs))
                            )
                        )
                    )
                )
            )
            (redraw)
        )
    )
    (princ)
)
 
;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix
 
(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)
 
;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices
 
(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)
 
;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n
 
(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Object Snap for grread: Snap Function  -  Lee Mac
;; Returns: [fun] A function requiring two arguments:
;; p - [lst] UCS Point to be snapped
;; o - [int] Object Snap bit code
;; The returned function returns either the snapped point (displaying an appropriate snap symbol)
;; or the supplied point if the snap failed for the given Object Snap bit code.

(defun LM:grsnap:snapfunction ( )
    (eval
        (list 'lambda '( p o / q )
            (list 'if '(zerop (logand 16384 o))
                (list 'if
                   '(setq q
                        (cdar
                            (vl-sort
                                (vl-remove-if 'null
                                    (mapcar
                                        (function
                                            (lambda ( a / b )
                                                (if (and (= (car a) (logand (car a) o)) (setq b (osnap p (cdr a))))
                                                    (list (distance p b) b (car a))
                                                )
                                            )
                                        )
                                       '(
                                            (0001 . "_end")
                                            (0002 . "_mid")
                                            (0004 . "_cen")
                                            (0008 . "_nod")
                                            (0016 . "_qua")
                                            (0032 . "_int")
                                            (0064 . "_ins")
                                            (0128 . "_per")
                                            (0256 . "_tan")
                                            (0512 . "_nea")
                                            (2048 . "_app")
                                            (8192 . "_par")
                                        )
                                    )
                                )
                               '(lambda ( a b ) (< (car a) (car b)))
                            )
                        )
                    )
                    (list 'LM:grsnap:displaysnap '(car q)
                        (list 'cdr
                            (list 'assoc '(cadr q)
                                (list 'quote
                                    (LM:grsnap:snapsymbols
                                        (atoi (cond ((getenv "AutoSnapSize")) ("5")))
                                    )
                                )
                            )
                        )
                        (LM:OLE->ACI
                            (if (= 1 (getvar 'cvport))
                                (atoi (cond ((getenv "Layout AutoSnap Color")) ("117761")))
                                (atoi (cond ((getenv  "Model AutoSnap Color")) ("104193")))
                            )
                        )
                    )
                )
            )
           '(cond ((car q)) (p))
        )
    )
)

;; Object Snap for grread: Display Snap  -  Lee Mac
;; pnt - [lst] UCS point at which to display the symbol
;; lst - [lst] grvecs vector list
;; col - [int] ACI colour for displayed symbol
;; Returns nil

(defun LM:grsnap:displaysnap ( pnt lst col / scl )
    (setq scl (/ (getvar 'viewsize) (cadr (getvar 'screensize)))
          pnt (trans pnt 1 2)
    )
    (grvecs (cons col lst)
        (list
            (list scl 0.0 0.0 (car  pnt))
            (list 0.0 scl 0.0 (cadr pnt))
            (list 0.0 0.0 scl 0.0)
           '(0.0 0.0 0.0 1.0)
        )
    )
)

;; Object Snap for grread: Snap Symbols  -  Lee Mac
;; p - [int] Size of snap symbol in pixels
;; Returns: [lst] List of vector lists describing each Object Snap symbol

(defun LM:grsnap:snapsymbols ( p / -p -q -r a c i l q r )
    (setq -p (- p) q (1+  p)
          -q (- q) r (+ 2 p)
          -r (- r) i (/ pi 6.0)
           a 0.0
    )
    (repeat 12
        (setq l (cons (list (* r (cos a)) (* r (sin a))) l)
              a (- a i)
        )
    )
    (setq c (apply 'append (mapcar 'list (cons (last l) l) l)))
    (list
        (list 1
            (list -p -p) (list p -p) (list p -p) (list p p) (list p p) (list -p p) (list -p p) (list -p -p)
            (list -q -q) (list q -q) (list q -q) (list q q) (list q q) (list -q q) (list -q q) (list -q -q)
        )
        (list 2
            (list -r -q) (list 0  r) (list 0  r) (list r -q)
            (list -p -p) (list p -p) (list p -p) (list 0  p) (list 0  p) (list -p -p)
            (list -q -q) (list q -q) (list q -q) (list 0  q) (list 0  q) (list -q -q)
        )
        (cons 4 c)
        (vl-list* 8 (list -r -r) (list r r) (list r -r) (list -r r) c)
        (list 16
            (list p 0) (list 0 p) (list 0 p) (list -p 0) (list -p 0) (list 0 -p) (list 0 -p) (list p 0)
            (list q 0) (list 0 q) (list 0 q) (list -q 0) (list -q 0) (list 0 -q) (list 0 -q) (list q 0)
            (list r 0) (list 0 r) (list 0 r) (list -r 0) (list -r 0) (list 0 -r) (list 0 -r) (list r 0)
        )
        (list 32
            (list  r r) (list -r -r) (list  r q) (list -q -r) (list  q r) (list -r -q)
            (list -r r) (list  r -r) (list -q r) (list  r -q) (list -r q) (list  q -r)
        )
        (list 64
            '( 0  1) (list  0  p) (list  0  p) (list -p  p) (list -p  p) (list -p -1) (list -p -1) '( 0 -1)
            '( 0 -1) (list  0 -p) (list  0 -p) (list  p -p) (list  p -p) (list  p  1) (list  p  1) '( 0  1)
            '( 1  2) (list  1  q) (list  1  q) (list -q  q) (list -q  q) (list -q -2) (list -q -2) '(-1 -2)
            '(-1 -2) (list -1 -q) (list -1 -q) (list  q -q) (list  q -q) (list  q  2) (list  q  2) '( 1  2)
        )
        (list 128
            (list (1+ -p) 0) '(0 0) '(0 0) (list 0 (1+ -p))
            (list (1+ -p) 1) '(1 1) '(1 1) (list 1 (1+ -p))
            (list -p q) (list -p -p) (list -p -p) (list q -p)
            (list -q q) (list -q -q) (list -q -q) (list q -q)
        )
        (vl-list* 256 (list -r r)  (list r r) (list -r (1+ r)) (list r (1+ r)) c)
        (list 512
            (list -p -p) (list  p -p) (list -p  p) (list p p) (list -q -q) (list  q -q)
            (list  q -q) (list -q  q) (list -q  q) (list q q) (list  q  q) (list -q -q)
        )
        (list 2048
            (list   -p     -p) (list    p      p) (list   -p      p) (list    p     -p)
            (list (+ p 05) -p) (list (+ p 06) -p) (list (+ p 05) -q) (list (+ p 06) -q)
            (list (+ p 09) -p) (list (+ p 10) -p) (list (+ p 09) -q) (list (+ p 10) -q)
            (list (+ p 13) -p) (list (+ p 14) -p) (list (+ p 13) -q) (list (+ p 14) -q)
            (list -p -p) (list p -p) (list p -p) (list p p) (list p p) (list -p p) (list -p p) (list -p -p)
            (list -q -q) (list q -q) (list q -q) (list q q) (list q q) (list -q q) (list -q q) (list -q -q)
        )
        (list 8192 (list r 1) (list -r -q) (list r 0) (list -r -r) (list r q) (list -r -1) (list r r) (list -r 0))
    )
)

;; Object Snap for grread: Parse Point  -  Lee Mac
;; bpt - [lst] Basepoint for relative point input, e.g. @5,5
;; str - [str] String representing point input
;; Returns: [lst] Point represented by the given string, else nil

(defun LM:grsnap:parsepoint ( bpt str / str->lst lst )
 
    (defun str->lst ( str / pos )
        (if (setq pos (vl-string-position 44 str))
            (cons (substr str 1 pos) (str->lst (substr str (+ pos 2))))
            (list str)
        )
    )

    (if (wcmatch str "`@*")
        (setq str (substr str 2))
        (setq bpt '(0.0 0.0 0.0))
    )           

    (if
        (and
            (setq lst (mapcar 'distof (str->lst str)))
            (vl-every 'numberp lst)
            (< 1 (length lst) 4)
        )
        (mapcar '+ bpt lst)
    )
)

;; Object Snap for grread: Snap Mode  -  Lee Mac
;; str - [str] Object Snap modifier
;; Returns: [int] Object Snap bit code for the given modifier, else nil

(defun LM:grsnap:snapmode ( str )
    (vl-some
        (function
            (lambda ( x )
                (if (wcmatch (car x) (strcat (strcase str t) "*"))
                    (progn
                        (princ (cadr x)) (caddr x)
                    )
                )
            )
        )
       '(
            ("endpoint"      " of " 00001)
            ("midpoint"      " of " 00002)
            ("center"        " of " 00004)
            ("node"          " of " 00008)
            ("quadrant"      " of " 00016)
            ("intersection"  " of " 00032)
            ("insert"        " of " 00064)
            ("perpendicular" " to " 00128)
            ("tangent"       " to " 00256)
            ("nearest"       " to " 00512)
            ("appint"        " of " 02048)
            ("parallel"      " to " 08192)
            ("none"          ""     16384)
        )
    )
)

;; OLE -> ACI  -  Lee Mac
;; Args: c - [int] OLE Colour

(defun LM:OLE->ACI ( c )
    (apply 'LM:RGB->ACI (LM:OLE->RGB c))
)

;; OLE -> RGB  -  Lee Mac
;; Args: c - [int] OLE Colour

(defun LM:OLE->RGB ( c )
    (mapcar '(lambda ( x ) (lsh (lsh (fix c) x) -24)) '(24 16 8))
)

;; RGB -> ACI  -  Lee Mac
;; Args: r,g,b - [int] Red, Green, Blue values

(defun LM:RGB->ACI ( r g b / c o )
    (if (setq o (vla-getinterfaceobject (LM:acapp) (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2))))
        (progn
            (setq c (vl-catch-all-apply '(lambda ( ) (vla-setrgb o r g b) (vla-get-colorindex o))))
            (vlax-release-object o)
            (if (vl-catch-all-error-p c)
                (prompt (strcat "\nError: " (vl-catch-all-error-message c)))
                c
            )
        )
    )
)

;; Application Object  -  Lee Mac
;; Returns the VLA Application Object

(defun LM:acapp nil
    (eval (list 'defun 'LM:acapp 'nil (vlax-get-acad-object)))
    (LM:acapp)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: Star.lsp | Version 1.2 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"star\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;