;;------------------------=={ Tip of the Day }==------------------------;;
;;                                                                      ;;
;;  This novelty program allows you to display a randomly chosen        ;;
;;  custom 'Tip of the Day' message in an on-screen dialog once a day   ;;
;;  upon opening AutoCAD.                                               ;;
;;                                                                      ;;
;;  The tips are sourced from a user-compiled text file placed in an    ;;
;;  AutoCAD Support File Search Path; such file must be formatted in    ;;
;;  the following way:                                                  ;;
;;                                                                      ;;
;;      Tip 1 Line 1                                                    ;;
;;      Tip 1 Line 2                                                    ;;
;;      ...                                                             ;;
;;      Tip 1 Line n                                                    ;;
;;      ***                                                             ;;
;;      Tip 2 Line 1                                                    ;;
;;      Tip 2 Line 2                                                    ;;
;;      ...                                                             ;;
;;      Tip 2 Line n                                                    ;;
;;                                                                      ;;
;;  In which each tip is separated by a line containing three           ;;
;;  asterisks. The messages may be several lines long and the width     ;;
;;  of the message displayed in the dialog may be controlled directly   ;;
;;  by the length of each line.                                         ;;
;;                                                                      ;;
;;  An example of such a text file may be downloaded from here:         ;;
;;  http://lee-mac.com/files/Tips.txt                                   ;;
;;                                                                      ;;
;;        (Hopefully you can come up with more imaginative tips!)       ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2011-01-09                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2014-02-02                                      ;;
;;                                                                      ;;
;;  - Program completely rewritten.                                     ;;
;;  - Tip dialog is now evaluated from the s::startup function.         ;;
;;  - Tip dialog definition is now written on-the-fly.                  ;;
;;  - Added parameters to allow user to control size of Tip dialog.     ;;
;;  - Implemented code to ensure that the same tip is never displayed   ;;
;;    twice consecutively.                                              ;;
;;----------------------------------------------------------------------;;

(defun LM:tip ( / *error* dch dcl des hgt idx len lst msg src tmp wid )

    (defun *error* ( msg )
        (if (= 'file (type des))
            (close des)
        )
        (if (< 0 dch)
            (unload_dialog dch)
        )
        (if (and (= 'str (type dcl)) (findfile dcl))
            (vl-file-delete dcl)
        )
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    (setq

;;----------------------------------------------------------------------;;
;;                          Program Parameters                          ;;
;;----------------------------------------------------------------------;;

        src "tips.txt" ;; Tip data file
        msg "Tip of the Day!" ;; Dialog title
        wid 75 ;; Dialog width
        hgt 25 ;; Dialog height

;;----------------------------------------------------------------------;;

    )
    (cond
        (   (/= 'str (type src))
            (princ "\nTip data file parameter must be a string.")
        )
        (   (not (and (numberp wid) (< 0 wid)))
            (princ "\nDialog width parameter invalid.")
        )
        (   (not (and (numberp hgt) (< 0 hgt)))
            (princ "\nDialog height parameter invalid.")
        )
        (   (not (setq lst (LM:tip:gettips src))))
        (   (not
                (and
                    (setq dcl (vl-filename-mktemp "tip" nil ".dcl"))
                    (setq des (open dcl "w"))
                    (write-line
                        (strcat
                            "tip : dialog { key = \"dcl\"; spacer; : list_box { key = \"lst\";"
                            "width = " (LM:num->str wid) "; height = " (LM:num->str hgt) "; "
                            "fixed_width = true; fixed_height = true; is_enabled = false; } spacer; ok_only; }"
                        )
                        des
                    )
                    (not (setq des (close des)))
                    (< 0 (setq dch (load_dialog dcl)))
                    (new_dialog "tip" dch)
                )
            )
            (princ "\nUnable to write & load Tip dialog.")
        )
        (   t
            (if
                (and
                    (< 0 (setq len (1- (length lst))))
                    (setq tmp (getenv "LMac\\tipoftheday-last"))
                    (setq tmp (atoi tmp))
                )
                (while (= tmp (setq idx (LM:randrange 0 len))))
                (setq idx (LM:randrange 0 len))
            )
            (setenv "LMac\\tipoftheday-last" (itoa idx))
            (set_tile "dcl" msg)

            (start_list "lst")
            (foreach lin (nth idx lst) (add_list lin))
            (end_list)
         
            (start_dialog)
        )
    )
    (*error* nil)
    (princ)
)

(defun LM:tip:gettips ( txt / des fnm lin lst tmp )
    (cond
        (   (null (setq fnm (findfile txt)))
            (prompt (strcat "\n\"" txt "\" not found."))
        )
        (   (null (setq des (open fnm "r")))
            (prompt (strcat "\nUnable to open \"" fnm "\" for reading."))
        )
        (   (progn
                (while (setq lin (read-line des))
                    (if (= "***" lin)
                        (if tmp
                            (setq lst (cons (reverse tmp) lst)
                                  tmp nil
                            )
                        )
                        (setq tmp (cons lin tmp))
                    )
                )
                (setq des (close des))
                (if tmp (setq lst (cons (reverse tmp) lst)))
                (null lst)
            )
            (prompt (strcat "\n\"" fnm "\" is empty or contains invalid data."))
        )
        (   (reverse lst))
    )
)

;; Number to String  -  Lee Mac
;; Converts a supplied numerical argument to a string

(defun LM:num->str ( x / dim rtn )
    (if (= 'int (type x))
        (itoa x)
        (progn
            (setq dim (getvar 'dimzin))
            (setvar 'dimzin 8)
            (setq rtn (vl-catch-all-apply 'rtos (list x 2 15)))
            (setvar 'dimzin dim)
            (if (not (vl-catch-all-error-p rtn)) rtn)
        )
    )
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
;; Returns a pseudo-random integral number in a given range (inclusive)

(defun LM:randrange ( a b )
    (fix (+ a (* (LM:rand) (- b a -1))))
)

;;----------------------------------------------------------------------;;

(   (lambda ( / tmp )
        (if
            (not
                (and
                    (setq tmp (getenv "LMac\\tipoftheday-date"))
                    (= (fix (getvar 'date)) (atoi tmp))
                )
            )
            (progn
                (setenv "LMac\\tipoftheday-date" (itoa (fix (getvar 'date))))
                (if (= 'list (type s::startup))
                    (if (not (member '(LM:tip) s::startup))
                        (setq s::startup (append s::startup '((LM:tip))))
                    )
                    (defun-q s::startup nil (LM:tip))
                )
            )
        )
        (princ)
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;