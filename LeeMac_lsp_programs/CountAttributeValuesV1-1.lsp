;;--------------------=={ Count Attribute Values }==--------------------;;
;;                                                                      ;;
;;  This program enables the user to count the number of occurrences    ;;
;;  of attribute values across all or specific attribute tags in a      ;;
;;  selection of attributed blocks.                                     ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'cav' (Count Attribute Values) at   ;;
;;  the AutoCAD command line, the user is prompted for a selection of   ;;
;;  attributed blocks whose attribute values are to be counted.         ;;
;;                                                                      ;;
;;  If  multiple attribute tags are found in the selection, the user    ;;
;;  will be presented with a dialog from which they may select the      ;;
;;  tags of attributes whose values are to be counted.                  ;;
;;                                                                      ;;
;;  The user is then prompted to specify an insertion point for the     ;;
;;  table displaying the results of the count, with values sorted       ;;
;;  ascending alphabetically.                                           ;;
;;                                                                      ;;
;;  Note that this program may only be run on CAD platforms which       ;;
;;  support table objects.                                              ;;
;;                                                                      ;;
;;  -----------------------------------                                 ;;
;;  Predefined Attribute Tags                                           ;;
;;  -----------------------------------                                 ;;
;;                                                                      ;;
;;  By default, the program will consider all attribute tags held by    ;;
;;  blocks in the selection, with the user prompted to select one or    ;;
;;  more tags whose values are to be counted if multiple attribute      ;;
;;  tags are encountered.                                               ;;
;;                                                                      ;;
;;  However, the user can optionally predefine the tag or set of tags   ;;
;;  to be counted (thereby avoiding the dialog prompt) by populating    ;;
;;  the tag list argument supplied to the main function below - this    ;;
;;  argument is empty by default.                                       ;;
;;                                                                      ;;
;;  The tag list argument is case-insensitive and also supports the     ;;
;;  use of wildcard operators.                                          ;;
;;                                                                      ;;
;;  The user also has the option to specify a title for the table and   ;;
;;  revise (or even remove) the table column headings, depending on     ;;
;;  the output desired.                                                 ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2011  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2011-02-24                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2024-11-03                                      ;;
;;                                                                      ;;
;;  - Program rewritten and updated to facilitate selection of          ;;
;;    one or more attribute tags whose values are to be counted.        ;;
;;----------------------------------------------------------------------;;

(defun c:cav ( )
    (cav:main
        ""                ;; Optional Table Title
       '("Value" "Count") ;; Optional Table Column Headings
       '( )               ;; Optional predefined tag list (may use wildcards)
    )
) 

;;----------------------------------------------------------------------;;

(defun cav:main ( ttl hdg tgs / att atx foo idx ins lst psp rtn sel spc tgl )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (setq tgs (mapcar 'strcase tgs)
          psp (= 1 (getvar 'cvport))
          spc (vlax-get-property (LM:acdoc) (if psp 'paperspace 'modelspace))
    )
    (cond
        (   (not (vlax-method-applicable-p spc 'addtable))
            (princ "\nTables are not supported on this CAD platform.")
        )
        (   (not
                (setq sel
                    (ssget
                        (list
                           '(000 . "INSERT")
                           '(066 . 1)
                            (if psp
                                (cons 410 (getvar 'ctab))
                               '(410 . "Model")
                            )
                        )
                    )
                )
            )
        )
        (   (progn
                (setq foo
                    (if tgs
                        (lambda ( tag val )
                            (if (vl-some '(lambda ( x ) (wcmatch tag x)) tgs)
                                (setq lst (cons (cons tag val) lst))
                            )
                        )
                        (lambda ( tag val )
                            (if (not (member tag tgl))
                                (setq tgl (cons tag tgl))
                            )
                            (setq lst (cons (cons tag val) lst))
                        )
                    )
                )
                (repeat (setq idx (sslength sel))
                    (setq idx (1- idx)
                          att (entnext (ssname sel idx))
                          atx (entget att)
                    )
                    (while (= "ATTRIB" (cdr (assoc 0 atx)))
                        (foo (strcase (cdr (assoc 2 atx))) (cdr (assoc 1 atx)))
                        (setq att (entnext att)
                              atx (entget  att)
                        )
                    )
                )
                (null lst)
            )
            (princ "\nNo attributes matching the supplied criteria were found.")
        )
        (   (not
                (or tgs
                    (null (cdr tgl))
                    (and (setq tgs (LM:filtlistbox "Select Attributes to Count" (acad_strlsort tgl) t))
                         (setq lst (vl-remove-if-not '(lambda ( x ) (member (car x) tgs)) lst))
                    )
                )
            )
            (princ "\n*Cancel*")
        )
        (   (progn
                (foreach itm lst
                    (setq rtn (LM:assoc++ (cdr itm) rtn))
                )
                (setq rtn
                    (vl-sort (mapcar '(lambda ( x ) (list (car x) (itoa (cdr x)))) rtn)
                       '(lambda ( a b )
                            (< (strcase (car a)) (strcase (car b)))
                        )
                    )
                )
                (if (cdr hdg)
                    (setq rtn (cons (list (car hdg) (cadr hdg)) rtn))
                )
                (setq ins (getpoint "\nSpecify point for table: "))
            )
            (LM:startundo (LM:acdoc))
            (LM:addtable spc (trans ins 1 0) (if (and ttl (/= "" ttl)) ttl) rtn nil)
            (LM:endundo   (LM:acdoc))
        )
    )
    (princ)
)

;; Assoc++  -  Lee Mac
;; Increments the value of a key in an association list if present, else adds key to the list.
;; key - [any] Key of an element in the list
;; lst - [lst] Association list (may be nil)

(defun LM:assoc++ ( key lst / itm )
    (if (setq itm (assoc key lst))
        (subst (cons key (1+ (cdr itm))) itm lst)
        (cons  (cons key 1) lst)
    )
)

;; Filtered List Box  -  Lee Mac
;; Displays a list box interface from which the user may select one or more items.
;; Includes an edit box filter to enable the user to filter the displayed list of items.
;; msg - [str] List box dialog title
;; lst - [lst] List of strings to display in the list box
;; mtp - [bol] T=Allow multiple items; nil=Single item selection
;; Returns: [lst] List of selected items, else nil

(defun LM:filtlistbox ( msg lst mtp / _addlist dch dcl des rtn sel tmp )

    (defun _addlist ( key lst )
        (start_list key)
        (foreach x lst (add_list x))
        (end_list)
        lst
    )

    (if
        (and
            (setq dcl (vl-filename-mktemp nil nil ".dcl"))
            (setq des (open dcl "w"))
            (write-line
                (strcat
                    "filtlistbox : dialog { label = \"" msg "\"; spacer;"
                    ": list_box { key = \"lst\"; width = 50; fixed_width = true; height = 15; fixed_height = true; allow_accept = true; "
                    "multiple_select = " (if mtp "true" "false") "; }"
                    ": edit_box { key = \"flt\"; width = 50; fixed_width = true; label = \"Filter:\"; }"
                    "spacer; ok_cancel; }"
                )
                des
            )
            (not (close des))
            (< 0 (setq dch (load_dialog dcl)))
            (new_dialog "filtlistbox" dch)
        )
        (progn
            (_addlist "lst" (setq tmp lst))
            (set_tile "lst" (setq rtn "0"))
            (set_tile "flt" "*")
            (action_tile "lst" "(setq rtn $value)")
            (action_tile "flt"
                (vl-prin1-to-string
                   '(progn
                        (setq flt (strcat "*" (strcase $value) "*")
                              sel (mapcar '(lambda ( n ) (nth n tmp)) (read (strcat "(" rtn ")")))
                        )
                        (_addlist "lst" (setq tmp (vl-remove-if-not '(lambda ( x ) (wcmatch (strcase x) flt)) lst)))
                        (set_tile "lst"
                            (setq rtn
                                (vl-string-trim "()"
                                    (vl-princ-to-string
                                        (cond
                                            (   (vl-sort (vl-remove nil (mapcar '(lambda ( x ) (vl-position x tmp)) sel)) '<))
                                            (  '(0)   )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (setq rtn
                (if (= 1 (start_dialog))
                    (mapcar '(lambda ( x ) (nth x tmp)) (read (strcat "(" rtn ")")))
                )
            )
        )
    )
    (if (< 0 dch)
        (setq dch (unload_dialog dch))
    )
    (if (and (= 'str (type dcl)) (findfile dcl))
        (vl-file-delete dcl)
    )
    rtn
)

;; Add Table  -  Lee Mac
;; Generates a table at the given point, populated with the given data and optional title.
;; spc - [vla] VLA Block object
;; ins - [lst] WCS insertion point for table
;; ttl - [str] [Optional] Table title
;; lst - [lst] Matrix list of table cell data
;; eqc - [bol] If T, columns are of equal width
;; Returns: [vla] VLA Table Object

(defun LM:addtable ( spc ins ttl lst eqc / dif hgt i j obj stn sty wid )
    (setq sty
        (vlax-ename->vla-object
            (cdr
                (assoc -1
                    (dictsearch (cdr (assoc -1 (dictsearch (namedobjdict) "acad_tablestyle")))
                        (getvar 'ctablestyle)
                    )
                )
            )
        )
    )
    (setq hgt (vla-gettextheight sty acdatarow))
    (if (LM:annotative-p (setq stn (vla-gettextstyle sty acdatarow)))
        (setq hgt (/ hgt (cond ((getvar 'cannoscalevalue)) (1.0))))
    )
    (setq wid
        (mapcar
           '(lambda ( col )
                (apply 'max (mapcar '(lambda ( str ) (LM:addtable:textwidth str hgt stn)) col))
            )
            (apply 'mapcar (cons 'list lst))
        )
    )
    (if (and  ttl (< 0.0 (setq dif (/ (- (LM:addtable:textwidth ttl hgt stn) (apply '+ wid)) (length wid)))))
        (setq wid (mapcar '(lambda ( x ) (+ x dif)) wid))
    )
    (setq obj
        (vla-addtable spc
            (vlax-3D-point ins)
            (1+ (length lst))
            (length (car lst))
            (* 2.0 hgt)
            (if eqc
                (apply 'max wid)
                (/ (apply '+ wid) (float (length (car lst))))
            )
        )
    )
    (vla-put-regeneratetablesuppressed obj :vlax-true)
    (vla-put-stylename obj (getvar 'ctablestyle))
    (setq i -1)
    (if (null eqc)
        (foreach col wid
            (vla-setcolumnwidth obj (setq i (1+ i)) col)
        )
    )
    (if ttl
        (progn
            (vla-settext obj 0 0 ttl)
            (setq i 1)
        )
        (progn
            (vla-deleterows obj 0 1)
            (setq i 0)
        )
    )
    (foreach row lst
        (setq j 0)
        (foreach val row
            (vla-settext obj i j val)
            (setq j (1+ j))
        )
        (setq i (1+ i))
    )
    (vla-put-regeneratetablesuppressed obj :vlax-false)
    obj
)

(defun LM:addtable:textwidth ( str hgt sty / box obj tmp )
    (if
        (and (wcmatch str "*%<*>%*")
            (setq tmp
                (entmakex
                    (list
                       '(00 . "TEXT")
                       '(10 0.0 0.0 0.0)
                        (cons 01 str)
                        (cons 40 hgt)
                        (cons 07 sty)
                    )
                )
            )
        )
        (progn
            (setq obj (vlax-ename->vla-object tmp))
            (vla-put-textstring obj "")
            (vla-put-textstring obj str)
            (setq str (vla-get-textstring obj))
            (entdel tmp)
        )
    )
    (if
        (setq box
            (textbox
                (list
                    (cons 01 str)
                    (cons 40 hgt)
                    (cons 07 sty)
                )
            )
        )
        (+ (* 2.5 hgt) (- (caadr box) (caar box)))
        0.0
    )
)

;; Annotative-p  -  Lee Mac
;; Returns T if the given Textstyle is annotative

(defun LM:annotative-p ( sty )
    (and (setq sty (tblobjname "style" sty))
         (setq sty (cadr (assoc -3 (entget sty '("acadannotative")))))
         (= 1 (cdr (assoc 1070 (reverse sty))))
    )
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: CountAttributeValues.lsp | Version 1.1 | \\U+00A9 Lee Mac "
        ((lambda ( y ) (if (= y (menucmd "m=$(edtime,0,yyyy)")) y (strcat y "-" (menucmd "m=$(edtime,0,yyyy)")))) "2011")
        " www.lee-mac.com ::"
        "\n:: Type \"cav\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;