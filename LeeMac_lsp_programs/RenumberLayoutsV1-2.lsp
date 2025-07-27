;;------------------------=={ Renumber Layouts }==----------------------;;
;;                                                                      ;;
;;  This program enables the user to automatically sequentially         ;;
;;  renumber all or specific Paperspace layouts, with an optional       ;;
;;  prefix and/or suffix.                                               ;;
;;                                                                      ;;
;;  The layouts are renumbered in the order in which they appear in     ;;
;;  the active drawing, with a configurable parameter defining the      ;;
;;  number of digits constituting the numerical portion of the layout   ;;
;;  name.                                                               ;;
;;                                                                      ;;
;;  The user may optionally predefine a fixed prefix and/or suffix      ;;
;;  within the 'Program Parameters' section of the source code below,   ;;
;;  or, if such parameters are set to nil, the program will prompt the  ;;
;;  user to specify the prefix and suffix upon invoking the command.    ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2020  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2020-01-26                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2020-09-05                                      ;;
;;                                                                      ;;
;;  - Added option to specify layout starting number.                   ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2023-12-31                                      ;;
;;                                                                      ;;
;;  - Added option to rename all or specific layouts.                   ;;
;;----------------------------------------------------------------------;;

(defun c:rl ( / *error* int lst lyn new ord pad pre sed sel suf tmp )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (setq

;;----------------------------------------------------------------------;;
;;                          Program Parameters                          ;;
;;----------------------------------------------------------------------;;

        ;; Optional Predefined Layout Prefix
        ;; Set to nil to prompt the user, or "" for no prefix.
        pre nil

        ;; Optional Predefined Layout Suffix
        ;; Set to nil to prompt the user, or "" for no suffix.
        suf nil

        ;; Optional Predefined Starting Number
        ;; Set to nil to prompt the user
        int nil

        ;; Number of Numerical Digits
        ;; e.g. 1 = "1", 2 = "01", 3 = "001"
        pad 2

;;----------------------------------------------------------------------;;

    )

    ;; Obtain a valid (optional) prefix & suffix
    (or (= 'str (type pre)) (setq pre (LM:rl:validstring "\nSpecify prefix <none>: ")))
    (or (= 'str (type suf)) (setq suf (LM:rl:validstring "\nSpecify suffix <none>: ")))
    (or (= 'int (type int)) (setq int (cond ((getint "\nSpecify starting number <1>: ")) (1))))
    (or (and (= 'int (type pad)) (<= 0 pad)) (setq pad 0))

    ;; Obtain list of layout objects, current names, and sort index
    (vlax-for lyt (vla-get-layouts (LM:acdoc))
        (if (= :vlax-false (vla-get-modeltype lyt))
            (setq lst (cons lyt lst)
                  lyn (cons (vla-get-name lyt) lyn)
                  ord (cons (vla-get-taborder lyt) ord)
            )
        )
    )

    ;; Decide whether to rename all or selected
    (initget "All Select")
    (cond
        (   (and (= "Select" (getkword "\nSelect layouts to rename? [All/Select] <All>: "))
                (null
                    (setq sel
                        (LM:filtlistbox "Select Layouts to Renumber"
                            (mapcar '(lambda ( n ) (nth n lyn)) (vl-sort-i ord '<)) t
                        )
                    )
                )
            )
        )
        (   t
            ;; Remove unselected layouts from objects & sort index
            (if sel
                (setq lst (vl-remove nil (mapcar '(lambda ( a b ) (if (member a sel) b nil)) lyn lst))
                      ord (vl-remove nil (mapcar '(lambda ( a b ) (if (member a sel) b nil)) lyn ord))
                )
            )

            ;; Construct a unique seed for temporary renaming
            (setq lyn (cons (strcase pre) (mapcar 'strcase lyn))
                  sed "%"
            )
            (while (vl-some '(lambda ( x ) (wcmatch x (strcat "*" sed "*"))) lyn)
                (setq sed (strcat sed "%"))
            )

            ;; Temporarily rename layouts to ensure no duplicate keys when renumbering
            (LM:startundo (LM:acdoc))
            (setq tmp 0)
            (foreach lyt lst
                (vla-put-name lyt (strcat sed (itoa (setq tmp (1+ tmp)))))
            )

            ;; Remove temporarily renamed layouts from name list
            (if sel
                (setq sel (mapcar 'strcase sel)
                      lyn (vl-remove-if '(lambda ( x ) (member x sel)) lyn)
                )
                (setq lyn nil)
            )

            ;; Rename layouts in tab order, with prefix & suffix
            (foreach idx (vl-sort-i ord '<)
                ;; In case new layout name appears in unselected layouts
                (while (member (strcase (setq new (strcat pre (LM:rl:padzeros (itoa int) pad) suf))) lyn)
                    (setq int (1+ int))
                )
                (vla-put-name (nth idx lst) new)
                (setq int (1+ int))
            )
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(defun LM:rl:validstring ( msg / rtn )
    (while
        (not
            (or
                (= "" (setq rtn (getstring t msg)))
                (snvalid (vl-string-trim " " rtn))
            )
        )
        (princ (strcat "\nThe layout name cannot contain the characters \\<>/?\":;*|,=`"))
    )
    rtn
)

;;----------------------------------------------------------------------;;

(defun LM:rl:padzeros ( str len )
    (if (< (strlen str) len) (LM:rl:padzeros (strcat "0" str) len) str)
)

;;----------------------------------------------------------------------;;

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

;;----------------------------------------------------------------------;;

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;;----------------------------------------------------------------------;;

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;----------------------------------------------------------------------;;

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
        "\n:: RenumberLayouts.lsp | Version 1.2 | \\U+00A9 Lee Mac "
        ((lambda ( y ) (if (= y (menucmd "m=$(edtime,0,yyyy)")) y (strcat y "-" (menucmd "m=$(edtime,0,yyyy)")))) "2020")
        " www.lee-mac.com ::"
        "\n:: Type \"RL\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;