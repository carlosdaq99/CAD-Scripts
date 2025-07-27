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