;; Copy Block Definition  -  Lee Mac
;; Duplicates a block definition, with the copied definition assigned the name provided.
;; blk - [str] name of block definition to be duplicated
;; new - [str] name to be assigned to copied block definition
;; Returns the copied VLA Block Definition Object, else nil

(defun LM:CopyBlockDefinition ( blk new / abc app dbc dbx def doc rtn vrs )
    (setq dbx
        (vl-catch-all-apply 'vla-getinterfaceobject
            (list (setq app (vlax-get-acad-object))
                (if (< (setq vrs (atoi (getvar 'acadver))) 16)
                    "objectdbx.axdbdocument" (strcat "objectdbx.axdbdocument." (itoa vrs))
                )
            )
        )
    )
    (cond
        (   (or (null dbx) (vl-catch-all-error-p dbx))
            (prompt "\nUnable to interface with ObjectDBX.")
        )
        (   (and
                (setq doc (vla-get-activedocument app)
                      abc (vla-get-blocks doc)
                      dbc (vla-get-blocks dbx)
                      def (LM:getitem abc blk)
                )
                (not (LM:getitem abc new))
            )
            (vlax-invoke doc 'copyobjects (list def) dbc)
            (vla-put-name (setq def (LM:getitem dbc  blk)) new)
            (vlax-invoke dbx 'copyobjects (list def) abc)
            (setq rtn (LM:getitem abc new))
        )
    )
    (if (= 'vla-object (type dbx))
        (vlax-release-object dbx)
    )
    rtn
)

;; VLA-Collection: Get Item  -  Lee Mac
;; Retrieves the item with index 'idx' if present in the supplied collection
;; col - [vla]     VLA Collection Object
;; idx - [str/int] Index of the item to be retrieved

(defun LM:getitem ( col idx / obj )
    (if (not (vl-catch-all-error-p (setq obj (vl-catch-all-apply 'vla-item (list col idx)))))
        obj
    )
)