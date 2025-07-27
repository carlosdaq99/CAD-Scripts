;; Evaluate Once on Startup  -  Lee Mac
;; Writes expressions to an existing or temporary acaddoc.lsp to be evaluated on drawing startup,
;; with the acaddoc.lsp restored to its original state or deleted following evaluation.
;; 
;; Call with quoted expressions to be evaluated, e.g.:
;; (LM:evalonceonstartup '((lambda ( / foo ) (setq foo 10.0) (print foo) (princ))))
;; 
;; Function returns T if expressions are written successfully.

(defun LM:evalonceonstartup ( expr / *error* cnt des lsp tmp )
    
    (defun *error* ( msg )
        (if (= 'file (type des)) (close des))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    ;; If acaddoc.lsp already exists, check how many lines it has
    (if (and (setq lsp (findfile "acaddoc.lsp"))
             (setq des (open lsp "r"))
        )
        (progn
            (setq cnt 0)
            (while (read-line des) (setq cnt (1+ cnt)))
            (setq des (close des))
        )
    )

    ;; Write expressions to new/existing acaddoc.lsp
    (if (or (and lsp (setq des (open lsp "a")))
            (setq des
                (open
                    (setq tmp
                        (strcat
                            (vl-string-right-trim "\\"
                                (vl-string-translate "/" "\\"
                                    (getvar 'roamablerootprefix)
                                )
                            )
                            "\\Support\\acaddoc.lsp"
                        )
                    )
                    "w"
                )
            )
        )
        (progn
            ;; Write blank line in case we are appending
            (write-line "" des)

            ;; Write expressions to be evaluated on startup
            (write-line
                (strcat
                    "(defun LM:evalonceonstartup:eval nil "
                    (vl-prin1-to-string expr)
                    ")"
                )
                des
            )

            ;; If acaddoc.lsp already existed, construct expressions to restore it
            (if (= 'str (type lsp))
                (write-line
                    (strcat
                        "(defun LM:evalonceonstartup:clean ( / des lsp lst )"
                        "    (if"
                        "        (and"
                        "            (setq lsp (findfile " (vl-prin1-to-string lsp) "))"
                        "            (setq des (open lsp \"r\"))"
                        "        )"
                        "        (progn"
                        "            (repeat " (itoa cnt)
                        "                (setq lst (cons (read-line des) lst))"
                        "            )"
                        "            (close des)"
                        "            (if (setq des (open lsp \"w\"))"
                        "                (progn"
                        "                    (foreach x (reverse lst) (write-line x des))"
                        "                    (close des)"
                        "                )"
                        "            )"
                        "        )"
                        "    )"
                        ")"
                    )
                    des
                )
                ;; Else delete it
                (write-line
                    (strcat
                        "(defun LM:evalonceonstartup:clean ( / lsp )"
                        "    (if (setq lsp (findfile " (vl-prin1-to-string tmp) "))"
                        "        (vl-file-delete lsp)"
                        "    )"
                        ")"
                    )
                    des
                )
            )

            ;; Construct s::startup function
            (write-line
                (vl-prin1-to-string
                    (quote
                        (if (= 'list (type s::startup))
                            (setq s::startup
                                (append s::startup
                                   '(
                                        (LM:evalonceonstartup:eval)
                                        (LM:evalonceonstartup:clean)
                                    )
                                )
                            )
                            (defun-q s::startup nil
                                (LM:evalonceonstartup:eval)
                                (LM:evalonceonstartup:clean)
                            )
                        )
                    )
                )
                des
            )
            
            (setq des (close des))
            t
        )
    )
)