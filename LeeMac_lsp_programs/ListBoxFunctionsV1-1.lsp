;; List Box: List Up  -  Lee Mac
;; Shifts the items at the supplied indexes by one position to a lower index
;; idx - [lst] List of zero-based indexes
;; lst - [lst] List of items
;; Returns: [lst] List of ((idx) (lst)) following operation

(defun LM:listup ( idx lst / foo )
    (defun foo ( cnt idx lst idx-out lst-out )
        (cond
            (   (not  (and idx lst))
                (list (reverse idx-out) (append (reverse lst-out) lst))
            )
            (   (= 0 (car idx))
                (foo (1+  cnt) (mapcar '1- (cdr idx)) (cdr lst) (cons cnt idx-out) (cons (car lst) lst-out))
            )
            (   (= 1 (car idx))
                (foo (1+  cnt) (mapcar '1- (cdr idx)) (cons (car lst) (cddr lst)) (cons cnt idx-out) (cons (cadr lst) lst-out))
            )
            (   (foo (1+  cnt) (mapcar '1- idx) (cdr lst) idx-out (cons (car lst) lst-out)))
        )
    )
    (foo 0 idx lst nil nil)
)

;; List Box: List Down  -  Lee Mac
;; Shifts the items at the supplied indexes by one position to a higher index
;; idx - [lst] List of zero-based indexes
;; lst - [lst] List of items
;; Returns: [lst] List of ((idx) (lst)) following operation

(defun LM:listdown ( idx lst / bar foo len )
    (setq len (length lst)
          foo (lambda ( x ) (- len x 1))
          bar (lambda ( a b ) (list (reverse (mapcar 'foo a)) (reverse b)))
    )
    (apply 'bar (apply 'LM:listup (bar idx lst)))
)

;; List Box: List Top  -  Lee Mac
;; Shifts the items at the supplied indexes to the lowest index
;; idx - [lst] List of zero-based indexes
;; lst - [lst] List of items
;; Returns: [lst] List of ((idx) (lst)) following operation

(defun LM:listtop ( idx lst / i )
    (setq i -1)
    (list
        (mapcar '(lambda ( x ) (setq i (1+ i))) idx)
        (append (mapcar '(lambda ( x ) (nth x lst)) idx) (LM:removeitems idx lst))
    )
)

;; List Box: List Bottom -  Lee Mac
;; Shifts the items at the supplied indexes to the highest index
;; idx - [lst] List of zero-based indexes
;; lst - [lst] List of items
;; Returns: [lst] List of ((idx) (lst)) following operation

(defun LM:listbottom ( idx lst / i )
    (setq i (length lst))
    (list
        (reverse (mapcar '(lambda ( x ) (setq i (1- i))) idx))
        (append (LM:removeitems idx lst) (mapcar '(lambda ( x ) (nth x lst)) idx))
    )
)

;; Remove Items  -  Lee Mac
;; Removes the items at the supplied indexes from a given list
;; idx - [lst] List of zero-based indexes
;; lst - [lst] List from which items are to be removed
;; Returns: [lst] List with items at the supplied indexes removed

(defun LM:removeitems ( idx lst / i )
    (setq i -1)
    (vl-remove-if '(lambda ( x ) (member (setq i (1+ i)) idx)) lst)
)