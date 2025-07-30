;;; HatchStrataBetweenProfiles.lsp
;;; Fills the space between selected Civil 3D profile polylines with colored hatching.
;;; Usage: Load this script, then run (HatchStrataBetweenProfiles)
;;; Follows Lee Mac and project conventions. See comments for details.

(defun c:HatchStrataBetweenProfiles ( / ss n i plines regions colors colorcount make-region make-hatch)
  (vl-load-com)
  ;; Helper: Create closed region between two polylines
  (defun make-region (pline1 pline2 / pts1 pts2 region)
    (setq pts1 (vl-remove-if 'null (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget pline1))))
          pts2 (reverse (vl-remove-if 'null (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget pline2)))))
    )
    (if (and pts1 pts2)
      (progn
        (setq region (entmakex (append (list (cons 0 "LWPOLYLINE") (cons 100 "AcDbEntity") (cons 100 "AcDbPolyline") (cons 90 (+ (length pts1) (length pts2))) (cons 70 1))
                                     (mapcar (lambda (pt) (cons 10 pt)) (append pts1 pts2)))))
        region
      )
    )
  )
  ;; Helper: Hatch a closed polyline/region with a color
  (defun make-hatch (region color / hatch)
    (setq hatch (vla-AddHatch (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
                              acHatchPatternTypePreDefined "SOLID" :vlax-true))
    (vla-AppendOuterLoop hatch (vlax-invoke (vlax-get-acad-object) 'ObjectIdToObject (vla-get-ObjectID region)))
    (vla-put-Color hatch color)
    (vla-Evaluate hatch)
    hatch
  )
  ;; Main logic
  (prompt "\nSelect all profile polylines (top to bottom): ")
  (setq ss (ssget '((0 . "LWPOLYLINE")))
        n (if ss (sslength ss) 0)
        colors (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
        colorcount 0
        plines nil
        regions nil
  )
  (if (< n 2)
    (prompt "\nSelect at least two polylines.")
    (progn
      ;; Collect polylines in selection order
      (repeat n (setq plines (cons (ssname ss (setq i (if i (1+ i) 0))) plines)))
      (setq plines (reverse plines))
      ;; For each adjacent pair, create region and hatch
      (while (> (length plines) 1)
        (setq region (make-region (car plines) (cadr plines)))
        (if region
          (progn
            (setq color (nth (mod colorcount (length colors)) colors))
            (make-hatch region color)
            (setq colorcount (1+ colorcount))
            (setq regions (cons region regions))
          )
        )
        (setq plines (cdr plines))
      )
      (prompt "\nHatching complete.")
    )
  )
  (princ)
)

(princ "\nHatchStrataBetweenProfiles loaded. Type HatchStrataBetweenProfiles to run.\n")
