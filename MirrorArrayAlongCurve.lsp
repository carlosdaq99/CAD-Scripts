
;;---------------------=={ Mirror Array Along Curve }==-------------------;;
;;  Mirrors objects or blocks along the normal to a selected curve        ;;
;;  at their closest point. Supports array and single-object modes.       ;;
;;                                                                       ;;
;;  Refactored for modularity, error handling, and undo marks             ;;
;;  Author: GitHub Copilot, refactored to Lee Mac standards               ;;
;;-----------------------------------------------------------------------;;
;;  Usage: Type MirrorArrayAlongCurve at the command line.                ;;
;;         Choose [A]rray or [S]ingle object mode.                        ;;
;;         Select objects and curve as prompted.                          ;;
;;-----------------------------------------------------------------------;;

;; Error handler
;; Ensures undo marks are closed and prints error message
(defun *error* (msg)
  (if doc (_EndUndo doc))
  (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
      (princ (strcat "\n** Error: " msg " **")))
  (princ)
)

;; Undo mark helpers
;; _StartUndo: Begins an undo mark for safe operations
;; _EndUndo: Ends the undo mark if active
(defun _StartUndo (doc) (_EndUndo doc) (vla-StartUndoMark doc))
(defun _EndUndo (doc)
  (if (= 8 (logand 8 (getvar 'UNDOCTL)))
    (vla-EndUndoMark doc)
  )
)

;; get-mirror-base-point
;; Returns a suitable base point for mirroring an entity
;; Tries centroid, bounding box, insertion/start point, or coordinates
(defun get-mirror-base-point (vlaObj ename)
  (cond
    ((vlax-method-applicable-p vlaObj 'GetCentroid)
      (vlax-invoke-method vlaObj 'GetCentroid)
    )
    ((vlax-property-available-p vlaObj 'Centroid)
      (vlax-get vlaObj 'Centroid)
    )
    ((acet-ent-geomextents ename)
      (let ((bbox (acet-ent-geomextents ename)))
        (mapcar '(lambda (a b) (/ (+ a b) 2.0)) (car bbox) (cadr bbox))
      )
    )
    ((vlax-property-available-p vlaObj 'InsertionPoint)
      (vlax-get vlaObj 'InsertionPoint)
    )
    ((vlax-property-available-p vlaObj 'StartPoint)
      (vlax-get vlaObj 'StartPoint)
    )
    ((vlax-method-applicable-p vlaObj 'GetStartPoint)
      (vlax-invoke-method vlaObj 'GetStartPoint)
    )
    ((vlax-property-available-p vlaObj 'Coordinates)
      (vlax-get vlaObj 'Coordinates)
    )
    (T nil)
  )
)


;; mirror-entity
;; Mirrors a single entity about the normal to the curve at its closest point
;; Arguments: ename - entity name, vlaObj - VLA object, curveVla - curve VLA object
(defun mirror-entity (ename vlaObj curveVla)
  (let* (
    (objPt (get-mirror-base-point vlaObj ename))
    (curvePt (and objPt (vlax-curve-getClosestPointTo curveVla objPt)))
    (tangent (and curvePt (vlax-curve-getFirstDeriv curveVla (vlax-curve-getParamAtPoint curveVla curvePt))))
    (mirrorPt1 (and curvePt (vlax-3D-point curvePt)))
    (mirrorPt2 (and curvePt tangent (vlax-3D-point (mapcar '+ curvePt tangent))))
    )
    (if (and objPt tangent (not (equal tangent '(0.0 0.0 0.0) 1e-8)))
      (progn
        (vla-mirror vlaObj mirrorPt1 mirrorPt2)
      )
      (prompt "\nWarning: Could not determine valid mirror axis. Skipping entity.")
    )
  )
)

;; process-selection-set
;; Mirrors all entities in a selection set, recursively explodes blocks
;; Arguments: ss - selection set, curveVla - curve VLA object
(defun process-selection-set (ss curveVla)
  (if ss
    (progn
      (setq objCount (sslength ss))
      (setq i 0)
      (while (< i objCount)
        (setq objEname (ssname ss i))
        (setq objData (entget objEname))
        (if (= (cdr (assoc 0 objData)) "INSERT")
          (progn
            (command "_.EXPLODE" objEname)
            (setq explodedSet (ssget "P"))
            (if explodedSet
              (process-selection-set explodedSet curveVla)
            )
          )
          (progn
            (setq objVla (vlax-ename->vla-object objEname))
            (mirror-entity objEname objVla curveVla)
          )
        )
        (setq i (1+ i))
      )
    )
  )
)


;; c:MirrorArrayAlongCurve
;; Main command. Prompts user for mode, objects, and curve.
;; Mirrors objects or blocks along the normal to the curve at closest point.
(defun c:MirrorArrayAlongCurve (/ *error* doc mode ss sel curveEname curveVla)
  (vl-load-com)
  (defun *error* (msg)
    (if doc (_EndUndo doc))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (_StartUndo doc)
  (prompt "\nMirrorArrayAlongCurve: [A]rray or [S]ingle object? <A>: ")
  (setq mode (getstring))
  (if (or (eq mode "") (eq (strcase mode) "A"))
    (progn
      (prompt "\nSelect the array of objects to mirror: ")
      (setq ss (ssget))
      (if (null ss)
        (progn (prompt "\nNo objects selected.") (_EndUndo doc) (exit))
      )
      (prompt "\nSelect the curve to mirror along: ")
      (setq curveEname (car (entsel)))
      (if (null curveEname)
        (progn (prompt "\nNo curve selected.") (_EndUndo doc) (exit))
      )
      (setq curveVla (vlax-ename->vla-object curveEname))
      (process-selection-set ss curveVla)
    )
    (progn
      (prompt "\nSelect the object to mirror: ")
      (setq sel (entsel))
      (if (null sel)
        (progn (prompt "\nNo object selected.") (_EndUndo doc) (exit))
      )
      (prompt "\nSelect the curve to mirror along: ")
      (setq curveEname (car (entsel)))
      (if (null curveEname)
        (progn (prompt "\nNo curve selected.") (_EndUndo doc) (exit))
      )
      (setq curveVla (vlax-ename->vla-object curveEname))
      (mirror-entity (car sel) (vlax-ename->vla-object (car sel)) curveVla)
    )
  )
  (_EndUndo doc)
  (princ "\nMirrorArrayAlongCurve complete.")
)

(princ "\nMirrorArrayAlongCurve.lsp loaded. Type MirrorArrayAlongCurve to run.\n")
