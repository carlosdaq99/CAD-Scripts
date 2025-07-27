; MirrorArrayAlongCurve.lsp
; Command: MirrorArrayAlongCurve
; Mirrors each object in a selected array along the normal to a selected curve at its closest point.
; Author: GitHub Copilot


;; Main command definition for MirrorArrayAlongCurve
(defun c:MirrorArrayAlongCurve (/ ss curveObj curveEname curveVla curveType objCount i objEname objVla objPt curvePt tangent normal mirrorPt1 mirrorPt2)
  ;; Prompt user to select the objects to be mirrored
  (prompt "\nSelect the array of objects to mirror: ")
  (setq ss (ssget))
  ;; If no objects selected, exit
  (if (null ss)
    (progn (prompt "\nNo objects selected.") (exit))
  )
  ;; Prompt user to select the curve to mirror along
  (prompt "\nSelect the curve to mirror along: ")
  (setq curveEname (car (entsel)))
  ;; If no curve selected, exit
  (if (null curveEname)
    (progn (prompt "\nNo curve selected.") (exit))
  )
  ;; Convert curve entity to VLA object for advanced property access
  (setq curveVla (vlax-ename->vla-object curveEname))
  ;; Get the number of objects in the selection set
  (setq objCount (sslength ss))
  (setq i 0)
  ;; Loop through each object in the selection set
  (while (< i objCount)
    ;; Get the entity name and DXF data for the current object
    (setq objEname (ssname ss i))
    (setq objData (entget objEname))
    ;; Check if the object is a block reference (INSERT)
    (if (= (cdr (assoc 0 objData)) "INSERT")
      (progn
        ;; If block reference, explode the block to access individual entities
        (prompt "\nBlock reference detected. Exploding block...")
        (command "_.EXPLODE" objEname)
        ;; After explode, collect new entities using previous selection set
        (setq explodedSet (ssget "P")) ; Previous selection set
        ;; If explodedSet is valid, process each entity inside the block
        (if explodedSet
          (progn
            (setq j 0)
            (setq explodedCount (sslength explodedSet))
            ;; Loop through each exploded entity
            (while (< j explodedCount)
              (setq explodedEname (ssname explodedSet j))
              (setq explodedVla (vlax-ename->vla-object explodedEname))
              ;; Try to get centroid (center of mass) for the entity
              (setq objPt nil)
              (if (vlax-method-applicable-p explodedVla 'GetCentroid)
                (progn
                  (setq objPt (vlax-invoke-method explodedVla 'GetCentroid))
                  (prompt (strcat "\n[LOG] Centroid for " (cdr (assoc 0 (entget explodedEname))) ": " (rtos (car objPt) 2 4) ", " (rtos (cadr objPt) 2 4) ", " (rtos (caddr objPt) 2 4)))
                )
              )
              ;; If not available, try 'Centroid' property (regions/solids)
              (if (and (null objPt) (vlax-property-available-p explodedVla 'Centroid))
                (progn
                  (setq objPt (vlax-get explodedVla 'Centroid))
                  (prompt (strcat "\n[LOG] Centroid property for " (cdr (assoc 0 (entget explodedEname))) ": " (rtos (car objPt) 2 4) ", " (rtos (cadr objPt) 2 4) ", " (rtos (caddr objPt) 2 4)))
                )
              )
              ;; If not available, use bounding box center
              (if (and (null objPt) (acet-ent-geomextents explodedEname))
                (progn
                  (setq bbox (acet-ent-geomextents explodedEname))
                  (setq objPt (mapcar '(lambda (a b) (/ (+ a b) 2.0)) (car bbox) (cadr bbox)))
                  (prompt (strcat "\n[LOG] Bounding box center for " (cdr (assoc 0 (entget explodedEname))) ": " (rtos (car objPt) 2 4) ", " (rtos (cadr objPt) 2 4) ", " (rtos (caddr objPt) 2 4)))
                )
              )
              ;; Fallback to previous logic if all else fails
              (if (null objPt)
                (cond
                  ((= (cdr (assoc 0 (entget explodedEname))) "SPLINE")
                    (setq objPt (cdr (assoc 11 (entget explodedEname))))
                    (if (null objPt)
                      (prompt "\nWarning: Spline has no fit point. Skipping.")
                    )
                  )
                  ((vlax-property-available-p explodedVla 'InsertionPoint)
                    (setq objPt (vlax-get explodedVla 'InsertionPoint))
                  )
                  ((vlax-property-available-p explodedVla 'StartPoint)
                    (setq objPt (vlax-get explodedVla 'StartPoint))
                  )
                  ((vlax-method-applicable-p explodedVla 'GetStartPoint)
                    (setq objPt (vlax-invoke-method explodedVla 'GetStartPoint))
                  )
                  ((vlax-property-available-p explodedVla 'Coordinates)
                    (setq objPt (vlax-get explodedVla 'Coordinates))
                  )
                  (T
                    (prompt "\nWarning: Could not determine base point for exploded entity. Skipping.")
                    (setq objPt nil)
                  )
                )
              )
              ;; If a valid point was found, proceed to mirror
              (if objPt
                (progn
                  ;; Find closest point on the curve to the entity's base point
                  (setq curvePt (vlax-curve-getClosestPointTo curveVla objPt))
                  (prompt (strcat "\n[LOG] Closest point on curve: " (rtos (car curvePt) 2 4) ", " (rtos (cadr curvePt) 2 4) ", " (rtos (caddr curvePt) 2 4)))
                  ;; Get tangent vector at the closest point
                  (setq tangent (vlax-curve-getFirstDeriv curveVla (vlax-curve-getParamAtPoint curveVla curvePt)))
                  (prompt (strcat "\n[LOG] Tangent vector: " (rtos (car tangent) 2 4) ", " (rtos (cadr tangent) 2 4) ", " (rtos (caddr tangent) 2 4)))
                  ;; Use tangent vector to define mirror axis (two points along tangent)
                  (prompt (strcat "\n[LOG] Tangent vector: " (rtos (car tangent) 2 4) ", " (rtos (cadr tangent) 2 4) ", " (rtos (caddr tangent) 2 4)))
                  ;; If tangent is zero, skip entity with warning
                  (if (equal tangent '(0.0 0.0 0.0) 1e-8)
                    (prompt "\nWarning: Zero tangent vector at exploded object. Skipping.")
                    (progn
                      ;; Define mirror axis using curvePt and curvePt + tangent
                      (setq mirrorPt1 curvePt)
                      ;; Calculate distance between points
                      (setq axisDist (distance mirrorPt1 (mapcar '+ curvePt tangent)))
                      ;; If axisDist is too small, scale tangent to minimum length (e.g., 10 units)
                      (if (< axisDist 1e-4)
                        (progn
                          (setq tangent
                            (mapcar
                              '(lambda (x)
                                 (* x (/ 10.0 (max 1e-8 (distance '(0 0 0) tangent))))
                               )
                               tangent
                            )
                          )
                          (prompt "\n[LOG] Tangent vector scaled to minimum length.")
                        )
                      )
                      (setq mirrorPt2 (mapcar '+ curvePt tangent))
                      (prompt (strcat "\n[LOG] Mirror axis (tangent): " (rtos (car mirrorPt1) 2 4) ", " (rtos (cadr mirrorPt1) 2 4) ", " (rtos (caddr mirrorPt1) 2 4) " to " (rtos (car mirrorPt2) 2 4) ", " (rtos (cadr mirrorPt2) 2 4) ", " (rtos (caddr mirrorPt2) 2 4)))
                      ;; If axis points are not distinct, skip entity with warning
                      (if (equal mirrorPt1 mirrorPt2 1e-8)
                        (prompt "\nWarning: Mirror axis points are not distinct. Skipping exploded object.")
                        ;; Perform the mirror operation using AutoCAD command
                        (command "_.MIRROR" explodedEname "" mirrorPt1 mirrorPt2 "N")
                      )
                    )
                  )
                )
              )
              ;; Move to next exploded entity
              (setq j (1+ j))
            )
          )
        )
      )
      (progn
        ;; For regular entities (not blocks), get VLA object
        (setq objVla (vlax-ename->vla-object objEname))
        ;; Try to get centroid (center of mass) for the entity
        (setq objPt nil)
        (if (vlax-method-applicable-p objVla 'GetCentroid)
          (progn
            (setq objPt (vlax-invoke-method objVla 'GetCentroid))
            (prompt (strcat "\n[LOG] Centroid for " (cdr (assoc 0 (entget objEname))) ": " (rtos (car objPt) 2 4) ", " (rtos (cadr objPt) 2 4) ", " (rtos (caddr objPt) 2 4)))
          )
        )
        ;; If not available, try 'Centroid' property (regions/solids)
        (if (and (null objPt) (vlax-property-available-p objVla 'Centroid))
          (progn
            (setq objPt (vlax-get objVla 'Centroid))
            (prompt (strcat "\n[LOG] Centroid property for " (cdr (assoc 0 (entget objEname))) ": " (rtos (car objPt) 2 4) ", " (rtos (cadr objPt) 2 4) ", " (rtos (caddr objPt) 2 4)))
          )
        )
        ;; If not available, use bounding box center
        (if (and (null objPt) (acet-ent-geomextents objEname))
          (progn
            (setq bbox (acet-ent-geomextents objEname))
            (setq objPt (mapcar '(lambda (a b) (/ (+ a b) 2.0)) (car bbox) (cadr bbox)))
            (prompt (strcat "\n[LOG] Bounding box center for " (cdr (assoc 0 (entget objEname))) ": " (rtos (car objPt) 2 4) ", " (rtos (cadr objPt) 2 4) ", " (rtos (caddr objPt) 2 4)))
          )
        )
        ;; Fallback to previous logic if all else fails
        (if (null objPt)
          (cond
            ((= (cdr (assoc 0 (entget objEname))) "SPLINE")
              (setq objPt (cdr (assoc 11 (entget objEname))))
              (if (null objPt)
                (prompt "\nWarning: Spline has no fit point. Skipping.")
              )
            )
            ((vlax-property-available-p objVla 'InsertionPoint)
              (setq objPt (vlax-get objVla 'InsertionPoint))
            )
            ((vlax-property-available-p objVla 'StartPoint)
              (setq objPt (vlax-get objVla 'StartPoint))
            )
            ((vlax-method-applicable-p objVla 'GetStartPoint)
              (setq objPt (vlax-invoke-method objVla 'GetStartPoint))
            )
            ((vlax-property-available-p objVla 'Coordinates)
              (setq objPt (vlax-get objVla 'Coordinates))
            )
            (T
              (prompt "\nWarning: Could not determine base point for entity. Skipping.")
              (setq objPt nil)
            )
          )
        )
        ;; If a valid point was found, proceed to mirror
        (if objPt
          (progn
            ;; Find closest point on the curve to the entity's base point
            (setq curvePt (vlax-curve-getClosestPointTo curveVla objPt))
            (prompt (strcat "\n[LOG] Closest point on curve: " (rtos (car curvePt) 2 4) ", " (rtos (cadr curvePt) 2 4) ", " (rtos (caddr curvePt) 2 4)))
            ;; Get tangent vector at the closest point
            (setq tangent (vlax-curve-getFirstDeriv curveVla (vlax-curve-getParamAtPoint curveVla curvePt)))
            (prompt (strcat "\n[LOG] Tangent vector: " (rtos (car tangent) 2 4) ", " (rtos (cadr tangent) 2 4) ", " (rtos (caddr tangent) 2 4)))
            ;; Use tangent vector to define mirror axis (two points along tangent)
            (prompt (strcat "\n[LOG] Tangent vector: " (rtos (car tangent) 2 4) ", " (rtos (cadr tangent) 2 4) ", " (rtos (caddr tangent) 2 4)))
            ;; If tangent is zero, skip entity with warning
            (if (equal tangent '(0.0 0.0 0.0) 1e-8)
              (prompt "\nWarning: Zero tangent vector at object. Skipping.")
              (progn
                ;; Define mirror axis using curvePt and curvePt + tangent
                (setq mirrorPt1 curvePt)
                ;; Calculate distance between points
                (setq tangentLen (distance '(0 0 0) tangent))
                (prompt (strcat "\n[LOG] Tangent vector length: " (rtos tangentLen 2 6)))
                (setq axisDist (distance mirrorPt1 (mapcar '+ curvePt tangent)))
                (prompt (strcat "\n[LOG] Axis distance: " (rtos axisDist 2 6)))
                ;; If axisDist is too small, scale tangent to minimum length (e.g., 10 units)
                (if (< axisDist 1e-4)
                  (progn
                    (prompt "\n[LOG] Axis distance too small, scaling tangent vector.")
                    (setq tangent
                      (mapcar
                        '(lambda (x)
                           (* x (/ 10.0 (max 1e-8 tangentLen)))
                         )
                         tangent
                      )
                    )
                    (setq tangentLen (distance '(0 0 0) tangent))
                    (prompt (strcat "\n[LOG] Tangent vector new length: " (rtos tangentLen 2 6)))
                  )
                )
                (setq mirrorPt2 (mapcar '+ curvePt tangent))
                (prompt (strcat "\n[LOG] Mirror axis point 1: " (rtos (car mirrorPt1) 2 6) ", " (rtos (cadr mirrorPt1) 2 6) ", " (rtos (caddr mirrorPt1) 2 6)))
                (prompt (strcat "\n[LOG] Mirror axis point 2: " (rtos (car mirrorPt2) 2 6) ", " (rtos (cadr mirrorPt2) 2 6) ", " (rtos (caddr mirrorPt2) 2 6)))
                (prompt (strcat "\n[LOG] Mirror axis (tangent): " (rtos (car mirrorPt1) 2 4) ", " (rtos (cadr mirrorPt1) 2 4) ", " (rtos (caddr mirrorPt1) 2 4) " to " (rtos (car mirrorPt2) 2 4) ", " (rtos (cadr mirrorPt2) 2 4) ", " (rtos (caddr mirrorPt2) 2 4)))
                ;; If axis points are not distinct, skip entity with warning
                (if (equal mirrorPt1 mirrorPt2 1e-8)
                  (prompt "\nWarning: Mirror axis points are not distinct. Skipping object.")
                  ;; Perform the mirror operation using AutoCAD command
                  (command "_.MIRROR" objEname "" mirrorPt1 mirrorPt2 "N")
                )
              )
            )
          )
        )
      )
    )
    ;; Move to next object in selection set
    (setq i (1+ i))
  )
  ;; Print completion message
  (princ "\nMirrorArrayAlongCurve complete.")
)

(princ "\nMirrorArrayAlongCurve.lsp loaded. Type MirrorArrayAlongCurve to run.\n")
