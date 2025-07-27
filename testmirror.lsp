; testmirror.lsp
; Command: testmirror
; Draws a tangent line (5 units) at the closest point on a curve from a selected object
; Author: GitHub Copilot


(defun c:testmirror (/ objEname objVla objPt curveEname curveVla curvePt tangent tangentLen tangentDir lineEndPt userLen)
  ;; Robust selection loop for object
  (setq objEname nil)
  (while (null objEname)
    (prompt "\nSelect the object to find tangent axis: ")
    (setq objEname (car (entsel)))
    (if (null objEname)
      (prompt "\nNo object selected. Try again.")
    )
  )
  (setq objVla (vlax-ename->vla-object objEname))
  (setq objPt nil)
  ;; Try to get a reference point from the object
  (if (vlax-property-available-p objVla 'Centroid)
    (setq objPt (vlax-get objVla 'Centroid))
  )
  (if (and (null objPt) (vlax-property-available-p objVla 'InsertionPoint))
    (setq objPt (vlax-get objVla 'InsertionPoint))
  )
  (if (and (null objPt) (acet-ent-geomextents objEname))
    (setq bbox (acet-ent-geomextents objEname)
          objPt (mapcar '(lambda (a b) (/ (+ a b) 2.0)) (car bbox) (cadr bbox)))
  )
  (if (null objPt)
    (progn (prompt "\nCould not determine object reference point.") (exit))
  )
  ;; Robust selection loop for curve
  (setq curveEname nil)
  (while (null curveEname)
    (prompt "\nSelect the curve to mirror along: ")
    (setq curveEname (car (entsel)))
    (if (null curveEname)
      (prompt "\nNo curve selected. Try again.")
    )
  )
  (setq curveVla (vlax-ename->vla-object curveEname))
  ;; Find closest point and tangent
  (setq curvePt (vlax-curve-getClosestPointTo curveVla objPt))
  (setq tangent (vlax-curve-getFirstDeriv curveVla (vlax-curve-getParamAtPoint curveVla curvePt)))
  (setq tangentLen (distance '(0 0 0) tangent))
  (if (> tangentLen 1e-8)
    (setq tangentDir (mapcar '(lambda (x) (/ x tangentLen)) tangent))
    (setq tangentDir '(1.0 0.0 0.0)) ; fallback direction
  )
  ;; Prompt user for tangent line length
  (setq userLen (getreal "\nEnter tangent line length <5.0>: "))
  (if (or (null userLen) (<= userLen 0.0)) (setq userLen 5.0))
  (setq lineEndPt (mapcar '+ curvePt (mapcar '(lambda (x) (* x userLen)) tangentDir)))
  ;; Ensure both points are lists of 3 numbers
  (if (or (not (listp curvePt)) (not (listp lineEndPt))
          (/= (length curvePt) 3) (/= (length lineEndPt) 3))
    (progn (prompt "\n[ERROR] Invalid point format for line.") (exit))
  )
  ;; Print actual values passed to entmake for debugging
  (prompt (strcat "\n[DIAG] curvePt: " (rtos (car curvePt) 2 6) ", " (rtos (cadr curvePt) 2 6) ", " (rtos (caddr curvePt) 2 6)))
  (prompt (strcat "\n[DIAG] lineEndPt: " (rtos (car lineEndPt) 2 6) ", " (rtos (cadr lineEndPt) 2 6) ", " (rtos (caddr lineEndPt) 2 6)))
  ;; Only draw if points are not equal
  (if (not (equal curvePt lineEndPt 1e-6))
    (progn
      (prompt (strcat "\n[DIAG] entmake args: "
        (vl-prin1-to-string curvePt) " -> " (vl-prin1-to-string lineEndPt)))
      (entmake
        (list
          '(0 . "LINE")
          '(8 . "0") ; layer 0
          (cons 10 (trans curvePt 1 0)) ; start point in WCS
          (cons 11 (trans lineEndPt 1 0)) ; end point in WCS
        )
      )
    )
    (prompt "\n[ERROR] Points are equal, not drawing line.")
  )
  (princ "\ntestmirror complete.")
)

(princ "\ntestmirror.lsp loaded. Type testmirror to run.\n")
