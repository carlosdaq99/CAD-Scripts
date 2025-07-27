# CAD Workspace Information & Script Documentation

## Environment
- Platform: Civil 3D 2024 (AutoCAD 2018+ compatible)
- Scripting: AutoLISP with ActiveX/vlax-curve support
- Shell: Windows PowerShell v5.1
- Workspace Path: `c:\Users\dea29431\Documents\LOCAL\CAD`

## Project Structure
- `ScaleCopyAlongCurve.lsp`: Main script for scaled/tapered object placement along curves

## Best Practices
- Place all LISP scripts in a folder included in AutoCAD's support file search path for easy loading.
- Use APPLOAD or drag-and-drop to load scripts.
- For multi-line Python execution, create a `.py` file and run with `python .\script.py` in PowerShell.
- Do not attempt to create or manage Python environments in automation; assume pre-configured environments.

## Script Overview
- **Command:** `ScaleCopyAlongCurve`
- **Purpose:** Places multiple scaled-down copies of a selected object along a user-defined curve, with both scale and spacing decreasing linearly from start to end.
- **Supported Curves:** Splines, polylines, arcs, circles, lines (via ActiveX/vlax-curve)
- **Key Functions:**
  - `validate-curve-object`: Robustly detects valid curve types
  - `get-curve-length`: Gets total curve length
  - `calculate-spacing-distances`: Computes cumulative distances for tapered spacing
  - `get-tangent-at-distance`: Gets tangent angle for alignment
  - `copy-and-scale-object`: Copies, scales, and rotates objects

## Troubleshooting
- If script fails to load, check folder path and ensure it is in a support file search path.
- For curve selection errors, ensure the object is a valid curve type (see supported list above).
- For PowerShell execution issues, avoid inline multi-line Python; use script files instead.

## Technical Appendix: ScaleCopyAlongCurve.lsp

```
;;; ========================================================================
;;; SCALED COPY ALONG CURVE - AutoLISP Script
;;; ========================================================================
;;; 
;;; Description: Places multiple scaled-down copies of a selected object 
;;;              along a user-defined curve with both object scale and 
;;;              spacing decreasing linearly from start to end.
;;;
;;; Command: ScaleCopyAlongCurve
;;; 
;;; Requirements:
;;; - AutoCAD 2018+ with AutoLISP and ActiveX support
;;; - Compatible with splines, arcs, polylines, and other vlax-curve objects
;;;
;;; Mathematical Formulas:
;;; - Scale: scale_i = max(min_scale, 1 - i/N)
;;; - Spacing: spacing_k = const * (1 - k/N)
;;; - Distance: d_i = sum of all spacing_k from k=1 to i
;;;
;;; Author: GitHub Copilot
;;; Date: 2025-07-26
;;; ========================================================================

(vl-load-com)

;;; ========================================================================
;;; HELPER FUNCTIONS
;;; ========================================================================

(defun validate-curve-object (curve-ename / curve-obj curve-type)
  "Validates if the selected entity is a valid curve object for vlax-curve functions (supports splines, polylines, arcs, lines)"
  (if curve-ename
    (progn
      (setq curve-obj (vlax-ename->vla-object curve-ename))
      (setq curve-type nil)
      (if (and curve-obj (vlax-property-available-p curve-obj 'ObjectName))
        (setq curve-type (vlax-get-property curve-obj 'ObjectName))
        (if (and curve-obj (vlax-method-applicable-p curve-obj 'get-ObjectName))
          (setq curve-type (vla-get-ObjectName curve-obj))
        )
      )
      (if curve-type
        (cond
          ((or
             (= curve-type "AcDbSpline")
             (= curve-type "AcDbPolyline")
             (= curve-type "AcDb2dPolyline")
             (= curve-type "AcDb3dPolyline")
             (= curve-type "AcDbArc")
             (= curve-type "AcDbCircle")
             (= curve-type "AcDbLine")
           ) curve-obj)
          (T nil)
        )
        nil
      )
    )
    nil
  )
)

(defun get-curve-length (curve-obj / end-param)
  "Gets the total length of a curve object"
  (setq end-param (vlax-curve-getEndParam curve-obj))
  (vlax-curve-getDistAtParam curve-obj end-param)
)

(defun calculate-spacing-distances (num-copies total-length / spacing-sum i spacing-val distances cumulative-dist)
  "Calculates cumulative distances for decreasing spacing along curve
   Returns list of distances from start of curve"
  (setq spacing-sum 0.0)
  (setq distances '())
  (setq cumulative-dist 0.0)
  
  ;; First, calculate the sum of all spacing values to normalize
  (setq i 1)
  (while (<= i num-copies)
    (setq spacing-val (- 1.0 (/ (float i) (float num-copies))))
    (setq spacing-sum (+ spacing-sum spacing-val))
    (setq i (1+ i))
  )
  
  ;; Calculate normalization constant
  (setq spacing-const (/ total-length spacing-sum))
  
  ;; Now calculate actual cumulative distances
  (setq i 1)
  (while (<= i num-copies)
    (setq spacing-val (* spacing-const (- 1.0 (/ (float i) (float num-copies)))))
    (setq cumulative-dist (+ cumulative-dist spacing-val))
    (setq distances (append distances (list cumulative-dist)))
    (setq i (1+ i))
  )
  
  distances
)

(defun get-tangent-at-distance (curve-obj distance / param tangent-vec angle)
  "Gets the tangent angle at a specific distance along the curve"
  (setq param (vlax-curve-getParamAtDist curve-obj distance))
  (setq tangent-vec (vlax-curve-get1stDeriv curve-obj param))
  (if tangent-vec
    (atan (cadr tangent-vec) (car tangent-vec))
    0.0
  )
)

(defun copy-and-scale-object (source-obj base-point target-point scale-factor rotation-angle / new-obj)
  "Copies an object to a new location and applies scaling and rotation"
  (command "_.COPY" source-obj "" base-point target-point)
  
  ;; Scale the last created object
  (if (> scale-factor 0.001) ; Avoid degenerate scaling
    (command "_.SCALE" "L" "" target-point scale-factor)
  )
  
  ;; Apply rotation if specified
  (if (and rotation-angle (/= rotation-angle 0.0))
    (command "_.ROTATE" "L" "" target-point (* rotation-angle (/ 180.0 pi)))
  )
)

;;; ========================================================================
;;; MAIN FUNCTION
;;; ========================================================================

;;; ========================================================================
;;; MAIN FUNCTION (Renamed to match file name)
;;; ========================================================================

(defun C:ScaleCopyAlongCurve ( / 
  ;; User input variables
  source-obj base-point curve-ename curve-obj num-copies min-scale align-to-tangent
  ;; Calculation variables  
  curve-length distances scale-factor distance target-point tangent-angle
  ;; Loop variables
  i current-distance
  ;; Error handling
  old-error old-cmdecho
  )
  
  ;; Error handler setup
  (defun temp-error (msg)
    (if old-cmdecho (setvar "CMDECHO" old-cmdecho))
    (if old-error (setq *error* old-error))
    (princ "\nTAPEREDCOPY interrupted or failed.")
    (princ)
  )
  
  (setq old-error *error*)
  (setq *error* temp-error)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  
  (princ "\n=== TAPERED COPY ALONG CURVE ===")
  (princ "\nThis command creates multiple copies of an object along a curve")
  (princ "\nwith both scale and spacing decreasing linearly from start to end.")
  
  ;; Step 1: Select source object
  (princ "\n\nStep 1: Select the object to copy and scale...")
  (setq source-obj (car (entsel "\nSelect object to copy: ")))
  (if (not source-obj)
    (progn
      (princ "\nNo object selected. Command cancelled.")
      (setq *error* old-error)
      (setvar "CMDECHO" old-cmdecho)
      (exit)
    )
  )
  
  ;; Step 2: Get base point
  (princ "\n\nStep 2: Specify the base point on the object...")
  (setq base-point (getpoint "\nSpecify base point (insertion point): "))
  (if (not base-point)
    (progn
      (princ "\nNo base point specified. Command cancelled.")
      (setq *error* old-error)
      (setvar "CMDECHO" old-cmdecho)
      (exit)
    )
  )
  
  ;; Step 3: Select curve path
  (princ "\n\nStep 3: Select the path curve...")
  (setq curve-ename (car (entsel "\nSelect curve (spline, arc, polyline, etc.): ")))
  (if (not curve-ename)
    (progn
      (princ "\nNo curve selected. Command cancelled.")
      (setq *error* old-error)
      (setvar "CMDECHO" old-cmdecho)
      (exit)
    )
  )
  
  ;; Validate curve object
  (setq curve-obj (validate-curve-object curve-ename))
  (if (not curve-obj)
    (progn
      (princ "\nSelected object is not a valid curve. Please select a spline, arc, polyline, or similar curve object.")
      (setq *error* old-error)
      (setvar "CMDECHO" old-cmdecho)
      (exit)
    )
  )
  
  ;; Step 4: Get number of copies
  (princ "\n\nStep 4: Specify parameters...")
  (setq num-copies (getint "\nNumber of copies [10]: "))
  (if (or (not num-copies) (< num-copies 1))
    (setq num-copies 10)
  )
  (if (> num-copies 1000)
    (progn
      (princ "\nToo many copies requested (max 1000). Using 1000.")
      (setq num-copies 1000)
    )
  )
  
  ;; Step 5: Get minimum scale (optional)
  (initget 6) ; Positive non-zero
  (setq min-scale (getreal "\nMinimum scale factor [0.1]: "))
  (if (or (not min-scale) (< min-scale 0.001))
    (setq min-scale 0.1)
  )
  (if (>= min-scale 1.0)
    (progn
      (princ "\nMinimum scale should be less than 1.0. Using 0.1.")
      (setq min-scale 0.1)
    )
  )
  
  ;; Step 6: Option to align to tangent
  (initget "Yes No")
  (setq align-to-tangent (getkword "\nAlign objects to curve tangent? [Yes/No] <No>: "))
  (if (not align-to-tangent)
    (setq align-to-tangent "No")
  )
  
  ;; Calculate curve length
  (setq curve-length (get-curve-length curve-obj))
  (if (<= curve-length 0.001)
    (progn
      (princ "\nCurve length is too small or invalid.")
      (setq *error* old-error)
      (setvar "CMDECHO" old-cmdecho)
      (exit)
    )
  )
  
  (princ (strcat "\nCurve length: " (rtos curve-length 2 3)))
  (princ (strcat "\nCreating " (itoa num-copies) " copies..."))
  
  ;; Calculate spacing distances
  (setq distances (calculate-spacing-distances num-copies curve-length))
  
  ;; Main placement loop
  (princ "\n\nPlacing objects...")
  (setq i 1)
  (foreach current-distance distances
    ;; Ensure distance doesn't exceed curve length
    (if (> current-distance curve-length)
      (setq current-distance curve-length)
    )
    ;; Get point on curve
    (setq target-point (vlax-curve-getPointAtDist curve-obj current-distance))
    ;; Calculate scale factor: scale_i = max(min_scale, 1 - i/N)
    (setq scale-factor (max min-scale (- 1.0 (/ (float i) (float num-copies)))))
    ;; Get tangent angle if alignment is requested
    (setq tangent-angle nil)
    (if (= align-to-tangent "Yes")
      (setq tangent-angle (get-tangent-at-distance curve-obj current-distance))
    )
    ;; Copy and transform object
    (copy-and-scale-object source-obj base-point target-point scale-factor tangent-angle)
    ;; Progress indicator
    (if (= (rem i 10) 0)
      (princ (strcat "\n  Placed " (itoa i) " of " (itoa num-copies) " objects..."))
    )
    (setq i (1+ i))
  )
  ;; Cleanup and completion
  (setq *error* old-error)
  (setvar "CMDECHO" old-cmdecho)
  (princ (strcat "\n\n=== OPERATION COMPLETE ==="))
  (princ (strcat "\nSuccessfully created " (itoa num-copies) " tapered copies along the curve."))
  (princ (strcat "\nScale range: 1.0 to " (rtos min-scale 2 3)))
  (if (= align-to-tangent "Yes")
    (princ "\nObjects aligned to curve tangent.")
  )
  (princ)
)

;;; ========================================================================
;;; COMMAND REGISTRATION AND INITIALIZATION
;;; ========================================================================

(princ "\n=== SCALE COPY ALONG CURVE LOADED ===")
(princ "\nCommand: ScaleCopyAlongCurve")
(princ "\nCreates multiple scaled copies along a curve with decreasing scale and spacing.")
(princ "\nCompatible with splines, arcs, polylines, and other curve objects.")
(princ "\n\nType ScaleCopyAlongCurve to start.")
(princ)
```
