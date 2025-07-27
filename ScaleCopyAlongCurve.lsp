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
;;; LOGGING SYSTEM
;;; ========================================================================

(defun log-debug (message / log-file current-time)
  "Writes debug messages to a log file with timestamp"
  (setq log-file "c:\\Users\\dea29431\\Documents\\LOCAL\\CAD\\debug\\scalecopy_debug.log")
  (setq current-time (rtos (getvar "CDATE") 2 6))
  (if (not *scalecopy_log_init*)
    (progn
      ;; First log entry: overwrite file
      (setq *scalecopy_log_init* T)
      (if (setq file-handle (open log-file "w"))
        (progn
          (write-line (strcat current-time " - " message) file-handle)
          (close file-handle)
        )
      )
    )
    (progn
      ;; Subsequent log entries: append
      (if (setq file-handle (open log-file "a"))
        (progn
          (write-line (strcat current-time " - " message) file-handle)
          (close file-handle)
        )
      )
    )
  )
  (princ (strcat "\nDEBUG: " message))
)

(defun log-start ()
  "Initializes logging session"
  (setq *scalecopy_log_init* nil)
  (log-debug "=== SCALECOPYALONGCURVE SESSION STARTED ===")
)

;;; ========================================================================
;;; HELPER FUNCTIONS
;;; ========================================================================

(defun validate-curve-object (curve-ename / curve-obj curve-type)
  "Validates if the selected entity is a valid curve object for vlax-curve functions (supports splines, polylines, arcs, lines)"
  (log-debug (strcat "Validating curve entity: " (vl-princ-to-string curve-ename)))
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
      (log-debug (strcat "Curve object type: " (if curve-type curve-type "UNKNOWN")))
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
           ) 
           (log-debug (strcat "Curve type " curve-type " is supported"))
           curve-obj)
          (T 
           (log-debug (strcat "Curve type " curve-type " is NOT supported"))
           nil)
        )
        (progn
          (log-debug "Failed to determine curve object type")
          nil
        )
      )
    )
    (progn
      (log-debug "No curve entity provided")
      nil
    )
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
  "Gets the tangent angle at a specific distance along the curve, with fallback if function is unavailable"
  (setq param (vlax-curve-getParamAtDist curve-obj distance))
  (setq tangent-vec (vlax-curve-getFirstDeriv curve-obj param))
  (if tangent-vec
    (atan (cadr tangent-vec) (car tangent-vec))
    (progn
      (log-debug "WARNING: Tangent vector not available, defaulting to 0.0")
      0.0
    )
  )
)

(defun copy-and-scale-object (source-obj base-point target-point scale-factor rotation-angle / new-obj cmd-result)
  "Copies an object to a new location and applies scaling and rotation with error handling (ActiveX only)"
  (log-debug (strcat "Copying object - Scale: " (rtos scale-factor 2 3) " Target: " (vl-princ-to-string target-point)))
  (setq new-obj nil)
  (vl-catch-all-apply
    '(lambda ()
       ;; Convert source to VLA object
       (setq new-obj (vla-Copy (vlax-ename->vla-object source-obj)))
       (if (not new-obj)
         (progn
           (log-debug "VLA copy failed.")
           (setq new-obj nil)
         )
         (progn
           (log-debug "VLA copy successful.")
           ;; Move to target point
           (vla-Move new-obj
             (vlax-3d-point base-point)
             (vlax-3d-point target-point)
           )
           (log-debug "VLA move successful.")
           ;; Scale
           (if (> scale-factor 0.001)
             (progn
               (vla-ScaleEntity new-obj (vlax-3d-point target-point) scale-factor)
               (log-debug "VLA scale successful.")
             )
           )
           ;; Rotate
           (if (and rotation-angle (/= rotation-angle 0.0))
             (progn
               (vla-Rotate new-obj (vlax-3d-point target-point) rotation-angle)
               (log-debug "VLA rotate successful.")
             )
           )
         )
       )
     )
  )
  (if new-obj T nil)
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
    (log-debug (strcat "ERROR: " (if msg msg "Unknown error")))
    (if old-cmdecho (setvar "CMDECHO" old-cmdecho))
    (if old-error (setq *error* old-error))
    (princ "\nSCALEDCOPY interrupted or failed.")
    (princ)
  )
  
  ;; Initialize logging
  (log-start)
  (log-debug "Starting ScaleCopyAlongCurve command")
  
  (setq old-error *error*)
  (setq *error* temp-error)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  
  (princ "\n=== SCALED COPY ALONG CURVE ===")
  (princ "\nThis command creates multiple copies of an object along a curve")
  (princ "\nwith both scale and spacing decreasing linearly from start to end.")
  
  ;; Step 1: Select source object
  (princ "\n\nStep 1: Select the object to copy and scale...")
  (log-debug "Requesting source object selection")
  (setq source-obj (car (entsel "\nSelect object to copy: ")))
  (if (not source-obj)
    (progn
      (log-debug "No object selected by user")
      (princ "\nNo object selected. Command cancelled.")
      (setq *error* old-error)
      (setvar "CMDECHO" old-cmdecho)
      (exit)
    )
  )
  (log-debug (strcat "Source object selected: " (vl-princ-to-string source-obj)))
  
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
  (log-debug "Validating curve object")
  (setq curve-obj (validate-curve-object curve-ename))
  (if (not curve-obj)
    (progn
      (log-debug "Curve validation failed - not a valid curve type")
      (princ "\nSelected object is not a valid curve. Please select a spline, arc, polyline, or similar curve object.")
      (setq *error* old-error)
      (setvar "CMDECHO" old-cmdecho)
      (exit)
    )
  )
  (log-debug "Curve validation successful")
  
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
  ;; If aligning, calculate original tangent angle at base point
  (setq original-tangent-angle nil)
  (if (= align-to-tangent "Yes")
    (progn
      (setq original-tangent-angle (get-tangent-at-distance curve-obj (vlax-curve-getDistAtParam curve-obj (vlax-curve-getParamAtPoint curve-obj base-point))))
      (log-debug (strcat "Original tangent angle at base point: " (rtos original-tangent-angle 2 6)))
    )
  )
  
  ;; Calculate curve length
  (log-debug "Calculating curve length")
  (setq curve-length (get-curve-length curve-obj))
  (if (<= curve-length 0.001)
    (progn
      (log-debug (strcat "Invalid curve length: " (rtos curve-length 2 6)))
      (princ "\nCurve length is too small or invalid.")
      (setq *error* old-error)
      (setvar "CMDECHO" old-cmdecho)
      (exit)
    )
  )
  (log-debug (strcat "Curve length calculated: " (rtos curve-length 2 3)))
  
  (princ (strcat "\nCurve length: " (rtos curve-length 2 3)))
  (princ (strcat "\nCreating " (itoa num-copies) " copies..."))
  
  ;; Calculate spacing distances
  (log-debug "Calculating spacing distances")
  (setq distances (calculate-spacing-distances num-copies curve-length))
  (log-debug (strcat "Number of distances calculated: " (itoa (length distances))))
  
  ;; Main placement loop
  (princ "\n\nPlacing objects...")
  (log-debug "Starting main placement loop")
  (setq i 1)
  (foreach current-distance distances
    (log-debug (strcat "Processing copy " (itoa i) " of " (itoa num-copies)))
    
    ;; Ensure distance doesn't exceed curve length
    (if (> current-distance curve-length)
      (progn
        (log-debug (strcat "Distance " (rtos current-distance 2 3) " exceeds curve length, clamping"))
        (setq current-distance curve-length)
      )
    )
    
    ;; Get point on curve with error handling
    (setq target-point (vl-catch-all-apply 'vlax-curve-getPointAtDist 
                                           (list curve-obj current-distance)))
    (if (vl-catch-all-error-p target-point)
      (progn
        (log-debug (strcat "Failed to get point at distance " (rtos current-distance 2 3)))
        (log-debug (strcat "Error: " (vl-catch-all-error-message target-point)))
      )
      (progn
        (log-debug (strcat "Target point: " (vl-princ-to-string target-point)))
        
        ;; Calculate scale factor: scale_i = max(min_scale, 1 - i/N)
        (setq scale-factor (max min-scale (- 1.0 (/ (float i) (float num-copies)))))
        
    ;; Calculate tangent angle at placement point if aligning
    (setq tangent-angle nil)
    (setq rotation-diff nil)
    (if (= align-to-tangent "Yes")
      (progn
        (setq tangent-angle (get-tangent-at-distance curve-obj current-distance))
        (log-debug (strcat "DEBUG: Copy " (itoa i) " - Original tangent angle: " (if original-tangent-angle (rtos original-tangent-angle 2 6) "nil") " | Placement tangent angle: " (if tangent-angle (rtos tangent-angle 2 6) "nil")))
        ;; Calculate rotation difference
        (setq rotation-diff (- tangent-angle original-tangent-angle))
        (log-debug (strcat "DEBUG: Copy " (itoa i) " - Rotation difference (radians): " (rtos rotation-diff 2 6)))
      )
    )
    (log-debug (strcat "DEBUG: Copy " (itoa i) " - Calling copy-and-scale-object with scale-factor: " (rtos scale-factor 2 3) " rotation: " (if (= align-to-tangent "Yes") (rtos rotation-diff 2 6) "nil")))
    ;; Copy and transform object
    (if (copy-and-scale-object source-obj base-point target-point scale-factor (if (= align-to-tangent "Yes") rotation-diff nil))
      (log-debug (strcat "Successfully placed copy " (itoa i)))
      (log-debug (strcat "Failed to place copy " (itoa i)))
    )
      )
    )
    
    ;; Progress indicator
    (if (= (rem i 10) 0)
      (princ (strcat "\n  Placed " (itoa i) " of " (itoa num-copies) " objects..."))
    )
    (setq i (1+ i))
  )
  ;; Cleanup and completion
  (log-debug "Main loop completed, cleaning up")
  (setq *error* old-error)
  (setvar "CMDECHO" old-cmdecho)
  (log-debug "=== SCALECOPYALONGCURVE SESSION COMPLETED SUCCESSFULLY ===")
  (princ (strcat "\n\n=== OPERATION COMPLETE ==="))
  (princ (strcat "\nSuccessfully created " (itoa num-copies) " scaled copies along the curve."))
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
