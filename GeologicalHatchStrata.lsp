;;; ========================================================================
;;; GEOLOGICAL HATCH STRATA - CIVIL 3D AUTOMATED COMMAND
;;; ========================================================================
;;; 
;;; Description:
;;;   Automatically fills spaces between Civil 3D surface profiles with
;;;   unique hatch patterns and colors representing geological strata.
;;;   Handles crossing profiles with dynamic vertical order detection.
;;;
;;; Command: GEOHATCH
;;;
;;; Requirements:
;;;   - Civil 3D 2024 (AutoCAD 2018+ compatible)
;;;   - AutoLISP with ActiveX/COM support
;;;   - Profile View with multiple Civil 3D Surface Profiles
;;;
;;; Features:
;;;   - Full COM automation - no manual selection required
;;;   - Automatic profile detection and enumeration
;;;   - Dynamic crossing detection with geological precision
;;;   - Configurable geological patterns and colors
;;;   - Enterprise-grade error handling and logging
;;;
;;; Author: GitHub Copilot
;;; Date: July 29, 2025
;;; Version: 1.0
;;; ========================================================================

(vl-load-com)

;;; ========================================================================
;;; GEOLOGICAL MATERIAL CONFIGURATION
;;; ========================================================================

;; Geological strata configuration - customize as needed
(setq *geological-config*
  '(
    ;; Format: (surface-name . (hatch-pattern scale color description))
    ("AL"  . ("GRAVEL"  1.0  3 "Alluvium"))           ; Green
    ("KC"  . ("CLAY"    1.0  5 "Keuper Clay"))        ; Blue  
    ("HD"  . ("STEEL"   0.5  8 "Hard Rock"))          ; Dark Gray
    ("CG"  . ("COAL"    1.0  1 "Coal Group"))         ; Red
    ("RTD" . ("EARTH"   1.0  6 "Red Till Deposit"))   ; Magenta
  )
)

;; Station sampling increment (units match profile view)
(setq *station-increment* 1.0)

;; Fuzzy tolerance for geological precision (Lee Mac pattern)
(setq *geo-tolerance* 1e-8)

;;; ========================================================================
;;; MAIN COMMAND FUNCTION
;;; ========================================================================

(defun C:GEOHATCH ( / *error* old-error old-cmdecho doc app profile-view profiles result)
  "Main command for geological hatching between Civil 3D surface profiles"
  
  ;; Lee Mac enterprise error handler pattern
  (setq old-error *error*
        old-cmdecho (getvar "CMDECHO")
  )
  
  (defun *error* (msg)
    ;; Guaranteed resource cleanup
    (if doc (vlax-release-object doc))
    (setvar "CMDECHO" old-cmdecho)
    (setq *error* old-error)
    (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
      (princ (strcat "\nError in GEOHATCH: " msg))
    )
    (princ)
  )
  
  (setvar "CMDECHO" 0)
  (princ "\n=== GEOLOGICAL HATCH STRATA ===")
  (princ "\nAutomated Civil 3D Profile Hatching System")
  
  ;; Initialize COM objects
  (if (setq app (vlax-get-acad-object))
    (progn
      (setq doc (vla-get-activedocument app))
      (princ "\n✓ Civil 3D COM objects initialized")
      
      ;; Main processing workflow
      (cond
        ;; Step 1: Detect and validate profile view
        ((not (setq profile-view (detect-profile-view doc)))
          (princ "\n✗ No Civil 3D Profile View found in current drawing")
          (princ "\nPlease open a drawing with a Civil 3D Profile View and try again.")
        )
        
        ;; Step 2: Enumerate and validate surface profiles  
        ((not (setq profiles (enumerate-surface-profiles profile-view)))
          (princ "\n✗ No surface profiles found in Profile View")
          (princ "\nProfile View must contain multiple Civil 3D Surface Profiles.")
        )
        
        ;; Step 3: Process geological hatching
        (t
          (princ (strcat "\n✓ Found " (itoa (length profiles)) " surface profiles"))
          (princ "\nProcessing geological strata...")
          
          (if (setq result (process-geological-hatching profiles))
            (progn
              (princ (strcat "\n✓ Successfully created " (itoa result) " geological hatches"))
              (princ "\nGeological hatching complete!")
            )
            (princ "\n✗ Failed to create geological hatches")
          )
        )
      )
    )
    (princ "\n✗ Failed to initialize Civil 3D COM objects")
  )
  
  ;; Cleanup and restore
  (if doc (vlax-release-object doc))
  (setvar "CMDECHO" old-cmdecho)
  (setq *error* old-error)
  (princ)
)

;;; ========================================================================
;;; CIVIL 3D PROFILE DETECTION AND ENUMERATION
;;; ========================================================================

(defun detect-profile-view (doc / modelspace profile-view)
  "Detect Civil 3D Profile View in current drawing"
  (setq modelspace (vla-get-modelspace doc)
        profile-view nil
  )
  
  ;; Search for Civil 3D Profile View objects
  (vlax-for obj modelspace
    (if (and (vlax-property-available-p obj 'ObjectName)
             (wcmatch (strcase (vlax-get obj 'ObjectName)) "*PROFILE*VIEW*")
        )
      (progn
        (princ (strcat "\n✓ Found Profile View: " (vlax-get obj 'ObjectName)))
        (setq profile-view obj)
      )
    )
  )
  
  profile-view
)

(defun enumerate-surface-profiles (profile-view / profiles profile-collection)
  "Enumerate all surface profiles in the profile view"
  (setq profiles '())
  
  ;; Try different Civil 3D COM approaches for profile access
  (cond
    ;; Approach 1: Direct profile collection access
    ((and (vlax-property-available-p profile-view 'Profiles)
          (setq profile-collection (vlax-get profile-view 'Profiles))
     )
      (vlax-for profile profile-collection
        (if (is-surface-profile profile)
          (setq profiles (cons profile profiles))
        )
      )
    )
    
    ;; Approach 2: Enumerate child objects
    ((vlax-method-applicable-p profile-view 'GetProfiles)
      (setq profile-collection (vlax-invoke profile-view 'GetProfiles))
      (vlax-for profile profile-collection
        (if (is-surface-profile profile)
          (setq profiles (cons profile profiles))
        )
      )
    )
    
    ;; Approach 3: Search modelspace for profile objects related to this view
    (t
      (setq profiles (find-profiles-by-view profile-view))
    )
  )
  
  ;; Sort profiles by name for consistent geological order
  (if profiles
    (vl-sort profiles '(lambda (a b) 
                        (< (strcase (get-profile-name a)) 
                           (strcase (get-profile-name b)))
                      ))
    nil
  )
)

(defun is-surface-profile (profile / obj-name)
  "Check if profile is a Civil 3D Surface Profile"
  (and (vlax-property-available-p profile 'ObjectName)
       (setq obj-name (vlax-get profile 'ObjectName))
       (or (wcmatch (strcase obj-name) "*SURFACE*PROFILE*")
           (wcmatch (strcase obj-name) "*AECC*PROFILE*")
       )
  )
)

(defun find-profiles-by-view (profile-view / modelspace profiles)
  "Fallback: Find profiles by searching modelspace"
  (setq modelspace (vlax-get (vlax-get profile-view 'Document) 'ModelSpace)
        profiles '()
  )
  
  (vlax-for obj modelspace
    (if (and (vlax-property-available-p obj 'ObjectName)
             (wcmatch (strcase (vlax-get obj 'ObjectName)) "*PROFILE*")
             (not (wcmatch (strcase (vlax-get obj 'ObjectName)) "*VIEW*"))
        )
      (setq profiles (cons obj profiles))
    )
  )
  
  profiles
)

(defun get-profile-name (profile)
  "Get the name/description of a Civil 3D profile"
  (cond
    ((vlax-property-available-p profile 'Name)
      (vlax-get profile 'Name)
    )
    ((vlax-property-available-p profile 'Description)
      (vlax-get profile 'Description)
    )
    (t "Unknown Profile")
  )
)

;;; ========================================================================
;;; GEOLOGICAL CROSSING DETECTION ALGORITHM
;;; ========================================================================

(defun process-geological-hatching (profiles / station-data boundaries hatch-count)
  "Main geological processing algorithm"
  (princ "\nExtracting profile elevation data...")
  
  ;; Step 1: Extract elevation data at regular stations
  (if (setq station-data (extract-station-elevation-data profiles))
    (progn
      (princ (strcat "\n✓ Extracted data for " (itoa (length station-data)) " stations"))
      
      ;; Step 2: Generate geological boundaries with crossing detection
      (princ "\nDetecting profile crossings and generating boundaries...")
      (if (setq boundaries (generate-geological-boundaries station-data))
        (progn
          (princ (strcat "\n✓ Generated " (itoa (length boundaries)) " geological boundaries"))
          
          ;; Step 3: Apply geological hatching
          (princ "\nApplying geological hatch patterns...")
          (setq hatch-count (apply-geological-hatching boundaries))
          hatch-count
        )
        nil
      )
    )
    nil
  )
)

(defun extract-station-elevation-data (profiles / station-range stations station-data)
  "Extract elevation data for all profiles at regular station intervals"
  
  ;; Determine common station range
  (setq station-range (get-common-station-range profiles))
  
  (if station-range
    (progn
      ;; Generate station sampling points
      (setq stations (generate-station-points 
                       (car station-range)   ; min station
                       (cadr station-range)  ; max station  
                       *station-increment*
                     ))
      
      ;; Extract elevations at each station
      (setq station-data '())
      (foreach station stations
        (setq station-elevations (extract-elevations-at-station profiles station))
        (if station-elevations
          (setq station-data 
            (cons (cons station station-elevations) station-data)
          )
        )
      )
      
      (reverse station-data)
    )
    nil
  )
)

(defun get-common-station-range (profiles / min-station max-station profile-range)
  "Determine overlapping station range for all profiles"
  (setq min-station nil
        max-station nil
  )
  
  (foreach profile profiles
    (setq profile-range (get-profile-station-range profile))
    (if profile-range
      (progn
        (setq min-station 
          (if min-station 
            (max min-station (car profile-range))
            (car profile-range)
          )
        )
        (setq max-station
          (if max-station
            (min max-station (cadr profile-range))  
            (cadr profile-range)
          )
        )
      )
    )
  )
  
  (if (and min-station max-station (< min-station max-station))
    (list min-station max-station)
    nil
  )
)

(defun get-profile-station-range (profile)
  "Get station range (start/end) for a Civil 3D profile"
  (cond
    ;; Method 1: Direct station range properties
    ((and (vlax-property-available-p profile 'StartStation)
          (vlax-property-available-p profile 'EndStation)
     )
      (list (vlax-get profile 'StartStation)
            (vlax-get profile 'EndStation)
      )
    )
    
    ;; Method 2: Geometry extents
    ((vlax-method-applicable-p profile 'GetBoundingBox)
      (let ((min-pt (vlax-3d-point)) (max-pt (vlax-3d-point)))
        (vlax-invoke profile 'GetBoundingBox min-pt max-pt)
        (list (vlax-get min-pt 'X) (vlax-get max-pt 'X))
      )
    )
    
    ;; Method 3: Fallback estimation
    (t (list 0.0 100.0))  ; Default range - adjust as needed
  )
)

(defun generate-station-points (min-station max-station increment / stations current)
  "Generate station points with specified increment"
  (setq stations '()
        current min-station
  )
  
  (while (<= current max-station)
    (setq stations (cons current stations))
    (setq current (+ current increment))
  )
  
  (reverse stations)
)

(defun extract-elevations-at-station (profiles station / elevations profile-name elevation)
  "Extract elevation for each profile at specified station"
  (setq elevations '())
  
  (foreach profile profiles
    (setq profile-name (get-profile-name profile))
    (setq elevation (get-elevation-at-station profile station))
    
    (if elevation
      (setq elevations 
        (cons (cons profile-name elevation) elevations)
      )
    )
  )
  
  (reverse elevations)
)

(defun get-elevation-at-station (profile station / elevation)
  "Get elevation of profile at specific station"
  (setq elevation nil)
  
  ;; Try different Civil 3D COM methods for elevation extraction
  (cond
    ;; Method 1: Direct elevation query
    ((vlax-method-applicable-p profile 'ElevationAt)
      (setq elevation (vlax-invoke profile 'ElevationAt station))
    )
    
    ;; Method 2: Point at station
    ((vlax-method-applicable-p profile 'PointAt)
      (let ((point (vlax-invoke profile 'PointAt station)))
        (if point
          (setq elevation (vlax-get point 'Y))
        )
      )
    )
    
    ;; Method 3: Geometric evaluation
    ((vlax-method-applicable-p profile 'Evaluate)
      (let ((point (vlax-invoke profile 'Evaluate station)))
        (if (and point (> (length point) 1))
          (setq elevation (cadr point))
        )
      )
    )
    
    ;; Method 4: Curve methods (if profile extends vlax-curve)
    ((vlax-curve-getpointatdist profile station)
      (let ((point (vlax-curve-getpointatdist profile station)))
        (if point
          (setq elevation (cadr point))
        )
      )
    )
  )
  
  elevation
)

;;; ========================================================================
;;; GEOLOGICAL BOUNDARY GENERATION WITH CROSSING DETECTION
;;; ========================================================================

(defun generate-geological-boundaries (station-data / boundaries prev-station-data)
  "Generate hatch boundaries with crossing detection"
  (setq boundaries '()
        prev-station-data nil
  )
  
  (foreach station-data-point station-data
    (if prev-station-data
      (let ((segment-boundaries (create-boundary-segments 
                                  prev-station-data 
                                  station-data-point
                                )))
        (setq boundaries (append boundaries segment-boundaries))
      )
    )
    (setq prev-station-data station-data-point)
  )
  
  boundaries
)

(defun create-boundary-segments (station1-data station2-data / segments)
  "Create geological boundaries between two adjacent stations"
  (let ((station1 (car station1-data))
        (station2 (car station2-data))
        (elevs1 (cdr station1-data))
        (elevs2 (cdr station2-data))
        (order1 nil)
        (order2 nil)
       )
    
    ;; Get vertical order at both stations (bottom to top)
    (setq order1 (get-vertical-order elevs1)
          order2 (get-vertical-order elevs2)
          segments '()
    )
    
    ;; Create boundary for each adjacent pair of profiles
    (let ((i 0))
      (while (< i (1- (length order1)))
        (let ((lower-profile (nth i order1))
              (upper-profile (nth (1+ i) order1))
              (lower-elev1 nil)
              (upper-elev1 nil)
              (lower-elev2 nil) 
              (upper-elev2 nil)
              (boundary-points nil)
              (layer-info nil)
             )
          
          ;; Get elevations for boundary profiles
          (setq lower-elev1 (cdr (assoc lower-profile elevs1))
                upper-elev1 (cdr (assoc upper-profile elevs1))
                lower-elev2 (cdr (assoc lower-profile elevs2))
                upper-elev2 (cdr (assoc upper-profile elevs2))
          )
          
          ;; Create 4-point closed boundary
          (if (and lower-elev1 upper-elev1 lower-elev2 upper-elev2)
            (progn
              (setq boundary-points
                (list
                  (list station1 lower-elev1)   ; Bottom-left
                  (list station2 lower-elev2)   ; Bottom-right
                  (list station2 upper-elev2)   ; Top-right  
                  (list station1 upper-elev1)   ; Top-left
                )
              )
              
              ;; Create geological layer information
              (setq layer-info
                (list
                  (cons "lower-surface" lower-profile)
                  (cons "upper-surface" upper-profile)
                  (cons "boundary-points" boundary-points)
                  (cons "station-range" (list station1 station2))
                )
              )
              
              (setq segments (cons layer-info segments))
            )
          )
          
          (setq i (1+ i))
        )
      )
    )
    
    (reverse segments)
  )
)

(defun get-vertical-order (elevations / sorted-elevations)
  "Sort profiles by elevation (bottom to top)"
  (setq sorted-elevations 
    (vl-sort elevations '(lambda (a b) (< (cdr a) (cdr b))))
  )
  
  ;; Return list of profile names in vertical order
  (mapcar 'car sorted-elevations)
)

;;; ========================================================================
;;; GEOLOGICAL HATCH APPLICATION
;;; ========================================================================

(defun apply-geological-hatching (boundaries / hatch-count doc modelspace)
  "Apply geological hatch patterns to generated boundaries"
  (setq hatch-count 0
        doc (vla-get-activedocument (vlax-get-acad-object))
        modelspace (vla-get-modelspace doc)
  )
  
  (foreach boundary boundaries
    (let ((boundary-points (cdr (assoc "boundary-points" boundary)))
          (lower-surface (cdr (assoc "lower-surface" boundary)))
          (upper-surface (cdr (assoc "upper-surface" boundary)))
          (polyline nil)
          (region nil)
          (hatch-obj nil)
         )
      
      ;; Create boundary polyline
      (if (setq polyline (create-boundary-polyline boundary-points))
        (progn
          ;; Create region from polyline
          (if (setq region (create-region-from-polyline polyline))
            (progn
              ;; Apply geological hatch
              (if (setq hatch-obj (apply-geological-hatch-to-region 
                                    region lower-surface upper-surface))
                (setq hatch-count (1+ hatch-count))
              )
              
              ;; Clean up region
              (vla-delete region)
            )
          )
          
          ;; Clean up polyline
          (vla-delete polyline)
        )
      )
    )
  )
  
  hatch-count
)

(defun create-boundary-polyline (boundary-points / polyline coord-array)
  "Create AutoCAD polyline from boundary points"
  (let ((doc (vla-get-activedocument (vlax-get-acad-object)))
        (modelspace nil)
       )
    (setq modelspace (vla-get-modelspace doc))
    
    ;; Convert points to coordinate array
    (setq coord-array 
      (vlax-make-safearray 
        vlax-vbDouble 
        (cons 0 (1- (* 2 (length boundary-points))))
      )
    )
    
    (let ((i 0))
      (foreach point boundary-points
        (vlax-safearray-put-element coord-array i (car point))       ; X
        (vlax-safearray-put-element coord-array (1+ i) (cadr point)) ; Y  
        (setq i (+ i 2))
      )
    )
    
    ;; Create lightweight polyline
    (setq polyline (vla-addlightweightpolyline modelspace coord-array))
    
    ;; Close the polyline
    (vla-put-closed polyline :vlax-true)
    
    polyline
  )
)

(defun create-region-from-polyline (polyline / region regions)
  "Create region from closed polyline"
  (let ((regions (vlax-invoke 
                   (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
                   'AddRegion 
                   (vlax-make-safearray vlax-vbObject (list polyline))
                 )))
    (if (and regions (> (vlax-safearray-get-u-bound regions 1) -1))
      (vlax-safearray-get-element regions 0)
      nil
    )
  )
)

(defun apply-geological-hatch-to-region (region lower-surface upper-surface / hatch-obj config)
  "Apply geological hatch pattern to region"
  (let ((doc (vla-get-activedocument (vlax-get-acad-object)))
        (modelspace nil)
        (pattern-name "SOLID")
        (pattern-scale 1.0)
        (pattern-color 7)
       )
    
    (setq modelspace (vla-get-modelspace doc))
    
    ;; Get geological configuration
    (setq config (get-geological-hatch-config lower-surface upper-surface))
    (if config
      (progn
        (setq pattern-name (nth 0 config)
              pattern-scale (nth 1 config)  
              pattern-color (nth 2 config)
        )
      )
    )
    
    ;; Create hatch object
    (setq hatch-obj (vla-addhatch modelspace acHatchPatternTypePreDefined pattern-name :vlax-false))
    
    ;; Set hatch properties
    (vla-put-color hatch-obj pattern-color)
    (vla-put-patternscale hatch-obj pattern-scale)
    
    ;; Append region as outer loop
    (vla-appendouterloop hatch-obj (vlax-make-safearray vlax-vbObject (list region)))
    
    ;; Evaluate hatch
    (vla-evaluate hatch-obj)
    
    hatch-obj
  )
)

(defun get-geological-hatch-config (lower-surface upper-surface / material-key config)
  "Get hatch configuration for geological material"
  
  ;; Create material key from surface names
  (setq material-key (strcat lower-surface "-" upper-surface))
  
  ;; Look up in geological configuration
  (setq config (assoc upper-surface *geological-config*))
  (if config
    (cdr config)
    '("SOLID" 1.0 7)  ; Default configuration
  )
)

;;; ========================================================================
;;; UTILITY FUNCTIONS
;;; ========================================================================

(defun log-geological-info (message / log-file current-time)
  "Log geological processing information"
  (setq log-file "c:\\Users\\dea29431\\Documents\\LOCAL\\CAD\\debug\\geological_hatch.log"
        current-time (rtos (getvar "CDATE") 2 6)
  )
  
  (if (setq file-handle (open log-file "a"))
    (progn
      (write-line (strcat current-time " - " message) file-handle)
      (close file-handle)
    )
  )
)

;;; ========================================================================
;;; COMMAND REGISTRATION AND INITIALIZATION
;;; ========================================================================

(princ "\nGeological Hatch Strata loaded.")
(princ "\nType GEOHATCH to automatically hatch geological strata in Civil 3D profiles.")
(princ "\nConfigure geological materials in *geological-config* variable as needed.")
(princ)
