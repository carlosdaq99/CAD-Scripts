;;; ========================================================================
;;; GEOLOGICAL HATCH STRATA - CIVIL 3D COM TESTING FRAMEWORK
;;; ========================================================================
;;; 
;;; Description:
;;;   Comprehensive testing framework for Civil 3D COM automation
;;;   Used to validate GeologicalHatchStrata.lsp functionality
;;;
;;; Commands: 
;;;   TESTCOM      - Test basic Civil 3D COM connectivity
;;;   TESTPROFILES - Test profile detection and enumeration
;;;   TESTGEO      - Test geological processing algorithms
;;;
;;; Author: GitHub Copilot
;;; Date: July 29, 2025
;;; Version: 1.0
;;; ========================================================================

(vl-load-com)

;;; ========================================================================
;;; BASIC COM CONNECTIVITY TESTING
;;; ========================================================================

(defun C:TESTCOM ( / app doc modelspace obj-count civil-objects)
  "Test basic Civil 3D COM connectivity and object access"
  
  (princ "\n=== CIVIL 3D COM CONNECTIVITY TEST ===")
  
  ;; Test 1: Basic AutoCAD COM objects
  (if (setq app (vlax-get-acad-object))
    (progn
      (princ "\n✓ AutoCAD Application object: OK")
      (setq doc (vla-get-activedocument app))
      (princ "\n✓ Active Document object: OK")
      (setq modelspace (vla-get-modelspace doc))
      (princ "\n✓ ModelSpace object: OK")
    )
    (progn
      (princ "\n✗ Failed to get AutoCAD Application object")
      (princ)
      (exit)
    )
  )
  
  ;; Test 2: Count objects in modelspace
  (setq obj-count 0)
  (vlax-for obj modelspace
    (setq obj-count (1+ obj-count))
  )
  (princ (strcat "\n✓ ModelSpace contains " (itoa obj-count) " objects"))
  
  ;; Test 3: Search for Civil 3D objects
  (setq civil-objects '())
  (vlax-for obj modelspace
    (if (vlax-property-available-p obj 'ObjectName)
      (let ((obj-name (vlax-get obj 'ObjectName)))
        (if (or (wcmatch (strcase obj-name) "*AECC*")
                (wcmatch (strcase obj-name) "*CIVIL*")
                (wcmatch (strcase obj-name) "*PROFILE*")
                (wcmatch (strcase obj-name) "*SURFACE*")
           )
          (setq civil-objects (cons (list obj obj-name) civil-objects))
        )
      )
    )
  )
  
  (if civil-objects
    (progn
      (princ (strcat "\n✓ Found " (itoa (length civil-objects)) " Civil 3D objects:"))
      (foreach civil-obj civil-objects
        (princ (strcat "\n  - " (cadr civil-obj)))
      )
    )
    (princ "\n⚠ No Civil 3D objects found in current drawing")
  )
  
  ;; Test 4: Civil 3D Document properties
  (if (vlax-property-available-p doc 'Application)
    (let ((app-name (vlax-get (vlax-get doc 'Application) 'Name)))
      (princ (strcat "\n✓ Application: " app-name))
      (if (wcmatch (strcase app-name) "*CIVIL*")
        (princ "\n✓ Civil 3D Application detected")
        (princ "\n⚠ Standard AutoCAD Application (Civil 3D may not be loaded)")
      )
    )
  )
  
  (princ "\n=== COM CONNECTIVITY TEST COMPLETE ===")
  (princ)
)

;;; ========================================================================
;;; PROFILE DETECTION AND ENUMERATION TESTING
;;; ========================================================================

(defun C:TESTPROFILES ( / app doc modelspace profile-view profiles)
  "Test Civil 3D profile detection and enumeration"
  
  (princ "\n=== CIVIL 3D PROFILE DETECTION TEST ===")
  
  ;; Initialize COM objects
  (if (setq app (vlax-get-acad-object))
    (setq doc (vla-get-activedocument app)
          modelspace (vla-get-modelspace doc)
    )
    (progn
      (princ "\n✗ Failed to initialize COM objects")
      (princ)
      (exit)
    )
  )
  
  ;; Test 1: Profile View Detection
  (princ "\nSearching for Profile Views...")
  (setq profile-view (test-detect-profile-view doc))
  
  (if profile-view
    (progn
      (princ "\n✓ Profile View found and accessible")
      (test-dump-profile-view-properties profile-view)
      
      ;; Test 2: Profile Enumeration
      (princ "\nEnumerating Surface Profiles...")
      (setq profiles (test-enumerate-surface-profiles profile-view))
      
      (if profiles
        (progn
          (princ (strcat "\n✓ Found " (itoa (length profiles)) " surface profiles"))
          (test-dump-profile-details profiles)
          
          ;; Test 3: Station and Elevation Extraction
          (princ "\nTesting elevation extraction...")
          (test-elevation-extraction profiles)
        )
        (princ "\n⚠ No surface profiles found")
      )
    )
    (princ "\n⚠ No Profile View found in current drawing")
  )
  
  (princ "\n=== PROFILE DETECTION TEST COMPLETE ===")
  (princ)
)

(defun test-detect-profile-view (doc / modelspace profile-view obj-count)
  "Test profile view detection with detailed diagnostics"
  (setq modelspace (vla-get-modelspace doc)
        profile-view nil
        obj-count 0
  )
  
  (princ "\nScanning ModelSpace objects...")
  (vlax-for obj modelspace
    (setq obj-count (1+ obj-count))
    (if (vlax-property-available-p obj 'ObjectName)
      (let ((obj-name (vlax-get obj 'ObjectName)))
        (princ (strcat "\n  Object " (itoa obj-count) ": " obj-name))
        
        ;; Check for profile view patterns
        (if (or (wcmatch (strcase obj-name) "*PROFILE*VIEW*")
                (wcmatch (strcase obj-name) "*AECC*PROFILE*VIEW*")
           )
          (progn
            (princ " [PROFILE VIEW CANDIDATE]")
            (setq profile-view obj)
          )
        )
      )
      (princ (strcat "\n  Object " (itoa obj-count) ": [No ObjectName property]"))
    )
  )
  
  profile-view
)

(defun test-dump-profile-view-properties (profile-view)
  "Dump all available properties of profile view"
  (princ "\nProfile View Properties:")
  
  ;; Common properties to check
  (let ((properties '("Name" "Description" "ObjectName" "Profiles" 
                     "StationStart" "StationEnd" "ElevationMin" "ElevationMax")))
    (foreach prop properties
      (if (vlax-property-available-p profile-view prop)
        (progn
          (princ (strcat "\n  " prop ": "))
          (princ (vlax-get profile-view prop))
        )
      )
    )
  )
  
  ;; Check for profile collection
  (if (vlax-property-available-p profile-view 'Profiles)
    (let ((profile-collection (vlax-get profile-view 'Profiles)))
      (if profile-collection
        (princ (strcat "\n  Profile Collection: " (vlax-get profile-collection 'Count) " profiles"))
        (princ "\n  Profile Collection: NULL")
      )
    )
  )
)

(defun test-enumerate-surface-profiles (profile-view)
  "Test surface profile enumeration with multiple approaches"
  (let ((profiles '())
        (approach-used nil))
    
    ;; Approach 1: Direct Profiles property
    (if (and (vlax-property-available-p profile-view 'Profiles)
             (null profiles))
      (progn
        (princ "\nTrying Approach 1: Direct Profiles property...")
        (let ((profile-collection (vlax-get profile-view 'Profiles)))
          (if profile-collection
            (progn
              (vlax-for profile profile-collection
                (princ (strcat "\n  Found profile: " (vlax-get profile 'ObjectName)))
                (if (test-is-surface-profile profile)
                  (setq profiles (cons profile profiles))
                )
              )
              (setq approach-used "Direct Profiles property")
            )
          )
        )
      )
    )
    
    ;; Approach 2: GetProfiles method
    (if (and (vlax-method-applicable-p profile-view 'GetProfiles)
             (null profiles))
      (progn
        (princ "\nTrying Approach 2: GetProfiles method...")
        (let ((profile-collection (vlax-invoke profile-view 'GetProfiles)))
          (if profile-collection
            (progn
              (vlax-for profile profile-collection
                (princ (strcat "\n  Found profile: " (vlax-get profile 'ObjectName)))
                (if (test-is-surface-profile profile)
                  (setq profiles (cons profile profiles))
                )
              )
              (setq approach-used "GetProfiles method")
            )
          )
        )
      )
    )
    
    ;; Approach 3: Search modelspace
    (if (null profiles)
      (progn
        (princ "\nTrying Approach 3: ModelSpace search...")
        (setq profiles (test-find-profiles-by-search))
        (if profiles
          (setq approach-used "ModelSpace search")
        )
      )
    )
    
    (if approach-used
      (princ (strcat "\n✓ Used approach: " approach-used))
    )
    
    profiles
  )
)

(defun test-is-surface-profile (profile)
  "Test if profile is a surface profile"
  (and (vlax-property-available-p profile 'ObjectName)
       (let ((obj-name (vlax-get profile 'ObjectName)))
         (or (wcmatch (strcase obj-name) "*SURFACE*PROFILE*")
             (wcmatch (strcase obj-name) "*AECC*SURFACE*PROFILE*")
         )
       )
  )
)

(defun test-find-profiles-by-search ( / doc modelspace profiles)
  "Search modelspace for profile objects"
  (setq doc (vla-get-activedocument (vlax-get-acad-object))
        modelspace (vla-get-modelspace doc)
        profiles '()
  )
  
  (vlax-for obj modelspace
    (if (vlax-property-available-p obj 'ObjectName)
      (let ((obj-name (vlax-get obj 'ObjectName)))
        (if (and (wcmatch (strcase obj-name) "*PROFILE*")
                 (not (wcmatch (strcase obj-name) "*VIEW*")))
          (progn
            (princ (strcat "\n  Found profile: " obj-name))
            (setq profiles (cons obj profiles))
          )
        )
      )
    )
  )
  
  profiles
)

(defun test-dump-profile-details (profiles)
  "Dump detailed information about each profile"
  (princ "\nProfile Details:")
  
  (let ((i 1))
    (foreach profile profiles
      (princ (strcat "\n--- Profile " (itoa i) " ---"))
      
      ;; Basic properties
      (let ((properties '("Name" "Description" "ObjectName" "Layer")))
        (foreach prop properties
          (if (vlax-property-available-p profile prop)
            (princ (strcat "\n  " prop ": " (vlax-get profile prop)))
          )
        )
      )
      
      ;; Station range
      (let ((station-range (test-get-profile-station-range profile)))
        (if station-range
          (princ (strcat "\n  Station Range: " 
                        (rtos (car station-range) 2 2) 
                        " to " 
                        (rtos (cadr station-range) 2 2)))
        )
      )
      
      (setq i (1+ i))
    )
  )
)

(defun test-get-profile-station-range (profile)
  "Test station range extraction"
  (cond
    ;; Method 1: Direct properties
    ((and (vlax-property-available-p profile 'StartStation)
          (vlax-property-available-p profile 'EndStation))
      (list (vlax-get profile 'StartStation)
            (vlax-get profile 'EndStation))
    )
    
    ;; Method 2: Bounding box
    ((vlax-method-applicable-p profile 'GetBoundingBox)
      (let ((min-pt (vlax-3d-point)) (max-pt (vlax-3d-point)))
        (vlax-invoke profile 'GetBoundingBox min-pt max-pt)
        (list (vlax-get min-pt 'X) (vlax-get max-pt 'X))
      )
    )
    
    ;; Method 3: Default
    (t '(0.0 100.0))
  )
)

(defun test-elevation-extraction (profiles)
  "Test elevation extraction at sample stations"
  (princ "\nTesting elevation extraction:")
  
  (let ((test-stations '(0.0 25.0 50.0 75.0 100.0)))
    (foreach station test-stations
      (princ (strcat "\n--- Station " (rtos station 2 1) " ---"))
      
      (foreach profile profiles
        (let ((profile-name (if (vlax-property-available-p profile 'Name)
                              (vlax-get profile 'Name)
                              "Unknown"))
              (elevation (test-get-elevation-at-station profile station)))
          (if elevation
            (princ (strcat "\n  " profile-name ": " (rtos elevation 2 3)))
            (princ (strcat "\n  " profile-name ": [No elevation]"))
          )
        )
      )
    )
  )
)

(defun test-get-elevation-at-station (profile station)
  "Test elevation extraction with multiple methods"
  (let ((elevation nil))
    
    ;; Method 1: ElevationAt
    (if (and (null elevation) (vlax-method-applicable-p profile 'ElevationAt))
      (progn
        (princ (strcat " [Method: ElevationAt]"))
        (setq elevation (vlax-invoke profile 'ElevationAt station))
      )
    )
    
    ;; Method 2: PointAt
    (if (and (null elevation) (vlax-method-applicable-p profile 'PointAt))
      (progn
        (princ (strcat " [Method: PointAt]"))
        (let ((point (vlax-invoke profile 'PointAt station)))
          (if point
            (setq elevation (vlax-get point 'Y))
          )
        )
      )
    )
    
    ;; Method 3: Curve methods
    (if (and (null elevation) (vlax-curve-getpointatdist profile station))
      (progn
        (princ (strcat " [Method: vlax-curve]"))
        (let ((point (vlax-curve-getpointatdist profile station)))
          (if point
            (setq elevation (cadr point))
          )
        )
      )
    )
    
    elevation
  )
)

;;; ========================================================================
;;; GEOLOGICAL PROCESSING TESTING
;;; ========================================================================

(defun C:TESTGEO ( / test-data boundaries)
  "Test geological processing algorithms"
  
  (princ "\n=== GEOLOGICAL PROCESSING TEST ===")
  
  ;; Create sample elevation data
  (setq test-data (create-sample-elevation-data))
  (princ "\n✓ Created sample elevation data")
  
  ;; Test boundary generation
  (setq boundaries (test-generate-boundaries test-data))
  (if boundaries
    (princ (strcat "\n✓ Generated " (itoa (length boundaries)) " boundaries"))
    (princ "\n✗ Failed to generate boundaries")
  )
  
  ;; Test hatch configuration
  (test-geological-config)
  
  (princ "\n=== GEOLOGICAL PROCESSING TEST COMPLETE ===")
  (princ)
)

(defun create-sample-elevation-data ()
  "Create sample elevation data for testing"
  '(
    (0.0  . (("AL" . 110.0) ("KC" . 105.0) ("HD" . 100.0)))
    (25.0 . (("AL" . 108.0) ("KC" . 104.0) ("HD" . 99.0)))
    (50.0 . (("AL" . 106.0) ("KC" . 103.0) ("HD" . 98.0)))
    (75.0 . (("AL" . 104.0) ("KC" . 102.0) ("HD" . 97.0)))
    (100.0 . (("AL" . 102.0) ("KC" . 101.0) ("HD" . 96.0)))
  )
)

(defun test-generate-boundaries (station-data)
  "Test boundary generation algorithm"
  (princ "\nTesting boundary generation:")
  
  (let ((boundaries '())
        (prev-station nil))
    
    (foreach station-data-point station-data
      (if prev-station
        (let ((segment-boundaries (test-create-boundary-segments 
                                    prev-station 
                                    station-data-point)))
          (setq boundaries (append boundaries segment-boundaries))
          (princ (strcat "\n  Segment " 
                        (rtos (car prev-station) 2 1) 
                        " to " 
                        (rtos (car station-data-point) 2 1)
                        ": " 
                        (itoa (length segment-boundaries)) 
                        " boundaries"))
        )
      )
      (setq prev-station station-data-point)
    )
    
    boundaries
  )
)

(defun test-create-boundary-segments (station1-data station2-data)
  "Test boundary segment creation"
  (let ((station1 (car station1-data))
        (station2 (car station2-data))
        (elevs1 (cdr station1-data))
        (elevs2 (cdr station2-data))
        (order1 (test-get-vertical-order elevs1))
        (order2 (test-get-vertical-order elevs2))
        (segments '())
       )
    
    ;; Create boundaries between adjacent profiles
    (let ((i 0))
      (while (< i (1- (length order1)))
        (let ((lower-profile (nth i order1))
              (upper-profile (nth (1+ i) order1))
              (boundary-points nil)
             )
          
          (setq boundary-points
            (list
              (list station1 (cdr (assoc lower-profile elevs1)))
              (list station2 (cdr (assoc lower-profile elevs2)))
              (list station2 (cdr (assoc upper-profile elevs2)))
              (list station1 (cdr (assoc upper-profile elevs1)))
            )
          )
          
          (setq segments 
            (cons (list
                    (cons "lower-surface" lower-profile)
                    (cons "upper-surface" upper-profile)
                    (cons "boundary-points" boundary-points)
                  ) 
                  segments)
          )
          
          (setq i (1+ i))
        )
      )
    )
    
    (reverse segments)
  )
)

(defun test-get-vertical-order (elevations)
  "Test vertical ordering of profiles"
  (let ((sorted-elevations 
          (vl-sort elevations '(lambda (a b) (< (cdr a) (cdr b))))))
    (mapcar 'car sorted-elevations)
  )
)

(defun test-geological-config ()
  "Test geological configuration system"
  (princ "\nTesting geological configuration:")
  
  (let ((test-surfaces '("AL" "KC" "HD" "CG" "RTD")))
    (foreach surface test-surfaces
      (let ((config (assoc surface *geological-config*)))
        (if config
          (princ (strcat "\n  " surface ": " 
                        (nth 0 (cdr config)) " / " 
                        (rtos (nth 1 (cdr config)) 2 1) " / " 
                        (itoa (nth 2 (cdr config))) " / " 
                        (nth 3 (cdr config))))
          (princ (strcat "\n  " surface ": [No configuration]"))
        )
      )
    )
  )
)

;;; ========================================================================
;;; INITIALIZATION
;;; ========================================================================

(princ "\nCivil 3D COM Testing Framework loaded.")
(princ "\nCommands available:")
(princ "\n  TESTCOM      - Test basic COM connectivity")
(princ "\n  TESTPROFILES - Test profile detection")
(princ "\n  TESTGEO      - Test geological algorithms")
(princ)
