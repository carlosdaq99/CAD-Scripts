# Civil 3D COM Object Feasibility Test
# Test script to determine if Civil 3D profile objects are accessible from AutoLISP

import os
import re

# Test script for Civil 3D COM object investigation
civil3d_test_script = """
;;; ========================================================================
;;; CIVIL 3D COM OBJECT FEASIBILITY TEST
;;; ========================================================================
;;; Purpose: Determine if Civil 3D AeccDb* objects are accessible from AutoLISP
;;; Environment: Civil 3D 2024 (AutoCAD 2018+ compatible)
;;; Date: July 27, 2025

(vl-load-com)

(defun C:TestCivil3DAccess ( / doc app civil3d-app profile-views result )
  "Test access to Civil 3D COM objects for profile data extraction"
  
  ;; Test 1: Basic Civil 3D Application Access
  (princ "\\n=== Civil 3D COM Object Feasibility Test ===\\n")
  (setq result (list))
  
  ;; Try to get Civil 3D application object
  (if (setq app (vlax-get-acad-object))
    (progn
      (princ "✓ AutoCAD Application Object: SUCCESS\\n")
      (setq result (cons "AutoCAD App: SUCCESS" result))
      
      ;; Test Civil 3D Document Access
      (if (setq doc (vla-get-activedocument app))
        (progn
          (princ "✓ Active Document: SUCCESS\\n")
          (setq result (cons "Active Document: SUCCESS" result))
          
          ;; Test Civil 3D specific objects - THIS IS THE CRITICAL TEST
          (setq civil3d-access-attempts
            (list
              ;; Attempt 1: Direct property access
              '(vlax-property-available-p doc 'ProfileViews)
              ;; Attempt 2: Method call approach  
              '(vlax-method-applicable-p doc 'GetProfileViews)
              ;; Attempt 3: Collection enumeration
              '(vlax-get-property doc 'ModelSpace)
            )
          )
          
          ;; Test each Civil 3D access method
          (foreach test civil3d-access-attempts
            (if (vl-catch-all-error-p (setq test-result (vl-catch-all-apply 'eval (list test))))
              (princ (strcat "✗ " (vl-princ-to-string test) ": FAILED\\n"))
              (if test-result
                (princ (strcat "✓ " (vl-princ-to-string test) ": SUCCESS\\n"))
                (princ (strcat "- " (vl-princ-to-string test) ": NOT AVAILABLE\\n"))
              )
            )
          )
        )
        (progn
          (princ "✗ Active Document: FAILED\\n")
          (setq result (cons "Active Document: FAILED" result))
        )
      )
    )
    (progn
      (princ "✗ AutoCAD Application Object: FAILED\\n")
      (setq result (cons "AutoCAD App: FAILED" result))
    )
  )
  
  ;; Test 2: Profile Object Enumeration
  (princ "\\n--- Testing Profile Object Detection ---\\n")
  
  ;; Try to enumerate all objects in ModelSpace to find profiles
  (if doc
    (progn
      (setq modelspace (vla-get-modelspace doc))
      (setq profile-count 0)
      (setq surface-count 0)
      
      ;; Iterate through all objects looking for Civil 3D types
      (vlax-for obj modelspace
        (setq obj-type (vla-get-objectname obj))
        (cond
          ;; Look for Civil 3D profile objects
          ((wcmatch (strcase obj-type) "*PROFILE*")
            (setq profile-count (1+ profile-count))
            (princ (strcat "Found Profile Object: " obj-type "\\n"))
          )
          ;; Look for Civil 3D surface objects  
          ((wcmatch (strcase obj-type) "*SURFACE*")
            (setq surface-count (1+ surface-count))
            (princ (strcat "Found Surface Object: " obj-type "\\n"))
          )
          ;; Look for any AeccDb objects
          ((wcmatch (strcase obj-type) "AECCDB*")
            (princ (strcat "Found Civil 3D Object: " obj-type "\\n"))
          )
        )
      )
      
      (princ (strcat "\\nProfile objects found: " (itoa profile-count) "\\n"))
      (princ (strcat "Surface objects found: " (itoa surface-count) "\\n"))
      
      (setq result (cons (strcat "Profiles: " (itoa profile-count)) result))
      (setq result (cons (strcat "Surfaces: " (itoa surface-count)) result))
    )
  )
  
  ;; Test 3: Object Introspection Pattern (Lee Mac Style)
  (princ "\\n--- Object Introspection Test ---\\n")
  
  ;; Try to create Civil 3D-specific COM objects
  (setq com-tests
    (list
      "AeccXUiLandDesktop.AeccApplication"
      "AeccXUiLandDesktop.AeccDocument" 
      "Civil3D.Application"
      "Autodesk.Civil.Application"
    )
  )
  
  (foreach com-id com-tests
    (if (vl-catch-all-error-p 
          (setq com-obj (vl-catch-all-apply 'vlax-create-object (list com-id))))
      (princ (strcat "✗ " com-id ": NOT AVAILABLE\\n"))
      (progn
        (princ (strcat "✓ " com-id ": SUCCESS\\n"))
        (vlax-release-object com-obj)
        (setq result (cons (strcat "COM " com-id ": SUCCESS") result))
      )
    )
  )
  
  ;; Summary Report
  (princ "\\n=== FEASIBILITY TEST SUMMARY ===\\n")
  (foreach item (reverse result)
    (princ (strcat item "\\n"))
  )
  
  ;; Critical Assessment
  (princ "\\n=== CRITICAL ASSESSMENT ===\\n")
  (if (> profile-count 0)
    (princ "✓ RECOMMENDATION: Direct Civil 3D COM integration FEASIBLE\\n")
    (progn
      (princ "⚠ WARNING: No Civil 3D profiles detected\\n")
      (princ "✓ RECOMMENDATION: Profile Export + Lee Mac Geometric Analysis\\n")
    )
  )
  
  (princ "\\nTest completed. Check command line output for detailed results.\\n")
  (princ)
)

;; Helper function for object property listing (Lee Mac pattern)
(defun ListObjectProperties ( obj / props )
  "List all available properties for a VLA object"
  (if (= 'vla-object (type obj))
    (progn
      (princ "\\nObject Properties:\\n")
      (vlax-dump-object obj T)
    )
    (princ "\\nError: Not a VLA object\\n")
  )
  (princ)
)

;; Helper function for method listing
(defun ListObjectMethods ( obj )
  "List all available methods for a VLA object"  
  (if (= 'vla-object (type obj))
    (progn
      (princ "\\nObject Methods:\\n")
      (vlax-dump-object obj)
    )
    (princ "\\nError: Not a VLA object\\n")
  )
  (princ)
)

;;; USAGE INSTRUCTIONS:
;;; 1. Load this script in Civil 3D (APPLOAD or drag-and-drop)
;;; 2. Type TestCivil3DAccess in command line
;;; 3. Review output to determine feasibility of Civil 3D COM integration
;;; 4. Use results to select optimal implementation approach

(princ "\\nCivil 3D COM Feasibility Test loaded. Type TestCivil3DAccess to run.")
(princ)
"""

# Write the test script
script_path = "c:/Users/dea29431/Documents/LOCAL/CAD/test_civil3d_com_access.lsp"
with open(script_path, "w", encoding="utf-8") as f:
    f.write(civil3d_test_script)

print("✓ Civil 3D COM feasibility test script created")
print(f"Location: {script_path}")
print("\nScript Purpose:")
print("- Test if Civil 3D AeccDb* objects are accessible from AutoLISP")
print("- Enumerate existing profiles and surfaces in current drawing")
print("- Validate COM object creation for Civil 3D integration")
print("- Provide implementation approach recommendation")
print("\nUsage:")
print("1. Load script in Civil 3D: APPLOAD or drag-and-drop")
print("2. Run command: TestCivil3DAccess")
print("3. Review output for technical feasibility assessment")

# Also analyze the technical challenges
print("\n" + "=" * 60)
print("TECHNICAL CHALLENGE ANALYSIS")
print("=" * 60)

challenges = {
    "Civil 3D COM Integration": {
        "risk": "HIGH",
        "factors": [
            "AeccDb* objects may not be accessible from AutoLISP",
            "Civil 3D API documentation limited for AutoLISP",
            "COM object lifecycle management complexity",
            "Version compatibility across Civil 3D releases",
        ],
    },
    "Profile Export Approach": {
        "risk": "MEDIUM",
        "factors": [
            "Manual profile-to-polyline conversion required",
            "Potential loss of Civil 3D metadata",
            "Complex geometric analysis for crossing detection",
            "Performance implications for large datasets",
        ],
    },
    "Geometric Crossing Detection": {
        "risk": "MEDIUM",
        "factors": [
            "Station-by-station elevation comparison algorithm",
            "Fuzzy tolerance handling for geological precision",
            "Dynamic vertical ordering when profiles cross",
            "Boundary generation for complex hatch regions",
        ],
    },
    "Integration with Existing Code": {
        "risk": "LOW",
        "factors": [
            "Proven VLA hatching framework exists",
            "Lee Mac geometric libraries available",
            "Error handling patterns established",
            "Logging infrastructure already implemented",
        ],
    },
}

for challenge, details in challenges.items():
    print(f"\n{challenge} (Risk: {details['risk']}):")
    for factor in details["factors"]:
        print(f"  • {factor}")

print(f"\n{'='*60}")
print("RECOMMENDATION: Run feasibility test to determine optimal approach")
print("Critical Path: Civil 3D COM object accessibility validation")
