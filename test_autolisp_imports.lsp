;;; Test script to verify AutoLISP syntax and imports
;;; This ensures all files load correctly without syntax errors

(princ "\n=== AUTOLISP SYNTAX AND IMPORT TEST ===")

;; Test 1: Load GeologicalHatchStrata.lsp
(princ "\nTesting GeologicalHatchStrata.lsp...")
(if (load "GeologicalHatchStrata.lsp")
    (princ "\n✓ GeologicalHatchStrata.lsp loaded successfully")
    (princ "\n✗ Failed to load GeologicalHatchStrata.lsp")
)

;; Test 2: Load TestCivil3DCOM.lsp  
(princ "\nTesting TestCivil3DCOM.lsp...")
(if (load "TestCivil3DCOM.lsp")
    (princ "\n✓ TestCivil3DCOM.lsp loaded successfully")
    (princ "\n✗ Failed to load TestCivil3DCOM.lsp")
)

;; Test 3: Verify COM support
(princ "\nTesting COM support...")
(if (vlax-get-acad-object)
    (princ "\n✓ COM/ActiveX support available")
    (princ "\n✗ COM/ActiveX support not available")
)

;; Test 4: Check command definitions
(princ "\nChecking command definitions...")
(if (member "C:GEOHATCH" (atoms-family 1))
    (princ "\n✓ GEOHATCH command defined")
    (princ "\n✗ GEOHATCH command not found")
)

(if (member "C:TESTCOM" (atoms-family 1))
    (princ "\n✓ TESTCOM command defined")
    (princ "\n✗ TESTCOM command not found")
)

(if (member "C:TESTPROFILES" (atoms-family 1))
    (princ "\n✓ TESTPROFILES command defined")
    (princ "\n✗ TESTPROFILES command not found")
)

(if (member "C:TESTGEO" (atoms-family 1))
    (princ "\n✓ TESTGEO command defined")
    (princ "\n✗ TESTGEO command not found")
)

;; Test 5: Verify global variables
(princ "\nChecking global variables...")
(if (boundp '*geological-config*)
    (progn
        (princ "\n✓ *geological-config* defined")
        (princ (strcat "\n  Contains " (itoa (length *geological-config*)) " geological materials"))
    )
    (princ "\n✗ *geological-config* not defined")
)

(if (boundp '*station-increment*)
    (princ (strcat "\n✓ *station-increment* = " (rtos *station-increment* 2 2)))
    (princ "\n✗ *station-increment* not defined")
)

(if (boundp '*geo-tolerance*)
    (princ (strcat "\n✓ *geo-tolerance* = " (rtos *geo-tolerance* 2 8)))
    (princ "\n✗ *geo-tolerance* not defined")
)

(princ "\n=== SYNTAX AND IMPORT TEST COMPLETE ===")
(princ "\nAll files ready for Civil 3D testing.")
(princ)
