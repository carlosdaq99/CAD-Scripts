;;---------------------=={ MirrorArrayAlongCurve Test }==-------------------;;
;;  Automated test script for MirrorArrayAlongCurve.lsp                     ;;
;;  Verifies import, syntax, and basic command registration.                 ;;
;;--------------------------------------------------------------------------;;

;; Test: Load the main script
(load "MirrorArrayAlongCurve.lsp")

;; Test: Check command registration
(if (not (fboundp 'c:MirrorArrayAlongCurve))
  (prompt "\n[TEST FAIL] c:MirrorArrayAlongCurve not registered.")
  (prompt "\n[TEST PASS] c:MirrorArrayAlongCurve command is available.")
)

;; Test: Syntax check for main helpers
(foreach fn '(get-mirror-base-point mirror-entity process-selection-set)
  (if (not (fboundp fn))
    (prompt (strcat "\n[TEST FAIL] Function not found: " (vl-symbol-name fn)))
    (prompt (strcat "\n[TEST PASS] Function found: " (vl-symbol-name fn)))
  )
)

(prompt "\nMirrorArrayAlongCurve test script complete.\n")
