;;; Comprehensive test and information script for Civil 3D 2024
;;; Run this first to verify environment and script compatibility

(princ "\n=== CIVIL 3D 2024 ENVIRONMENT TEST ===")

;; Test AutoLISP environment
(princ "\n1. AutoLISP Environment:")
(princ (strcat "\n   Version: " (getvar "ACADVER")))
(princ (strcat "\n   Product: " (getvar "PRODUCT")))

;; Test ActiveX/COM support
(princ "\n\n2. ActiveX/COM Support:")
(if (and (= (type vl-load-com) 'SUBR) (vl-load-com))
  (princ "\n   ✓ ActiveX/COM loaded successfully")
  (princ "\n   ✗ ActiveX/COM loading failed")
)

;; Test vlax-curve functions
(princ "\n\n3. vlax-curve Functions:")
(setq vlax-functions '(vlax-curve-getEndParam vlax-curve-getDistAtParam 
                       vlax-curve-getPointAtDist vlax-curve-getParamAtDist 
                       vlax-curve-get1stDeriv))
(foreach func vlax-functions
  (if (= (type (eval func)) 'SUBR)
    (princ (strcat "\n   ✓ " (vl-symbol-name func)))
    (princ (strcat "\n   ✗ " (vl-symbol-name func) " - MISSING"))
  )
)

;; Test logging directory
(princ "\n\n4. Logging Setup:")
(if (findfile "c:\\temp\\")
  (princ "\n   ✓ c:\\temp\\ directory exists for logging")
  (princ "\n   ✗ c:\\temp\\ directory missing - logs will not be saved")
)

;; Test script loading
(princ "\n\n5. Script Loading Test:")
(setq script-path (findfile "ScaleCopyAlongCurve.lsp"))
(if script-path
  (progn
    (princ (strcat "\n   ✓ ScaleCopyAlongCurve.lsp found at: " script-path))
    (if (vl-catch-all-error-p (vl-catch-all-apply 'load (list script-path)))
      (princ "\n   ✗ Script loading failed")
      (princ "\n   ✓ Script loaded successfully")
    )
  )
  (princ "\n   ✗ ScaleCopyAlongCurve.lsp not found in search path")
)

;; Recommendations
(princ "\n\n=== RECOMMENDATIONS ===")
(princ "\n• Ensure Civil 3D 2024 is fully updated")
(princ "\n• Place script file in a folder included in AutoCAD support file search paths")
(princ "\n• Test with simple geometry first (arc or polyline)")
(princ "\n• Check log file at c:\\temp\\scalecopy_debug.log after running")

(princ "\n\n=== TEST COMPLETE ===")
(princ)
