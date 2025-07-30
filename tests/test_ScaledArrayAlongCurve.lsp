;;; ========================================================================
;;; TEST SCRIPT FOR SCALEDARRAYALONGCURVE.LSP
;;; ========================================================================
;; This script tests syntax, imports, and command registration for ScaledArrayAlongCurve.lsp
;; Place in the tests/ folder as per project instructions

(defun test-ScaledArrayAlongCurve-imports ()
  (princ "\nTesting imports and syntax for ScaledArrayAlongCurve.lsp...")
  (if (and (fboundp 'C:ScaledArrayAlongCurve)
           (fboundp 'validate-curve-object)
           (fboundp 'copy-and-scale-object)
           (fboundp 'log-debug))
    (princ "\nAll required functions are defined.")
    (princ "\nERROR: One or more required functions are missing.")
  )
  (princ)
)

(defun test-ScaledArrayAlongCurve-command ()
  (princ "\nTesting command registration...")
  (if (fboundp 'C:ScaledArrayAlongCurve)
    (princ "\nCommand ScaledArrayAlongCurve is registered.")
    (princ "\nERROR: Command ScaledArrayAlongCurve is NOT registered.")
  )
  (princ)
)

(defun C:test_ScaledArrayAlongCurve ()
  (test-ScaledArrayAlongCurve-imports)
  (test-ScaledArrayAlongCurve-command)
  (princ "\nTest complete.")
  (princ)
)

(princ "\nType test_ScaledArrayAlongCurve to run basic tests for ScaledArrayAlongCurve.lsp.")
(princ)
