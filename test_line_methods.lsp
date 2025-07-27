; test_line_methods.lsp
; Command: testlinemethods
; Draws lines using different methods to diagnose correct AutoLISP usage
; Author: GitHub Copilot

(defun c:testlinemethods (/ pt1 pt2)
  (prompt "\n[testlinemethods] Starting line method tests...")
  ;; Method 1: Pass points as lists
  (setq pt1 '(0 0 0) pt2 '(10 10 0))
  (prompt (strcat "\n[Method 1] (command '_.LINE' pt1 pt2 '') args: " (vl-prin1-to-string pt1) ", " (vl-prin1-to-string pt2)))
  (command "_.LINE" pt1 pt2 "")

  ;; Method 2: Pass unpacked coordinates
  (prompt (strcat "\n[Method 2] (command '_.LINE' x1 y1 z1 x2 y2 z2 '') args: "
    (rtos (car pt1) 2 2) ", " (rtos (cadr pt1) 2 2) ", " (rtos (caddr pt1) 2 2) ", "
    (rtos (car pt2) 2 2) ", " (rtos (cadr pt2) 2 2) ", " (rtos (caddr pt2) 2 2)))
  (command "_.LINE" (car pt1) (cadr pt1) (caddr pt1) (car pt2) (cadr pt2) (caddr pt2) "")

  ;; Method 3: Use hardcoded numbers
  (prompt "\n[Method 3] (command '_.LINE' 20 0 0 30 10 0 '')")
  (command "_.LINE" 20 0 0 30 10 0 "")

  ;; Method 4: Use 2D points (omit z)
  (prompt "\n[Method 4] (command '_.LINE' 40 0 50 10 '')")
  (command "_.LINE" 40 0 50 10 "")

  ;; Method 5: Use variables for coordinates
  (setq x1 60 y1 0 z1 0 x2 70 y2 10 z2 0)
  (prompt (strcat "\n[Method 5] (command '_.LINE' x1 y1 z1 x2 y2 z2 '') args: "
    (rtos x1 2 2) ", " (rtos y1 2 2) ", " (rtos z1 2 2) ", "
    (rtos x2 2 2) ", " (rtos y2 2 2) ", " (rtos z2 2 2)))
  (command "_.LINE" x1 y1 z1 x2 y2 z2 "")

  (prompt "\n[testlinemethods] Line method tests complete.")
  (princ)
)

(princ "\ntest_line_methods.lsp loaded. Type testlinemethods to run.\n")
