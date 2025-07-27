# AutoLISP Function List Export - Progress Summary

## Objective
Export a complete list of available AutoLISP functions in the current Civil 3D/AutoCAD environment to a text file for reference and troubleshooting.

---

## Current Code (save_functions.lsp)
```lisp
(defun C:SaveFunctions ()
  (setq log-file "c:/Users/dea29431/Documents/LOCAL/CAD/debug/all_functions.txt")
  (if (setq file-handle (open log-file "w"))
    (progn
      (foreach sym (atoms-family 1)
        (if (and (= (type sym) 'ATOM) (= (type (eval sym)) 'SUBR))
          (write-line (vl-symbol-name sym) file-handle)
        )
      )
      (close file-handle)
      (princ (strcat "\nFunction list saved to: " log-file))
    )
    (princ "\nERROR: Could not open file for writing.")
  )
  (princ)
)
```

---

## Testing Done
1. **File writing tested:**
   - Manual test with `(write-line "test" f)` succeeded; file permissions and path are correct.
2. **Function list export tested:**
   - `SAVEFUNCTIONS` command runs with no errors.
   - Message: `Function list saved to: c:/Users/dea29431/Documents/LOCAL/CAD/debug/all_functions.txt` appears.
   - Output file is created but remains empty.
3. **Symbol count tested:**
   - `(length (atoms-family 1))` returns 4611, confirming symbols are present.
4. **Manual write-line test:**
   - Attempting to write a single symbol from `atoms-family` fails due to type issues.
   - Adjusted code to use `ATOM` type instead of `symbolp` (not available in this environment).

---

## Results
- File writing works for manual strings.
- Export function runs without errors but does not write any function names to the file.
- No error messages are shown in the command line or log.
- The output file is always empty after running the command.

---

## Problems & Analysis
- **Type detection:**
  - `atoms-family` returns a mix of strings and symbols/atoms; not all are valid for `vl-symbol-name`.
  - The check `(= (type sym) 'ATOM)` may not be sufficient in this environment.
- **Function detection:**
  - The environment may not treat function symbols as `ATOM` or may require a different approach to filter valid function names.
- **No error feedback:**
  - The code does not produce errors, making debugging difficult.
- **AutoCAD/Civil 3D LISP limitations:**
  - Some standard LISP predicates (like `symbolp`) are missing, and type handling is inconsistent.

---

## Next Steps & Recommendations
- Investigate alternative ways to filter valid function symbols in this environment (e.g., try `(type sym)` values, or use `eval` and check for `'SUBR`).
- Test writing all items from `atoms-family` to the file to inspect their types and contents.
- Consider using `(atoms-family)` with no arguments or with `0` to see if results differ.
- Consult AutoCAD LISP documentation for environment-specific symbol handling.
- If possible, try on a different version of AutoCAD or with a different LISP interpreter for comparison.

---

## Summary
Despite correct file writing and symbol enumeration, the current approach does not export function names as intended due to environment-specific type handling. Further investigation into symbol filtering and AutoCAD LISP limitations is required.
