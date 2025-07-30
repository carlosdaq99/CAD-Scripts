# IteratedFunctionSystemsV1-2.lsp Line-by-Line Explanation

## Overview
This report provides a detailed, line-by-line explanation of the `IteratedFunctionSystemsV1-2.lsp` script by Lee Mac. The script implements an Iterated Function System (IFS) fractal generator for AutoCAD, using affine transformations and a DCL-based user interface. The code is professional-grade, modular, and leverages several advanced AutoLISP and Visual LISP techniques.

---

## Script Header and Metadata

```lisp
;;--------------=={ Iterated Function System }==--------------;;  ;;; Script title and separator
;;                                                            ;;  ;;; Blank line for readability
;;  Recursively calculates point positions using a set of two ;;  ;;; Brief description of script purpose
;;  or three affine linear transformations as specified in    ;;  ;;; More detail on the method
;;  the user interface; each with equal probability.          ;;  ;;; Explains random selection
;;                                                            ;;  ;;; Blank line
;;  Points are subsequently plotted in the specified colour   ;;  ;;; Explains output (fractal image)
;;  to create a fractal image.                                ;;  ;;;
;;------------------------------------------------------------;;  ;;; Separator
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;  ;;; Author and copyright
;;------------------------------------------------------------;;  ;;;
```
**Purpose:**
- Describes the script's function: recursively plotting points using 2 or 3 affine transformations to create fractals.
- Credits the author and copyright.

---

## Main Command Definition

```lisp
(defun c:ifs
    (                                   ;;; Start of main command definition
        /
        *error*                        ;;; Custom error handler
        _writedcl                      ;;; Function to write DCL file
        _imgtile                       ;;; Function to fill image tile in dialog
        _point                         ;;; Function to create a point entity
        _rand                          ;;; Function to generate random number
        app col dcl file fun id lst no pt ;;; Local variables
    )
```
**Purpose:**
- Defines the main command `ifs` (callable as `IFS` in AutoCAD).
- Declares local variables and helper functions for encapsulation.

---

## Error Handler

```lisp
(defun *error* ( msg )
    (if (and file (eq 'FILE (type file))) (close file)) ;;; Close file if open
    (if (< 0 id) (unload_dialog id))                    ;;; Unload dialog if loaded
    (if (and dcl (findfile dcl)) (vl-file-delete dcl))  ;;; Delete temp DCL file if exists
    (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")) ;;; If not a normal exit
        (princ (strcat "\nError: " msg))              ;;; Print error message
    )
    (princ)                                            ;;; Exit quietly
)
```
**Purpose:**
- Cleans up resources (file handles, dialogs, temp files) on error or user cancel.
- Prints error messages unless the error is a normal exit/cancel.

---

## DCL File Writer

```lisp
(defun _writedcl ( / tmp file )
    (if (setq file (open (setq tmp (vl-filename-mktemp nil nil ".dcl")) "w")) ;;; Create temp DCL file
        (progn
            (foreach line
               '(
                    "pop : popup_list   { alignment = centered; }" ;;; Popup list control
                    "col : boxed_column { alignment = centered; fixed_width = true; width = 60; }" ;;; Boxed column
                    "txt : text         { alignment = right; }" ;;; Text label
                    "ed3 : edit_box     { alignment = centered; edit_width = 3; }" ;;; Edit box (3 chars)
                    "spc : spacer       { fixed_height = true; height = 0.1; }" ;;; Small vertical spacer
                    "" ;;; Blank line for readability
                    "ifs : dialog { label = \"Iterated Function Systems\"; spacer;" ;;; Main dialog start
                    "    : text { label = \"Copyright (c) 2010 Lee Mac\"; alignment = right; }" ;;; Copyright
                    "    : col  { label = \"Rule 1\"; spacer;" ;;; Rule 1 group
                    "        : row {" ;;; Row for Rule 1
                    "            : column { spc; : txt { label = \"Rotation:\"; } spc; }" ;;; Rotation label
                    "            : pop { key = \"r1\"; }" ;;; Popup for rotation
                    "            spacer_1;" ;;; Spacer
                    "            : column { spc; : txt { label = \"Translation:\"; } spc; }" ;;; Translation label
                    "            : column { spc; : txt { label = \"x:\"; } spc; }" ;;; X label
                    "            : ed3 { key = \"tx1\"; }" ;;; X input
                    "            : column { spc; : txt { label = \"y:\"; } spc; }" ;;; Y label
                    "            : ed3 { key = \"ty1\"; }" ;;; Y input
                    "        }" ;;; End row
                    "        spacer;" ;;; Spacer
                    "    }" ;;; End Rule 1
                    "    : col  { label = \"Rule 2\"; spacer;" ;;; Rule 2 group
                    "        : row {" ;;; Row for Rule 2
                    "            : column { spc; : txt { label = \"Rotation:\"; } spc; }" ;;; Rotation label
                    "            : pop { key = \"r2\"; }" ;;; Popup for rotation
                    "            spacer_1;" ;;; Spacer
                    "            : column { spc; : txt { label = \"Translation:\"; } spc; }" ;;; Translation label
                    "            : column { spc; : txt { label = \"x:\"; } spc; }" ;;; X label
                    "            : ed3 { key = \"tx2\"; }" ;;; X input
                    "            : column { spc; : txt { label = \"y:\"; } spc; }" ;;; Y label
                    "            : ed3 { key = \"ty2\"; }" ;;; Y input
                    "        }" ;;; End row
                    "        spacer;" ;;; Spacer
                    "    }" ;;; End Rule 2
                    "    : col  { label = \"Rule 3\"; spacer;" ;;; Rule 3 group
                    "        : row {" ;;; Row for Rule 3
                    "            : column { spc; : txt { label = \"Rotation:\"; } spc; }" ;;; Rotation label
                    "            : pop { key = \"r3\"; }" ;;; Popup for rotation
                    "            spacer_1;" ;;; Spacer
                    "            : column { spc; : txt { label = \"Translation:\"; } spc; }" ;;; Translation label
                    "            : column { spc; : txt { label = \"x:\"; } spc; }" ;;; X label
                    "            : ed3 { key = \"tx3\"; }" ;;; X input
                    "            : column { spc; : txt { label = \"y:\"; } spc; }" ;;; Y label
                    "            : ed3 { key = \"ty3\"; }" ;;; Y input
                    "        }" ;;; End row
                    "        spacer;" ;;; Spacer
                    "    }" ;;; End Rule 3
                    "    : col { label =  \"Options\";" ;;; Options group
                    "        : row { spacer;" ;;; Row for options
                    "            : radio_row {" ;;; Radio buttons for rule count
                    "                : radio_button { label = \"2 Rules\"; key = \"2r\"; }" ;;; 2 rules
                    "                : radio_button { label = \"3 Rules\"; key = \"3r\"; }" ;;; 3 rules
                    "            }" ;;; End radio_row
                    "            spacer;" ;;; Spacer
                    "            : column { spc; : txt { label = \"Colour:\"; } spc; }" ;;; Colour label
                    "            : image_button" ;;; Color preview button
                    "            { alignment = centered; height = 1.5; width = 4.0; key = \"icol\";" ;;; Image button properties
                    "              fixed_width = true; fixed_height = true; color = 2;" ;;; Fixed size, default color
                    "            }" ;;; End image_button
                    "            : button { key = \"col\"; label = \"Select\"; fixed_width = true; width = 12; }" ;;; Color select button
                    "            spacer;" ;;; Spacer
                    "        }" ;;; End row
                    "        spacer;" ;;; Spacer
                    "    }" ;;; End Options
                    "    spacer; ok_cancel;" ;;; Spacer and OK/Cancel buttons
                    "}" ;;; End dialog
                )
                (write-line line file) ;;; Write each DCL line
            )
            (setq file (close file))   ;;; Close file
            (while (null (findfile tmp))) ;;; Wait for file to exist
            tmp                        ;;; Return filename
        )
    )
)
```
**Purpose:**
- Dynamically writes a temporary DCL (dialog control language) file for the UI.
- Returns the path to the DCL file.

---

## Image Tile Helper

```lisp
(defun _imgtile ( key color )
    (start_image key)                                 ;;; Begin drawing in image tile
    (fill_image 0 0 (dimx_tile key) (dimy_tile key) color) ;;; Fill tile with color
    (end_image)                                       ;;; End drawing
)
```
**Purpose:**
- Fills a dialog image tile with a solid color (for color selection preview).

---

## Point Plotter

```lisp
(defun _point ( x c )
    (entmake (list '(0 . "POINT") (cons 10 x) (cons 62 c))) ;;; Create point at x, color c
)
```
**Purpose:**
- Creates a point entity at coordinates `x` with color `c`.

---

## Random Number Generator

```lisp
(defun _rand ( / m )
    (/                                             ;;; Return value divided by m
        (setq
            m 4294967296.0                         ;;; Modulus for RNG
            s (rem (1+ (* 1664525.0 (cond (s) ((getvar 'DATE))))) m) ;;; Update seed
        )
        m
    )
)
```
**Purpose:**
- Generates a pseudo-random float in [0,1) using a linear congruential generator.
- Used to randomly select which transformation to apply.

---

## Rotation Matrix List

```lisp
(setq lst
   '(
        (0 (( 0.5  0.0) ( 0.0  0.5)))   ;;; 0° rotation/scale
        (1 (( 0.5 -0.5) ( 0.5  0.5)))   ;;; 45°
        (2 (( 0.0 -0.5) ( 0.5  0.0)))   ;;; 90°
        (3 ((-0.5 -0.5) ( 0.5 -0.5)))   ;;; 135°
        (4 ((-0.5  0.0) ( 0.0 -0.5)))   ;;; 180°
        (5 ((-0.5  0.5) (-0.5 -0.5)))   ;;; 225°
        (6 (( 0.0  0.5) (-0.5  0.0)))   ;;; 270°
        (7 (( 0.5  0.5) (-0.5  0.5)))   ;;; 315°
    )
)
```
**Purpose:**
- Associates rotation indices (0–7) with 2x2 rotation/scale matrices for affine transforms.

---

## Dialog Setup and User Input

```lisp
(cond
    (   (null (setq dcl (_writedcl)))                ;;; Write DCL file, check success
        (princ "\nError Writing DCL File.")         ;;; Error if failed
    )
    (   (<= (setq id (load_dialog dcl)) 0)           ;;; Load dialog, check success
        (princ "\nDCL Definition File not Found.")  ;;; Error if failed
    )
    (   (null (new_dialog "ifs" id))                ;;; Create dialog instance
        (princ "\nDialog Could not be Loaded.")      ;;; Error if failed
    )
    (   t
        (set_tile "3r" "1")                        ;;; Default: 3 rules
        (setq no 3)                                  ;;; Set rule count
        (_imgtile "icol" (setq col 3))              ;;; Set default color
        (foreach tile '("r1" "r2" "r3")           ;;; For each rotation selector
            (start_list tile)
            (foreach x '("0º" "45º" "90º" "135º" "180º" "225º" "270º" "315º") (add_list x)) ;;; Add options
            (end_list)
        )
        (foreach tile '("tx1" "tx2" "tx3" "ty1" "ty2" "ty3") ;;; For each translation input
            (set_tile tile (set (read tile) "0.0")) ;;; Set default value
        )
        (foreach tile '("r1" "r2" "r3")
            (set_tile tile (set (read tile) "0"))   ;;; Set default rotation
        )
        (foreach tile '("tx1" "tx2" "tx3" "ty1" "ty2" "ty3" "r1" "r2" "r3")
            (action_tile tile (strcat "(setq " tile " $value)")) ;;; Bind input to variable
        )
        (action_tile "2r" "(setq no 2) (foreach tile '(\"r3\" \"tx3\" \"ty3\") (mode_tile tile 1))") ;;; 2 rules disables 3rd
        (action_tile "3r" "(setq no 3) (foreach tile '(\"r3\" \"tx3\" \"ty3\") (mode_tile tile 0))") ;;; 3 rules enables 3rd
        (action_tile "icol" "(_imgtile \"icol\" (setq col (cond ((acad_colordlg col nil)) (col))))") ;;; Color picker
        (action_tile "col"  "(_imgtile \"icol\" (setq col (cond ((acad_colordlg col nil)) (col))))") ;;; Color picker
        (action_tile "accept"
            (vl-prin1-to-string
                (quote
                    (progn
                        (foreach sym '(tx1 tx2 tx3 ty1 ty2 ty3)
                            (if (eq "" (eval sym)) (set sym "0.0")) ;;; Default empty to 0.0
                        )
                        (cond
                            (   (null (vl-every 'distof (list tx1 tx2 tx3 ty1 ty2 ty3)))
                                (alert "Translation Vectors Must be Numerical!") ;;; Validate input
                            )
                            (   t
                                (mapcar
                                   '(lambda ( a b c ) (set a (list (distof b) (distof c))))
                                   '(v1 v2 v3)
                                    (list tx1 tx2 tx3)
                                    (list ty1 ty2 ty3)
                                )
                                (done_dialog 1)
                            )
                        )
                    )
                )
            )
        )
        (if (= 1 (start_dialog))
            (progn ... )
            (princ "\n*Cancel*")
        )
    )
)
```
**Purpose:**
- Handles dialog creation, user input, and validation.
- Sets up UI controls for rotation, translation, rule count, and color.
- On accept, parses and validates user input, then prepares transformation matrices and translation vectors.

---

## Transformation Function Construction

```lisp
(setq fun
    (eval
        (list 'lambda '( / p )
            (list 'repeat 1000                      ;;; Repeat 1000 times
                (list 'setq 'pt
                    (if (= 3 no)                    ;;; If 3 rules
                       '(cond
                            (   (< (setq p (_rand)) 0.333)
                                (mapcar '+ (mxv r1 pt) v1) ;;; Apply rule 1
                            )
                            (   (<= 0.333 p 0.666)
                                (mapcar '+ (mxv r2 pt) v2) ;;; Apply rule 2
                            )
                            (   t
                                (mapcar '+ (mxv r3 pt) v3) ;;; Apply rule 3
                            )
                        )
                       '(cond
                            (   (< (setq p (_rand)) 0.5)
                                (mapcar '+ (mxv r1 pt) v1) ;;; Apply rule 1
                            )
                            (   t
                                (mapcar '+ (mxv r2 pt) v2) ;;; Apply rule 2
                            )
                        )
                    )
                )
               '(_point pt col)                      ;;; Plot the point
            )
        )
    )
)
```
**Purpose:**
- Dynamically builds a lambda function that:
    - Repeats 1000 times:
        - Randomly selects a rule (2 or 3, based on user input)
        - Applies the corresponding affine transformation to the current point
        - Plots the new point
- Uses the matrices and vectors set up from user input.

---

## Iteration Loop

```lisp
(while
    (progn
        (initget "Yes No")                           ;;; Set up Yes/No input
        (not (eq "No" (getkword "\nIterate System? [Yes/No] <Yes> : "))) ;;; Ask user
    )
    (fun)                                            ;;; Run the iteration
    (vla-zoomextents app)                            ;;; Zoom to extents
)
```
**Purpose:**
- Asks the user if they want to iterate again.
- Each iteration runs the fractal point plotting and zooms to extents.

---

## Cleanup

```lisp
(if (< 0 id) (unload_dialog id))                      ;;; Unload dialog if loaded
(if (and dcl (findfile dcl)) (vl-file-delete dcl))    ;;; Delete DCL file if exists
(princ)                                              ;;; Exit quietly
```
**Purpose:**
- Unloads the dialog and deletes the temporary DCL file.
- Exits cleanly.

---

## Matrix x Vector Helper

```lisp
(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m) ;;; Matrix x vector multiplication
)
```
**Purpose:**
- Multiplies a 2x2 matrix by a 2D vector (for affine transforms).

---

## Finalization

```lisp
(vl-load-com) (princ)                                 ;;; Load Visual LISP COM, suppress output
```
**Purpose:**
- Loads Visual LISP COM support (required for some functions).
- Prints a null string to suppress output.

---

## Summary
This script provides a robust, interactive IFS fractal generator for AutoCAD. It demonstrates advanced use of dialogs, dynamic code generation, and matrix math in AutoLISP. The code is modular, well-structured, and follows professional standards, making it a strong reference for similar projects.
