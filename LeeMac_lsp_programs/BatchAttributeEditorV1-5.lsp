;;-----------------------=={  Batch Attribute Editor  }==------------------------;;
;;                                                                               ;;
;;               --------------------------------------------------              ;;
;;                              Program Description                              ;;
;;               --------------------------------------------------              ;;
;;                                                                               ;;
;;  This program allows the user to modify the values of multiple attributes     ;;
;;  residing within multiple attributed blocks across multiple drawings.         ;;
;;                                                                               ;;
;;  Upon starting the program using the command syntax 'batte'                   ;;
;;  (Batch Attribute Editor) at the command-line, the user is presented with the ;;
;;  first of two dialog interface screens: the first screen for input of         ;;
;;  attribute data; the second for drawing selection.                            ;;
;;                                                                               ;;
;;  ----------------------------------------                                     ;;
;;  Entering Attribute Data                                                      ;;
;;  ----------------------------------------                                     ;;
;;                                                                               ;;
;;  At the top of the first screen there are three edit boxes in which the       ;;
;;  user can input a Block Name, Attribute Tag and a new value for such          ;;
;;  attribute.                                                                   ;;
;;                                                                               ;;
;;  The Block Name field specifies the block in which the attribute to be        ;;
;;  modified is located. This field is not case-sensitive and may use wildcards  ;;
;;  to match multiple blocks containing the same attribute tag.                  ;;
;;                                                                               ;;
;;  For example, specifying a Block Name: "*BLOCK" (without the quotes), will    ;;
;;  match all blocks whose block name ends with "BLOCK", or any case variation   ;;
;;  of that pattern.                                                             ;;
;;                                                                               ;;
;;  The Attribute Tag field specifies the tag name of the attribute to be        ;;
;;  modified (Note: this is not the attribute prompt string). This field is      ;;
;;  also not case-sensitive and, as per the restrictions on attribute tag names, ;;
;;  the attribute tag cannot contain spaces.                                     ;;
;;                                                                               ;;
;;  Finally, the Value field specifies the new content for the attribute.        ;;
;;  There are no restrictions on this field and it may be left blank if the      ;;
;;  attribute value is to be removed.                                            ;;
;;                                                                               ;;
;;  After specifying the Block Name, Attribute Tag and Value, the item may be    ;;
;;  added to the list of items to be modified by clicking the 'Add Item' button, ;;
;;  or by pressing 'Enter' from within the Value edit box.                       ;;
;;                                                                               ;;
;;  ----------------------------------------                                     ;;
;;  Incrementing Attribute Values                                                ;;
;;  ----------------------------------------                                     ;;
;;                                                                               ;;
;;  The Editor also allows the user to automatically increment attribute values  ;;
;;  or sections of attribute values across layouts or drawings as they are       ;;
;;  processed by the program. This functionality may be particularly useful      ;;
;;  where attributes displaying drawing numbers are concerned.                   ;;
;;                                                                               ;;
;;  Incrementing by Layout   -  <L# ... #L>                                      ;;
;;  ----------------------------------------                                     ;;
;;                                                                               ;;
;;  To increment an attribute value for every layout in a drawing, enclose a     ;;
;;  numerical section of the new attribute value with the markers "<L#" & "#L>". ;;
;;                                                                               ;;
;;  The numerical value surrounded by such markers will then be incremented to   ;;
;;  match the order of the layout tabs within each drawing processed.            ;;
;;                                                                               ;;
;;  For example:                                                                 ;;
;;  ---------------------                                                        ;;
;;                                                                               ;;
;;  Attribute value entered into program (minus quotes):                         ;;
;;                                                                               ;;
;;      "Layout <L#1#L> of 3"                                                    ;;
;;                                                                               ;;
;;  Attribute value in 1st Paperspace layout:  "Layout 1 of 3"                   ;;
;;  Attribute value in 2nd Paperspace layout:  "Layout 2 of 3"                   ;;
;;  Attribute value in 3rd Paperspace layout:  "Layout 3 of 3"                   ;;
;;                   ...                            ...                          ;;
;;                                                                               ;;
;;  Note:                                                                        ;;
;;  ---------------------                                                        ;;
;;  The program has been designed with the assumption that the first Paperspace  ;;
;;  layout should represent layout number 1. If, however, the Modelspace layout  ;;
;;  forms one of the sheets in your drawing, commence the numbering at layout 2, ;;
;;  e.g.: "Layout <L#2#L> of 3".                                                 ;;
;;                                                                               ;;
;;  Incrementing by Drawing  -  <D# ... #D>                                      ;;
;;  ----------------------------------------                                     ;;
;;                                                                               ;;
;;  To increment an attribute value across drawings, enclose a numerical section ;;
;;  of the attribute with the markers "<D#" and "#D>"                            ;;
;;                                                                               ;;
;;  For example:                                                                 ;;
;;  ---------------------                                                        ;;
;;                                                                               ;;
;;  Attribute value entered into program (minus quotes):                         ;;
;;                                                                               ;;
;;      "DWG <D#1#D> of 100"                                                     ;;
;;                                                                               ;;
;;  Attribute value in 1st drawing processed:  "DWG 1 of 100"                    ;;
;;  Attribute value in 2nd drawing processed:  "DWG 2 of 100"                    ;;
;;  Attribute value in 3rd drawing processed:  "DWG 3 of 100"                    ;;
;;                   ...                            ...                          ;;
;;                                                                               ;;
;;  ----------------------------------------                                     ;;
;;  Referencing Existing Attribute Values                                        ;;
;;  ----------------------------------------                                     ;;
;;                                                                               ;;
;;  The Editor also provides the ability to reference an existing attribute      ;;
;;  value, either that of the attribute being modified (so as to facilitate      ;;
;;  a prefix and/or suffix), or of another attribute held by the same block.     ;;
;;                                                                               ;;
;;  To reference the value of an existing attribute, enclose the attribute tag   ;;
;;  with the markers "<[" and "]>"                                               ;;
;;                                                                               ;;
;;  For example:                                                                 ;;
;;  ---------------------                                                        ;;
;;                                                                               ;;
;;  Assume we have block containing the attribute 'MyTag' which has a value of   ;;
;;  'Value1' in drawing 1, 'Value2' in drawing 2 etc.                            ;;
;;                                                                               ;;
;;  Attribute value entered into program (minus quotes):                         ;;
;;                                                                               ;;
;;      "Prefix<[MyTag]>Suffix"                                                  ;;
;;                                                                               ;;
;;  Attribute value in 1st drawing processed:  "PrefixValue1Suffix"              ;;
;;  Attribute value in 2nd drawing processed:  "PrefixValue2Suffix"              ;;
;;  Attribute value in 3rd drawing processed:  "PrefixValue3Suffix"              ;;
;;                   ...                               ...                       ;;
;;                                                                               ;;
;;  ----------------------------------------                                     ;;
;;  Selecting Blocks                                                             ;;
;;  ----------------------------------------                                     ;;
;;                                                                               ;;
;;  Attribute items may also be added to the list by selecting attributed blocks ;;
;;  from the active drawing. Upon the clicking the 'Select Blocks' button on the ;;
;;  main dialog, the user is prompted to make a selection of attributed blocks.  ;;
;;                                                                               ;;
;;  The values of all attributes within each block in the selection are then     ;;
;;  displayed in an intermediate dialog interface, allowing the user to choose   ;;
;;  which items from the selection should be added to the list of items to be    ;;
;;  modified by the program.                                                     ;;
;;                                                                               ;;
;;  Duplicate items, (that is, where a block and tag combination appears more    ;;
;;  than once in the selection), will be removed from the selection and the user ;;
;;  will be notified of the items that have been discarded.                      ;;
;;                                                                               ;;
;;  The user will also be informed of an item clash, that is, when selected      ;;
;;  block and tag combinations from the block selection already exist in the     ;;
;;  main attribute data list. If the user decides to proceed at this prompt,     ;;
;;  these selected items will replace those already present in the main list.    ;;
;;                                                                               ;;
;;  ----------------------------------------                                     ;;
;;  The Attribute Data List                                                      ;;
;;  ----------------------------------------                                     ;;
;;                                                                               ;;
;;  The list box panel displays a list of attributes to be modified by the       ;;
;;  program. Items in this list may be edited by double-clicking on them.        ;;
;;                                                                               ;;
;;  Multiple items may be removed from the list by selecting them and clicking   ;;
;;  the 'Remove Item' button. Or the whole list may be removed by clicking the   ;;
;;  'Clear' button.                                                              ;;
;;                                                                               ;;
;;  The list of attribute data may be exported to a CSV or Text file by clicking ;;
;;  the 'Save to File' button and creating an appropriate file saved to a        ;;
;;  desired location using the dialog that subsequently appears.                 ;;
;;                                                                               ;;
;;  Similarly, attribute data be loaded from a CSV or Text file by clicking the  ;;
;;  'Load from File' button and selecting a file from the dialog that is         ;;
;;  displayed. For information about the required format of data for importing,  ;;
;;  see the section below entitled 'Notes on Importing Attribute Data'.          ;;
;;                                                                               ;;
;;  ----------------------------------------                                     ;;
;;  Selecting Drawings                                                           ;;
;;  ----------------------------------------                                     ;;
;;                                                                               ;;
;;  The second screen of the dialog is dedicated to selection of drawings        ;;
;;  to be processed by the program. Here, the user may select a directory using  ;;
;;  either the 'Browse' button at the top of the dialog, or by entering a folder ;;
;;  path into the 'Folder' edit box; then proceed to browse the files and        ;;
;;  folders within that directory from the left-hand list pane.                  ;;
;;                                                                               ;;
;;  The folder structure may be navigated from within the left-hand list pane    ;;
;;  by double-clicking on a listed folder, or on the parent folder symbol ('..') ;;
;;                                                                               ;;
;;  Drawing files may be added to the right-hand list pane by double-clicking    ;;
;;  on a file, or by selecting multiple files and clicking the 'Add Files'       ;;
;;  button. Similarly, files may be removed from the right-hand list pane by     ;;
;;  double-clicking on a file or by selecting a group of files and clicking the  ;;
;;  'Remove Files' button.                                                       ;;
;;                                                                               ;;
;;  Drawing files in the right-hand list pane are displayed using a relative     ;;
;;  path, relative to the directory that is currently selected in the left-hand  ;;
;;  list pane.                                                                   ;;
;;                                                                               ;;
;;  The order in which the list of drawings will be processed may be controlled  ;;
;;  using the five buttons located between the two list panes. The 'Sort' button ;;
;;  will cause the list to be sorted alphanumerically.                           ;;
;;                                                                               ;;
;;  After attribute data has been entered and a number of drawings have been     ;;
;;  selected to be processed, the user may click the 'Run' button to modify the  ;;
;;  listed attributes in each of the select drawings.                            ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;               --------------------------------------------------              ;;
;;                       Notes on Importing Attribute Data                       ;;
;;               --------------------------------------------------              ;;
;;                                                                               ;;
;;  The program will accept two file formats:                                    ;;
;;                                                                               ;;
;;      -   Comma Separated Value files (.csv)                                   ;;
;;      -   Text files (.txt)                                                    ;;
;;                                                                               ;;
;;  The file to be imported should have three columns for Block Name, Attribute  ;;
;;  Tag and Attribute Value (in that order). If using a Text file, the columns   ;;
;;  should be tab-delimited.                                                     ;;
;;                                                                               ;;
;;  As per adding items from within the program, the Block and Tag values must   ;;
;;  be present; however, the Value item may be empty if required.                ;;
;;  The Tag values must not contain spaces.                                      ;;
;;                                                                               ;;
;;  Items that are not in the format required by the program will be removed     ;;
;;  from the list, and the user will be notified of which items have been        ;;
;;  removed.                                                                     ;;
;;                                                                               ;;
;;  Duplicate items will be removed from the list. Duplicate items arise when    ;;
;;  the same block and tag combination appears more than once in the list. For   ;;
;;  these cases, the first occurrence of the item will be used.                  ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;               --------------------------------------------------              ;;
;;                                Hints and Tips!                                ;;
;;               --------------------------------------------------              ;;
;;                                                                               ;;
;;  When the dialog first appears, the Block Name field will take focus.         ;;
;;  The user can then cycle between the Block Name, Attribute Tag and Attribute  ;;
;;  Value fields by pressing 'Enter', finally adding the item to the list by     ;;
;;  pressing 'Enter' from within the Value field.                                ;;
;;                                                                               ;;
;;  Drawing files may be added and removed to and from the left and right list   ;;
;;  box panels by double clicking on the files.                                  ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Function Syntax:  batte                                                      ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Author:                                                                      ;;
;;                                                                               ;;
;;  Copyright � 2012 Lee Mac  -  www.lee-mac.com                                 ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  TERMS AND CONDITIONS OF USE                                                  ;;
;;                                                                               ;;
;;  This license and disclaimer statement constitutes a legal agreement between  ;;
;;  you (either as an individual or a single entity) and Lee Mac (the "Author"), ;;
;;  for this software product (the "Software").                                  ;;
;;                                                                               ;;
;;  By downloading, installing, copying, or otherwise using the software, you    ;;
;;  agree to be bound by all of the following terms and conditions of this       ;;
;;  license and disclaimer agreement.                                            ;;
;;                                                                               ;;
;;  If you do not agree with all the terms and conditions of this agreement,     ;;
;;  you must immediately cease use of the Software and destroy all copies of the ;;
;;  Software and all of its component or constituent parts in your possession    ;;
;;  or under your control.                                                       ;;
;;                                                                               ;;
;;  The Software is freeware. You may use it royalty free for private use.       ;;
;;                                                                               ;;
;;  You may redistribute the Software providing you have written consent from    ;;
;;  the Author, and that no modifications are made to the original content.      ;;
;;                                                                               ;;
;;  You may not charge any fees for the redistribution or use of this Software.  ;;
;;                                                                               ;;
;;  The Software is provided "as is", and with all faults. All warranties,       ;;
;;  expressed or implied, including, but not limited to implied warranties of    ;;
;;  fitness for a particular use or purpose are hereby disclaimed. There is no   ;;
;;  guarantee that the operation of this Software will be uninterrupted or       ;;
;;  error free.                                                                  ;;
;;                                                                               ;;
;;  You acknowledge and agree that your use of the Software is at your own risk. ;;
;;                                                                               ;;
;;  The Software is a copyrighted work and is protected by copyright law and     ;;
;;  international copyright treaty.                                              ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Version History:                                                             ;;
;;  --------------------------------------                                       ;;
;;                                                                               ;;
;;  1.0:  2012-01-30  -  First Release.                                          ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  1.1:  2012-02-10  -  Fixed scripting bug involving save overwrite prompt     ;;
;;                                                                               ;;
;;                    -  'My Documents' folder is now redirected to 'Documents'  ;;
;;                       in Vista and 7 OS (similarly for 'My Music' etc.)       ;;
;;                                                                               ;;
;;                    -  Split interface into two separate dialogs to allow for  ;;
;;                       users without a widescreen resolution.                  ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  1.2:  2012-03-05  -  Implemented another attempted fix to overcome the       ;;
;;                       scripting bug involving save overwrite prompt.          ;;
;;                                                                               ;;
;;                    -  Added ability to select multiple attributed blocks      ;;
;;                       from the drawing and select items from the selection    ;;
;;                       to add to the attribute data list.                      ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  1.3:  2012-05-05  -  Fixed directory sorting bug resulting from the use of   ;;
;;                       an ASCIIbetical sort causing the parent directory       ;;
;;                       symbol (..) to not appear at the top of the list when   ;;
;;                       folders beginning with characters whose ASCII code is   ;;
;;                       less than 46 are present.                               ;;
;;                                                                               ;;
;;                    -  Attribute modification module rewritten to use Visual   ;;
;;                       LISP methods, enabling processing of Multiline          ;;
;;                       Attributes and allowing the use of Field Expressions in ;;
;;                       attribute values.                                       ;;
;;                                                                               ;;
;;                    -  Added ability to use incrementing attribute values in   ;;
;;                       attribute data. If a value contains markers "<#" and    ;;
;;                       "#>", the numerical data between these markers will be  ;;
;;                       incremented by one for each drawing processed.          ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  1.4:  2012-08-02  -  Implemented a file and folder sorting function to       ;;
;;                       emulate the Windows Explorer filename sort.             ;;
;;                                                                               ;;
;;                    -  Increased edit_box character limits to allow for the    ;;
;;                       use of long Field Expressions in attribute values.      ;;
;;                                                                               ;;
;;                    -  Fixed behaviour of 'Add Files' button following the     ;;
;;                       addition of a file to the list of drawings to be        ;;
;;                       processed by double-clicking.                           ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  1.5:  2023-05-02  -  Duplicated call to 'QSAVE' command at the end of the    ;;
;;                       Script to account for later versions of AutoCAD which   ;;
;;                       still prompt for confirmation before closing.           ;;
;;                                                                               ;;
;;                    -  Corrected bug in batte:filesort function.               ;;
;;                                                                               ;;
;;                    -  Escaped wildcard operators found in drawing layout      ;;
;;                       names to avoid false negative when detecting presence   ;;
;;                       of attributed blocks.                                   ;;
;;                                                                               ;;
;;                    -  Added the ability to increment attribute values either  ;;
;;                       by drawing (using "<D#" "#D>" markers), or by layout    ;;
;;                       (using "<L#" "#L>" markers).                            ;;
;;                                                                               ;;
;;                    -  Added the ability to reference existing attribute       ;;
;;                       values held by the target attribute or other attributes ;;
;;                       in the target block within the new attribute value      ;;
;;                       (using "<[" "]>" markers).                              ;;
;;                                                                               ;;
;;                    -  Added buttons to allow the user to adjust the order in  ;;
;;                       which drawings are processed, or revert to an           ;;
;;                       alphanumerical sort order.                              ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;

(setq batteversion "1.5")

;;-------------------------------------------------------------------------------;;

(defun c:batte

    (
        /
        *error*
        _validate

        *wsh*
        base
        block
        blocks-p
        cfg
        cfgfname
        data
        dclfname
        dclid
        dclstatus
        dcltitle
        dir
        dirdata
        file
        filelist
        files
        lspfname
        result
        savepath
        scrfname
        tag
        value
    )

    (defun *error* ( msg )
        (if (and (= 'vla-object (type *wsh*)) (not (vlax-object-released-p *wsh*)))
            (vlax-release-object *wsh*)
        )
        (if (= 'file (type file))
            (close file)
        )
        (if (< 0 dclID)
            (unload_dialog dclID)
        )
        (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (cond
        (   (not (vl-file-directory-p (setq savepath (batte:GetSavePath))))
            (batte:Popup (batte:WSH) "Warning" 16 "Save Path not Valid.")
        )
        (   (progn
                (setq base (strcat savepath "\\LMAC_batte_V" (vl-string-translate "." "-" batteVersion)))
                (foreach pair
                   '(
                        (dclfname . ".dcl")
                        (lspfname . ".lsp")
                        (cfgfname . ".cfg")
                        (scrfname . ".scr")
                    )
                    (set (car pair) (strcat base (cdr pair)))
                )
                (setq dcltitle (strcat "Batch Attribute Editor V" batteVersion))
                (not (batte:WriteDCL dclfname))
            )
            (batte:Popup
                (batte:WSH)
                "Warning" 16
                (strcat
                    "The Dialog Definition (DCL) File could not be Written.\n\n"
                    "Please ensure that you have write permission for the following directory:\n\n"
                    savepath
                )
            )
        )
        (   (not (batte:WriteLSP lspfname))
            (batte:Popup
                (batte:WSH)
                "Warning" 16
                (strcat
                    "The LISP Utilities File could not be Written.\n\n"
                    "Please ensure that you have write permission for the following directory:\n\n"
                    savepath
                )
            )
        )
        (   (<= (setq dclID (load_dialog dclfname)) 0)
            (batte:Popup
                (batte:WSH)
                "Warning" 16
                (strcat
                    "The following DCL File could not be loaded:\n\n"
                    dclfname
                    "\n\nThe file either does not exist or contains an error."
                )
            )
        )
        (   t
         
            (if (setq cfg (batte:ReadConfig cfgfname))
                (setq data   (car   cfg)
                      dir    (cadr  cfg)
                      result (caddr cfg)
                )
            )

            (setq blocks-p
                (and
                    (ssget "_X"
                        (list
                           '(0 . "INSERT")
                           '(66 . 1)
                            (cons 410
                                (if (= 1 (getvar 'cvport))
                                    (batte:EscapeWildcards (getvar 'ctab))
                                    "Model"
                                )
                            )
                        )
                    )
                )
            )

            (while (not (member dclStatus '(1 0)))
                (cond
                    (
                        (or
                            (null dclStatus)
                            (= 3  dclStatus)
                        )
                        (cond
                            (   (not (new_dialog "batte1" dclID))
                                (batte:Popup
                                    (batte:WSH)
                                    "Warning" 16
                                    (strcat
                                        "The program dialog could not be loaded.\n\n"
                                        "The corresponding DCL file resides at the following location:\n\n"
                                        dclfname
                                        "\n\nThis file contains an error, please contact the program author."
                                    )
                                )
                                (setq dclStatus 0)
                            )
                            (   t
                                (set_tile "dcltitle" dcltitle)

                                ;;-------------------------------------------------------------------------------;;
                                ;;                             Attribute Data Screen                             ;;
                                ;;-------------------------------------------------------------------------------;;

                                ;;-------------------------------------------------------------------------------;;
                                ;; Initial Setup                                                                 ;;
                                ;;-------------------------------------------------------------------------------;;

                                (if data
                                    (progn
                                        (setq data (batte:UpdateAttributeData "list" data))
                                        (mode_tile "clear"   0)
                                        (mode_tile "save"    0)
                                    )
                                    (progn
                                        (mode_tile "clear"   1)
                                        (mode_tile "save"    1)
                                    )
                                )
                                (mode_tile "delitem" 1)
                                (mapcar 'batte:AddList '("h1" "h2" "h3") '(("\tBlock") ("\tTag") ("\tValue")))

                                (if blocks-p
                                    (mode_tile "select" 0)
                                    (mode_tile "select" 1)
                                )

                                ;;-------------------------------------------------------------------------------;;

                                (setq _validate
                                    (lambda ( / tmp )
                                        (cond
                                            (   (or (null block) (eq "" block))
                                                (batte:Popup
                                                    (batte:WSH)
                                                    "Information" 48
                                                    (strcat
                                                        "Please Enter a Block Name.\n\n"
                                                        "Note: Block names are not case-sensitive and may use wildcard patterns"
                                                        " to match multiple blocks containing the same attribute tag."
                                                    )
                                                )
                                                (mode_tile "block" 2)
                                            )
                                            (   (or (null tag) (eq "" tag))
                                                (batte:Popup
                                                    (batte:WSH)
                                                    "Information" 48
                                                    (strcat
                                                        "Please Enter an Attribute Tag.\n\n"
                                                        "Note: Attribute tags are not case-sensitive and cannot contain spaces."
                                                    )
                                                )
                                                (mode_tile "tag" 2)
                                            )
                                            (   (vl-string-position 32 tag)
                                                (batte:Popup (batte:WSH) "Information" 48 "Attribute tag cannot contain spaces.")
                                                (mode_tile "tag" 2)
                                            )
                                            (   (setq tmp
                                                    (vl-some
                                                        (function
                                                            (lambda ( item )
                                                                (if
                                                                    (and
                                                                        (eq (car  item) (strcase block))
                                                                        (eq (cadr item) (strcase   tag))
                                                                    )
                                                                    item
                                                                )
                                                            )
                                                        )
                                                        data
                                                    )
                                                )
                                                (batte:Popup
                                                    (batte:WSH)
                                                    "Item Clash"
                                                    48
                                                    (strcat
                                                        "The attribute tag '"
                                                        (cadr tmp)
                                                        "' within block '"
                                                        (car  tmp)
                                                        "' already appears in the list to be set to value \""
                                                        (caddr tmp)
                                                        "\""
                                                    )
                                                )
                                                (mode_tile "block" 2)
                                            )
                                            (   t
                                                (if (null value)
                                                    (setq value "")
                                                )
                                                (setq data (batte:UpdateAttributeData "list" (cons (list (strcase block) (strcase tag) value) data))
                                                      block nil
                                                      tag   nil
                                                      value nil
                                                )
                                                (foreach tile '("block" "tag" "value")
                                                    (set_tile tile "")
                                                )
                                                (mode_tile "delitem" 1)
                                                (mode_tile "clear"   0)
                                                (mode_tile "save"    0)
                                                (mode_tile "block"   2)                                    
                                            )
                                        )
                                    )
                                )

                                ;;-------------------------------------------------------------------------------;;
                                ;; Top Buttons                                                                   ;;
                                ;;-------------------------------------------------------------------------------;;

                                (action_tile "block"
                                    (vl-prin1-to-string
                                       '(progn
                                            (setq block $value)
                                            (if (= 1 $reason) (mode_tile "tag" 2))
                                        )
                                    )
                                )

                                (action_tile "tag"
                                    (vl-prin1-to-string
                                       '(progn
                                            (setq tag $value)
                                            (if (= 1 $reason) (mode_tile "value" 2))
                                        )
                                    )
                                )
                             
                                (action_tile "value"
                                    (vl-prin1-to-string
                                       '(progn
                                            (setq value $value)
                                            (if (= 1 $reason) (_validate))
                                        )
                                    )
                                )

                                ;;-------------------------------------------------------------------------------;;
                                ;; List Box Panel                                                                ;;
                                ;;-------------------------------------------------------------------------------;;

                                (action_tile "list"
                                    (vl-prin1-to-string
                                       '(
                                            (lambda ( / item tmp )
                                                (mode_tile "delitem" 0)
                                                (if (= 4 $reason)
                                                    (progn
                                                        (setq item (nth (atoi $value) data)
                                                              tmp  data
                                                              data (batte:UpdateAttributeData "list" (batte:EditItem dclID item data))
                                                        )
                                                        (set_tile "list" "")
                                                        (set_tile "list"
                                                            (if (equal tmp data) ; Cancel pressed
                                                                $value
                                                                (itoa
                                                                    (vl-position
                                                                        (car (vl-remove-if '(lambda ( x ) (member x tmp)) data))
                                                                        data
                                                                    )
                                                                )
                                                            )
                                                        )                
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )

                                ;;-------------------------------------------------------------------------------;;
                                ;; Add / Select / Remove Buttons                                                 ;;
                                ;;-------------------------------------------------------------------------------;;

                                (action_tile "additem" "(_validate)")

                                (action_tile "select" "(done_dialog 4)")

                                (action_tile "delitem"
                                    (vl-prin1-to-string
                                       '(
                                            (lambda ( / items )
                                                (if (setq items (read (strcat "(" (get_tile "list") ")")))
                                                    (setq data  (batte:UpdateAttributeData "list" (batte:RemoveItems items data)))
                                                )
                                                (mode_tile "delitem" 1)
                                                (if (null data)
                                                    (progn
                                                        (mode_tile "clear" 1)
                                                        (mode_tile "save"  1)
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )

                                ;;-------------------------------------------------------------------------------;;
                                ;; Bottom Buttons                                                                ;;
                                ;;-------------------------------------------------------------------------------;;

                                (action_tile "load"
                                    (vl-prin1-to-string
                                       '(
                                            (lambda ( / tmp )
                                                (if (setq tmp  (batte:LoadFromFile))
                                                    (progn
                                                        (setq data (batte:UpdateAttributeData "list" tmp))
                                                        (mode_tile "clear"   0)
                                                        (mode_tile "save"    0)
                                                        (mode_tile "block"   2)
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )

                                (action_tile "clear"
                                    (vl-prin1-to-string
                                       '(progn
                                            (setq data (batte:UpdateAttributeData "list" nil))
                                            (mode_tile "delitem" 1)
                                            (mode_tile "clear"   1)
                                            (mode_tile "save"    1)
                                        )
                                    )
                                )

                                (action_tile "save" "(batte:SaveToFile data)")

                                ;;-------------------------------------------------------------------------------;;
                                ;; Base of Dialog                                                                ;;
                                ;;-------------------------------------------------------------------------------;;

                                (action_tile "accept"
                                    (vl-prin1-to-string
                                       '(
                                            (lambda ( / removed )
                                                (cond
                                                    (   (null data)
                                                        (batte:Popup
                                                            (batte:WSH)
                                                            "Information" 64
                                                            (strcat
                                                                "No attribute data found.\n\n"
                                                                "Enter a block name, attribute tag and a new value for the attribute"
                                                                " in the relevant fields at the top of the dialog.\n\n"
                                                                "Click 'Add Item' to add the entered data to the list of attributes to be updated."
                                                            )
                                                        )
                                                    )
                                                    (   (done_dialog 2)   )
                                                )
                                            )
                                        )
                                    )
                                )

                                (setq dclStatus (start_dialog))
                            )
                        )
                    )
                    (   (= 2 dclStatus)
                        (cond
                            (   (not (new_dialog "batte2" dclID))
                                (batte:Popup
                                    (batte:WSH)
                                    "Warning" 16
                                    (strcat
                                        "The program dialog could not be loaded.\n\n"
                                        "The corresponding DCL file resides at the following location:\n\n"
                                        dclfname
                                        "\n\nThis file contains an error, please contact the program author."
                                    )
                                )
                                (setq dclStatus 0)
                            )
                            (   t
                                (set_tile "dcltitle" dcltitle)

                                ;;-------------------------------------------------------------------------------;;
                                ;;                                Drawings Panel                                 ;;
                                ;;-------------------------------------------------------------------------------;;

                                ;;-------------------------------------------------------------------------------;;
                                ;; Initial Setup                                                                 ;;
                                ;;-------------------------------------------------------------------------------;;

                                (set_tile "directory"
                                    (setq dir
                                        (batte:FixDir
                                            (if
                                                (or
                                                    (null dir)
                                                    (null (vl-file-directory-p (batte:FixDir dir)))
                                                 )
                                                 (getvar 'dwgprefix)
                                                 dir
                                            )
                                        )
                                    )
                                )
                                (setq files  (batte:UpdateFileList dir result)
                                      result (batte:UpdateSelected dir result)
                                )
                                (mode_tile "add" 1)
                                (mode_tile "del" 1)
                                (mapcar 'mode_tile '("top" "up" "down" "bottom" "sort") (batte:KeyMode nil result))

                                ;;-------------------------------------------------------------------------------;;
                                ;; Top Items                                                                     ;;
                                ;;-------------------------------------------------------------------------------;;

                                (action_tile "browse"
                                    (vl-prin1-to-string
                                       '(if (setq tmp (batte:BrowseForFolder "" nil 512))
                                            (mapcar 'mode_tile '("top" "up" "down" "bottom" "sort")
                                                (batte:KeyMode nil
                                                    (setq files  (batte:UpdateFileList (set_tile "directory" (setq dir tmp)) result)
                                                          result (batte:UpdateSelected dir result)
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )

                                (action_tile "directory"
                                    (vl-prin1-to-string
                                       '(if (= 1 $reason)
                                            (mapcar 'mode_tile '("top" "up" "down" "bottom" "sort")
                                                (batte:KeyMode nil
                                                    (setq files  (batte:UpdateFileList (set_tile "directory" (setq dir (batte:FixDir $value))) result)
                                                          result (batte:UpdateSelected dir result)
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )

                                ;;-------------------------------------------------------------------------------;;
                                ;; File List Box Panels                                                          ;;
                                ;;-------------------------------------------------------------------------------;;

                                (action_tile "box1"
                                    (vl-prin1-to-string
                                       '(
                                            (lambda ( / index items tmp )
                                                (setq index (read (strcat "(" $value ")"))
                                                      items (mapcar (function (lambda ( n ) (nth n files))) index)
                                                )
                                                (if (= 4 $reason)
                                                    (progn
                                                        (cond
                                                            (   (equal '("..") items)
                                                                (setq files  (batte:UpdateFileList (set_tile "directory" (setq dir (batte:UpDir dir))) result)
                                                                      result (batte:UpdateSelected dir result)
                                                                )
                                                            )
                                                            (   (vl-file-directory-p (setq tmp (batte:CheckRedirect (strcat dir "\\" (car items)))))
                                                                (setq files  (batte:UpdateFileList (set_tile "directory" (setq dir tmp)) result)
                                                                      result (batte:UpdateSelected dir result)
                                                                )
                                                            )
                                                            (   t                                     
                                                                (setq result (batte:UpdateSelected dir (append result (mapcar '(lambda ( file ) (strcat dir "\\" file)) items)))
                                                                      files  (batte:UpdateFileList dir result)
                                                                )
                                                            )
                                                        )
                                                        (setq index nil)
                                                        (mode_tile "add" 1)
                                                    )
                                                    (if (vl-some 'vl-filename-extension items)
                                                        (mode_tile "add" 0)
                                                    )
                                                )
                                                (mapcar 'mode_tile '("top" "up" "down" "bottom" "sort") (batte:KeyMode index result))
                                            )
                                        )
                                    )
                                )

                                (action_tile "box2"
                                    (vl-prin1-to-string
                                       '(
                                            (lambda ( / index items )
                                                (setq index (read (strcat "(" $value ")"))
                                                      items (mapcar (function (lambda ( n ) (nth n result))) index)
                                                )
                                                (if (= 4 $reason)
                                                    (setq result (batte:UpdateSelected dir (vl-remove (car items) result))
                                                          files  (batte:UpdateFileList dir result)
                                                          index  nil
                                                    )
                                                    (mode_tile "del" 0)
                                                )
                                                (mapcar 'mode_tile '("top" "up" "down" "bottom" "sort") (batte:KeyMode index result))
                                            )
                                        )
                                    )
                                )

                                ;;-------------------------------------------------------------------------------;;
                                ;; Central Buttons                                                               ;;
                                ;;-------------------------------------------------------------------------------;;

                                (mapcar
                                    (function
                                        (lambda ( key fnc )
                                            (action_tile key
                                                (vl-prin1-to-string
                                                    (list
                                                        (list 'lambda '( / items )
                                                            (list 'if '(setq items (read (strcat "(" (get_tile "box2") ")")))
                                                                (list 'apply
                                                                    (list 'function
                                                                       '(lambda ( idx lst )
                                                                            (setq result (batte:UpdateSelected dir lst))
                                                                            (set_tile "box2" (vl-string-trim "()" (vl-princ-to-string idx)))
                                                                            (mapcar 'mode_tile '("top" "up" "down" "bottom" "sort") (batte:KeyMode idx lst))
                                                                        )
                                                                    )
                                                                    (list fnc 'items 'result)
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                   '(
                                        "top"
                                        "up"
                                        "down"
                                        "bottom"
                                    )
                                   '(
                                        batte:ListTop
                                        batte:ListUp
                                        batte:ListDown
                                        batte:ListBottom
                                    )
                                )

                                (action_tile "sort"
                                    (vl-prin1-to-string
                                       '(
                                            (lambda ( / index items )
                                                (if result
                                                    (progn
                                                        (setq items
                                                            (mapcar
                                                                (function
                                                                    (lambda ( n )
                                                                        (nth n result)
                                                                    )
                                                                )
                                                                (read (strcat "(" (get_tile "box2") ")"))
                                                            )
                                                        )
                                                        (setq result (batte:UpdateSelected dir (batte:FileSort result)))
                                                        (set_tile "box2" "")
                                                        (if
                                                            (setq index
                                                                (vl-sort
                                                                    (vl-remove nil
                                                                        (mapcar
                                                                            (function
                                                                                (lambda ( item )
                                                                                    (vl-position item result)
                                                                                )
                                                                            )
                                                                            items
                                                                        )
                                                                    )
                                                                    '<
                                                                )
                                                            )
                                                            (set_tile "box2" (vl-string-trim "()" (vl-princ-to-string index)))
                                                        )
                                                        (mapcar 'mode_tile '("top" "up" "down" "bottom" "sort") (batte:KeyMode index result))
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )

                                ;;-------------------------------------------------------------------------------;;
                                ;; Add/Remove Buttons                                                            ;;
                                ;;-------------------------------------------------------------------------------;;

                                (action_tile "add"
                                    (vl-prin1-to-string
                                       '(
                                            (lambda ( / items )
                                                (if
                                                    (setq items
                                                        (vl-remove-if-not 'vl-filename-extension
                                                            (mapcar
                                                                (function
                                                                    (lambda ( n ) (nth n files))
                                                                )
                                                                (read (strcat "(" (get_tile "box1") ")"))
                                                            )
                                                        )
                                                    )
                                                    (progn
                                                        (setq result (batte:UpdateSelected dir (append result (mapcar '(lambda ( file ) (strcat dir "\\" file)) items)))
                                                              files  (batte:UpdateFileList dir result)
                                                        )
                                                        (mapcar 'mode_tile '("top" "up" "down" "bottom" "sort") (batte:KeyMode nil result))
                                                    )
                                                )
                                                (mode_tile "add" 1)
                                                (mode_tile "del" 1)
                                            )
                                        )
                                    )
                                )

                                (action_tile "del"
                                    (vl-prin1-to-string
                                       '(
                                            (lambda ( / items )
                                                (if (setq items  (read (strcat "(" (get_tile "box2") ")")))
                                                    (progn
                                                        (setq result (batte:UpdateSelected dir (batte:RemoveItems items result))
                                                              files  (batte:UpdateFileList dir result)
                                                        )
                                                        (mapcar 'mode_tile '("top" "up" "down" "bottom" "sort") (batte:KeyMode nil result))
                                                    )
                                                )
                                                (mode_tile "add" 1)
                                                (mode_tile "del" 1)
                                            )
                                        )
                                    )
                                )

                                ;;-------------------------------------------------------------------------------;;
                                ;; Base of Dialog                                                                ;;
                                ;;-------------------------------------------------------------------------------;;

                                (action_tile "back" "(done_dialog 3)")

                                (action_tile "accept"
                                    (vl-prin1-to-string
                                       '(
                                            (lambda ( / removed )
                                                (cond
                                                    (   (null result)
                                                        (batte:Popup
                                                            (batte:WSH)
                                                            "Information" 64
                                                            (strcat
                                                                "No Drawings selected.\n\n"
                                                                "Navigate to a directory using either the left-hand list pane, 'Browse' button, or by"
                                                                " specifying a directory in the 'Folder' edit box and pressing 'Enter'.\n\n"
                                                                "Select files from the directory by double-clicking on a file, or selecting a group of"
                                                                " files and clicking the 'Add Files' button."
                                                            )
                                                        )
                                                    )
                                                    (   (null
                                                            (setq filelist
                                                                (vl-remove-if
                                                                    (function
                                                                        (lambda ( file / dwl )
                                                                            (if
                                                                                (and
                                                                                    (setq dwl (findfile (strcat (substr file 1 (- (strlen file) 3)) "dwl")))
                                                                                    (null (vl-file-delete dwl))
                                                                                )
                                                                                (setq removed
                                                                                    (cons
                                                                                        (strcat
                                                                                            (vl-filename-base file) ".dwg\t"
                                                                                            (
                                                                                                (lambda ( / tmp usr )
                                                                                                    (if (setq tmp (open dwl "r"))
                                                                                                        (progn
                                                                                                            (setq usr (read-line tmp)
                                                                                                                  tmp (close tmp)
                                                                                                            )
                                                                                                            usr
                                                                                                        )
                                                                                                        "<Unknown>"
                                                                                                    )
                                                                                                )
                                                                                            )
                                                                                        )
                                                                                        removed
                                                                                    )
                                                                                )
                                                                            )           
                                                                        )
                                                                    )
                                                                    result
                                                                )
                                                            )
                                                        )
                                                        (batte:Popup
                                                            (batte:WSH)
                                                            "All Files in Use" 48
                                                            (strcat
                                                                "All of the selected files are currently in use and cannot be processed:\n\n"
                                                                "Filename\t\t\tOpen By\n"
                                                                (batte:lst->str (reverse removed) "\n")
                                                            )                                                
                                                        )
                                                    )
                                                    (   removed
                                                        (if
                                                            (= 6
                                                                (batte:Popup
                                                                    (batte:WSH)
                                                                    "Files in Use" (+ 32 4)
                                                                    (strcat
                                                                        "The following files are in use and will not be processed:\n\n"
                                                                        "Filename\t\tOpen By\n"
                                                                        (batte:lst->str (reverse removed) "\n")
                                                                        "\n\nContinue?"
                                                                    )
                                                                )
                                                            )
                                                            (done_dialog 1)
                                                        )
                                                    )
                                                    (   (done_dialog 1)   )
                                                )
                                            )
                                        )
                                    )
                                )

                                (setq dclStatus (start_dialog))
                            )
                        )
                    )
                    (   (= 4 dclStatus)
                        (setq data (batte:SelectBlocks dclID data))

                        (setq dclStatus 3)
                    )
                )
            )

            ;;-------------------------------------------------------------------------------;;

            (if (= 1 dclStatus)
                (progn
                    (batte:WriteConfig cfgfname data dir result)

                    (if (setq file (open scrfname "w"))
                        (progn
                            (foreach name filelist
                                (write-line
                                    (strcat
                                        "_.open \"" name "\" "
                                        "(load " (vl-prin1-to-string lspfname) " nil) "
                                        "(if (and batte:SetAttributes (vl-bb-ref 'batte:data)) (batte:SetAttributes (vl-bb-ref 'batte:data))) "
                                        "(if (vl-bb-ref 'batte:dwgcounter) (vl-bb-set 'batte:dwgcounter (1+ (vl-bb-ref 'batte:dwgcounter)))) "
                                        "_.qsave _.qsave _.close"
                                    )
                                    file
                                )
                            )
                            (setq file (close file))
                            (vl-bb-set 'batte:data (batte:ConvertDataList data))
                            (vl-bb-set 'batte:dwgcounter 0)

                            (if
                                (and
                                    dclID
                                    (< 0 dclID)
                                )
                                (setq dclID (unload_dialog dclID))
                            )
                            (if
                                (and
                                    *wsh*
                                    (eq 'VLA-OBJECT (type *wsh*))
                                    (not (vlax-object-released-p *wsh*))
                                )
                                (progn
                                    (vlax-release-object *wsh*)
                                    (setq *wsh* nil)
                                )
                            )
                            (vl-cmdf "_.script" scrfname)
                        )
                        (batte:Popup
                            (batte:WSH)
                            "Warning" 16
                            (strcat
                                "The Script File could not be Written.\n\n"
                                "Please ensure that you have write permission for the following directory:\n\n"
                                savepath
                            )
                        )
                    )
                )
                (princ "\n*Cancel*")
            )
        )
    )
    (if (< 0 dclID)
        (setq dclID (unload_dialog dclID))
    )
    (if (and
            (= 'vla-object (type *wsh*))
            (not (vlax-object-released-p *wsh*))
        )
        (vlax-release-object *wsh*)
    )
    (princ)
)

;;-------------------------------------------------------------------------------;;

(defun batte:AddQuotes ( str / pos )
    (cond
        (   (wcmatch str "*[`,\"]*")
            (setq pos 0)    
            (while (setq pos (vl-string-position 34 str pos))
                (setq str (vl-string-subst "\"\"" "\"" str pos)
                      pos (+ pos 2)
                )
            )
            (strcat "\"" str "\"")
        )
        (   str   )
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:ReplaceQuotes ( str / pos )
    (setq pos 0)
    (while (setq pos (vl-string-search  "\"\"" str pos))
        (setq str (vl-string-subst "\"" "\"\"" str pos)
              pos (1+ pos)
        )
    )
    str
)

;;-------------------------------------------------------------------------------;;

(defun batte:lst->str ( lst del )
    (if (cdr lst)
        (strcat (car lst) del (batte:lst->str (cdr lst) del))
        (car lst)
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:str->lst ( str del / pos )
    (if (setq pos (vl-string-search del str))
        (cons (substr str 1 pos) (batte:str->lst (substr str (+ pos 1 (strlen del))) del))
        (list str)
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:lst->csv ( lst )
    (if (cdr lst)
        (strcat (batte:AddQuotes (car lst)) "," (batte:lst->csv (cdr lst)))
        (batte:AddQuotes (car lst))
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:csv->lst ( str pos / s )
    (cond
        (   (null (setq pos (vl-string-position 44 str pos)))
            (if (wcmatch str "\"*\"")
                (list (batte:ReplaceQuotes (substr str 2 (- (strlen str) 2))))
                (list str)
            )
        )
        (   (wcmatch (setq s (substr str 1 pos)) "\"*\"")
            (cons
                (batte:ReplaceQuotes (substr str 2 (- pos 2)))
                (batte:csv->lst (substr str (+ pos 2)) 0)
            )
        )
        (   (wcmatch s "\"*[~\"]")
            (batte:csv->lst str (+ pos 2))
        )
        (   (cons s (batte:csv->lst (substr str (+ pos 2)) 0)))
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:ReadConfig ( name / data dir file files line )
    (if
        (and
            (setq name (findfile name))
            (setq file (open name "r"))
        )
        (progn
            (while
                (and
                    (setq line (read-line file))
                    (not (eq "" line))
                )
                (setq data (cons (batte:str->lst line "\t") data))
            )
            (setq dir (read-line file))
            (while (setq line (read-line file))
                (setq files (cons line files))
            )
            (setq file (close file))
            (list
                (reverse data)
                dir
                (reverse (vl-remove-if-not 'findfile files))
            )                
        )
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:WriteConfig ( name data dir files / file )
    (if
        (and
            data dir files
            (setq file (open name "w"))
        )
        (progn
            (foreach item data
                (write-line (batte:lst->str item "\t") file)
            )
            (write-line ""  file)
            (write-line dir file)
            (foreach item files
                (write-line item file)
            )
            (setq file (close file))
            t
        )
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:WriteLSP ( fname / file )
    (cond
        (   (findfile fname)
        )
        (   (setq file (open fname "w"))
            (foreach line
               '(
                    "(defun batte:setattributes ( lst / atl bln cnt cur ent idx itm lyl lyt ord rfl sel tgl val )"
                    "    (if"
                    "        (setq sel"
                    "            (ssget \"_X\""
                    "                (append"
                    "                   '("
                    "                        (000 . \"INSERT\")"
                    "                        (066 . 1)"
                    "                        (-04 . \"<OR\")"
                    "                        (002 . \"`*U*\")"
                    "                    )"
                    "                    (mapcar '(lambda ( x ) (cons 002 x)) (mapcar 'car lst))"
                    "                   '("
                    "                        (-04 . \"OR>\")"
                    "                    )"
                    "                )"
                    "            )"
                    "        )"
                    "        (progn"
                    "            (vlax-for lyt (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))"
                    "                (setq lyl (cons (strcase (vla-get-name lyt)) lyl)"
                    "                      ord (cons (vla-get-taborder lyt) ord)"
                    "                )"
                    "            )"
                    "            (repeat (setq idx (sslength sel))"
                    "                (setq idx (1- idx)"
                    "                      ent (ssname sel idx)"
                    "                      lyt (strcase (cdr (assoc 410 (entget ent))))"
                    "                )"
                    "                (if (setq itm (assoc lyt rfl))"
                    "                    (setq rfl (subst (vl-list* lyt (vlax-ename->vla-object ent) (cdr itm)) itm rfl))"
                    "                    (setq rfl (cons  (list lyt (vlax-ename->vla-object ent)) rfl))"
                    "                )"
                    "            )"
                    "            (setq cnt -1)"
                    "            (foreach idx (vl-sort-i ord '<)"
                    "                (foreach obj (cdr (assoc (nth idx lyl) rfl))"
                    "                    (if (setq bln (strcase (batte:blockname obj))"
                    "                              tgl (vl-some '(lambda ( x ) (if (wcmatch bln (strcase (car x))) (cdr x))) lst)"
                    "                        )"
                    "                        (progn"
                    "                            (setq atl (vlax-invoke obj 'getattributes)"
                    "                                  cur (mapcar '(lambda ( a ) (cons (strcase (vla-get-tagstring a)) (vla-get-textstring a))) atl)"
                    "                            )"
                    "                            (foreach att atl"
                    "                                (if (setq val (cdr (assoc (strcase (vla-get-tagstring att)) tgl)))"
                    "                                    (if (vlax-write-enabled-p att)"
                    "                                        (vla-put-textstring att"
                    "                                            (batte:specialoperators \"<[\" \"]>\" cur"
                    "                                                (batte:specialoperators \"<D#\" \"#D>\" (cond ((vl-bb-ref 'batte:dwgcounter)) (1))"
                    "                                                    (batte:specialoperators \"<L#\" \"#L>\" cnt val)"
                    "                                                )"
                    "                                            )"
                    "                                        )"
                    "                                    )"
                    "                                )"
                    "                            )"
                    "                        )"
                    "                    )"
                    "                )"
                    "                (setq cnt (1+ cnt))"
                    "            )"
                    "        )"
                    "    )"
                    ")"
                    ""
                    "(defun batte:specialoperators ( pt1 pt2 arg str / num ps1 ps2 )"
                    "    (if"
                    "        (wcmatch"
                    "            (strcase str)"
                    "            (strcat \"*\""
                    "                (batte:escapewildcards (strcase pt1)) \"*\""
                    "                (batte:escapewildcards (strcase pt2)) \"*\""
                    "            )"
                    "        )"
                    "        (progn"
                    "            (setq ps1 (vl-string-search (strcase pt1) (strcase str))"
                    "                  ps2 (vl-string-search (strcase pt2) (strcase str))"
                    "                  val (substr str (+ 1 ps1 (strlen pt1)) (- ps2 ps1 (strlen pt1)))"
                    "            )"
                    "            (strcat"
                    "                (substr str 1 ps1)"
                    "                (if (wcmatch pt1 \"<?`#\")"
                    "                    (if (member (type (read val)) '(int real))"
                    "                        (batte:num->str (+ arg (read val)))"
                    "                        val"
                    "                    )"
                    "                    (cond ((cdr (assoc (strcase val) arg))) (\"\"))"
                    "                )"
                    "                (batte:specialoperators pt1 pt2 arg (substr str (+ 1 ps2 (strlen pt2))))"
                    "            )"
                    "        )"
                    "        str"
                    "    )"
                    ")"
                    ""
                    "(defun batte:num->str ( num / dim rtn )"
                    "    (if (equal num (atoi (rtos num 2 0)) 1e-8)"
                    "        (rtos num 2 0)"
                    "        (progn"
                    "            (setq dim (getvar 'dimzin))"
                    "            (setvar 'dimzin 8)"
                    "            (setq rtn (rtos num 2 8))"
                    "            (setvar 'dimzin dim)"
                    "            rtn"
                    "        )"
                    "    )"
                    ")"
                    ""
                    "(defun batte:escapewildcards ( str )"
                    "    (vl-list->string"
                    "        (apply 'append"
                    "            (mapcar"
                    "               '(lambda ( c )"
                    "                    (if (member c '(35 64 46 42 63 126 91 93 45 44))"
                    "                        (list 96 c)"
                    "                        (list c)"
                    "                    )"
                    "                )"
                    "                (vl-string->list str)"
                    "            )"
                    "        )"
                    "    )"
                    ")"
                    ""
                    "(defun batte:blockname ( obj )"
                    "    (if (vlax-property-available-p obj 'effectivename)"
                    "        (defun batte:blockname ( obj ) (vla-get-effectivename obj))"
                    "        (defun batte:blockname ( obj ) (vla-get-name obj))"
                    "    )"
                    "    (batte:blockname obj)"
                    ")"
                    ""
                    "(vl-load-com) (princ)"
                )
                (write-line line file)
            )
            (setq file (close file))
            (while (not (findfile fname)))
            fname
        )
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:WriteDCL ( fname / file )
    (cond
        (   (findfile fname)
        )
        (   (setq file (open fname "w"))
            (foreach line
               '(
                    "//---------------=={ Batch Attribute Editor }==---------------//"
                    "//                                                            //"
                    "//  batte.dcl dialog definition file to be used in            //"
                    "//  conjunction with batte.lsp                                //"
                    "//------------------------------------------------------------//"
                    "//  Author: Lee Mac, Copyright � 2012 - www.lee-mac.com       //"
                    "//------------------------------------------------------------//"
                    ""
                    "//------------------------------------------------------------//"
                    "//                  Sub-Assembly Definitions                  //"
                    "//------------------------------------------------------------//"
                    ""
                    "head : list_box"
                    "{"
                    "    is_enabled = false;"
                    "    fixed_height = true;"
                    "    fixed_width = true;"
                    "    height = 2;"
                    "    vertical_margin = none;"
                    "    horizontal_margin = none;"
                    "}"
                    ""
                    "txt : text     { vertical_margin = none; }"
                    "edt : edit_box { vertical_margin = 0.1; edit_limit = 1024; }"
                    ""
                    "but1 : button"
                    "{"
                    "    fixed_width = true;"
                    "    fixed_height = true;"
                    "    width = 20;"
                    "    height = 1.8;"
                    "    alignment = centered;"
                    "}"
                    ""
                    "but2 : button"
                    "{"
                    "    fixed_width = true;"
                    "    fixed_height = true;"
                    "    width = 15;"
                    "    height = 1.8;"
                    "}"
                    ""
                    "but3 : button"
                    "{"
                    "    fixed_width = true;"
                    "    fixed_height = true;"
                    "    width = 18;"
                    "    height = 2.2;"
                    "}"
                    ""
                    "but4 : button"
                    "{"
                    "    fixed_width = true;"
                    "    fixed_height = true;"
                    "    width = 10;"
                    "    height = 1.0;"
                    "}"
                    ""
                    "dwgbox : list_box"
                    "{"
                    "    width = 40;"
                    "    height = 24;"
                    "    fixed_width = true;"
                    "    fixed_height = true;"
                    "    alignment = centered;"
                    "    multiple_select = true;"
                    "    vertical_margin = none;"
                    "    tab_truncate = true;"
                    "}"
                    ""
                    "tagbox : list_box"
                    "{"
                    "    width = 80;"
                    "    height = 19;"
                    "    fixed_height = true;"
                    "    fixed_width = true;"
                    "    tabs = \"22 44\";"
                    "    vertical_margin = none;"
                    "    horizontal_margin = none;"
                    "    multiple_select = true;"
                    "    tab_truncate = true;"
                    "}"
                    ""
                    "editbox : edit_box"
                    "{"
                    "    width = 65;"
                    "    fixed_width = true;"
                    "    edit_limit = 1024;"
                    "}"
                    ""
                    "edittxt : text"
                    "{"
                    "    alignment = right;"
                    "}"
                    ""
                    "spacer0 : spacer"
                    "{"
                    "    width = 0.1;"
                    "    height = 0.1;"
                    "    fixed_width = true;"
                    "    fixed_height = true;"
                    "}"
                    ""
                    "//------------------------------------------------------------//"
                    "//                    Edit Dialog Definition                  //"
                    "//------------------------------------------------------------//"
                    ""
                    "edit : dialog"
                    "{"
                    "    initial_focus = \"block\";"
                    "    label = \"Edit Item\";"
                    "    spacer;"
                    "    : row"
                    "    {"
                    "        : column"
                    "        {"
                    "            spacer0;"
                    "            : edittxt { label = \"Block:\"; }"
                    "            spacer0;"
                    "        }"
                    "        : editbox { key = \"block\"; }"
                    "    }"
                    "    : row"
                    "    {"
                    "        : column"
                    "        {"
                    "            spacer0;"
                    "            : edittxt { label = \"Tag:\"; }"
                    "            spacer0;"
                    "        }"
                    "        : editbox { key = \"tag\"; }"
                    "    }"
                    "    : row"
                    "    {"
                    "        : column"
                    "        {"
                    "            spacer0;"
                    "            : edittxt { label = \"Value:\"; }"
                    "            spacer0;"
                    "        }"
                    "        : editbox { key = \"value\"; }"
                    "    }"
                    "    spacer_1;"
                    "    ok_cancel;"
                    "}"
                    ""
                    "//------------------------------------------------------------//"
                    "//                Block Selection Dialog Definition           //"
                    "//------------------------------------------------------------//"
                    ""
                    "select : dialog"
                    "{"
                    "    label = \"Select Items to Add\";"
                    "    spacer_1;"
                    "    : row"
                    "    {"
                    "        fixed_width = true;"
                    "        alignment = left;"
                    "        spacer;"
                    "        : text { label = \"Select items to add to the attribute data list:\"; }"
                    "    }"
                    "    spacer;"
                    "    : row"
                    "    {"
                    "        spacer;"
                    "        : head { key = \"h1\"; width = 22; tabs = \"8\"; }"
                    "        : head { key = \"h2\"; width = 22; tabs = \"8\"; }"
                    "        : head { key = \"h3\"; width = 36; tabs = \"15\";}"
                    "        spacer;"
                    "    }"
                    "    : row"
                    "    {"
                    "        spacer;"
                    "        : tagbox { key = \"list\"; }"
                    "        spacer;"
                    "    }"
                    "    : row"
                    "    {"
                    "        fixed_width = true;"
                    "        alignment = left;"
                    "        spacer;"
                    "        : toggle { label = \"Select All\"; key = \"all\"; }"
                    "    }"
                    "    ok_cancel;"
                    "}"
                    ""
                    "//------------------------------------------------------------//"
                    "//                    Main Dialog Definition                  //"
                    "//------------------------------------------------------------//"
                    ""
                    "//------------------------------------------------------------//"
                    "//                          Screen 1                          //"
                    "//------------------------------------------------------------//"
                    ""
                    "batte1 : dialog"
                    "{"
                    "    initial_focus = \"block\";"
                    "    key = \"dcltitle\";"
                    "    spacer;"
                    "    : text { alignment = right; label = \"Copyright (c) Lee Mac 2012  -  www.lee-mac.com\"; }"
                    "    : boxed_column"
                    "    {"
                    "        label = \"Attribute Data\";"
                    "        : column"
                    "        {"
                    "            : row"
                    "            {"
                    "                : column"
                    "                {"
                    "                    : txt { label = \"Block Name\"; }"
                    "                    : edt { key = \"block\"; }"
                    "                }"
                    "                : column"
                    "                {"
                    "                    : txt { label = \"Attribute Tag\"; }"
                    "                    : edt { key = \"tag\"; }"
                    "                }"
                    "                : column"
                    "                {"
                    "                    : txt { label = \"Attribute Value\"; }"
                    "                    : edt { key = \"value\"; }"
                    "                }"
                    "            }"
                    "            spacer;"
                    "        }"
                    "        spacer;"
                    "        : row"
                    "        {"
                    "            fixed_width = true;"
                    "            alignment = centered;"
                    "            : but1 { key = \"additem\"; label = \"&Add Item\"; mnemonic = \"A\"; }"
                    "            spacer;"
                    "            : but2 { key = \"select\";  label = \"Select &Blocks...\"; mnemonic = \"B\"; }"
                    "            spacer;"
                    "            : but1 { key = \"delitem\"; label = \"&Remove Item\"; mnemonic = \"R\"; }"
                    "        }"
                    "        spacer;"
                    "        : row"
                    "        {"
                    "            fixed_width = true;"
                    "            alignment = left;"
                    "            spacer;"
                    "            : text { label = \"Double-click to edit item\"; }"
                    "        }"
                    "        : row"
                    "        {"
                    "            spacer;"
                    "            : head { key = \"h1\"; width = 22; tabs = \"8\"; }"
                    "            : head { key = \"h2\"; width = 22; tabs = \"8\"; }"
                    "            : head { key = \"h3\"; width = 36; tabs = \"15\";}"
                    "            spacer;"
                    "        }"
                    "        : row"
                    "        {"
                    "            spacer;"
                    "            : tagbox { key = \"list\"; }"
                    "            spacer;"
                    "        }"
                    "        : row"
                    "        {"
                    "            fixed_width = true;"
                    "            alignment = centered;"
                    "            : but1 { key = \"load\"; label = \"&Load from File\"; mnemonic = \"L\"; }"
                    "            spacer;"
                    "            : but2 { key = \"clear\"; label = \"&Clear\"; mnemonic = \"C\"; }"
                    "            spacer;"
                    "            : but1 { key = \"save\"; label = \"&Save to File\"; mnemonic = \"S\"; }"
                    "        }"
                    "        spacer;"
                    "    }"
                    "    spacer;"
                    "    : row"
                    "    {"
                    "        fixed_width = true;"
                    "        alignment = centered;"
                    "        : but3 { key = \"accept\"; is_default = true; label = \"&Next\"; mnemonic = \"N\"; }"
                    "        spacer_1;"
                    "        : but3 { key = \"cancel\"; is_cancel = true; label = \"&Exit\"; mnemonic = \"E\"; }"
                    "    }"
                    "}"
                    ""
                    "//------------------------------------------------------------//"
                    "//                          Screen 2                          //"
                    "//------------------------------------------------------------//"
                    ""
                    "batte2 : dialog"
                    "{"
                    "    key = \"dcltitle\";"
                    "    spacer;"
                    "    : text { alignment = right; label = \"Copyright (c) Lee Mac 2012  -  www.lee-mac.com\"; }"
                    "    : boxed_column"
                    "    {"
                    "        label = \"Drawings to Process\";"
                    "        : column"
                    "        {"
                    "            : txt { label = \"Folder\"; }"
                    "            : row"
                    "            {"
                    "                : edt { key = \"directory\"; }"
                    "                : button { key = \"browse\"; label = \"&Browse\"; mnemonic = \"B\"; fixed_width = true; }"
                    "            }"
                    "        }"
                    "        spacer_1;"
                    "        : row"
                    "        {"
                    "            : column"
                    "            {"
                    "                fixed_width = true;"
                    "                alignment = centered;"
                    "                : dwgbox { key = \"box1\"; }"
                    "                : but1   { key = \"add\" ; label = \"&Add Files\"; mnemonic = \"A\"; }"
                    "                spacer;"
                    "            }"
                    "            : column"
                    "            {"
                    "                fixed_width = true;"
                    "                fixed_height = true;"
                    "                alignment = centered;"
                    "                : but4   { key = \"top\";    label = \"&Top\";    mnemonic = \"T\"; }"
                    "                : but4   { key = \"up\";     label = \"&Up\";     mnemonic = \"U\"; }"
                    "                : but4   { key = \"down\";   label = \"&Down\";   mnemonic = \"D\"; }"
                    "                : but4   { key = \"bottom\"; label = \"B&ottom\"; mnemonic = \"O\"; }"
                    "                : but4   { key = \"sort\";   label = \"&Sort\";   mnemonic = \"S\"; }"
                    "                : spacer"
                    "                {"
                    "                    height = 1.8;"
                    "                    fixed_height = true;"
                    "                }"
                    "                spacer;"
                    "            }"
                    "            : column"
                    "            {"
                    "                fixed_width = true;"
                    "                alignment = centered;"
                    "                : dwgbox { key = \"box2\"; }"
                    "                : but1   { key = \"del\" ; label = \"Remove &Files\"; mnemonic = \"F\"; }"
                    "                spacer;"
                    "            }"
                    "        }"
                    "    }"
                    "    spacer;"
                    "    : row"
                    "    {"
                    "        fixed_width = true;"
                    "        alignment = centered;"
                    "        : but3 { key = \"back\"; label = \"Ba&ck\"; mnemonic = \"c\"; }"
                    "        spacer_1;"
                    "        : but3 { key = \"accept\"; is_default = true; label = \"&Run\"; mnemonic = \"R\"; }"
                    "        spacer_1;"
                    "        : but3 { key = \"cancel\"; is_cancel = true; label = \"&Exit\"; mnemonic = \"E\"; }"
                    "    }"
                    "}"
                    ""
                    "//------------------------------------------------------------//"
                    "//                         End of File                        //"
                    "//------------------------------------------------------------//"
                )
                (write-line line file)
            )
            (setq file (close file))
            (while (not (findfile fname)))
            fname
        )
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:GetSavePath ( / tmp )
    (cond      
        (   (setq tmp (getvar 'ROAMABLEROOTPREFIX))
            (strcat (vl-string-right-trim "\\" (vl-string-translate "/" "\\" tmp)) "\\Support")
        )
        (   (setq tmp (findfile "ACAD.pat"))
            (vl-string-right-trim "\\" (vl-string-translate "/" "\\" (vl-filename-directory tmp)))
        )
        (   (vl-string-right-trim "\\" (vl-filename-directory (vl-filename-mktemp))))
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:WSH nil
    (cond
        (   *wsh*   )
        (   (setq *wsh* (vlax-create-object "WScript.Shell")   )
        )
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:Popup ( wsh title flags msg / err )
    (setq err (vl-catch-all-apply 'vlax-invoke-method (list wsh 'popup msg 0 title flags)))
    (if (null (vl-catch-all-error-p err))
        err
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:AddList ( key lst )
    (start_list key)
    (foreach x lst (add_list x))
    (end_list)
    lst
)

;;-------------------------------------------------------------------------------;;

(defun batte:KeyMode ( idx lst / foo )
    (setq foo
        (lambda ( a b / r )
            (repeat a (setq r (cons (setq b (1- b)) r)))
        )
    )
    (cond
        (   (null lst)
           '(1 1 1 1 1)
        )
        (   (or (null idx) (= (length idx) (length lst)))
           '(1 1 1 1 0)
        )
        (   (equal idx (foo (length idx) (length idx)))
           '(1 1 0 0 0)
        )
        (   (equal idx (foo (length idx) (length lst)))
           '(0 0 1 1 0)
        )
        (  '(0 0 0 0 0))
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:ListUp ( idx lst / foo )
    (defun foo ( cnt idx lst idx-out lst-out )
        (cond
            (   (not  (and idx lst))
                (list (reverse idx-out) (append (reverse lst-out) lst))
            )
            (   (= 0 (car idx))
                (foo (1+  cnt) (mapcar '1- (cdr idx)) (cdr lst) (cons cnt idx-out) (cons (car lst) lst-out))
            )
            (   (= 1 (car idx))
                (foo (1+  cnt) (mapcar '1- (cdr idx)) (cons (car lst) (cddr lst)) (cons cnt idx-out) (cons (cadr lst) lst-out))
            )
            (   (foo (1+  cnt) (mapcar '1- idx) (cdr lst) idx-out (cons (car lst) lst-out)))
        )
    )
    (foo 0 idx lst nil nil)
)

;;-------------------------------------------------------------------------------;;

(defun batte:ListDown ( idx lst / bar foo len )
    (setq len (length lst)
          foo (lambda ( x ) (- len x 1))
          bar (lambda ( a b ) (list (reverse (mapcar 'foo a)) (reverse b)))
    )
    (apply 'bar (apply 'batte:ListUp (bar idx lst)))
)

;;-------------------------------------------------------------------------------;;

(defun batte:ListTop ( idx lst / i )
    (setq i -1)
    (list
        (mapcar '(lambda ( x ) (setq i (1+ i))) idx)
        (append (mapcar '(lambda ( x ) (nth x lst)) idx) (batte:RemoveItems idx lst))
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:ListBottom ( idx lst / i )
    (setq i (length lst))
    (list
        (reverse (mapcar '(lambda ( x ) (setq i (1- i))) idx))
        (append (batte:RemoveItems idx lst) (mapcar '(lambda ( x ) (nth x lst)) idx))
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:CheckRedirect ( dir / itm pos )
    (cond
        (   (vl-directory-files dir)
            dir
        )
        (   (and
                (eq
                    (strcase (getenv "UserProfile"))
                    (strcase (substr dir 1 (setq pos (vl-string-position 92 dir nil t))))
                )
                (setq itm
                    (cdr
                        (assoc (substr (strcase dir t) (+ pos 2))
                           '(
                                ("my documents" . "Documents")
                                ("my pictures"  . "Pictures")
                                ("my videos"    . "Videos")
                                ("my music"     . "Music")
                            )
                        )
                    )
                )
                (vl-file-directory-p (setq itm (strcat (substr dir 1 pos) "\\" itm)))
            )
            itm
        )
        (   dir   )
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:GetFiles ( dir files )
    (vl-remove-if
        (function (lambda ( file ) (member (strcat dir "\\" file) files)))
        (cond
            (   (cdr (assoc dir dirdata))   )
            (   (cdar
                    (setq dirdata
                        (cons
                            (cons dir
                                (append
                                    (batte:filesort (vl-remove "." (vl-directory-files dir nil -1)))
                                    (batte:filesort (vl-directory-files dir "*.dwg" 1))
                                )
                            )
                            dirdata
                        )
                    )
                )
            )
        )
    )
)

;;-------------------------------------------------------------------------------;;

;; The following sorting function is adapted from code originally authored by ElpanovEvgeniy
;; Ref: http://www.theswamp.org/index.php?topic=41463.msg465729#msg465729

(defun batte:filesort ( l )
    (mapcar (function (lambda ( a ) (nth a l)))
        (vl-sort-i (mapcar 'strcase l)
            (function
                (lambda ( a b / i )
                    (cond
                        (   (= ".." a)
                            t
                        )
                        (   (= ".." b)
                            nil
                        )
                        (   (zerop (setq i (vl-string-mismatch a b)))
                            (if (and (< 47 (ascii a) 58)
                                     (< 47 (ascii b) 58)
                                )
                                (< (atoi a) (atoi b))
                                (< a b)
                            )
                        )
                        (   (= i (strlen a)))
                        (   (= i (strlen b))
                            nil
                        )
                        (   (or (< 47 (vl-string-elt a i) 58)
                                (< 47 (vl-string-elt b i) 58)
                            )
                            (cond
                                (   (< 47 (vl-string-elt a (1- i)) 58)
                                    (< (atoi (substr a i)) (atoi (substr b i)))
                                )
                                (   (and (< 47 (vl-string-elt a i) 58)
                                         (< 47 (vl-string-elt b i) 58)
                                    )
                                    (< (atoi (substr a (1+ i))) (atoi (substr b (1+ i))))
                                )
                                (   (< a b))
                            )
                        )
                        (   (< a b))
                    )
                )
            )
        )
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:BrowseForFolder ( msg dir flag / err shell fold self path )
    (setq err
        (vl-catch-all-apply
            (function
                (lambda ( / acapp hwnd )
                    (if (setq acapp (vlax-get-acad-object)
                              shell (vla-getinterfaceobject acapp "Shell.Application")
                              hwnd  (vl-catch-all-apply 'vla-get-hwnd (list acapp))
                              fold  (vlax-invoke-method shell 'BrowseForFolder (if (vl-catch-all-error-p hwnd) 0 hwnd) msg flag dir)
                        )
                        (setq self (vlax-get-property fold 'self)
                              path (vlax-get-property self 'path)
                              path (vl-string-right-trim "\\" (vl-string-translate "/" "\\" path))
                        )
                    )
                )
            )
        )
    )
    (if self  (vlax-release-object  self))
    (if fold  (vlax-release-object  fold))
    (if shell (vlax-release-object shell))
    (if (vl-catch-all-error-p err)
        (prompt (vl-catch-all-error-message err))
        path
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:Full->Relative ( dir path / p q )
    (setq dir (vl-string-right-trim "\\" dir))
    (cond
        (   (and
                (setq p (vl-string-position 58  dir))
                (setq q (vl-string-position 58 path))
                (not (eq (strcase (substr dir 1 p)) (strcase (substr path 1 q))))
            )
            path
        )
        (   (and
                (setq p (vl-string-position 92  dir))
                (setq q (vl-string-position 92 path))
                (eq (strcase (substr dir 1 p)) (strcase (substr path 1 q)))
            )
            (batte:Full->Relative (substr dir (+ 2 p)) (substr path (+ 2 q)))
        )
        (   (and
                (setq q (vl-string-position 92 path))
                (eq (strcase dir) (strcase (substr path 1 q)))
            )
            (strcat ".\\" (substr path (+ 2 q)))
        )
        (   (eq "" dir)
            path
        )
        (   (setq p (vl-string-position 92 dir))
            (batte:Full->Relative (substr dir (+ 2 p)) (strcat "..\\" path))
        )
        (   (batte:Full->Relative "" (strcat "..\\" path)))
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:UpdateFileList ( dir files )
    (batte:AddList "box1" (batte:GetFiles dir files))
)

;;-------------------------------------------------------------------------------;;

(defun batte:UpdateSelected ( dir files )
    (batte:AddList "box2" (mapcar (function (lambda ( file ) (batte:Full->Relative dir file))) files))
    files
)

;;-------------------------------------------------------------------------------;;

(defun batte:UpdateAttributeData ( key data )
    (start_list key)
    (foreach item
        (setq data
            (vl-sort data
                (function
                    (lambda ( a b )
                        (cond
                            (   (eq
                                    (car a)
                                    (car b)
                                )
                                (< (cadr a) (cadr b))
                            )
                            (   t
                                (<
                                    (car a)
                                    (car b)
                                )
                            )
                        )
                    )
                )
            )
        )
        (add_list (batte:lst->str item "\t"))
    )
    (end_list)
    data
)

;;-------------------------------------------------------------------------------;;

(defun batte:UpDir ( dir )
    (substr dir 1 (vl-string-position 92 dir nil t))
)

;;-------------------------------------------------------------------------------;;

(defun batte:FixDir ( dir )
    (vl-string-right-trim "\\" (vl-string-translate "/" "\\" dir))
)

;;-------------------------------------------------------------------------------;;

(defun batte:RemoveItems ( items lst / i )
    (setq i -1)
    (vl-remove-if '(lambda ( x ) (vl-position (setq i (1+ i)) items)) lst)
)

;;-------------------------------------------------------------------------------;;

(defun batte:SelectBlocks ( id lst / _ssget blk ent idx inc itm sel )

    (defun _ssget ( msg filter / sel )
        (setvar 'nomutt 1)
        (princ msg)
        (setq sel (vl-catch-all-apply 'ssget (list filter)))
        (setvar 'nomutt 0)
        (if (and sel (null (vl-catch-all-error-p sel)))
            sel
        )
    )

    (cond
        (   (null
                (setq sel
                    (_ssget "\nSelect Blocks: "
                        (list
                           '(0 . "INSERT")
                           '(66 . 1)
                            (cons 410
                                (if (= 1 (getvar 'cvport))
                                    (batte:EscapeWildcards (getvar 'ctab))
                                    "Model"
                                )
                            )
                        )
                    )
                )
            )
        )
        (   (progn
                (repeat (setq inc (sslength sel))
                    (setq ent (ssname sel (setq inc (1- inc)))
                          blk (strcase (batte:EffectiveName ent))
                    )
                    (setq itm
                        (append
                            (mapcar
                                (function
                                    (lambda ( att )
                                        (list
                                            blk
                                            (strcase (vla-get-tagstring att))
                                            (vla-get-textstring att)
                                        )
                                    )
                                )
                                (vlax-invoke (vlax-ename->vla-object ent) 'getattributes)
                            )
                            itm
                        )
                    )
                )
                (and
                    (cadr (setq itm (batte:RemoveAttribDuplicates itm)))
                    (/= 6
                        (batte:Popup
                            (batte:WSH)
                            "Duplicate Items Found" (+ 32 4)
                            (strcat
                                "A number of duplicate items were found in the block selection.\n\n"
                                "Duplicate items arise when the same block and tag combination appear more than once in the list.\n\n"
                                "The following duplicate items have been removed from the list:\n\n"
                                (batte:lst->str (cadr itm) "\n")
                                "\n\nContinue?"
                            )
                        )
                    )
                )
            )
        )
        (   (progn
                (setq itm (car itm))
                (null (new_dialog "select" id))
            )
            (batte:Popup
                (batte:WSH)
                "Warning" 16
                (strcat
                    "The Block Selection dialog could not be loaded.\n\n"
                    "The corresponding DCL file resides at the following location:\n\n"
                    dclfname
                    "\n\nThis file contains an error, please contact the program author."
                )
            )
        )
        (   t
            (mapcar 'batte:AddList '("h1" "h2" "h3") '(("\tBlock") ("\tTag") ("\tValue")))

            (setq itm (batte:UpdateAttributeData "list" itm))

            (action_tile "list"
                (vl-prin1-to-string
                   '(
                        (lambda ( )
                            (setq idx $value)
                            (if (= (length (read (strcat "(" idx ")"))) (length itm))
                                (set_tile "all" "1")
                                (set_tile "all" "0")
                            )
                        )
                    )
                )
            )

            (action_tile "all"
                (vl-prin1-to-string
                   '(
                        (lambda ( / i l )
                            (if (eq "1" $value)
                                (set_tile "list"
                                    (setq idx
                                        (vl-string-trim "()"
                                            (vl-princ-to-string
                                                (repeat (setq i (length itm))
                                                    (setq l (cons (itoa (setq i (1- i))) l))
                                                )
                                            )
                                        )
                                    )
                                )
                                (progn
                                    (set_tile "list" "")
                                    (setq idx nil)
                                )
                            )
                        )
                    )
                )
            )

            (action_tile "accept"
                (vl-prin1-to-string
                   '(
                        (lambda ( / dupes items tmp )
                            (cond
                                (   (or (null idx) (eq "" idx))
                                    (if
                                        (= 6
                                            (batte:Popup
                                                (batte:WSH)
                                                "No Items Selected" (+ 32 4)
                                                (strcat
                                                    "No items have been selected to be added to the Attribute Data list.\n\n"
                                                    "Continue to Main Dialog?"
                                                )
                                            )
                                        )
                                        (done_dialog 1)
                                    )
                                )
                                (   (progn
                                        (setq items
                                            (mapcar
                                                (function
                                                    (lambda ( n ) (nth n itm))
                                                )
                                                (read (strcat "(" idx ")"))
                                            )
                                        )
                                        (setq tmp
                                            (append
                                                (mapcar
                                                    (function
                                                        (lambda ( item1 )
                                                            (cond
                                                                (
                                                                    (vl-some
                                                                        (function
                                                                            (lambda ( item2 )
                                                                                (if
                                                                                    (and
                                                                                        (eq (car  item1) (car  item2))
                                                                                        (eq (cadr item1) (cadr item2))
                                                                                    )
                                                                                    (progn
                                                                                        (setq dupes (cons (batte:lst->str item2 "\t") dupes)
                                                                                              items (vl-remove item2 items)
                                                                                        )
                                                                                        item2
                                                                                    )
                                                                                )
                                                                            )
                                                                        )
                                                                        items
                                                                    )
                                                                )
                                                                (   item1   )
                                                            )
                                                        )
                                                    )
                                                    lst
                                                )
                                                items
                                            )
                                        )
                                        (setq dupes (reverse dupes))
                                    )
                                    (if
                                        (= 6
                                            (batte:Popup
                                                (batte:WSH)
                                                "Item Clash" (+ 32 4)
                                                (strcat
                                                    "The Block & Tag combination for the following selected items already appears in the "
                                                    "Attribute Data list:\n\n"
                                                    (batte:lst->str dupes "\n")
                                                    "\n\nIf you continue, the above items will replace those already in the list."
                                                    "\n\nContinue?"
                                                )
                                            )
                                        )
                                        (progn
                                            (setq lst tmp)
                                            (done_dialog 1)
                                        )
                                    )
                                )
                                (   t
                                    (setq lst tmp)
                                    (done_dialog 1)
                                )                                    
                            )
                        )
                    )
                )
            )                    
         
            (start_dialog)
        )
    )
    lst
)

;;-------------------------------------------------------------------------------;;

(defun batte:EditItem ( id item lst / _validate block tag value )
    (setq _validate
        (lambda ( / tmp )
            (cond
                (   (or (null block) (eq "" block))
                    (batte:Popup
                        (batte:WSH)
                        "Information" 48
                        (strcat
                            "Please Enter a Block Name.\n\n"
                            "Note: Block names are not case-sensitive and may use wildcard patterns"
                            " to match multiple blocks containing the same attribute tag."
                        )
                    )
                    (mode_tile "block" 2)
                )
                (   (or (null tag) (eq "" tag))
                    (batte:Popup
                        (batte:WSH)
                        "Information" 48
                        (strcat
                            "Please Enter an Attribute Tag.\n\n"
                            "Note: Attribute tags are not case-sensitive and cannot contain spaces."
                        )
                    )
                    (mode_tile "tag" 2)
                )
                (   (vl-string-position 32 tag)
                    (batte:Popup (batte:WSH) "Information" 48 "Attribute tag cannot contain spaces.")
                    (mode_tile "tag" 2)
                )
                (   (setq tmp
                        (vl-some
                            (function
                                (lambda ( item )
                                    (if
                                        (and
                                            (eq (car  item) (strcase block))
                                            (eq (cadr item) (strcase   tag))
                                        )
                                        item
                                    )
                                )
                            )
                            (vl-remove item lst)
                        )
                    )
                    (batte:Popup
                        (batte:WSH)
                        "Item Clash"
                        48
                        (strcat
                            "The attribute tag '"
                            (cadr tmp)
                            "' within block '"
                            (car  tmp)
                            "' already appears in the list to be set to value \""
                            (caddr tmp)
                            "\""
                        )
                    )
                    (mode_tile "block" 2)
                )
                (   t
                    (if (null value)
                        (setq value "")
                    )
                    (done_dialog 1)
                )
            )
        )
    )
    (cond
        (   (null (new_dialog "edit" id))
            (batte:Popup
                (batte:WSH)
                "Warning" 16
                (strcat
                    "The Edit Item dialog could not be loaded.\n\n"
                    "The corresponding DCL file resides at the following location:\n\n"
                    dclfname
                    "\n\nThis file contains an error, please contact the program author."
                )
            )
        )
        (   t
            (mapcar
                (function
                    (lambda ( _tile _value )
                        (set (read _tile) (set_tile _tile _value))
                    )
                )
               '("block" "tag" "value") item
            )
         
            (action_tile "block"
                (vl-prin1-to-string
                   '(progn
                        (setq block $value)
                        (if (= 1 $reason) (mode_tile "tag" 2))
                    )
                )
            )

            (action_tile "tag"
                (vl-prin1-to-string
                   '(progn
                        (setq tag $value)
                        (if (= 1 $reason) (mode_tile "value" 2))
                    )
                )
            )
         
            (action_tile "value"
                (vl-prin1-to-string
                   '(progn
                        (setq value $value)
                        (if (= 1 $reason) (_validate))
                    )
                )
            )
         
            (action_tile "accept" "(_validate)")
         
            (if (= 1 (start_dialog))
                (setq lst (subst (list (strcase block) (strcase tag) value) item lst))
            )
        )
    )
    lst
)

;;-------------------------------------------------------------------------------;;

(defun batte:SaveToFile ( data / file name )    
    (cond
        (   (null data)
            nil
        )
        (   (null (setq name (getfiled "Create Output File" "" "csv;txt" 1)))
            nil
        )
        (   (null (setq file (open name "w")))
            (batte:Popup
                (batte:WSH)
                "Unable to Write to File" 16
                (strcat
                    "The following file could not be opened for writing:\n\n"
                    name
                    "\n\nPlease ensure that you have write permission for the selected directory."
                )
            )
            nil
        )
        (   t         
            (if (eq ".TXT" (strcase (vl-filename-extension name)))
                (foreach item data
                    (write-line (batte:lst->str item "\t") file)
                )
                (foreach item data
                    (write-line (batte:lst->csv item) file)
                )
            )
            (setq file (close file))
            (batte:Popup
                (batte:WSH)
                "Save Successful" 64
                (strcat
                    "Attribute data was successfully written to the following location:\n\n"
                    name
                )
            )
            t
        )
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:LoadFromFile ( / data file line name removed )
    (cond
        (   (null (setq name (getfiled "Select File to Load" "" "csv;txt" 16)))
            nil
        )
        (   (null (setq file (open name "r")))
            (batte:Popup
                (batte:WSH)
                "Unable to Read File" 16
                (strcat
                    "The following file could not be opened for reading:\n\n"
                    name
                    "\n\nPlease ensure that you have read permission for the above file."
                )
            )
            nil
        )
        (   t
            (if (eq ".TXT" (strcase (vl-filename-extension name)))
                (while (setq line (read-line file))
                    (setq data
                        (cons
                            (batte:str->lst line "\t")
                            data
                        )
                    )
                )
                (while (setq line (read-line file))
                    (setq data
                        (cons
                            (batte:csv->lst line 0)
                            data
                        )
                    )
                )
            )
            (setq file (close file))
            (cond
                (   (null (setq data (reverse data)))
                    (batte:Popup
                        (batte:WSH)
                        "File Empty" 48
                        "The selected file contained no data."
                    )
                    nil
                )
                (   (null
                        (setq data
                            (apply 'append
                                (mapcar
                                    (function
                                        (lambda ( item )
                                            (if
                                                (or
                                                    (< (length item) 2)
                                                    (eq "" (car  item))
                                                    (eq "" (cadr item))
                                                    (vl-string-position 32 (cadr item))
                                                )
                                                (progn
                                                    (setq removed
                                                        (cons
                                                            (batte:lst->str item "\t")
                                                            removed
                                                        )
                                                    )
                                                    nil
                                                )
                                                (list
                                                    (list
                                                        (strcase (car  item))
                                                        (strcase (cadr item))
                                                        (cond ( (caddr item) ) ( "" ))
                                                    )
                                                )
                                            )                                                        
                                        )
                                    )
                                    data
                                )
                            )
                        )
                    )
                    (batte:Popup
                        (batte:WSH)
                        "Incorrect File Format" 48
                        (strcat
                            "The data in the selected file is not in the format required by this program.\n\n"
                            "The file should have three columns for Block, Tag and Value data."
                            " If using a Text File, the columns should be tab delimited.\n\n"
                            "The Attribute Tag data cannot contain spaces."
                        )
                    )
                    nil
                )
                (   (progn
                        (setq tmp (batte:RemoveAttribDuplicates data))
                        (setq removed (reverse removed))
                    )
                    (if
                        (= 6
                            (batte:Popup
                                (batte:WSH)
                                "Items Removed" (+ 32 4)
                                (strcat
                                    "A number of items in the selected file are not in the format required by this program.\n\n"
                                    "The following items have been removed from the list because either the block or tag values are missing"
                                    " or the tag value contains spaces:\n\n"
                                    (batte:lst->str removed "\n")
                                    "\n\nContinue?"
                                )
                            )
                        )
                        (if (cadr tmp)
                            (if
                                (= 6
                                    (batte:Popup
                                        (batte:WSH)
                                        "Duplicate Items Found" (+ 32 4)
                                        (strcat
                                            "A number of duplicate items were found in the selected file. Duplicate items arise when"
                                            " the same block and tag combination appear more than once in the list.\n\n"
                                            "The following duplicate items have been removed from the list:\n\n"
                                            (batte:lst->str (cadr tmp) "\n")
                                            "\n\nContinue?"
                                        )
                                    )
                                )
                                (car tmp)
                            )
                            data
                        )
                    )
                )
                (   (cadr tmp)
                    (if
                        (= 6
                            (batte:Popup
                                (batte:WSH)
                                "Duplicate Items Found" (+ 32 4)
                                (strcat
                                    "A number of duplicate items were found in the selected file. Duplicate items arise when"
                                    " the same block and tag combination appear more than once in the list.\n\n"
                                    "The following duplicate items have been removed from the list:\n\n"
                                    (batte:lst->str (cadr tmp) "\n")
                                    "\n\nContinue?"
                                )
                            )
                        )
                        (car tmp)
                    )
                )                 
                (   data   )
            )
        )
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:EscapeWildcards ( str )
    (vl-list->string
        (apply 'append
            (mapcar
               '(lambda ( c )
                    (if (member c '(35 64 46 42 63 126 91 93 45 44))
                        (list 96 c)
                        (list c)
                    )
                )
                (vl-string->list str)
            )
        )
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:EffectiveName ( blockentity / obj )
    (if (vlax-property-available-p (setq obj (vlax-ename->vla-object blockentity)) 'effectivename)
        (vla-get-effectivename obj)
        (vla-get-name obj)
    )
)

;;-------------------------------------------------------------------------------;;

(defun batte:RemoveAttribDuplicates ( lst / dup ret )
    (while lst
        (setq ret (cons (car lst) ret))
        (setq lst
            (vl-remove-if
                (function
                    (lambda ( x )
                        (if
                            (and
                                (eq (caar  lst) (car  x))
                                (eq (cadar lst) (cadr x))
                            )
                            (setq dup (cons (batte:lst->str x "\t") dup))
                        )
                    )
                )
                (cdr lst)
            )
        )
    )
    (list (reverse ret) (reverse dup))
)

;;-------------------------------------------------------------------------------;;

(defun batte:ConvertDataList ( lst / ass result )
    (while lst
        (if (setq ass (assoc (caar lst) result))
            (setq result
                (subst
                    (cons
                        (car ass)
                        (cons (cons (cadar lst) (caddar lst)) (cdr ass))
                    )
                    ass
                    result
                )
            )
            (setq result
                (cons
                    (list (caar lst) (cons (cadar lst) (caddar lst)))
                    result
                )
            )
        )
        (setq lst (cdr lst))
    )
    result
)

;;-------------------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: BatchAttributeEditor.lsp | Version "
        batteversion
        " | \\U+00A9 Lee Mac "
        ((lambda ( y ) (if (= y (menucmd "m=$(edtime,0,yyyy)")) y (strcat y "-" (menucmd "m=$(edtime,0,yyyy)")))) "2012")
        " www.lee-mac.com ::"
        "\n:: Type \"batte\" to Invoke ::"
    )
)
(princ)

;;-------------------------------------------------------------------------------;;
;;                                 End of File                                   ;;
;;-------------------------------------------------------------------------------;;