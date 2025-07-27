;;------------------=={ Get True Content }==------------------;;
;;                                                            ;;
;;  Returns the unformatted string associated with the        ;;
;;  supplied entity, in formats compatible with Text & MText  ;;
;;  objects.                                                  ;;
;;                                                            ;;
;;  The arguments *dtextstring & *mtextstring should be       ;;
;;  supplied with quoted symbols (other than those symbols    ;;
;;  used by the arguments themselves). The unformatted        ;;
;;  strings suitable for Text & MText objects will henceforth ;;
;;  be bound to the supplied symbol arguments respectively.   ;;
;;                                                            ;;
;;  Note that it is the caller's responsibility to create and ;;
;;  release the RegularExpressions (RegExp) object. This      ;;
;;  object may be created using the                           ;;
;;  Programmatic Identifier: "VBScript.RegExp".               ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  RegExp       - RegularExpressions (RegExp) Object         ;;
;;  entity       - Ename whose text content is to be returned ;;
;;  *dtextstring - (output) Unformatted string compatible     ;;
;;                 with Text entities                         ;;
;;  *mtextstring - (output) Unformatted string compatible     ;;
;;                 with MText entities                        ;;
;;------------------------------------------------------------;;
;;  Returns:    This function always returns nil              ;;
;;------------------------------------------------------------;;

(defun LM:GetTrueContent ( RegExp entity *dtextstring *mtextstring / _Replace _AllowsFormatting _GetTextString )

  (defun _Replace ( new old string )
    (vlax-put-property RegExp 'pattern old) (vlax-invoke RegExp 'replace string new)
  )

  (defun _AllowsFormatting ( entity / object )    
    (or (wcmatch (cdr (assoc 0 (entget entity))) "MTEXT,MULTILEADER")      
      (and
        (eq "ATTRIB" (cdr (assoc 0 (entget entity))))
        (vlax-property-available-p (setq object (vlax-ename->vla-object entity)) 'MTextAttribute)
        (eq :vlax-true (vla-get-MTextAttribute object))
      )
    )
  )

  (defun _GetTextString ( entity )
    (
      (lambda ( entity / _type elist )
        (cond
          ( (wcmatch (setq _type (cdr (assoc 0 (setq elist (entget entity))))) "TEXT,*DIMENSION")
           
            (cdr (assoc 1 (reverse elist)))
          )
          ( (eq "MULTILEADER" _type)

            (cdr (assoc 304 elist))
          )
          ( (wcmatch _type "ATTRIB,MTEXT")

            (
              (lambda ( string )
                (mapcar
                  (function
                    (lambda ( pair )
                      (if (member (car pair) '(1 3))
                        (setq string (strcat string (cdr pair)))
                      )
                    )
                  )
                  elist
                )
                string
              )
              ""
            )
          )
        )
      )
      (if (eq 'VLA-OBJECT (type entity))
        (vlax-vla-object->ename entity)
        entity
      )
    )
  )

  (
    (lambda ( string )
      (if string
        (progn
          (mapcar
            (function
              (lambda ( x ) (vlax-put-property RegExp (car x) (cdr x)))
            )
            (list (cons 'global actrue) (cons 'ignorecase acfalse) (cons 'multiline actrue))
          )
          (if (_AllowsFormatting entity)
            (mapcar
              (function
                (lambda ( x ) (setq string (_Replace (car x) (cdr x) string)))
              )
             '(
                ("Ð"       . "\\\\\\\\")
                (" "       . "\\\\P|\\n|\\t")
                ("$1"      . "\\\\(\\\\[ACcFfHLlOopQTW])|\\\\[ACcFfHLlOopQTW][^\\\\;]*;|\\\\[ACcFfHLlOopQTW]")
                ("$1$2/$3" . "([^\\\\])\\\\S([^;]*)[/#\\^]([^;]*);")
                ("$1$2"    . "\\\\(\\\\S)|[\\\\](})|}")
                ("$1"      . "[\\\\]({)|{")
              )
            )
            (setq string (_Replace "" "%%[OoUu]" (_Replace "Ð" "\\\\" string)))
          )
          (set *mtextstring (_Replace "\\\\" "Ð" (_Replace "\\$1$2$3" "(\\\\[ACcFfHLlOoPpQSTW])|({)|(})" string)))
          (set *dtextstring (_Replace "\\"   "Ð" string))
        )
      )
    )
    (_GetTextString entity)
  )
  nil
)


(defun c:test ( / *error* _AllowsFormatting RegExp src des text mtext )
  (vl-load-com)
  ;; © Lee Mac 2010

  (defun *error* ( msg )
    (if RegExp (vlax-release-object RegExp))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (defun _AllowsFormatting ( entity / object )    
    (or (wcmatch (cdr (assoc 0 (entget entity))) "MTEXT,MULTILEADER")      
      (and
        (eq "ATTRIB" (cdr (assoc 0 (entget entity))))
        (vlax-property-available-p (setq object (vlax-ename->vla-object entity)) 'MTextAttribute)
        (eq :vlax-true (vla-get-MTextAttribute object))
      )
    )
  )

  (while (and (setq src (car (nentsel "\nSelect Source Object: ")))
              (not (wcmatch (cdr (assoc 0 (entget src))) "*TEXT,ATTRIB,MULTILEADER")))
    (princ "\n** Source Object must Contain Text **")
  )

  (while (and (setq des (car (nentsel "\nSelect Destination Object: ")))
              (not (wcmatch (cdr (assoc 0 (entget des))) "*TEXT,ATTRIB,MULTILEADER")))
    (princ "\n** Destination Object must Contain Text **")
  )

  (if (and src des)
    (progn
      (setq RegExp (vlax-get-or-create-object "VBScript.RegExp"))      
      (LM:GetTrueContent RegExp src 'text 'mtext)

      (vla-put-TextString (vlax-ename->vla-object des)
        (if (_AllowsFormatting des)
          mtext
          text
        )
      )

      (vlax-release-object RegExp)
    )
  )

  (princ)
)

      
