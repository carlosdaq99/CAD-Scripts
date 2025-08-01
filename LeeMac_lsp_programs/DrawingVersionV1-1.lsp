;; Drawing Version  -  Lee Mac
;; Returns the version of the supplied filename (dwg/dws/dwt/dxf)
;; dwg - [str] Drawing filename

(defun LM:dwgversion ( dwg / des vrs )
    (cond
        (   (not
                (and
                    (setq dwg (findfile dwg))
                    (setq des (open dwg "r"))
                )
            )
        )
        (   (wcmatch (strcase dwg t) "*`.dw[gst]")
            (setq vrs (strcase (substr (read-line des) 1 6)))
        )
        (   (wcmatch (strcase dwg t) "*`.dxf")
            (repeat 7 (read-line des))
            (setq vrs (strcase (read-line des)))
        )
    )
    (if (= 'file (type des)) (close des))
    (cdr
        (assoc vrs
           '(
                ("AC1032" . "2018")
                ("AC1027" . "2013-2015")
                ("AC1024" . "2010-2012")
                ("AC1021" . "2007-2009")
                ("AC1018" . "2004-2006")
                ("AC1015" . "2000-2002")
                ("AC1014" . "Release 14")
                ("AC1012" . "Release 13")
                ("AC1009" . "Release 11/12")
                ("AC1006" . "Release 10")
                ("AC1004" . "Release 9")
                ("AC1003" . "Release 2.60")
                ("AC1002" . "Release 2.50")
                ("AC1001" . "Release 2.22")
                ("AC2.22" . "Release 2.22")
                ("AC2.21" . "Release 2.21")
                ("AC2.10" . "Release 2.10")
                ("AC1.50" . "Release 2.05")
                ("AC1.40" . "Release 1.40")
                ("AC1.2"  . "Release 1.2")
                ("MC0.0"  . "Release 1.0")
            )
        )
    )
)