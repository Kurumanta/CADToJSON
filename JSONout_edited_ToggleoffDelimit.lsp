;JSONout and JSONin (like DXFout/DXFin)
;ARKANCE 2025  -  www.arkance.world  -  www.cadforum.cz
;core JSON functions by Denon Deterding
;UI, tables by Vladimir Michl
;
;JSON export and import of drawing entities
;only modelspace, visible entities are supported
;limited layer, block, style, dim, linetype definitions (no dependencies)!
;no reactors, no dynamic blocks
;big polylines and meshes may be limited by string length
;
;add: assoc list to convert dxf codes to symbolic names

(setq _JSON_decimal_precision 15) ; adjust if needed (for JSONout)
; (setq _JSON_no_friendy_names T) ; use DXF codes (8) of friendly names (Layer) for export

;assoc list for friendly names
(setq _JSON_DXF_Name '((-2 . "EntNameRef")(-1 . "EntName")
						(0 . "EntType")(1 . "TextString")(2 . "ObjName")(3 . "Description_Prompt")(4 . "BlkDescr")(5 . "EntHandle")
						(6 . "LineType")(7 . "TextStyle")(8 . "Layer")(9 . "LtText")
						(10 . "RefPoint")(11 . "SecondPoint")(12 . "TargetPt")(13 . "ImgSize")(14 . "ClipVertex")
						(38 . "Elevation")(39 . "Thickness")(40 . "Size")(41 . "ScaleX_StartA")(42 . "ScaleY_EndA")(43 . "ScaleZ")
						(44 . "OffsetX_LineSp")(45 . "OffsetY")(46 . "ElemScale")(48 . "LtScale")(49 . "Segments")
						(50 . "Rotation_StartA")(51 . "ObliqueA_EndA")(53 . "DimAngle")(60 . "Visibility")(62 . "Color")(63 . "BgColor")(64 . "CellColor")
						(66 . "EntFollow")(67 . "EntSpace")
						(70 . "Modes")(71 . "Attach_Degree_Assoc")(72 . "Direction")(73 . "Spacing_Len")(74 . "ElemType_Just")
						(75 . "HatchStyle_ColType_Shape")(76 "LeaderNodes")(77 . "LeaderColor")
						(90 . "BgFill")(91 . "Rows_ClipNodes_LdCol")(92 . "Columns_Vertices")
						(100 . "SubClass")(102 . "AppGroup")(140 . "TextHg")(141 . "RowHg")(142 . "ColHg")(144 . "CellBlkScale")(145 . "CellRot")
						(170 . "LeaderLT_Align")(171 . "LeaderLW_CellType")(172 . "LeaderType")(174 . "LeaderAng")(174 . "LeaderAlign")
						(210 . "ExtrusionVec")(280 . "ClipMode_Constrain")(281 . "RenderMode_Bright")(282 . "Contrast")(283 . "Fade")
						(290 . "PlotFlag")(370 . "LineWeight")(380 . "PlotStyle")
						(410 . "Layout")(420 . "TrueColor")(440 . "Transparency")(441 . "BgTransparency")(1071 . "TTFparams")
					  ))
(setq _JSON_DXF_Code _JSON_DXF_Name ; reverse to (("EntNameRef" . -2)("EntName" . -1)...)
	  _JSON_DXF_Code (foreach x _JSON_DXF_Code (setq _JSON_DXF_Code (subst (cons (cdr x) (car x)) x _JSON_DXF_Code))))


;EXPORT JSON -----------------------------------------
(defun C:JSONout ( / ss ent i filepath aux file edata)

;; Converts AutoLISP list to JSON string
;; by Denon Deterding, 20201130, v1.0
;; p - int or nil, from 0 to 15 of desired real precision. if nil, current acad precision is used
;; l - list, of data to be converted to json
;; returns - string, of data converted to json
  ; start lisptojson function ----------------
(defun lisptojson (p l / _lisptojson dpp)
  
  
  ;internal recursive function (??nh ngh?a hàm ?? quy bên trong)
  (defun _lisptojson (p e / typ arr str hand)
    ; e - element to be converted
    ; typ - type of element
    ; arr - flag, if true then e is an array (list of lists)
    ; str - string, result of conversion
    ; hand - entity handle, if e is an entity name
    ; hàm này có thể gọi lại chính nó để xử lý các phần tử con trong danh sách
    ; set default flag
    (setq typ (type e)  arr nil) ;set default flag
    (cond
      ((eq 'ENAME typ) (if (not (setq hand (cdr (assoc 5 (entget e)))))(setq hand "00"))(strcat "\"" hand "\""))
      ((eq 'INT typ) (itoa e))
      ((eq 'LIST typ)
        (if (or (dpp e)
                (and (= 2 (length e))
                     (atom (car e))
                     (listp (cadr e))
                     (setq arr t)))
          ; true branch
          ; ti?p t?c ki?m tra n?u c?p ??u tiên là string (key) ?? t?o object b?ng cách ki?m tra symbol trên ph?n t? ??u tiên.
          ; k?t qu? c?a tr??ng h?p này s? là m?t object
          ; "{k?t qu? ?? quy hàm con trên ph?n t? ??u tiên c?a e:k?t qu? ?? quy c?a hàm con trên ph?n t? th? hai c?a e, n?u arr thì l?y ph?n t? th? hai tr? ?i, else l?y ph?n t? cu?i cùng}"
          (if (eq 'STR (type (car e)))
            (strcat "{" (_lisptojson p (car e)) ":" (_lisptojson p (if arr (cadr e) (cdr e))) "}")
          ;else
            (progn (prompt "\n; 'lisptojson' error: invalid json object") (exit))
          );if
        ;else nhánh nil
          ; x? lý n?i chu?i ?? k?t xu?t str
          (setq str (mapcar '(lambda (x) (strcat "," (_lisptojson p x
                                                     )
                                         )
                               ; ánh x? t?ng ph?n t? con trong danh sách e thành chu?i con (???c n?i v?i d?u ph?y, ?? quy ph?n t? k? ti?p ?? x? lý thành chu?i JSON)
                               ; nguyên t?c ?? quy: m? ??c thông tin và ghi
                               ; ánh x? str c? thành str m?i b?ng vi?c n?i v?i result c?a hàm ?? quy
                             ) 
                    e
                    )
			; k?t qu? ", str1, str2, str3..."
                ; ánh x? e hi?n t?i thành th?c th? chu?i m?i n?i v?i ph?n t? tr??c ?ó b?i d?u ph?y
                str (apply 'append (list '("[") (list (substr (car str) 2)) (cdr str) '("]")))
                ; th?c thi n?i t?t c? các chu?i con thành m?t chu?i l?n
                str (apply 'strcat str))
        );if
      )
      ((eq 'REAL typ) (if p (rtos e 2 p) (rtos e 2)))
  ;end internal recursive function
      
      
      ;      ((eq 'STR typ) (strcat "\"" e "\""))
      ((eq 'STR typ) (vl-prin1-to-string e)) ; escape e (mod VM)
      ((eq 'SYM typ) (if (eq t e) "true" "null"))
      (t "null")
    );cond
  );defun _lisptojson end
  
  
  (setq dpp (lambda (x) (not (or (atom x) (listp (cdr x))))))
    ;flag để xác định xem đối tượng có phải là dot pair predicated hay không, định nghĩa dpp trong hàm đệ quy
  (if (and (listp l)
           (or (null p)
               (and (eq 'INT (type p))
                    (<= 0 p 15)
                              )
                              )
                              )
    (_lisptojson p l)
  );if điều kiện thực thi hàm con khi và chỉ khi l là danh sách và p là số nguyên từ 0 đến 15 hoặc nil
  )

  ; end lisptojson function ----------------
;[{"-1":"B1BF"},{"0":"MTEXT"},{"330":"AACF"},{"5":"B1BF"},{"100":"AcDbEntity"},{"67":0},{"410":"Model"},{"8":"EMPLOYEE"},{"370":-3},{"100":"AcDbMText"},["10",1434.872766197735,3309.867400197119,0.000000000000000],{"40":6.000000000000000},{"41":24.87585266030014},{"46":0.000000000000000},{"71":1},{"72":5},{"1":"\\A1;Robert\\PNelson"},{"7":"ARCHITXT"},["210",0.000000000000000,0.000000000000000,1.000000000000000],["11",1.000000000000000,0.000000000000000,0.000000000000000],{"42":28.00000000000001},{"43":16.00000000000000},{"50":0.000000000000000},{"73":1},{"44":1.000000000000000}],
;[{"EntName":"B1BF"},{"EntType":"MTEXT"},{"330":"AACF"},{"EntHandle":"B1BF"},{"SubClass":"AcDbEntity"},{"EntSpace":0},{"Layout":"Model"},{"Layer":"EMPLOYEE"},{"370":-3},{"SubClass":"AcDbMText"},["RefPoint",1434.872766197735,3309.867400197119,0.000000000000000],{"Size":6.000000000000000},{"ScaleX_StartA":24.87585266030014},{"ElemScale":0.000000000000000},{"Attach_Degree_Assoc":1},{"Direction":5},{"TextString":"\\A1;Robert\\PNelson"},{"TextStyle":"ARCHITXT"},["ExtrusionVec",0.000000000000000,0.000000000000000,1.000000000000000],["SecondPoint",1.000000000000000,0.000000000000000,0.000000000000000],{"ScaleY_EndA":28.00000000000001},{"ScaleZ":16.00000000000000},{"Rotation_StartA":0.000000000000000},{"Spacing_Len":1},{"OffsetX_LineSp":1.000000000000000}],
  
  
  ; start write-JSON function ----------------
  
(defun write-JSON (edata file delimit / str) ; write JSON line
 (setq str (lisptojson
                _JSON_decimal_precision
                (mapcar
                 '(lambda (x / dxf) ; itoa over assoc list! ***
                   (if (not _JSON_no_friendy_names) (setq dxf (assoc (car x) _JSON_DXF_Name)))
                   (cons (if dxf (cdr dxf)(itoa (car x)))
                         (cdr x))
                  )
                 edata
                )
               ))
 (if delimit (setq str (strcat str ",")))
 (write-line str file) ; JSON entity
)
  ; end write-JSON function ----------------
  
  
(defun dxf (e code)
 (cdr (assoc code e))
)

(defun JSONoutTbl ( tname / a ent)
 (write-line (strcat "[\n\"" tname "\",") file)
 (while
  (setq a (tblnext tname (null a))) ; BLOCK, LAYER....
   (if (not (and (= tname "BLOCK") (>= (cdr (assoc 70 a)) 4))) ; no block xrefs
     (progn
       (setq ent (tblobjname tname (cdr (assoc 2 a))))
;(PRINT (entget ent))
       (write-JSON (entget ent) file T)
       (while
        (setq ent (entnext ent))
;(PRINT (entget ent))
        (write-JSON (entget ent) file T)
       )
       (if (= tname "BLOCK")(write-JSON '((0 . "ENDBLK") (100 . "AcDbBlockEnd") (8 . "0")) file T))
     )
   )
 );while
 (write-line (strcat "\"END" tname "\"\n],") file)
)

;--START Export--
 (setq filepath (strcat (getvar "DWGPREFIX") "\\" (getvar "DWGNAME") ".json")) ; WORKINGFOLDER
 (if (zerop (getvar "FILEDIA"))
  (setq aux (getstring (strcat "\nExport JSON file name <" filepath ">: "))  filepath (if (= aux "") filepath aux))
  (setq filepath (getfiled "Export JSON file" filepath "json" 1))
 )
 (if filepath (progn
  (setq file (open filepath "w"))
  (if file (progn
   (prompt "\nSelect objects to JSONout <all>: ")
   (setq ss (ssget))
   (if (not ss)(setq ss (ssget "_X")))
;OBJ.TABLES?
   (initget "Yes No")
   (setq aux (getkword "\nInclude def. tables (layers, blocks, linetypes...) [Yes/No] <No>: "))
   (write-line "[" file) ; JSON start array
   (if (= aux "Yes")(progn
   (princ "\nExporting def. tables...")
    (JSONoutTbl "BLOCK")
    (JSONoutTbl "LAYER")
    (JSONoutTbl "LTYPE")
    (JSONoutTbl "STYLE")
    (JSONoutTbl "DIMSTYLE")
    (JSONoutTbl "VIEW")
    (JSONoutTbl "UCS")
   ))

   (setq i -1)
   (write-line "[\n\"ENTITIES\"," file) ; JSON start array
   (princ "\nExporting entities...")
   (while (< (setq i (1+ i)) (sslength ss)) ; all selected
     (setq ent (ssname ss i))
     (write-JSON (setq edata (entget ent)) file T) ; simple entity
     (if (and (= (dxf edata 0) "INSERT") ; BLOCK?
              (= (dxf edata 66) 1))(progn ; attributes follows - nested
      (setq ent (entnext ent))
      (while ent
       (write-JSON (setq edata (entget ent)) file T)
       (if (= (dxf edata 0) "SEQEND")
         (setq ent nil)
         (setq ent (entnext ent))
       )
      );while attr
     ));BLOCK
     (if (and (= (dxf edata 0) "POLYLINE") ; POLYLINE?
              (= (dxf edata 66) 1))(progn ; verices follows - nested
      (setq ent (entnext ent))
      (while ent
       (write-JSON (setq edata (entget ent)) file T)
       (if (= (dxf edata 0) "SEQEND")
         (setq ent nil)
         (setq ent (entnext ent))
       )
      );while attr
     ));POLYLINE
   );while
   (write-line "\"ENDENTITIES\"\n]\n]" file) ; JSON end array
   (close file)
   (princ (strcat "\n" (itoa i) " entit" (if (> i 1) "ies" "y") " exported to " filepath))
  )); file
 )); filepath
 (princ)
)
; - Finish c:JSONout definition



;---------------------------------------------------------------------------




;IMPORT JSON (exported by JSONout, not general JSON!) ----------------------


;uses (entmake (list '(0 . "POLYLINE") '(70 . 64) (cons 71 13) (cons 72 15) ...))

(defun C:JSONin ( / filepath file aux i j line parsed pair entm dxf)

 (defun ParseJSON->List (json / data newData inStr dPrev strPos)
  ;Converts json string to list. By Denon Deterding (VL2 - 20200924)
  ;NOTE: json object strings will be truncated to 2300 characters MAX ('read' function limitation)
  ;----- (the 'json' input variable is NOT limited to the 2300 chr limit)
  ;json - string, as json data
  ;returns - list, of data from json
  (setq data (vl-string->list json))
  (foreach d data
    (cond
      ;if we're in a string, test length and continue
      (inStr
        (if (or (setq inStr (and (= 34 d) (/= 92 dPrev)))
                (<= strPos 2300)) ;[--- MAX string length is 2300 characters ---]
          (setq strPos (1+ strPos) newData (cons d newData))
        );if
        (setq instr (not inStr))
      );cond 1
      ;this signals that we're starting a string
      ((= 34 d)
        (setq inStr t strPos 1 newData (cons d newData))
      );cond 2
      ;replacing relevant characters Outside of json object strings
      ((member d '(44 58 91 93 123 125)) ; ("," ":" "[" "]" "{" "}")
        (foreach n (cond
                     ((= 44 d) '(41 32 40))             ; ") ("
                     ((= 58 d) '(32))                   ; " "
                     ((or (= 91 d) (= 123 d)) '(40 40)) ; "(("
                     ((or (= 93 d) (= 125 d)) '(41 41)) ; "))"
                   );cond
          (setq newData (cons n newData))
        );foreach
      );cond 3
      ;these will signal a "true" value
      ((or (= 116 d) (= 84 d)) ; ("t" "T")
        (setq newData (cons 116 newData)) ; "t"
      );cond 4
      ;these will signal a "null" or "false" value (both are nil in lisp)
      ((or (= 110 d) (= 102 d) (= 78 d) (= 70 d)) ; ("n" "f" "N" "F")
        (foreach n '(110 105 108) (setq newData (cons n newData))) ; "nil"
      );cond 5
      ;this handles numbers.. real/int, pos/neg
      ((member d '(45 46 48 49 50 51 52 53 54 55 56 57)) ; ("-" "." "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")
        (setq newData (cons d newData))
      );cond 6
    );cond
    (setq dPrev d)
  );foreach
  (read (vl-list->string (reverse newData)))
 );defun

 (defun getDXF (src / ret) ; return DXF code - uses _JSON_DXF_Code
  (if (= (type (read src)) 'INT) ; classic
   (setq ret (read src)) ; else
   (if (= (type (read src)) 'SYM) ; friendly
    (if (not (setq ret (cdr (assoc src _JSON_DXF_Code))))
     (princ (strcat " *UNKNOWN NAME " src " * "))
    )
  ))
  ret
 )

;--START Import--
;check TILEMODE? make independent on end-of-lines?
(setq filepath (strcat (getvar "DWGPREFIX") "\\" (getvar "DWGNAME") ".json")) ; WORKINGFOLDER
 (if (zerop (getvar "FILEDIA"))
  (setq aux (getstring (strcat "\nImport JSON file name <" filepath ">: "))  filepath (if (= aux "") filepath aux))
  (setq filepath (getfiled "Import JSON file" filepath "json" 4))
 )
 (if filepath (progn
  (setq file (open filepath "r"))
  (if file (progn
   (setq i 0  j 0)
   (princ "\nImporting...")
   (while (setq line (read-line file))
    (setq i (1+ i))
;(PRINT line)
    (if (not (wcmatch line "`[,`],`]`,,`,"))(progn ; skip lines with just "[", "]", "]," or ","
     (setq parsed (ParseJSON->List line))
;(PRINT parsed)
     (if (= (type parsed) 'LIST)(progn
      (setq entm (list)) ; entmake list
      (foreach pair parsed ; pair= (((100 AcDbPolyline))) or (((("10") (1705.5) (3294.01))))
       (if (and (= (length (car pair)) 1) ; simple value
                (not (member (setq dxf (getDXF (caaar pair))) '(-1 -2 330 337 338 340 342 343 347 348 350 360 390 480 999)))) ; skip pointers and aux
           (setq entm (append (list (cons dxf (cadaar pair))) entm)))
       (if (> (length (car pair)) 1) ; array/vector/list
           (setq entm (append (list (cons (getDXF (caaar pair))(cdr (mapcar 'car (car pair))))) entm)))
      );foreach dxf
      (setq entm (reverse entm))
;(princ "\nFINAL-ENTM=====")(prin1 entm)
      (if (> (length entm) 1)
       (if (entmake entm)(setq j (1+ j))) ; create entity
      )
     )); LIST?
    ));[]?
   );while file
  (princ (strcat "\nImported " (itoa i) " lines, created " (itoa j) " entities."))
  ));file
 ));filepath
 (princ)
)

;CZ version aliases:
(defun C:PIŠJSON ( / )(C:JSONout)(princ))
(defun C:ÈTIJSON ( / )(C:JSONin)(princ))

(princ "\nJSONout/JSONin commands loaded (2025, www.arkance.world, www.cadforum.cz)")
(princ)
