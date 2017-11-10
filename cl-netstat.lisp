;;;; cl-stats.lisp

#|
|#

(in-package #:cl-netstat)

;;; "cl-stats" goes here. Hacks and glory await!

(let ((xb (ash 1 53)) ;; 8xb
      (tb (ash 1 43)) ;; 8tb
      (gb (ash 1 33)) ;; 8gb
      (mb (ash 1 23)) ;; 8mb
      (kb (ash 1 13)));; 8kb
  (defun format-size (size &optional (when-zero nil when-zero-given?))
    "formats given size (number) to a more readable format (string),
    when-zero can be given to return it instead of \"0Byt\""
    (if (and (eql 0 size)
             when-zero-given?)
        when-zero
        (values
         (cond
           ((> size xb) (format nil "~4d PiB" (ash size -50)))
           ((> size tb) (format nil "~4d TiB" (ash size -40)))
           ((> size gb) (format nil "~4d GiB" (ash size -30)))
           ((> size mb) (format nil "~4d MiB" (ash size -20)))
           ((> size kb) (format nil "~4d KiB" (ash size -10)))
           (t           (format nil "~4d Byt" size)          ))
         (list :black
               (if (eql 0 size)
                   :white
                   (list :number (get-match (get-size-color size)))))))))

(defun get-interface-data ()
  (with-open-file (stream "/proc/net/dev"
                          :direction :input)
    ;; ignore first 2 lines
    (dotimes (ignored 2) (read-line stream nil nil))
    (loop :for line = (read-line stream nil nil)
          :while line
          :collect (destructuring-bind (interface data)
                       (cl-ppcre:split ":" (string-trim " " line))
                     (cons interface (mapcar (lambda (val data)
                                               (cons val
                                                     (parse-integer data)))
                                             (list :rec-bytes :rec-packets :rec-errs
                                                   :rec-drop :rec-fifo :rec-frame
                                                   :rec-compressed :rec-multicast
                                                   :trans-bytes :trans-packets :trans-errs
                                                   :trans-drop :trans-fifo :trans-colls
                                                   :trans-carrier :trans-compressed)
                                             (cdr (cl-ppcre:split "\\s+" data))))))))


(defmacro assoc-chain (args data)
  `(assoc ,(car (last args))
          ,(if (cdr args)
               `(cdr (assoc-chain ,(butlast args)
                                  ,data))
               data)
          :test #'equalp))

(defmacro with-assocs (bindings data &body body)
  (let ((data-sym (gensym "data")))
    `(let ((,data-sym ,data))
       (let ,(loop :for (var chain) :in bindings
                   :collect (list var `(assoc-chain ,chain ,data-sym)))
         ,@body))))

(defmacro mapassoc (function list &rest more-lists)
  (let ((val (gensym "val"))
        (args (gensym "args")))
    `(mapcar (lambda (&rest ,args)
               (cons (caar ,args)
                     (apply ,function
                            (mapcar #'cdr ,args))))
             ,list
             ,@more-lists)))

(defmacro with-style ((window color-pair &optional attributes) &body body)
  (let ((old-color-pair (gensym "old-color-pair"))
        (old-attributes (gensym "old-attributes"))
        (new-color-pair (gensym "new-color-pair"))
        (new-attributes (gensym "new-attributes")))
    `(let ((,old-color-pair (croatoan:.color-pair ,window))
           (,old-attributes (croatoan:.attributes ,window))
           (,new-color-pair ,color-pair)
           (,new-attributes ,attributes))
       (when ,new-color-pair
         (setf (croatoan:.color-pair ,window) ,new-color-pair))
       (when ,new-attributes
         (setf (croatoan:.attributes ,window) ,new-attributes))
       (prog1 (progn ,@body)
         (when ,new-color-pair
           (setf (croatoan:.color-pair ,window)
                 ,old-color-pair))
         (when ,new-attributes
            (setf (croatoan:.attributes ,window)
                  ,old-attributes))))))

(defun format-interface-data (data)
  (loop :for (interface . data) :in data
        :collect (cons interface
                       (mapassoc #'format-size
                                 data))))

(defun diff-interface-data (a b)
  (loop :for (interface-a . data-a) :in a
        :for (interface-b . data-b) :in b
        :when (string= interface-a interface-b)
        :collect (cons interface-a
                       (mapassoc #'- data-b data-a))))

(defparameter *last-stats* nil)
(setf *last-stats* (get-interface-data))

(defun format-interfaces (window stats)
  (with-style (window '(:white :black) '(:bold :underline))
    (croatoan:add-string window
                         (format nil "~12,,,' a~{ ~8,,,' a~}"
                                 "NETWORK"
                                 (list "Total Rx" "Total Tx" "Rx/s" "Tx/s"))))
  (loop :for stat :in stats
        :do
        (croatoan:new-line window)
        (croatoan:add-string window
                             (format nil "~12,,,' a" (car stat)))
        (loop :for bytes :in (cdr stat)
              :do (multiple-value-bind (str color)
                      (format-size bytes)
                    (croatoan:add-char window #\Space)
                    (with-style (window color)
                      (croatoan:add-string window
                                           (format nil "~8,,,' a" str)))))))

(defun gen-stats (last cur)
  (loop :for (interface . data) :in (diff-interface-data last cur)
        :collect (list interface
                       (cdr (assoc-chain (interface :rec-bytes) cur))
                       (cdr (assoc-chain (interface :trans-bytes) cur))
                       (cdr (assoc-chain (:rec-bytes) data))
                       (cdr (assoc-chain (:trans-bytes) data)))))

(defparameter *win* nil)

(defun draw (scr)
  (sleep 1.0)
  (croatoan:clear scr)
  (croatoan:move scr 0 0)
  (setf (croatoan:.color-pair scr)
        '(:white :black))
  (let ((stats (gen-stats *last-stats* (setf *last-stats* (get-interface-data)))))
    ;;(croatoan:new-line scr)
    ;; (let ((window (make-instance'croatoan:window))))
    (format-interfaces scr stats)
    ;; (loop :for i :from 16 :to 255
    ;;       :do
    ;;       (when (eql 0 (mod (- i 4) 6))
    ;;         (croatoan:new-line scr))
    ;;       (with-style (scr (list :black (list :number i)))
    ;;         (croatoan:add-string scr (format nil "~4,,,' a " i))))
    (croatoan:refresh scr)))

(defun window ()
  (croatoan:with-screen (scr :input-echoing nil
                             :input-blocking nil
                             :enable-fkeys t
                             :cursor-visibility nil)
    (croatoan:clear scr)
    (croatoan:box scr)
    (croatoan:refresh scr)
    (croatoan:event-case (scr event)
      (#\q (return-from croatoan:event-case))
      ((nil)
       (draw scr)))))

(defun red-yellow-green-gradient-generator (count)
  (let ((red 255)
        (green 0)
        (step-size (/ 255 (/ count 2))))
    (flet ((fmt (red green)
             (format nil "#~2,'0X~2,'0X00" (round red) (round green))))
      (reverse
       (append
        (loop :while (< green 255)
              :do (incf green step-size)
              :when (> green 255)
              :do (setf green 255)
              :collect (fmt red green))
        (loop :while (> red 0)
              :do (decf red step-size)
              :when (< red 0)
              :do (setf red 0)
              :collect (fmt red green)))))))

(let ((lookup (make-array '(42)
                          :initial-contents (red-yellow-green-gradient-generator 42)
                          :adjustable nil)))
  (defun get-size-color (size)
    (let ((spot (integer-length size)))
      (if (> spot 41)
          (aref lookup 41)
          (aref lookup spot)))))

(defun color-hashtag-p (color)
  (if (char-equal #\# (aref color 0)) t nil))

(defun color-rgb->string (r g b &optional hashtag-p)
  (concatenate 'string
               (when hashtag-p "#")
               (write-to-string r :base 16)
               (write-to-string g :base 16)
               (write-to-string b :base 16)))

(defun color-string->rgb (color)
  (when (color-hashtag-p color)
    (setf color (subseq color 1 7)))
  (values (parse-integer (subseq color 0 2) :radix 16)
          (parse-integer (subseq color 2 4) :radix 16)
          (parse-integer (subseq color 4 6) :radix 16)))


(let ((table (make-hash-table)))
  (defun get-match (color)
    (let ((match (gethash color table)))
      (when match
        (return-from get-match match)))
    (let ((best-match-diff (* 3 255))
          (best-match 0))
      (multiple-value-bind (r g b)
          (color-string->rgb color)
        (loop :for (k . (rr gg bb)) :in (mapassoc (lambda (rgb)
                                                    (mapcar (lambda (val)
                                                              (parse-integer val :radix 16))
                                                            rgb))
                                                  *term->rgb*)
              :do (let ((diff (reduce #'+
                                      (mapcar (alexandria:compose #'abs #'-)
                                              (list r g b)
                                              (list rr gg bb)))))
                    (when (< diff best-match-diff)
                      (setf best-match k
                            best-match-diff diff))
                    (when (eql 0 best-match-diff)
                      (return)))))
      (setf (gethash color table) best-match))))

(defparameter *term->rgb*
  '((  0 . ("00" "00" "00")) (  1 . ("80" "00" "00")) (  2 . ("00" "80" "00")) (  3 . ("80" "80" "00"))
    (  4 . ("00" "00" "80")) (  5 . ("80" "00" "80")) (  6 . ("00" "80" "80")) (  7 . ("c0" "c0" "c0"))

    (  8 . ("80" "80" "80")) (  9 . ("ff" "00" "00")) ( 10 . ("00" "ff" "00")) ( 11 . ("ff" "ff" "00"))
    ( 12 . ("00" "00" "ff")) ( 13 . ("ff" "00" "ff")) ( 14 . ("00" "ff" "ff")) ( 15 . ("ff" "ff" "ff"))

    ( 16 . ("00" "00" "00")) ( 17 . ("00" "00" "5f")) ( 18 . ("00" "00" "87")) ( 19 . ("00" "00" "af"))
    ( 20 . ("00" "00" "d7")) ( 21 . ("00" "00" "ff")) ( 22 . ("00" "5f" "00")) ( 23 . ("00" "5f" "5f"))
    ( 24 . ("00" "5f" "87")) ( 25 . ("00" "5f" "af")) ( 26 . ("00" "5f" "d7")) ( 27 . ("00" "5f" "ff"))
    ( 28 . ("00" "87" "00")) ( 29 . ("00" "87" "5f")) ( 30 . ("00" "87" "87")) ( 31 . ("00" "87" "af"))
    ( 32 . ("00" "87" "d7")) ( 33 . ("00" "87" "ff")) ( 34 . ("00" "af" "00")) ( 35 . ("00" "af" "5f"))
    ( 36 . ("00" "af" "87")) ( 37 . ("00" "af" "af")) ( 38 . ("00" "af" "d7")) ( 39 . ("00" "af" "ff"))
    ( 40 . ("00" "d7" "00")) ( 41 . ("00" "d7" "5f")) ( 42 . ("00" "d7" "87")) ( 43 . ("00" "d7" "af"))
    ( 44 . ("00" "d7" "d7")) ( 45 . ("00" "d7" "ff")) ( 46 . ("00" "ff" "00")) ( 47 . ("00" "ff" "5f"))
    ( 48 . ("00" "ff" "87")) ( 49 . ("00" "ff" "af")) ( 50 . ("00" "ff" "d7")) ( 51 . ("00" "ff" "ff"))
    ( 52 . ("5f" "00" "00")) ( 53 . ("5f" "00" "5f")) ( 54 . ("5f" "00" "87")) ( 55 . ("5f" "00" "af"))
    ( 56 . ("5f" "00" "d7")) ( 57 . ("5f" "00" "ff")) ( 58 . ("5f" "5f" "00")) ( 59 . ("5f" "5f" "5f"))
    ( 60 . ("5f" "5f" "87")) ( 61 . ("5f" "5f" "af")) ( 62 . ("5f" "5f" "d7")) ( 63 . ("5f" "5f" "ff"))
    ( 64 . ("5f" "87" "00")) ( 65 . ("5f" "87" "5f")) ( 66 . ("5f" "87" "87")) ( 67 . ("5f" "87" "af"))
    ( 68 . ("5f" "87" "d7")) ( 69 . ("5f" "87" "ff")) ( 70 . ("5f" "af" "00")) ( 71 . ("5f" "af" "5f"))
    ( 72 . ("5f" "af" "87")) ( 73 . ("5f" "af" "af")) ( 74 . ("5f" "af" "d7")) ( 75 . ("5f" "af" "ff"))
    ( 76 . ("5f" "d7" "00")) ( 77 . ("5f" "d7" "5f")) ( 78 . ("5f" "d7" "87")) ( 79 . ("5f" "d7" "af"))
    ( 80 . ("5f" "d7" "d7")) ( 81 . ("5f" "d7" "ff")) ( 82 . ("5f" "ff" "00")) ( 83 . ("5f" "ff" "5f"))
    ( 84 . ("5f" "ff" "87")) ( 85 . ("5f" "ff" "af")) ( 86 . ("5f" "ff" "d7")) ( 87 . ("5f" "ff" "ff"))
    ( 88 . ("87" "00" "00")) ( 89 . ("87" "00" "5f")) ( 90 . ("87" "00" "87")) ( 91 . ("87" "00" "af"))
    ( 92 . ("87" "00" "d7")) ( 93 . ("87" "00" "ff")) ( 94 . ("87" "5f" "00")) ( 95 . ("87" "5f" "5f"))
    ( 96 . ("87" "5f" "87")) ( 97 . ("87" "5f" "af")) ( 98 . ("87" "5f" "d7")) ( 99 . ("87" "5f" "ff"))
    (100 . ("87" "87" "00")) (101 . ("87" "87" "5f")) (102 . ("87" "87" "87")) (103 . ("87" "87" "af"))
    (104 . ("87" "87" "d7")) (105 . ("87" "87" "ff")) (106 . ("87" "af" "00")) (107 . ("87" "af" "5f"))
    (108 . ("87" "af" "87")) (109 . ("87" "af" "af")) (110 . ("87" "af" "d7")) (111 . ("87" "af" "ff"))
    (112 . ("87" "d7" "00")) (113 . ("87" "d7" "5f")) (114 . ("87" "d7" "87")) (115 . ("87" "d7" "af"))
    (116 . ("87" "d7" "d7")) (117 . ("87" "d7" "ff")) (118 . ("87" "ff" "00")) (119 . ("87" "ff" "5f"))
    (120 . ("87" "ff" "87")) (121 . ("87" "ff" "af")) (122 . ("87" "ff" "d7")) (123 . ("87" "ff" "ff"))
    (124 . ("af" "00" "00")) (125 . ("af" "00" "5f")) (126 . ("af" "00" "87")) (127 . ("af" "00" "af"))
    (128 . ("af" "00" "d7")) (129 . ("af" "00" "ff")) (130 . ("af" "5f" "00")) (131 . ("af" "5f" "5f"))
    (132 . ("af" "5f" "87")) (133 . ("af" "5f" "af")) (134 . ("af" "5f" "d7")) (135 . ("af" "5f" "ff"))
    (136 . ("af" "87" "00")) (137 . ("af" "87" "5f")) (138 . ("af" "87" "87")) (139 . ("af" "87" "af"))
    (140 . ("af" "87" "d7")) (141 . ("af" "87" "ff")) (142 . ("af" "af" "00")) (143 . ("af" "af" "5f"))
    (144 . ("af" "af" "87")) (145 . ("af" "af" "af")) (146 . ("af" "af" "d7")) (147 . ("af" "af" "ff"))
    (148 . ("af" "d7" "00")) (149 . ("af" "d7" "5f")) (150 . ("af" "d7" "87")) (151 . ("af" "d7" "af"))
    (152 . ("af" "d7" "d7")) (153 . ("af" "d7" "ff")) (154 . ("af" "ff" "00")) (155 . ("af" "ff" "5f"))
    (156 . ("af" "ff" "87")) (157 . ("af" "ff" "af")) (158 . ("af" "ff" "d7")) (159 . ("af" "ff" "ff"))
    (160 . ("d7" "00" "00")) (161 . ("d7" "00" "5f")) (162 . ("d7" "00" "87")) (163 . ("d7" "00" "af"))
    (164 . ("d7" "00" "d7")) (165 . ("d7" "00" "ff")) (166 . ("d7" "5f" "00")) (167 . ("d7" "5f" "5f"))
    (168 . ("d7" "5f" "87")) (169 . ("d7" "5f" "af")) (170 . ("d7" "5f" "d7")) (171 . ("d7" "5f" "ff"))
    (172 . ("d7" "87" "00")) (173 . ("d7" "87" "5f")) (174 . ("d7" "87" "87")) (175 . ("d7" "87" "af"))
    (176 . ("d7" "87" "d7")) (177 . ("d7" "87" "ff")) (178 . ("d7" "af" "00")) (179 . ("d7" "af" "5f"))
    (180 . ("d7" "af" "87")) (181 . ("d7" "af" "af")) (182 . ("d7" "af" "d7")) (183 . ("d7" "af" "ff"))
    (184 . ("d7" "d7" "00")) (185 . ("d7" "d7" "5f")) (186 . ("d7" "d7" "87")) (187 . ("d7" "d7" "af"))
    (188 . ("d7" "d7" "d7")) (189 . ("d7" "d7" "ff")) (190 . ("d7" "ff" "00")) (191 . ("d7" "ff" "5f"))
    (192 . ("d7" "ff" "87")) (193 . ("d7" "ff" "af")) (194 . ("d7" "ff" "d7")) (195 . ("d7" "ff" "ff"))
    (196 . ("ff" "00" "00")) (197 . ("ff" "00" "5f")) (198 . ("ff" "00" "87")) (199 . ("ff" "00" "af"))
    (200 . ("ff" "00" "d7")) (201 . ("ff" "00" "ff")) (202 . ("ff" "5f" "00")) (203 . ("ff" "5f" "5f"))
    (204 . ("ff" "5f" "87")) (205 . ("ff" "5f" "af")) (206 . ("ff" "5f" "d7")) (207 . ("ff" "5f" "ff"))
    (208 . ("ff" "87" "00")) (209 . ("ff" "87" "5f")) (210 . ("ff" "87" "87")) (211 . ("ff" "87" "af"))
    (212 . ("ff" "87" "d7")) (213 . ("ff" "87" "ff")) (214 . ("ff" "af" "00")) (215 . ("ff" "af" "5f"))
    (216 . ("ff" "af" "87")) (217 . ("ff" "af" "af")) (218 . ("ff" "af" "d7")) (219 . ("ff" "af" "ff"))
    (220 . ("ff" "d7" "00")) (221 . ("ff" "d7" "5f")) (222 . ("ff" "d7" "87")) (223 . ("ff" "d7" "af"))
    (224 . ("ff" "d7" "d7")) (225 . ("ff" "d7" "ff")) (226 . ("ff" "ff" "00")) (227 . ("ff" "ff" "5f"))
    (228 . ("ff" "ff" "87")) (229 . ("ff" "ff" "af")) (230 . ("ff" "ff" "d7")) (231 . ("ff" "ff" "ff"))

    (232 . ("08" "08" "08")) (233 . ("12" "12" "12")) (234 . ("1c" "1c" "1c")) (235 . ("26" "26" "26"))
    (236 . ("30" "30" "30")) (237 . ("3a" "3a" "3a")) (238 . ("44" "44" "44")) (239 . ("4e" "4e" "4e"))
    (240 . ("58" "58" "58")) (241 . ("62" "62" "62")) (242 . ("6c" "6c" "6c")) (243 . ("76" "76" "76"))
    (244 . ("80" "80" "80")) (245 . ("8a" "8a" "8a")) (246 . ("94" "94" "94")) (247 . ("9e" "9e" "9e"))
    (248 . ("a8" "a8" "a8")) (249 . ("b2" "b2" "b2")) (250 . ("bc" "bc" "bc")) (251 . ("c6" "c6" "c6"))
    (252 . ("d0" "d0" "d0")) (253 . ("da" "da" "da")) (254 . ("e4" "e4" "e4")) (255 . ("ee" "ee" "ee"))))
