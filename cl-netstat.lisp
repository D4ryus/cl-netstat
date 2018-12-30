;;;; cl-stats.lisp

(in-package #:cl-netstat)

;;; "cl-stats" goes here. Hacks and glory await!

(defparameter *max* (* 1024 1024))
(defparameter *refresh-time* 0.25)
(defparameter *print-time-p* nil)
(defparameter *color-mode* :256)
(defparameter *unicode-mode* t)
(defparameter *icons* (list #\Space #\▁ #\▂ #\▃ #\▄ #\▅ #\▆ #\▇ #\█))

(defparameter *rgy-percentage*
  (list 90 ;; over 90% -> red
        75 ;; over 75% -> yellow
        10)) ;; under 10% -> green

(defmacro with-thing ((window new thing) &body body)
  (let ((old-thing (gensym "old-thing"))
        (new-thing (gensym "new-thing")))
    `(let ((,old-thing (,thing ,window))
           (,new-thing ,new))
       (flet ((call-body ()
                ,@body))
         (if ,new-thing
             (progn
               (setf (,thing ,window) ,new-thing)
               (prog1 (call-body)
                 (setf (,thing ,window) ,old-thing)))
             (call-body))))))

(defmacro with-style ((window color-pair &optional attributes) &body body)
  (cond
    ((and (null color-pair) (null attributes))
     `(progn ,@body))
    ((and (null color-pair) attributes)
     `(with-thing (,window ,attributes croatoan:.attributes)
        (progn ,@body)))
    ((and color-pair (null attributes))
     `(with-thing (,window ,color-pair croatoan:.color-pair)
        (progn ,@body)))
    (t
     `(with-thing (,window ,attributes croatoan:.attributes)
        (with-thing (,window ,color-pair croatoan:.color-pair)
          (progn ,@body))))))

(defclass array-loop ()
  ((size :initarg :size)
   (data :initarg :data)
   (index :initform 0)))

(defmethod initialize-instance :after ((array-loop array-loop) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (size data) array-loop
    (setf data (make-array size :initial-element 0))
    (let ((data-arg (getf initargs :data)))
      (when data-arg
        (mapc (lambda (element)
                (push-element array-loop element))
              (reverse data-arg))))))

(defmethod push-element ((array-loop array-loop) element)
  (with-slots (size data index) array-loop
    (setf (aref data (setf index (mod (1+ index) size)))
          element)))

(defmethod get-list ((array-loop array-loop))
  (with-slots (size data index) array-loop
    (loop :for i :from index :downto (1+ (- index size))
          :collect (aref data
                         (if (>= i 0)
                             i
                             (+ i size))))))

(defmethod get-max ((array-loop array-loop))
  (reduce #'max (slot-value array-loop 'data)))

(defun to-icon (value &key (max 100) (icons *icons*))
  (unless *unicode-mode*
    (setf icons (list #\Space #\. #\o #\O #\^)))
  (let ((icon-cnt (length icons)))
    (cond ((<=  value 0) (car icons))
          ((= max 0) (car icons))
          ((>= value max) (car (last icons)))
          ((< icon-cnt 3) (car (last icons)))
          (t (nth (+ 1 (truncate (/ value (/ max (- icon-cnt 2)))))
                  icons)))))

(defun format-graph-part (window number &optional (max *max*))
  (multiple-value-bind (_ color)
      (format-size number :max *max*)
    (declare (ignore _))
    (with-style (window (reverse color))
      (croatoan:add-wide-char window
                              (to-icon number :max max)))))

(defmethod format-graph ((array-loop array-loop) window)
  (let* ((lst (get-list array-loop))
         (first (car lst)))
    (loop :for nbr :in lst
          :do (format-graph-part window nbr))))

(defun get-rgy-color (size max)
  (when max
    (if (eql 0 size)
        '(:black (:number 2))
        (let ((percentage (/ size (/ max 100))))
          (destructuring-bind (up mid low)
              *rgy-percentage*
            (cond
              ((> percentage up) '(:black :red))
              ((> percentage mid) '(:black :yellow))
              ((< percentage low) '(:black :green))
              (t nil)))))))

(let ((xb (ash 1 53)) ;; 8xb
      (tb (ash 1 43)) ;; 8tb
      (gb (ash 1 33)) ;; 8gb
      (mb (ash 1 23)) ;; 8mb
      (kb (ash 1 13)));; 8kb
  (defun format-size (size &key (when-zero nil when-zero-given?) (max nil))
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
         (case *color-mode*
           (:8 (get-rgy-color size max))
           (:256
            (list :black
                  (if (eql 0 size)
                      :white
                      (list :number (color-size->term size max)))))
           (t nil))))))

(defun get-interface-data ()
  (with-open-file (stream "/proc/net/dev"
                          :direction :input)
    ;; ignore first 2 lines
    (dotimes (ignored 2) (read-line stream nil nil))
    (sort
     (loop :for line = (read-line stream nil nil)
           :while line
           :collect
           (destructuring-bind (interface data)
               (cl-ppcre:split ":" (string-trim " " line))
             (cons interface
                   (mapcar (lambda (val data)
                             (cons val
                                   (parse-integer data)))
                           (list :rec-bytes :rec-packets :rec-errs
                                            :rec-drop :rec-fifo :rec-frame
                                 :rec-compressed :rec-multicast
                                 :trans-bytes :trans-packets :trans-errs
                                            :trans-drop :trans-fifo :trans-colls
                                 :trans-carrier :trans-compressed)
                           (cdr (cl-ppcre:split "\\s+" data))))))
     #'string< :key #'car)))

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
  (let ((args (gensym "args")))
    `(mapcar (lambda (&rest ,args)
               (cons (caar ,args)
                     (apply ,function
                            (mapcar #'cdr ,args))))
             ,list
             ,@more-lists)))

(defun diff-interface-data (a b)
  (loop :for (interface-a . data-a) :in a
        :for (interface-b . data-b) :in b
        :when (string= interface-a interface-b)
        :collect (cons interface-a
                       ;; (mapassoc #'- data-b data-a)
                       (mapassoc (lambda (b a)
                                   (truncate (/ (- b a) *refresh-time*)))
                                 data-b data-a))))

(defparameter *interface-graphs* nil)

(let ((width 0))
  (defun update-graphs (stats current-width)
    ;; add new interfaces
    (loop :for (interface . stat) :in stats
          :unless (gethash interface *interface-graphs*)
          :do (setf (gethash interface *interface-graphs*)
                    (make-instance 'array-loop :size current-width)))
    ;; update length
    (unless (and (eql current-width width)
                 (> current-width 57))
      (setf width current-width)
      (loop :for key :being :the :hash-key :of *interface-graphs*
            :do (let ((data (gethash key *interface-graphs*)))
                  (setf (gethash key *interface-graphs*)
                        (make-instance 'array-loop :size (- width 57)
                                                   :data (get-list data))))))
    ;; update data
    (loop :for key :being :the :hash-key :of *interface-graphs*
          :do (let ((data (cdr (assoc key stats :test #'equal))))
                (push-element (gethash key *interface-graphs*)
                              (if data
                                  (+ (nth 2 data)
                                     (nth 3 data))
                                  0))))))

(defun format-bytes (window bytes &key max)
  (multiple-value-bind (str color) (format-size bytes :max max)
    (with-style (window color)
      (croatoan:add-string window
                           (format nil "~8,,,' a" str)))))

(defun format-interfaces (window stats)
  (let ((refresh-time (when *print-time-p*
                        (format nil " ~,2f" *refresh-time*))))
    (with-style (window '(:white :black) '(:bold :underline))
      (croatoan:add-string window
                           (format nil "~a~a~a~a~a~a~a"
                                   "NETWORK          "
                                   "Total Rx "
                                   "Total Tx "
                                   "    Rx/s  "
                                   "  Tx/s     "
                                   "Graph"
                                   (if *print-time-p*
                                       refresh-time
                                       "")))))
  (loop :for stat :in stats
        :do
        (croatoan:new-line window)
        (destructuring-bind (interface total-rx total-tx rx tx)
            stat
          (croatoan:add-string window
                               (format nil "~16,,,' a" interface))
          (croatoan:add-char window #\Space)
          (format-bytes window total-rx)
          (croatoan:add-char window #\Space)
          (format-bytes window total-tx)
          (croatoan:add-char window #\Space)
          (format-bytes window rx :max *max*)
          (croatoan:add-char window #\Space)
          (format-graph-part window rx)
          (format-graph-part window tx)
          (croatoan:add-char window #\Space)
          (format-bytes window tx :max *max*)
          (croatoan:add-char window #\Space))
        (format-graph (gethash (car stat) *interface-graphs*) window)))

(defun gen-stats (last cur)
  (loop :for (interface . data) :in (diff-interface-data last cur)
        :collect (list interface
                       (cdr (assoc-chain (interface :rec-bytes) cur))
                       (cdr (assoc-chain (interface :trans-bytes) cur))
                       (cdr (assoc-chain (:rec-bytes) data))
                       (cdr (assoc-chain (:trans-bytes) data)))))


(defun draw-stats (window stats)
  (croatoan:move window 0 0)
  (setf (croatoan:.color-pair window)
        '(:white :black))
  (update-graphs stats (croatoan:.width window))
  (format-interfaces window stats))

(defparameter *last-stats* nil)

(defun draw (screen)
  (let ((stats (gen-stats *last-stats*
                          (setf *last-stats*
                                (get-interface-data)))))
    (croatoan:clear screen)
    (draw-stats screen stats)))

(defun clear (scr)
  (croatoan:clear scr)
  (croatoan:refresh scr))

(defun reset (scr)
  (setf *last-stats* (get-interface-data))
  (setf *interface-graphs* (make-hash-table :test 'equal))
  (clear scr))

(defun window ()
  (croatoan:with-screen (scr :input-echoing nil
                             :input-blocking nil
                             :enable-fkeys t
                             :cursor-visibility nil)
    (reset scr)
    (let ((refresh-step 0.05))
      (croatoan:event-case (scr event)
        (#\q (return-from croatoan:event-case))
        (#\+ (incf *refresh-time* refresh-step))
        (#\- (when (< (decf *refresh-time* refresh-step) refresh-step)
               (setf *refresh-time* refresh-step)))
        (#\r (reset scr))
        (#\c (clear scr))
        (#\Space (setf *print-time-p* (not *print-time-p*)))
        ((nil)
         (restart-case (progn
                         (sleep *refresh-time*)
                         (draw scr))
           (continue ()
             :report (lambda (stream)
                       (format stream "Continue Croatoan Event-Loop"))
             (values nil t))
           (reset ()
             :report (lambda (stream)
                       (format stream "Reset values"))
             (reset scr)
             (values nil t))
           (abort ()
             :report (lambda (stream)
                       (format stream "Quit Croatoan Event-Loop"))
             (return-from croatoan:event-case))))))))

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
  (defun get-size-color (size &optional max)
    (let ((spot (if max
                    (truncate (* 41 (/ size max)))
                    (integer-length size))))
      (if (> spot 41)
          (aref lookup 41)
          (aref lookup spot)))))

(defun color-size->term (size &optional max)
  (get-match (get-size-color size max)))

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

(defun parse-hex (string)
  (parse-integer string :radix 16))

(defun color-diff (a b)
  (declare (type list a b))
  (reduce #'+
          (mapcar (alexandria:compose #'abs #'-) a b)))

(let ((table (make-hash-table)))
  (defun get-match (color)
    (let ((match (gethash color table)))
      (when match
        (return-from get-match match)))
    (let ((best-match-diff (* 3 255))
          (best-match 0))
      (multiple-value-bind (r g b)
          (color-string->rgb color)
        (loop :for (k . (rr gg bb))
              :in (mapassoc (lambda (rgb)
                              (mapcar #'parse-hex rgb))
                            *term->rgb*)
              :do (let ((diff (color-diff (list r g b)
                                          (list rr gg bb))))
                    (when (< diff best-match-diff)
                      (setf best-match k
                            best-match-diff diff))
                    (when (eql 0 best-match-diff)
                      (return)))))
      (setf (gethash color table) best-match))))

(defun usage (&optional error-msg &rest args)
  (when error-msg
    (apply #'format t error-msg args))
  (format t "usage: cl-netstat [--color | -c (8 256 none)] [--max | -m number] ~
            [--graph | -g \".oO\"] [--start-swank |-s] [--no-unicode | -n] ~
            [--graph-length | -l number] [--help | -h]~%")
  (when error-msg
    (error error-msg args)))

(defmacro switch-args (args usage-fn &rest cases)
  `(with-args (,args ,usage-fn)
     (str-case arg
       ,@cases
       (t (funcall ,usage-fn "unknown argument ~a~%" arg)))))

(defmacro with-args ((args usage-fn) &body body)
  (let ((cur (gensym "current")))
    `(let* ((,cur ,args)
            (arg (car ,cur)))
       (flet ((shift (&optional (print-error t))
                (setf ,cur (cdr ,cur))
                (setf arg (car ,cur))
                (when (and print-error
                           (not arg))
                  (funcall ,usage-fn "out of arguments~%"))))
         (loop :while ,cur
               :do
               (prog1 (progn ,@body)
                 (shift nil)))))))

(defmacro str-case (string-form &body cases)
  "'case' for strings, expands to cond clause with string=
   as compare function"
  (let ((result (gensym "result")))
    `(let ((,result ,string-form))
       (declare (ignorable ,result))
       (cond
         ,@(loop :for (str form) :in cases
                 :collect
                 (typecase str
                   (boolean (if str
                                `(t ,form)
                                (error "nil is not a string")))
                   (string `((string= ,result ,str) ,form))
                   (list `((or ,@(loop :for s :in str
                                       :collect `(string= ,result ,s)))
                           ,form))))))))

(defun help ()
  (format t
          "cl-netstat: Showing per interface Network stats~%")
  (usage)
  (fresh-line)
  (format t "Arguments:~%")
  (format t "    --color | -c (8 | 256 | none)~%")
  (format t "        Show output in colors:~%")
  (format t "            8: only Red, Green and Yellow~%")
  (format t "            256: 256 Colors~%")
  (format t "            none: dont use colors~%")
  (format t "        Default: 256~%")
  (format t "    --max | -m number~%")
  (format t "        Given number describes maximum network traffic,~%")
  (format t "        used for color output.~%")
  (format t "        Default: 1048576 (* 1024 1024)~%")
  (format t "    --refresh-time | -r number~%")
  (format t "        Refresh timeout given in milliseconds.~%")
  (format t "        Default: 250 (1/4 second)~%")
  (format t "    --graph | -g string~%")
  (format t "        Specify graph characters from lowest to highest.~%")
  (format t "        Default:  ▁▂▃▄▅▆▇█ ( .oO^)~%")
  (format t "    --start-swank | -s~%")
  (format t "        Run (swank:create-server).~%")
  (format t "    --no-unicode | -n~%")
  (format t "        Show .oO instead of unicode bars as graph.~%")
  (format t "        This will overwrite --graph~%")
  (format t "    --help | -h~%")
  (format t "        Show this message.~%"))

;; prevent deploy to print status messages
(defun deploy:status (level format-string &rest format-args)
  nil)

(defun main ()
  (when (uiop:command-line-arguments)
    (switch-args (uiop:command-line-arguments) #'usage
      (("--color" "-c")
       (progn
         (shift)
         (setf *color-mode*
               (str-case arg
                 ("8" :8)
                 ("256" :256)
                 (("None" "none") nil)
                 (t (usage "unknown color: ~a~%" arg))))))
      (("--max" "-m")
       (progn
         (shift)
         (setf *max* (parse-integer arg))))
      (("--refresh-time" "-r")
       (progn
         (shift)
         (setf *refresh-time* (* 0.001 (parse-integer arg)))))
      (("--graph" "-g")
       (progn
         (shift)
         (setf *icons* (map 'list #'identity arg))))
      (("--no-unicode" "-n")
       (setf *unicode-mode* nil))
      (("--start-swank" "-s")
       (swank:create-server))
      (("--help" "-h")
       (progn
         (help)
         (uiop:quit 0)))
      ("--options"
       (progn
         (mapcar (lambda (op)
                   (princ op)
                   (fresh-line))
                 (list "--color" "-c"
                       "--max" "-m"
                       "--refresh-time" "-r"
                       "--graph" "-g"
                       "--no-unicode" "-n"
                       "--graph-length" "-l"
                       "--help" "-h"
                       "--start-swank" "-s"))
         (uiop:quit 0)))))
  (window))
