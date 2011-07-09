(defun run ()
	(setf codes (convertFile (read-file "13.txt")))
	(format t "~&~s" codes))

(defun read-file (filename)
	(setf paths nil)
	(with-open-file (file filename)
		(loop until (null (setf line (read-next file)))do
			(setf paths (append paths (list line)))
		))
		paths)
;(defun read-file (filename)
;	(setf paths nil)
;	(with-open-file (file filename)
;		(loop until (null (first (setf line (list (read-next file)))))do
;			(setf paths (append paths line))
;		))
;		paths)

(defun convertFile (f)
	(setf lines nil)
	(loop for i in f do
		(setf lines (append lines (list (intConversion i)))))
	lines)

(defun intConversion (l)
	(setf ints nil)
	(loop for i in l do
		(setf ints (append ints (parse-integer i))))
	ints)
		
(defun read-next (fin)
	(read-line fin nil))


(defun split-string (str)
	(loop for i = 0 then (1+ j)
		as j = (position #\Space str :start i)
		collect (subseq str i j)
		while j))

	
