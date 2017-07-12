;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Your solution goes in this file ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PART 1;
;all-length; //done
(defun all-length (mylist)
    (cond
        ((null mylist) 0)
        ((atom mylist) 1)
        (t (+ (all-length (car mylist))(all-length (cdr mylist))))))
        
;range; //done
(defun range (mylist)
    (list (apply 'min mylist) (apply 'max mylist)))
    
;before;
(defun before (begin end mylist)
    (cond
        ((null mylist) nil)
        ((and (null begin) (equal (car mylist) end)) (list (car mylist)))
        ((null begin) (cons (car mylist) (before nil end (cdr mylist))))
        ((equal (car mylist) begin) (cons (car mylist) (before nil end (cdr mylist))))
        (t (before begin end (cdr mylist)))))
        
;split-if; //done
(defun split-if (func mylist)
    (list (remove-if func mylist) (remove-if-not func mylist)))
    
;group; 
(defun group (mylist num)
    (group2 mylist '() num)
)

(defun group2 (mylist sub num)
    (cond
        ((and (null mylist) (null sub)) sub)
        ((null mylist) (list sub))
        ((equal (length sub) num) (cons sub (group2 mylist '() num)))
        (t (group2 (cdr mylist) (append sub (list (car mylist))) num))
    )
    
)

;mostn; 
(defun mostn (func mylist) 
    (mostn_helper func mylist '() 0)
)
(defun mostn_helper (func mylist sub num)
    
    (if (null mylist) (list sub num)
        (let* ((score (funcall func (car mylist))))
            (cond
                ((> score num) (mostn_helper func (cdr mylist) (setf sub (list (car mylist))) (car (mapcar func (list (car mylist))))))
                ((equal score num) (mostn_helper func (cdr mylist) (append sub (list(car mylist))) (funcall func(car mylist))))
                (t (mostn_helper func (cdr mylist) sub num))
                
            )
        )
    )
)

;PART 2;

;match;
(defun match (mylist1 mylist2)
    (cond
        ((or (null mylist1) (null mylist2)) (equal mylist1 mylist2))
        ((equal (car mylist1) '?) (match (cdr mylist1) (cdr mylist2)))
        ((equal (car mylist1) '!) (or (match (cdr mylist1) (cdr mylist2)) (match mylist1 (cdr mylist2))))
        (t (and (equal (car mylist1) (car mylist2)) (match (cdr mylist1) (cdr mylist2))))
    )
)

;PART 3;
;match-var;

(defun match-var (mylist1 mylist2)
    (cond
	((or (null mylist1) (null mylist2)) (equal mylist1 mylist2))
	((and (atom (car mylist1)) (atom (car mylist2))) (and (equal (car mylist1) (car mylist2)) (match-var (cdr mylist1) (cdr mylist2))))
	((and (listp (car mylist1)) (equal (caar mylist1) '?))
			(setf (symbol-value (cadar mylist1)) (car mylist2)) (match-var (cdr mylist1) (cdr mylist2)))
	((and (listp (car mylist1)) (equal (caar mylist1) '!) (match-var (cdr mylist1) (cdr mylist2)))
		(if (equal (boundp (cadar mylist1)) nil) (setf (symbol-value (cadar mylist1)) nil))
		(setf (symbol-value (cadar mylist1)) (append (eval (cadar mylist1)) (list (car mylist2)))) t)
	((and (listp (car mylist1)) (equal (caar mylist1) '!))
		(if (equal (boundp (cadar mylist1)) nil) (setf (symbol-value (cadar mylist1)) nil))
		(setf (symbol-value (cadar mylist1)) (append (eval (cadar mylist1)) (list (car mylist2))))
		(match-var mylist1 (cdr mylist2)))
	(t (and (equal (car mylist1) (car mylist2)) (match-var (cdr mylist1) (cdr mylist2))))
	    
	 )
) 




