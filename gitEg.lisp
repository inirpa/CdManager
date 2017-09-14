(defvar *db* nil)
(defvar *file* "./test.db")


(princ "***********Welcome!!! I am CyantoBot *************")
(format t "~%Type Command (help to list commands)")

(defun add-record (cd) (push cd *db*))

(defun add-record-unless-exists (cd)
  (if (select (where :rating (getf cd :rating) :artist (getf cd :artist) :title (getf cd :title) :genre (getf cd :genre) :ripped (getf cd :ripped)))
    ()
    (add-record cd )
    )
  )

(defun make-cd ( title artist genre rating ripped )
  (list :title title :artist artist :genre genre :rating rating :ripped ripped))

(defun dump-db ()
  (dolist (cd *db*)
    (print (format t "~%~{~a:~10t~a~%~}%" cd)))
(main)	
)

(defun prompt-read ( prompt )
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (prompt-read "Genre")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
      (if (not (y-or-n-p "Another? [y/n]: ")) (save-and-quit)))		
)

(defun save-and-quit ()
  (save-db *file*)
	(main)
 )

(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun update (selector-fn &key title artist genre rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
			   (if genre   (setf (getf row :genre) genre))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*))
	(save-and-quit)		 
	(main)		 
)

(defun where (&key title artist genre rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
	   (if genre   (equal (getf cd :genre) genre) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*))
  (save-and-quit)
  )

  
  
 (defun searchBy()
	(format t "~%Search By Artist | Genre | Rating ~%")
	(let ((term (prompt-read "Search Term : ")))
		(cond
			((string\= term "artist") (print(select (where :artist (prompt-read "Value")))))
			((string\= term "genre") (print(select (where :genre (prompt-read "Value")))))
			((string\= term "rating") (print(select (where :rating (parse-integer(prompt-read "Value"))))))
		)
	)
	(main)
 )
 
 (defun updateThis()
	(format t "~%Update Artist | Genre | Rating ~%")
	 (let ((term (prompt-read "Update Term : ")))
		(cond
			((string\= term "genre")
					(update (where :title (prompt-read "Title")) :genre (prompt-read "New Genre"))
			)
			
			((string\= term "artist")
					(update (where :title (prompt-read "Title")) :artist (prompt-read "New Artist"))
			)
			
			((string\= term "rating")
					
					(update (where :title (prompt-read "Title")) :rating (prompt-read "New Rating"))
			)
			
		)
	)
	 
 )
 
 (defun deleteThis()
	(format t "~%Delete Row By Artist | Genre | Rating ~%")
	
	(let ((term (prompt-read "Delete By : ")))
		(cond
			((string\= term "genre")
					(delete-rows (where :genre (prompt-read "Genre")) )
			)
			
			((string\= term "artist")
					(delete-rows (where :artist (prompt-read "Artist")) )
			)
			
			((string\= term "rating")
					(delete-rows (where :rating (parse-integer(prompt-read "Rating"))))
			)
			
		)
	)
	
 )
 
 (defun shutDown() 
		(format t "~%Powering down Bot. ~%Bye! Have a Good Day")
		(ext::bye)
	)
	
(defun help()

		(format t "~%You are Here >> Help")
		(format t "~%List of Available Commands")
		(format t "~%1.Save ~%2.Delete ~%3.Update ~%4.Search ~%5.Bye ~%5.Help")
		(main)
	)	
  
(defun main()
		(format t "~%~%BotCommand:> ")
		(setq command (read))
		
		(let ((value command)) 
			(cond
				((string\= value "HELP") (help)) 
				((string\= value "SAVE") (add-cds)) 
				((string\= value "VIEW") (dump-db)) 
				((string\= value "SEARCH") (searchBy)) 
				((string\= value "BYE") (shutDown))
				((string\= value "UPDATE") (updateThis))
				((string\= value "DELETE") (deleteThis))					
			)
		)
	)
(load-db *file*)

;seed data
(add-record-unless-exists (make-cd "Ordinary Love!" "U2" "pop" 8 nil ))
(add-record-unless-exists (make-cd "Parelima" "1974 A.D." "pop" 8 t))
(add-record-unless-exists (make-cd "Resham" "Nepthaya" "rock/pop" 9 t))

(main)	




