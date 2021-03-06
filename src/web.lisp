(in-package :cl-user)
(defpackage cl-md-notes.web
  (:use :cl
        :caveman2
        :cl-md-notes.config
        :cl-md-notes.view
        :cl-md-notes.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :cl-md-notes.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(defun find-note-by-id (id)
  (car (remove-if #'(lambda (note)
		      (if (= (getf note :id) (parse-integer id))
			  nil
			  t
			  )) *notes*)))

(defun get-param (name parsed)
  "Get param values from _parsed"
  (cdr (assoc name parsed :test #'string=)))

(defun format-goss (html)
  "Format result from get-output-stream-string"
  (format nil "~a" (get-output-stream-string html)))

(defun filter-notes (query)
  "Filter notes based on the query with title"
  (remove-if #'(lambda (note)
                 (let ((title (getf note :title)))
                   (if (search query title :test #'char-equal)
                       nil
                       t))) *notes*))

(defun get-time ()
(local-time:format-timestring
                            nil
                            (local-time:now)
                            :format (list :day " " :short-month " " :year " at " (list :hour12 2) ":" (list :min 2) " " :ampm)))

(defvar *notes* `((:id 1 :title "Meeting Notes" :created-at ,(get-time) :updated-at ,(get-time) :content "This is an example note. It contains **Markdown**!")
                  (:id 2 :title "Make a thing" :created-at ,(get-time) :updated-at ,(get-time) :content "It's very easy to make some words **bold** and other words *italic* with
Markdown. You can even [link to htmx's website!](https://htmx.org).")
                  (:id 3 :title "A note with a very long title because sometimes you need more words" :created-at ,(get-time) :updated-at ,(get-time) :content "You can write all kinds of [amazing](https://en.wikipedia.org/wiki/The_Amazing)
notes in this app! These note live on the server in the \`notes\` folder.
![This app is powered by htmx](images/htmx-logo.png)")
                  (:id 4 :title "I wrote this note today" :created-at ,(get-time) :updated-at ,(get-time) :content "It was an excellent note.")))


;;
;; Routing rules

;; Home page
(defroute "/" ()
  (print *notes*)
  (render #P"index.html" (list :notes *notes*)))
 
;; Show note
(defroute "/note/:id" (&key id)
  (let* ((note (find-note-by-id id))
	 (html (make-string-output-stream)))
    (cl-markdown:markdown (getf note :content) :stream html)
    (render #P"_note.html" (list :note note
				 :html (format-goss html)))))

;; New note page
(defroute "/new" ()
  (render #P"_new-note.html"))

;; Preview markdown
(defroute ("/preview" :method :POST) (&key _parsed)
  (let ((draft (cdr (assoc "draft" _parsed :test #'string=)))
	(html (make-string-output-stream)))
    (cl-markdown:markdown draft :stream html)
    (format-goss html)))

;; Create new note
(defroute ("/note" :method :POST) (&key _parsed)
  (let* ((title (get-param "title" _parsed))
	 (draft (get-param "draft" _parsed))
	 (html (make-string-output-stream))
         (note (list
                :title title
                :content draft
                :created-at (get-time)
                :updated-at (get-time)
		     :id (get-universal-time))))
    (push note *notes*)
    (cl-markdown:markdown (getf note :content) :stream html)
    (concatenate 'string 
		 (render #P"_notes-list.html" (list :notes *notes*))
		 (render #P"_note.html" (list :note note
					      :html (format-goss html))))))

;; Update note
(defroute ("/note/:id" :method :PUT) (&key id _parsed)
  (let* ((title (get-param "title" _parsed))
	 (draft (get-param "draft" _parsed))
	 (html (make-string-output-stream))
	 (note (find-note-by-id id)))
    (setf (getf note :title) title
          (getf note :content) draft
          (getf note :updated-at) (get-time))
    (cl-markdown:markdown (getf note :content) :stream html)
    (concatenate 'string 
		 (render #P"_notes-list.html" (list :notes *notes*))
		 (render #P"_note.html" (list :note note
					      :html (format-goss html))))))


;; Edit note
(defroute "/edit/:id" (&key id)
  (let ((note (find-note-by-id id))
	(html (make-string-output-stream)))
    (cl-markdown:markdown (getf note :content) :stream html)
    (render #P"_edit-note.html" (list :note note
				      :html (format-goss html)))))

;; Delete note
(defroute ("/note/:id" :method :DELETE) (&key id)
  (setf *notes* (remove-if #'(lambda (note)
			       (if (= (getf note :id) (parse-integer id))
				   t
				   nil)) *notes*))
  (render #P"_notes-list.html" (list :notes *notes*)))

;; Search notes
(defroute ("/search" :method :POST) (&key _parsed)
  (let ((query (get-param "query" _parsed)))
    (render #P"_notes-list.html" (list :notes (filter-notes query)))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
		   *template-directory*))
