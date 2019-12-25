(setf spinneret:*unvalidated-attribute-prefixes* '(""))

;Webserver control
(defun start-webserver () 
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :document-root "d:/portacle/projects/thelittleguy/" :port 4242))) ;Start the basic webserver using easy-acceptor on port 4242

;Disable pretty printing to produce more compact and fast html. Can be removed or set to T to make nicely formatted again.
;(setq spinneret:*print-pretty* 'nil)

;Searching functions
(setf lparallel:*kernel* (lparallel:make-kernel 4))

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defvar index (get-file "urlsdupesnuked.db"))

(defun search-for-words-simple (lines wordlist)
  (declare (optimize (speed 3) (safety 0)) ;Add (debug 0) for even stronger optimisation, only do in build/release versions though : )
           (type list wordlist lines))
  (lparallel:premove-if-not (lambda (line)
                              (declare (type simple-string line))
                              (every (lambda (word)
                                       (declare (type simple-string word))
                                       (search word line))
                                     wordlist))
                            lines))

(defun starts-with (string prefix)
  (declare (optimize (speed 3) (safety 0))
           (type simple-string prefix string))
  (and (>= (length string)
           (length prefix))
       (string= prefix string :end2 (length prefix))))

(defun dosort (list substr)
  (sort list
        (lambda (a b) (and a (not b)))
        :key #'(lambda (str) (starts-with str substr))))

(defun search-index (lines wordlist)
  (declare (optimize (speed 3) (safety 0)) ;Add (debug 0) for even stronger optimisation, only do in build/release versions though : )
           (type list wordlist lines))
  (let ((results (search-for-words-simple lines wordlist)))
    (setq wordlist (push (concatenate 'string (first wordlist) ".") wordlist))
    (loop for search in (nreverse wordlist)
       do (setq results (dosort results search)))
    results))

(defun handle-search (search)
  (search-index index (split-sequence:split-sequence #\Space search)))

;Page generating functions
(defmacro with-page ((&key title) &body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title)
       (:link :rel "stylesheet" :href "main.css" :type "text/css"))
      (:body ,@body))))

(defun get-index ()
  (with-page (:title "TheLittleGuy - Search you can trust.")
     (:form :class "main-form" :method "get"
            (:div (:img :src "logo.png"))
            (:div (:img :src "img1.png") (:br))
            (:div (:label :for "q" (:input :type "text" :name "q" :size "45"))
                  (:input :type "submit" :class "search" :value "Search"))
            (:br))))

(defun get-search (search)
  (with-page (:title (first search))
    (:form :method "get"
           (:div (:label :for "q" (:input :type "text" :name "q" :size "45")) (:input :type "submit" :class "search" :value "Search"))
           (:br))
    (dolist (item search)
      (:a :href item item)
      (:br))))

(defvar currsearch '())

(hunchentoot:define-easy-handler (foo :uri "/search") (q)
  (setf (hunchentoot:content-type*) "text/html")
  (if (not (= (length q) 0)) (get-search (handle-search q)) (get-index)))
