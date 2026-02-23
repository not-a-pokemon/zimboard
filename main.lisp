(defpackage #:zimboard
  (:use #:cl #:cl-who))

(in-package #:zimboard)

(defvar *port* 8080)
(defvar *db-path* "sqlite.db")

(defvar *mem-db* (sqlite:connect *db-path*))
(defvar *db*)

(defvar *tables*
  '(("users" . "create table users (id integer primary key, name text, passwd text)")
    ;; TODO this may be not the best way to do rights (also naming)
    ("user_rights" . "create table user_rights (id integer, action integer)")
    ("users_pending" . "create table users_pending (id integer primary key, name text, passwd text)")
    ;; A session is described by its identifier - a string of 32 alphanumeric characters
    ;; The expiration date is in standard UNIX time
    ;; TODO Currently, session expiration isn't implemented
    ("sessions" . "create table sessions (id text primary key, expiration integer, user_id integer)")
    ("comments" . "create table comments (id integer primary key, in_post integer, parent_comment integer, msg text, user integer, date integer)")
    ("posts" . "create table posts (id integer primary key, md5 text, complete integer, creation_date integer, posted_by integer)")
    ("tags" . "create table tags (id integer primary key, name text)")
    ("tags_to_posts" . "create table tags_to_posts (tag_id integer, post_id integer)")
    ("cache_entries" . "create table cache_entries (id integer primary key, last_used integer)")
    ("cache_tags" . "create table cache_tags (cache_id integer, tag_id integer)")
    ;; TODO should it be like that? Or the creation date rather would be better
    ;; left only where it already is?
    ("cache_posts" . "create table cache_posts (cache_id integer, post_id integer, post_creation_date integer)")
    ;; action: 0 -> remove ; 1 -> add
    ("tag_edits" . "create table tag_edits (post_id integer, tag_id integer, edited_by integer, date integer, action integer)")))

(defvar *table-indices*
  '(("idx_tags_name" . "create index idx_tags_name on tags (name)")
    ("idx_tags_id" . "create index idx_tags_id on tags (id)")
    ("idx_tags_to_posts" . "create index idx_tags_to_posts on tags_to_posts (tag_id, post_id)")
    ("idx_posts_to_tags" . "create index idx_posts_to_tags on tags_to_posts (post_id, tag_id)")
    ("idx_comments_by_post" . "create index idx_comments_by_post on comments (in_post)")
    ("idx_posts_by_date" . "create index idx_posts_by_date on posts (creation_date)")
    ("idx_cache_tags_by_tag" . "create index idx_cache_tags_by_tag on cache_tags (tag_id)")
    ("idx_cache_tags_by_cache" . "create index idx_cache_tags_by_cache on cache_tags (cache_id)")
    ("idx_cache_posts_by_cache" . "create index idx_cache_posts_by_cache on cache_posts (cache_id)")
    ("idx_cache_posts_by_post" . "create index idx_cache_posts_by_post on cache_posts (post_id)")
    ("idx_cache_posts_by_date" . "create index idx_cache_posts_by_date on cache_posts (post_creation_date)")
    ("idx_tag_edits_by_post" . "create index idx_tag_edits_by_post on tag_edits (post_id, date)")
    ("idx_tag_edits_by_date" . "create index idx_tag_edits_by_date on tag_edits (date)")))

(defvar *posts-per-page* 64)
;; TODO cache priority should probably be more complicated than simply LRU, but
;; that'll work for now
(defvar *max-cache-entries* 1024)
(defvar *teapot-mode* t
  "A mode for development that responds with 'I'm a
teapot' to every request coming outside of localhost")
(defvar *tags-per-page* 128)
(defvar *max-post-tags* 256)

;; Either :manual or :open
(defvar *registration-mode* :manual)

(defun table-exists-p (db name)
  (sqlite:execute-single db "select name from sqlite_master where type='table' and name=?" name))

(defun index-exists-p (db name)
  (sqlite:execute-single db "select name from sqlite_master where type='index' and name=?" name))

(defun init-database (db)
  (loop for i in *tables*
        unless (table-exists-p db (car i))
        do (sqlite:execute-non-query db (cdr i)))
  (loop for i in *table-indices*
        unless (index-exists-p db (car i))
        do (sqlite:execute-non-query db (cdr i))))

(defvar *mem-db-initialized* nil)
;; The session cookie only lives a month.
;; NOTE that this can become a per-user setting in the future
(defvar *max-session-age* 2592000)

;; TODO is this safe?
(defvar *z-random-state* (make-random-state t))

(unless *mem-db-initialized*
  (setf *mem-db-initialized* t)
  (init-database *mem-db*))

;; NOTE that this function is mainly for interactive/admin use
(defun clear-uncomplete-posts ()
  (sqlite:execute-non-query *mem-db* "delete from posts where complete=0"))

(defun clear-cache ()
  (sqlite:execute-non-query *mem-db* "delete from cache_entries")
  (sqlite:execute-non-query *mem-db* "delete from cache_tags")
  (sqlite:execute-non-query *mem-db* "delete from cache_posts"))

;; NOTE that this runs under the assumption a..z and likes are sequential in the codespace
(defun latin-char-p (c)
  (or (<= (char-code #\a) (char-code c) (char-code #\z))
      (<= (char-code #\A) (char-code c) (char-code #\Z))))

(defun numer-char-p (c)
  (<= (char-code #\0) (char-code c) (char-code #\9)))

(defun white-char-p (c)
  (or (char= c #\Tab)
      (char= c #\Newline)
      (char= c #\Return)
      (char= c #\Space)))

(defun int-to-hex (n)
  "Returns the corresponding hex character for n in range 0-15 included"
  (if (< n 10)
    (code-char (+ 48 n))
    (code-char (+ 87 n))))

(defun int-to-hex-upcase (n)
  (if (< n 10)
    (code-char (+ 48 n))
    (code-char (+ 55 n))))

(defun hex-to-int (c &optional (default -1))
  (cond
    ((numer-char-p c) (- (char-code c) (char-code #\0)))
    ((<= (char-code #\A) (char-code c) (char-code #\F))
     (+ 10 (- (char-code c) (char-code #\A))))
    (t default)))

(defun array-to-hex (a)
  "Convert a byte array into a hex string"
  (loop with s = (make-string (* 2 (length a)))
        for i across a
        for ix from 0 do
        (progn
          (setf (elt s (+ ix ix)) (int-to-hex (logand 15 i)))
          (setf (elt s (+ ix ix 1)) (int-to-hex (ash i -4))))
        finally (return s)))


(defun percent-decode (str)
  (with-output-to-string (result)
    (let ((i 0)
          (len (length str)))
      (loop while (< i len) do
            (let ((c (char str i)))
              (cond
                ((char= #\% c)
                 (if (< (+ i 2) len)
                   (let ((c1 (hex-to-int (char str (+ 1 i)) nil))
                         (c2 (hex-to-int (char str (+ 2 i)) nil)))
                     (when (and c1 c2)
                       (write-char (code-char (+ c2 (* 16 c1))) result))
                     (incf i 3))
                   (incf i)))
                ((char= #\+ c)
                 (write-char #\Space result)
                 (incf i))
                (t
                  (write-char c result)
                  (incf i))))))))

(defvar *percent-non-encoded* "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_~.")
(defun percent-encode (str)
  ;; TODO perf
  (with-output-to-string (result)
    (loop for i across str do
          (cond
            ((position i *percent-non-encoded*)
             (write-char i result))
            ((char= #\Space i)
             (write-char #\+ result))
            (t
              (write-char #\% result)
              (write-char (int-to-hex-upcase (ash (char-code i) -4))
                          result)
              (write-char (int-to-hex-upcase (logand #xF (char-code i)))
                          result))))
    result))

;; TODO should have I used a fill pointer here?
(defun read-till-rn (r)
  (loop with a = (make-array 1024
                             :adjustable t
                             :element-type '(unsigned-byte 8))
        with l = 0
        for y = (read-byte r nil)
        while y do
        (progn
          (when (>= l (length a))
            (adjust-array a (+ 1024 (length a))))
          (setf (aref a l) y)
          (incf l)
          (when (and (= y 10) (>= l 2) (= (aref a (- l 2)) 13))
            (return (adjust-array a l))))
        finally (return (adjust-array a l))))

(defun arr-int-equal-till-newline (a b &optional n)
  (let ((nx (or n (length a))))
    (if (<= nx (length b))
      (loop for i from 0 below nx
            until (= 13 (aref a i))
            always (= (aref a i) (aref b i)))
      nil)))

(defun boundary-kind (boundary tested)
  (cond
    ((= (length boundary) (length tested))
     (if (arr-int-equal-till-newline boundary tested) 1 0))
    ((= (length boundary) (- (length tested) 2))
     (if (arr-int-equal-till-newline boundary tested) 2 0))
    (t 0)))

(defun fix-line-end (a)
  (let ((n (length a)))
    (cond
      ((and (>= n 2)
            (= 13 (aref a (- n 2)))
            (= 10 (aref a (1- n))))
       a)
      ((and (>= n 1)
            (= 13) (aref a (1- n)))
       (adjust-array a (1+ n))
       (setf (aref a n) 10)
       a)
      (t
        (adjust-array a (+ 2 n))
        (setf (aref a n) 13)
        (setf (aref a (1+ n)) 10)
        a))))

(defun parse-multipart (r)
  (let ((boundary (read-till-rn r))
        (parts nil)
        (state 1))
    (labels
      ((add-part
         ()
         (push (list :headers nil
                     :body (make-array
                             0
                             :element-type '(unsigned-byte 8)
                             :adjustable t
                             :fill-pointer 0)) parts))
       (finish-part
         ()
         (assert (= 10 (vector-pop (getf (car parts) :body))))
         (assert (= 13 (vector-pop (getf (car parts) :body))))))
      (add-part)
      (loop for y = (read-till-rn r)
            until (zerop (length y)) do
            (progn
              ; (fix-line-end y) ; TODO figure out if I need this
              (case state
                (1 (cond
                     ((= 2 (length y))
                      (setf state 2))
                     (t
                       (push (flexi-streams:octets-to-string y) (getf (car parts) :headers)))))
                (2 (case (boundary-kind boundary y)
                     (1
                      (finish-part)
                      (add-part)
                      (setf state 1))
                     (2
                      (finish-part)
                      (return))
                     (otherwise
                       (loop for i across y do
                             (vector-push-extend i (getf (car parts) :body)))))))))
      (nreverse parts))))

(defun parse-simple (request-body &optional (sep #\&))
  (let ((l (typecase request-body
             (null nil)
             (string request-body)
             (array (flexi-streams:octets-to-string request-body))
             (t (read-line request-body nil)))))
    ;; NOTE that such a condition is redudant. (length nil) is zero
    (if (and l (not (zerop (length l))))
      (let ((r (make-hash-table :test #'equal)) (cur-name nil) (cpos 0))
        (labels ((finish
                   (pos)
                   (if cur-name
                     (setf (gethash cur-name r) (subseq l cpos pos)
                           cur-name nil)
                     (setf (gethash (subseq l cpos pos) r) nil))))
          (loop for i across l
                for ix from 0 do
                (cond
                  ((and (white-char-p i) (= cpos ix))
                   (incf cpos))
                  ((char= i sep) (finish ix) (setf cpos (1+ ix)))
                  ((char= i #\=) (unless cur-name
                         (setf cur-name (subseq l cpos ix)
                               cpos (1+ ix))))
                  (t nil))
                finally (progn (finish (length l)) (return r)))))
      (make-hash-table :test #'equal))))

(defmacro simple-page (head &rest body)
  (declare (type list head))
  (let ((status (getf head :status 200))
        (use-navbar (getf head :use-navbar t))
        (use-head (getf head :use-head t))
        (output (getf head :output))
        (args (getf head :args))
        (title (getf head :title)))
    ;(declare (type symbol args))
    (declare (type symbol output))
    `(list
       ,status '(:content-type "text/html")
       (list
         (with-html-output-to-string
           (,output nil :prologue t)
           (setf (cl-who:html-mode) :html5)
           (:html
           ,(if use-head
              `(:head (:title ,title)
                      (:meta :charset "utf-8")
                      (:link :rel "icon"
                             :type "image/png"
                             :href "/favicon.png")
                      (:link :rel "stylesheet"
                             :href "/static/style.css"))
              `(:head))
             ,(if use-navbar
                `(:body (page-navbar ,output (getf ,args :deduced-user))
                        ,(nconc (list :div :id "main")
                                body))
                (cons :body body))))))))

(defun page-error-div (output str)
  (with-html-output
    (output)
    (:p :class "error" (format output "~A" str))))

(defparameter *page-error-kinds*
  '(("i" . "Invalid username")
    ("p" . "Empty password not allowed")
    ("x" . "User already exists")
    ("n" . "Passwords do not match")
    ("s" . "Server error: unknown registration mode")
    ("lx" . "User doesn't exist")
    ("lp" . "Password incorrect")
    ("pi" . "Failed to parse image")
    ("ps" . "Failed to write file: such hash already exists")
    ("pp" . "Failed to write the preview file")
    ("pn" . "No image file specified?")
    ("pm" . "Must be logged in in order to post")
    ("cm" . "Must be logged in in order to comment")
    ("ci" . "Invalid post id")
    ("em" . "Must be logged in in order to edit tags")
    ("ej" . "Stop sending junk to the server, dumbass")
    ("et" . "Invalid tagnames")
    ("el" . "Too many tagnames")
    ("au" . "Need to be registered to approve others")
    ("ai" . "Invalid request")
    ("an" . "Pending user not found")))

(defun page-error-case (output error-kind)
  (loop for i in *page-error-kinds*
        when (string-equal (car i) error-kind)
        return (page-error-div output (cdr i))))

(defun page-post-input (p)
  (with-html-output (p)
    (:form :method "post" :enctype "multipart/form-data" ;; any other kind won't let me have file transmission
           :action "/id"
      (:input :type "file" :name "image")
      (:textarea :name "tags" :style "display: block"
                 :placeholder "post tags (separate with commas or whitespace)")
      (:input :type "submit" :value "Submit"))))

(defun page-navbar (p &optional logged-as)
  (with-html-output
    (p)
    (:div :id "page-navbar"
          (:a :class "nav-link" :href "/" "Home") " "
          (:a :class "nav-link" :href "/search" "Search") " "
          (:a :class "nav-link" :href "/post" "Make a post") " "
          (:a :class "nav-link" :href "/list-tags" "Tag list") " "
          (if (second logged-as)
            (with-html-output
              (p)
              (:span :class "nav-span"
                     (format p "user/~A" (first logged-as))) " "
              (:form :method "post" :action "/delete-session"
                     (:input :class "nav-button" :type "submit" :value "Log out"))
              (when (approval-right-p (second logged-as))
                (with-html-output (p)
                  (:a :class "nav-link" :href "/approval" "Approval"))))
            (with-html-output
              (p)
              (:a :class "nav-link" :href "/register" "Register")
              " "
              (:a :class "nav-link" :href "/login" "Login"))))))

(defun page-home (args)
  (let ((start-time (get-internal-real-time)))
    (simple-page
      (:title "Zimboard" :args args :output output)
      (:p "This is a simple imageboard, providing tag search. The 'search' page may be of interest.")
      (:img :alt "May contain trace amounts of LISP"
            :src "/imgs/lisplogo_warning_128.png")
      (format
        output "<p>Page generated in ~,5F</p>"
        (/ (- (get-internal-real-time) start-time)
           internal-time-units-per-second)))))

(defun content-disposition-name (p)
  (loop with state = 1
        with compar = "Content-Disposition"
        with this-name = (make-array
                           32
                           :fill-pointer 0
                           :element-type 'character)
        with result = (make-array
                        32
                        :fill-pointer 0
                        :element-type 'character)
        for i across p
        for ix from 0 do
        (case state
          (1 (cond
               ((char= #\: i) (setf state 2))
               ((not (char= (elt compar ix) i))
                (return))))
          (2 (cond
               ((char= #\; i) (setf (fill-pointer this-name) 0))
               ((char= #\= i)
                (when (string-equal this-name "name")
                  (setf state 3)))
               ((not (char= #\Space i)) (vector-push i this-name))))
          (3 (cond
               ((or (char= #\; i)
                    (char= #\Newline i)
                    (char= #\Return i))
                (return result))
               (t (vector-push i result)))))
        finally (return result)))

(defun multipart-pull-out-name (p)
  (dolist (i p)
    (let ((x (content-disposition-name i)))
      (when x (return x)))))

(defun image-preview-path (id)
  (format nil "pre/~D/~D/~D.jpg"
          (ash id -16)
          (logand #xFF (ash id -8))
          (logand #xFF id)))

(defun image-orig-path (hash)
  (format nil "orig/~A/~A/~A.jpg"
          (subseq hash 0 2)
          (subseq hash 2 4)
          (subseq hash 4)))

(defun valid-tagname-p (name)
  (loop for i across name
        always (or (latin-char-p i)
                   (numer-char-p i)
                   (case i
                     (#\. t)
                     (#\_ t)
                     (#\- t)
                     (#\( t)
                     (#\) t)
                     (#\' t)
                     (#\% t)
                     (#\~ t)
                     (otherwise nil)))))

(defun tagname-to-id (name &key create-if-missing)
  ;; TODO check for name validity
  (or (sqlite:execute-single *db* "select id from tags where name=?" name)
      (when create-if-missing
        (sqlite:execute-non-query
          *db* "insert into tags (name) values (?)" name)
        (sqlite:last-insert-rowid *db*))))

(defun parse-tags (s)
  (let ((r nil) (cpos 0))
    (labels ((finish
               (pos)
               (unless (= pos cpos)
                 (setf r (cons (subseq s cpos pos) r)))
               (setf cpos (1+ pos))))
      (loop for i across s
            for ix from 0 do
            (when (or (white-char-p i)
                      (char= i #\,))
              (finish ix))
            finally (progn (finish (length s))
                           (return (nreverse r)))))))

(defun mark-used-cache (cache-id)
  (sqlite:execute-non-query
    *db* "update cache_entries set last_used=? where id=?"
    (get-universal-time) cache-id))

(defun purge-old-cache (n)
  "Remove N least recently used entries"
  (sqlite:execute-non-query
    *db* "delete top ? from cache_entries order by last_used asc" n))

(defun update-cache (post-id)
  "Update all query caches related to post-id."
  (let ((ncache (sqlite:execute-single *db* "select count() from cache_entries;")))
    (when (> ncache *max-cache-entries*)
      (purge-old-cache (- ncache *max-cache-entries*))))
  (sqlite:execute-non-query *db* "delete from cache_posts where post_id=?" post-id)
  (let ((post-date (sqlite:execute-single *db* "select creation_date from posts where id=?" post-id))
        (tags (mapcar #'car (sqlite:execute-to-list *db* "select tag_id from tags_to_posts where post_id=?" post-id))))
    (sqlite:execute-non-query/named
      *db*
      (format nil
"insert into cache_posts (cache_id, post_id, post_creation_date)
select cache_id, :post_id, :post_date from cache_entries
join cache_tags on cache_entries.id=cache_tags.cache_id
group by cache_entries.id
having count(cache_tags.tag_id) = count(case when cache_tags.tag_id in (~{~D~^,~}) then 1 end)" tags)
      ":post_id" post-id ":post_date" post-date)))

(defun page-post-create (args)
  (let ((parsed (parse-multipart (getf args :body)))
        (image-body nil)
        (tags nil)
        (cur-id nil)
        (user-id (username-to-id (first (getf args :deduced-user))))
        (parsed-tags nil))
    (dolist (i parsed)
      (let ((x (multipart-pull-out-name (getf i :headers))))
        (cond
          ((string-equal "\"image\"" x)
           (setf image-body (getf i :body)))
          ((string-equal "\"tags\"" x)
           (setf tags (getf i :body)
                 parsed-tags (parse-tags (flexi-streams:octets-to-string tags)))))))
    (labels ((l-error
               (text)
               (return-from
                 page-post-create
                 `(303 (:content-type "text/plain"
                        :location ,(format nil "/post?e=~A" text))
                   nil))))
      (unless user-id
        (l-error "pm"))
      (when (zerop (length image-body))
        (l-error "pn"))
      (unless (every #'valid-tagname-p parsed-tags)
        (l-error "et"))
      (when (< *max-post-tags* (length parsed-tags))
        (l-error "el"))
      (let* ((clean-body (or (magick-util:make-clean-blob (copy-seq image-body))
                             (l-error "pi")))
             (hash (array-to-hex (md5:md5sum-sequence clean-body))))
        ;; Was seemingly breaking on hashes starting with [digits]e... because of automatic type conversion
        ;; FIXed by changing the type from the non-existent 'string' to 'text'
        (sqlite:execute-non-query
          *db* "insert into posts (complete, md5, creation_date, posted_by) values (0, ?, ?, ?)"
          hash (get-universal-time) user-id)
        (setf cur-id (sqlite:last-insert-rowid *db*))
        (let ((orig-name (format nil "./imgs/~A" (image-orig-path hash))))
          (ensure-directories-exist (directory-namestring orig-name))
          ;; TODO make some error checking
          (with-open-file (orig-f orig-name
                                  :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists nil)
            (if orig-f
              (loop for i across clean-body
                    do (write-byte i orig-f))
              (l-error "ps")))
          (let ((preview-name (format nil "./imgs/~A" (image-preview-path cur-id))))
            (ensure-directories-exist (directory-namestring preview-name))
            (unless (magick-util:make-thumbnail ;; TODO idk if this is thread-safe
                      clean-body
                      128 128
                      preview-name)
              (l-error "pp")))
          (loop for i in parsed-tags do
                (sqlite:execute-non-query
                  *db* "insert into tags_to_posts (tag_id, post_id) values (?, ?)"
                  (tagname-to-id i :create-if-missing t) cur-id))
          (update-cache cur-id)
          (sqlite:execute-non-query/named
            *db* "insert into tag_edits (post_id, tag_id, edited_by, date, action)
select :post_id, tag_id, :user_id, :date, 1 from tags_to_posts where post_id=:post_id"
            ":post_id" cur-id
            ":user_id" user-id
            ":date" (get-universal-time))
          (sqlite:execute-non-query
            *db* "update posts set complete=1 where id=?" cur-id)
          `(303 (:content-type "text/plain" :location ,(format nil "/id/~D" cur-id))
            ("sending image")))))))

(defun page-post-form (args)
  (simple-page
    (:title "Make a post" :args args :output output)
    (page-error-case output (gethash "e" (getf args :query-parsed)))
    (if (car (getf args :deduced-user))
      (page-post-input output)
      (with-html-output
        (output)
        (:p :class "note" "Only logged users can post")))))

(defun page-register (args)
  (let ((qp (parse-simple (getf args :query))))
    (simple-page
      (:title "Register" :args args :output output)
      (page-error-case output (gethash "e" qp))
      (:form :method "post" :action "/user"
             (:table
               (:tr
                 (:td (:label :for "un" "Username"))
                 (:td (:input :name "username" :id "un" :type "text" :pattern "([A-Za-z0-9_-.])+")))
               (:tr
                 (:td (:label :for "paswd" "Password"))
                 (:td (:input :name "password" :id "paswd" :type "password" :required "")))
               (:tr
                 (:td (:label :for "paswd1" "Password again"))
                 (:td (:input :name "password1" :id "paswd1" :type "password" :required "")))
               (:tfoot (:td (:input :type "submit" :value "Register account"))))))))

(defun page-login (args)
  (let ((qp (parse-simple (getf args :query))))
    (simple-page
      (:title "Login" :args args :output output)
      (page-error-case output (gethash "e" qp))
      (:form :method "post" :action "/session"
             (:table
               (:tr
                 (:td (:label :for "un" "Username"))
                 (:td (:input :name "username" :id "un" :type "text" :pattern "([A-Za-z0-9_-.])+")))
               (:tr
                 (:td (:label :for "paswd" "password"))
                 (:td (:input :name "password" :id "paswd" :type "password")))
               (:tfoot
                 (:td (:input :type "submit" :value "Log in"))))))))

(defun page-approval (args)
  (simple-page
    (:title "Approval" :args args :output output)
    (:table
      (page-error-case output (gethash "e" (getf args :query-parsed)))
      (loop with empty = t
            with stmt = (sqlite:prepare-statement *db* "select name, id from users_pending")
            while (sqlite:step-statement stmt) do
            (progn
              (setf empty nil)
              (format output "<tr><td>~A</td><td><form method=\"post\" action=\"/approval/~D\"><input type=\"submit\" value=\"Approve\"></form></td></tr>"
                      (sqlite:statement-column-value stmt 0)
                      (sqlite:statement-column-value stmt 1)))
            finally (when empty
                      (format output "<p class=\"note\">No approval requests</p>"))))))

(defun approval-right-p (user-id)
  (sqlite:execute-single *db* "select 1 from user_rights where action=0 and id=?" user-id))

(defun page-approve-user (args)
  (labels ((l-error (text)
             (return-from
               page-approve-user
               `(303 (:content-type "text/plain"
                      :location ,(format nil "/approval?e=~A" text))
                 nil))))
    (if (or (not (second (getf args :deduced-user)))
            (not (approval-right-p (second (getf args :deduced-user)))))
      (l-error "au"))
    (let* ((id (or (parse-integer (second (getf args :path-s))
                                  :junk-allowed t)
                   (l-error "ai")))
           (user-to-approve
             (multiple-value-list
               (sqlite:execute-one-row-m-v
                 *db* "select name, passwd from users_pending where id=?" id))))
      (unless (car user-to-approve)
        (l-error "an"))
      (sqlite:execute-non-query *db* "delete from users_pending where id=?" id)
      (sqlite:execute-non-query
        *db* "insert into users (name, passwd) values (?, ?)"
        (first user-to-approve) (second user-to-approve))
      (simple-page
        (:title "Approval successful" :args args :output output)
        (:p :class "note"
            (format output "Confirmed registration for user (~A)"
                    (first user-to-approve)))))))

(defun tag-count (tag-id)
  (sqlite:execute-single *db* "select count(*) from tags_to_posts where post_id=?" tag-id))

(defun collect-post-tags (post-id)
  ;; TODO improve perf if needed
  (mapcar #'car (sqlite:execute-to-list
                  *db* "select tags.name from tags_to_posts inner join tags on tags.id=tags_to_posts.tag_id where tags_to_posts.post_id=?" post-id)))

(defun print-date (d)
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
    (decode-universal-time d 0)
    (declare (ignore zone daylight-p day))
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D by UTC" year month date hour minute second)))

(defun page-display-post (p post-id)
  (multiple-value-bind (id md5 date by)
    (sqlite:execute-one-row-m-v
      *db* "select id, md5, creation_date, posted_by from posts where id=?" post-id)
    (when id
      (let ((username (or (id-to-username by) "~~")))
        (format p "<p>id:~D md5:~A date:(~A) posted_by:<a href=\"/user/~A\">~A</a> tags:"
                id md5 (print-date date)
                (percent-encode username)
                username))
      (loop with first = t
            for i in (collect-post-tags post-id)
            if first do (setf first nil)
            else do (format p ", ")
            do (format p "<a href=\"/search?s=~A\">~S</a>"
                       (percent-encode i) i))
      (format p "</p><img src=\"/imgs/~A\" alt=\"post image\">"
              (image-orig-path md5)))))

(defun page-display-comments (p post-id)
  (loop with stmt = (sqlite:prepare-statement *db* "select id, user, msg, date from comments where in_post=?")
        initially (sqlite:bind-parameter stmt 1 post-id)
        while (sqlite:step-statement stmt)
        do (format p "<p class=\"comment-p\">comment id:~D by <a href=\"/user/~A\">~:*~A</a> at ~A<br>~A</p>"
                   (sqlite:statement-column-value stmt 0)
                   (id-to-username (sqlite:statement-column-value stmt 1))
                   (print-date (sqlite:statement-column-value stmt 3))
                   (escape-string (sqlite:statement-column-value stmt 2)))
        finally (sqlite:finalize-statement stmt)))

(defun page-display-post-preview (p post-id)
  (format p "<article class=\"article-preview\"><a href=\"/id/~D\"><img src=\"/imgs/~A\" alt=\"post image\"><h3>post #~A</h3></a></article>"
          post-id (image-preview-path post-id) post-id))

(defun post-matches-tag-p (post-id tag-id)
  (sqlite:execute-single *db* "select post_id from tags_to_posts where post_id=? and tag_id=?" post-id tag-id))

(defun post-matches-tags-p (id x)
  (loop for i in x always (post-matches-tag-p id i)))

(defun page-search-list-null (p page-begin number)
  (loop with stmt =
        (sqlite:prepare-statement
          *db* "select id from posts order by creation_date desc limit ? offset ?")
        initially
        (progn
          (sqlite:bind-parameter stmt 1 number)
          (sqlite:bind-parameter stmt 2 page-begin))
        while (sqlite:step-statement stmt)
        do (page-display-post-preview
             p (sqlite:statement-column-value stmt 0))
        finally (sqlite:finalize-statement stmt))
  (sqlite:execute-single *db* "select count() from posts"))

(defun page-search-list-single (p tags page-begin number)
  (loop with stmt =
        (sqlite:prepare-statement
          *db*
          "select posts.id from posts join tags_to_posts on posts.id=tags_to_posts.post_id where tags_to_posts.tag_id = ? order by posts.creation_date desc limit ? offset ?")
        initially
        (progn
          (sqlite:bind-parameter stmt 1 (car tags))
          (sqlite:bind-parameter stmt 2 number)
          (sqlite:bind-parameter stmt 3 page-begin))
        while (sqlite:step-statement stmt)
        do (page-display-post-preview
             p (sqlite:statement-column-value stmt 0))
        finally (sqlite:finalize-statement stmt))
  (sqlite:execute-single *db* "select count() from tags_to_posts where tag_id = ?" (car tags)))

(defun build-cached-check (tags)
  (format nil "select cache_entries.id from cache_entries join cache_tags on cache_entries.id=cache_tags.cache_id group by cache_tags.cache_id having count(*) = ~D and count(case when cache_tags.tag_id in (~{~D~^,~}) then 1 end) = ~D" (length tags) tags (length tags)))

(defun search-query-cached-p (tags)
  "Returns the ID of the cache entry matching TAGS exactly. In
case such entry doesn't exist, returns nil"
  (sqlite:execute-single *db* (build-cached-check tags)))

(defun build-cache-creation (tags)
  (format nil
"insert into cache_posts (cache_id, post_id, post_creation_date)
select :cache_id, posts.id, posts.creation_date from posts
join tags_to_posts as tags1 on posts.id=tags1.post_id
join tags_to_posts on posts.id=tags_to_posts.post_id
where tags1.tag_id=:smallest_tag
and tags_to_posts.tag_id in (~{~D~^,~})
group by posts.id
having count(distinct tags_to_posts.tag_id) = :tag_count"
          tags))

(defun create-cache-entry (tags)
  (sqlite:execute-non-query
    *db* "insert into cache_entries (last_used) values (?)"
    (get-universal-time))
  (let ((id (sqlite:last-insert-rowid *db*)))
    (dolist (i tags)
      (sqlite:execute-non-query
        *db* "insert into cache_tags (cache_id, tag_id) values (?, ?)"
        id i))
    id))

(defun search-query-cache (tags)
  "Cache a search query. Is quite slow"
  (assert (< 0 (length tags)))
  (let ((ctag (car tags)))
    ;; TODO rewrite this into a query
    (loop with ccount = (tag-count ctag)
          for i in (cdr tags) do
          (let ((y (tag-count i)))
            (when (< y ccount)
              (setf ccount y
                    ctag i))))
    (let ((cache-id (create-cache-entry tags)))
      (sqlite:execute-non-query/named
        *db* (build-cache-creation tags)
        ":cache_id" cache-id
        ":smallest_tag" ctag
        ":tag_count" (length tags))
      cache-id)))

(defun page-search-list-cached (p cache-id page-begin number)
  (mark-used-cache cache-id)
  (loop with stmt = (sqlite:prepare-statement *db* "select post_id from cache_posts where cache_id=? order by post_creation_date desc limit ? offset ?")
        initially
        (progn
          (sqlite:bind-parameter stmt 1 cache-id)
          (sqlite:bind-parameter stmt 2 number)
          (sqlite:bind-parameter stmt 3 page-begin))
        while (sqlite:step-statement stmt)
        do (page-display-post-preview p (sqlite:statement-column-value stmt 0))
        finally (sqlite:finalize-statement stmt)))

(defun page-search-list (p s page)
  (let* ((page-begin (* page *posts-per-page*))
         (query-parsed (parse-tags s))
         (x (mapcar (lambda (name) (tagname-to-id name)) query-parsed)))
    (cond
      ((some #'null x)
       (values
         nil
         (format
           nil "nonexistent tags: ~{~A~^, ~}"
           (loop for i in x
                 for j in query-parsed
                 unless i
                 collect j))))
      ((null x)
       (page-search-list-null p page-begin *posts-per-page*))
      ((= 1 (length x))
       (page-search-list-single p x page-begin *posts-per-page*))
      (t
       (let ((cache-id
               (or (search-query-cached-p x)
                   (search-query-cache x))))
         (page-search-list-cached p cache-id page-begin *posts-per-page*))
       0))))

(defun page-search (args)
  (let* ((qp (getf args :query-parsed))
         (s (percent-decode (gethash "s" qp "")))
         (page (or (parse-integer (gethash "p" qp "") :junk-allowed t)
                   0))
         (post-count nil)
         (error-text nil))
    (simple-page
      (:title "Search" :args args :output output)
      (:form :action "/search"
             (:label :for "s" "Search string: ")
             (:input :id "s" :name "s" :type "text" :placeholder "empty"
                     :value (escape-string s)))
      (:div :id "post-list"
            (multiple-value-setq (post-count error-text)
              (page-search-list output s page)))
      (:ul
        :class "page-list"
        (when post-count
          (unless (zerop page)
            (with-html-output (output)
              (:li (:a :href (format nil "/search?s=~A&p=~D" s (1- page)) "<-"))))
          (loop for i from 0 to (floor (1- post-count) *posts-per-page*) do
                (with-html-output (output)
                  (:li (:a :href (format nil "/search/?s=~A&p=~D" s i)
                           :class (when (= i page) "cur-page-link")
                           (format output "~:[[~D]~;(~D)~]" (= i page) i)))))))
      (format output "<p>~:[no~;~:*~D~] posts found.</p>" post-count)
      (when error-text
        (with-html-output (output)
          (:p :class "note"
              (format output "It appears that you have made an invalid request: ~A" error-text)))))))

(defun page-list-tags (args)
  (let* ((query-parsed (getf args :query-parsed))
         (page-string (or (gethash "p" query-parsed) ""))
         (page (or (parse-integer page-string :junk-allowed t)
                   0)))
    (simple-page
      (:title "Tag list" :args args :output output)
      (:ul
        (loop with stmt = (sqlite:prepare-statement *db* "select name, id from tags order by name limit ? offset ?")
              initially
              (progn
                (sqlite:bind-parameter stmt 1 *tags-per-page*)
                (sqlite:bind-parameter stmt 2 (* page *tags-per-page*)))
              while (sqlite:step-statement stmt)
              do (format output "<li><a href=\"/search?s=~A\">~A</a> (id ~D)</li>"
                         (percent-encode (sqlite:statement-column-value stmt 0))
                         (sqlite:statement-column-value stmt 0)
                         (sqlite:statement-column-value stmt 1))
              finally (sqlite:finalize-statement stmt)))
      (:ul :class "page-list"
           (loop for i from 0 to (floor (sqlite:execute-single *db* "select count() from tags") *tags-per-page*)
                 do (with-html-output (output)
                      (:li (:a :href (format nil "/list-tags?p=~D" i)
                               :class (when (= i page) "cur-page-link")
                               (format output "~:[[~D]~;(~D)~]" (= i page) i)))))))))

(defun page-user (args)
  (let* ((path-s (getf args :path-s))
         (username (second path-s))
         (user-id (username-to-id username)))
    (simple-page
      (:output output
       :title (format output "User ~A~:[(not found)~;~]" username user-id)
       :args args)
      (if user-id
        (with-html-output (output)
          (:p :class "note" "I dunno what to put here")
          (:p (format output "This is user [<span class=\"username\">~A</span>][~D]." username user-id))
          (:p "Maybe user description could be.")
          (:p "A post/comment list? A pfp?"))
        (with-html-output (output)
          (:p :class "error" "User not found"))))))

(defun valid-username-p (name)
  "Username is valid whenever it is at least 3 characters long and
consists only of Latin script, numerics, and any of [._-]"
  (and (>= (length name) 3)
       (loop for i across name
             always (or (latin-char-p i)
                        (numer-char-p i)
                        (char= #\. i)
                        (char= #\_ i)
                        (char= #\- i)))))

(defun invalid-username-p (name)
  (not (valid-username-p name)))

(defun username-to-id (name)
  (sqlite:execute-single *db* "select id from users where name=?" name))

(defun id-to-username (id)
  (when id
    (sqlite:execute-single *db* "select name from users where id=?" id)))

(defun page-post-user (args)
  (let ((c (parse-simple (getf args :body))))
    (cond
      ((not (gethash "password" c))
       (list 303 '(:content-type "text/plain" :location "/register?e=p")
             '("Invalid username")))
      ((invalid-username-p (gethash "username" c))
       (list 303 '(:content-type "text/plain" :location "/register?e=i")
             '("Invalid username")))
      ((username-to-id (gethash "username" c))
       (list 303 '(:content-type "text/plain" :location "/register?e=x")
             '("Username already exists")))
      ((not (string= (gethash "password" c)
                     (gethash "password1" c)))
       (list 303 '(:content-type "text/plain" :location "/register?e=n")
             '("Passwords not equal")))
      ((eq *registration-mode* :open)
       (sqlite:execute-non-query *db* "insert into users (name, passwd) values (?, ?)"
                                 ;; TODO make this SHA256
                                 (gethash "username" c) (array-to-hex (md5:md5sum-sequence (gethash "password" c))))
       (simple-page
         (:title "Account registered" :args args :output output)
         (:p (format output "Account (~A) succesfully registered" (gethash "username" c)))))
      ((eq *registration-mode* :manual)
       (sqlite:execute-non-query *db* "insert into users_pending (name, passwd) values (?, ?)"
                                 ;; TODO same, make this SHA256
                                 (gethash "username" c) (array-to-hex (md5:md5sum-sequence (gethash "password" c))))
       (simple-page
         (:title "Account in manual approval queue" :args args :output output)
         (:p (format output "Account (~A) is now waiting for approval" (gethash "username" c)))))
      (t
        (list 303 '(:content-type "text/plain" :location "/register?e=s")
              '("Server error: unknown registration mode"))))))

(defun password-match-p (username password)
  ;; TODO replace md5 with SHA256
  (if (sqlite:execute-single
        *db* "select 1 from users where name=? and passwd=?"
        username
        (array-to-hex (md5:md5sum-sequence password)))
    t))

(defvar base64-alphabet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+/")
(assert (= 64 (length base64-alphabet)))

(defun int-to-base64 (x)
  (elt base64-alphabet x))

(defun generate-session-cookie (db user-id)
  ;; TODO check out cryptographic RNGs
  (loop with r = (make-string 32)
        for i from 0 below (length r)
        do (setf (elt r i) (int-to-base64 (random 64 *z-random-state*)))
        finally (progn
                  (sqlite:execute-non-query
                    db "insert into sessions (id, expiration, user_id) values (?, ?, ?)"
                    r (+ *max-session-age* (get-universal-time)) user-id)
                  (return r))))

(defun get-cookie-user (cookie-parsed)
  (sqlite:execute-one-row-m-v
    *db* "select users.name, users.id from sessions inner join users on users.id=sessions.user_id where sessions.id=?"
    (gethash "session" cookie-parsed)))

(defun delete-session-by-id (session)
  (sqlite:execute-non-query *db* "delete from sessions where id=?" session))

(defun page-post-session (args)
  (when (getf args :cookie-session)
    (delete-session-by-id (getf args :cookie-session)))
  (let ((qp (parse-simple (getf args :body))))
    (cond
      ((not (username-to-id (gethash "username" qp)))
       (list 303 '(:content-type "text/plain" :location "/login?e=lx")))
      ((not (password-match-p (gethash "username" qp) (gethash "password" qp)))
       (list 303 '(:content-type "text/plain" :location "/login?e=lp")))
      (t
        (list 303 `(:content-type "text/plain"
                    :set-cookie ,(format nil "session=~A; Max-Age=~D"
                                         (generate-session-cookie *db* (username-to-id (gethash "username" qp))) *max-session-age*)
                    :location "/"))))))

;; Logging out is simple, just set the cookie to die and redirect back
;; Also remove the database entry
(defun page-logout (args)
  (when (getf args :cookie-session)
    (delete-session-by-id (getf args :cookie-session)))
  `(303 (:content-type "text/plain"
         :set-cookie "session=; Max-Age=0"
         :location ,(or (gethash "goto" (parse-simple (getf args :query))) "/"))))

(defun page-display-comment-form (p post-id)
  (with-html-output
    (p)
    (:form :method "post" :action (format nil "/id/~D/comment" post-id)
           (:textarea :name "comment" :style "display: block"
                      :placeholder "Comment text")
           (:input :type "submit" :value "Send comment"))))

(defun post-exists (id)
  (sqlite:execute-single *db* "select id from posts where id=?" id))

(defun page-id (args)
  (simple-page
   (:title "Id" :args args :output output)
   (let* ((id-s (second (getf args :path-s)))
          (c (parse-integer (or id-s "") :junk-allowed t)))
     (cond
       ((and c (post-exists c))
        (with-html-output (output)
          (:a :href (format nil "/id/~D/edit-tags" c) "Edit tags") " "
          (:a :href (format nil "/id/~D/hist" c) "History"))
        (page-display-post output c)
        (format output "<div class=\"comments\">")
        (page-display-comments output c)
        (format output "</div>")
        (if (first (getf args :deduced-user))
            (page-display-comment-form output c)
            (with-html-output (output)
              (:p :class "note" "Only logged users can comment"))))
       (t
        (with-html-output (output)
          (:p :class "note" (format output "Some crap instead of post id: ~S" id-s))))))))

(defun page-id-history (args)
  (simple-page
    (:title "Tag edit history" :args args :output output)
    (let* ((id-s (second (getf args :path-s)))
           (c (parse-integer (or id-s "") :junk-allowed t)))
      (cond
        ((and c (post-exists c))
         (format output "<table><thead><th>Tag</th><th>User</th><th>Action</th><th>Date</th></thead>")
         (loop with cnt = 0
               with stmt = (sqlite:prepare-statement *db* "select tags.name, users.name, tag_edits.date, tag_edits.action from tag_edits join tags on tag_edits.tag_id=tags.id join users on tag_edits.edited_by=users.id where post_id=? order by tag_edits.date")
               initially (sqlite:bind-parameter stmt 1 c)
               while (sqlite:step-statement stmt) do
               (progn
                 (incf cnt)
                 (format output "<tr><td>~A</td> <td>~A</td> <td>~A</td> <td>~A</td></tr>"
                         (sqlite:statement-column-value stmt 0)
                         (sqlite:statement-column-value stmt 1)
                         (if (zerop (sqlite:statement-column-value stmt 3))
                           "REMOVAL" "ADDITION")
                         (print-date (sqlite:statement-column-value stmt 2))))
               finally (when (zerop cnt)
                         (format output "No tag edits here.")))
         (format output "</table>")
         )
        (t
          (with-html-output (output)
            (:p :class "note" (format output "Some crap instead of pist id: ~S" id-s))))))))

(defun page-id-edit-tags (args)
  (simple-page
    (:title "Edit tags" :args args :output output)
    (let* ((id-s (second (getf args :path-s)))
           (c (parse-integer (or id-s "") :junk-allowed t))
           (query-parsed (parse-simple (getf args :query))))
      (page-error-case output (gethash "e" query-parsed))
      (cond
        ((and c (post-exists c))
         (page-display-post output c)
         (if (first (getf args :deduced-user))
           (with-html-output (output)
             (:form
               :method "post" :action (format nil "/id/~D/edit-tags" c)
               (:textarea
                 :name "tags"
                 :style "display: block"
                 (format output "~{~A~^ ~}" (collect-post-tags c)))
               (:input :type "submit" :name "submit" :value "Edit tags")))
           (with-html-output (output)
             (:p :class "note" "Only logged users can edit post tags"))))
        (t
         (with-html-output (output)
           (:p :class "note" (format output "Some crap instead of post id: ~S" id-s))))))))

(defun page-id-post-tags (args)
  (let* ((id-s (second (getf args :path-s)))
         (c (parse-integer (or id-s "") :junk-allowed t))
         (args-body-parsed (parse-simple (getf args :body)))
         (tag-string (gethash "tags" args-body-parsed ""))
         (tags-parsed (parse-tags (percent-decode tag-string))))
    (labels ((l-error
               (text)
               (return-from
                 page-id-post-tags
                 `(303 (:location ,(format nil "/id/~D/edit-tags?e=~A" c text))
                       nil))))
    (cond
      ((not (first (getf args :deduced-user)))
       (l-error "em"))
      ((not (and c (post-exists c)))
       (l-error "ci"))
      ((not tag-string)
       (l-error "ej"))
      ((not (every #'valid-tagname-p tags-parsed))
       (l-error "et"))
      ((< *max-post-tags* (length tags-parsed))
       (l-error "el"))
      (t
       (let* ((tags-id (mapcar (lambda (x) (tagname-to-id x :create-if-missing t))
                               tags-parsed))
              (tags-add (loop for i in tags-id
                              unless (sqlite:execute-single
                                       *db* "select tag_id from tags_to_posts where post_id=? and tag_id=?" c i)
                              collect i))
              (current-time (get-universal-time))
              (user (second (getf args :deduced-user))))
         (sqlite:execute-non-query/named
           *db* (format nil "insert into tag_edits (post_id, tag_id, edited_by, date, action)
select :post_id, tag_id, :user_id, :date, 0 from tags_to_posts
where post_id=:post_id and tag_id not in (~{~D~^,~})" tags-id)
                        ":post_id" c
                        ":user_id" user
                        ":date" current-time)
           (sqlite:execute-non-query/named
             *db* "insert into tag_edits (post_id, tag_id, edited_by, date, action)
select :post_id, tag_id, :user_id, :date, 1 from tags_to_posts
where post_id=:post_id and tag_id in ()"
             ":post_id" c
             ":user_id" user
             ":date" current-time)
           (sqlite:execute-non-query
             *db* (format nil "delete from tags_to_posts where post_id=? and tag_id not in (~{~D~^,~})" tags-id)
             c)
           (dolist (i tags-add)
             (sqlite:execute-non-query
               *db* "insert into tags_to_posts (post_id, tag_id) values (?, ?)"
               c i)
             (sqlite:execute-non-query
               *db* "insert into tag_edits (post_id, tag_id, edited_by, date, action) values (?, ?, ?, ?, 1)" c i user current-time))
           (update-cache c)
           `(303 (:location ,(format nil "/id/~D" c))
             nil)))))))

(defun page-post-comment (args)
  (let* ((parsed (parse-simple (getf args :body)))
         (path-s (getf args :path-s))
         (post-id (parse-integer (or (second path-s) "") :junk-allowed t))
         (user-id (second (getf args :deduced-user))))
    (labels ((l-error (text)
               (return-from page-post-comment
                 `(303 (:content-type "text/plain"
                        :location ,(format nil "/~{~A~^/~}?e=~A" (butlast path-s) text))
                       nil))))
      (unless post-id (l-error "ci"))
      (unless (post-exists post-id) (l-error "ci"))
      (unless user-id (l-error "cm"))
      (sqlite:execute-non-query
        *db* "insert into comments (in_post, parent_comment, msg, user, date) values (?, -1, ?, ?, ?)"
        (second path-s)
        (percent-decode (gethash "comment" parsed "-the text was lost-"))
        user-id
        (get-universal-time))
      (list 303 `(:content-type "text/plain"
                  :location ,(format nil "/~{~A~^/~}" (butlast path-s)))))))

(defun static-directory (args)
  ;; TODO figure out a more safe/better way to serve files
  ;; TODO figure out if I even need this crap
  (when (notany (lambda (x) (or (string= "." x)
                                (string= ".." x)))
                (getf args :path-s))
    (pathname (format nil "./~{~A~^/~}" (getf args :path-s)))))

(defun static-favicon (args)
  (declare (ignore args))
  #P"./static/favicon.png")

(defun page-notfound (args)
  (list 404 '(:content-type "text/plain")
        (list (format nil "Requested page not found ~S ~A"
                      (getf args :path)
                      (getf args :method)))))

(defun path-alike (path x)
  (and (or (= (length path)
              (length x))
           (eq :any* (car (last x))))
       (loop for i in path
             for j in x
             until (eq :any* j)
             always (or (eq :any j)
                        (equal i j)))))

(defparameter *route-paths*
  '((page-home         :get  nil)
    (page-search       :get  ("search"))
    (page-list-tags    :get  ("list-tags"))
    (page-user         :get  ("user" :any))
    (page-post-form    :get  ("post"))
    (page-register     :get  ("register"))
    (page-login        :get  ("login"))
    (page-approval     :get  ("approval"))
    (page-approve-user :post ("approval" :any))
    (page-id           :get  ("id" :any))
    (page-id-history   :get  ("id" :any "hist"))
    (page-id-edit-tags :get  ("id" :any "edit-tags"))
    (page-id-post-tags :post ("id" :any "edit-tags"))
    (page-post-create  :post ("id"))
    (page-post-user    :post ("user"))
    (page-post-session :post ("session"))
    (page-logout       :post ("delete-session"))
    (page-post-comment :post ("id" :any "comment"))
    (static-directory  :get  ("static" :any*))
    (static-directory  :get  ("imgs" :any*))
    (static-favicon    :get  ("favicon.png"))))

(defun route (method path-s)
  (loop for i in *route-paths*
        when (and (eql method (second i))
                  (path-alike path-s (third i)))
          return (car i)
        finally (return #'page-notfound)))

(defun split-path (s)
  (let ((r nil) (cpos 0))
    (labels ((finish (pos)
               (unless (= pos cpos)
                 (setf r (cons (subseq s cpos pos) r)))))
      (loop for i across s
            for ix from 0
            do (case i
                 (#\/ (finish ix) (setf cpos (1+ ix)))
                 (otherwise nil))
            finally (progn
                      (finish (length s))
                      (return (nreverse r)))))))

(defun page-entry (env)
  (let* ((path (getf env :path-info))
         (path-s (split-path path))
         (query (getf env :query-string))
         (body (getf env :raw-body))
         (method (getf env :request-method))
         (cookie (cdr (assoc :cookie (getf env :headers))))
         (cookie-parsed (parse-simple cookie #\;)))
    (when (and *teapot-mode*
               (not (equalp "127.0.0.1" (hunchentoot:remote-addr*))))
      (return-from page-entry
        (simple-page
          (:title "Zimboard" :args '() :output output :status 418
           :use-navbar nil :use-head nil)
          (:p :class "note" "I'm a teapot"))))
    (funcall (route method path-s)
             (list :query query
                   :body body
                   :cookie cookie-parsed
                   :query-parsed (parse-simple query)
                   :path path
                   :path-s path-s
                   :method method
                   :cookie-session (gethash "session" cookie-parsed)
                   :deduced-user (multiple-value-list (get-cookie-user cookie-parsed))))))

(defun page-with-db-entry (env)
  ;; TODO move the connection so it runs only once per thread
  (let ((*db* (sqlite:connect *db-path*)))
    (unwind-protect
      (page-entry env)
      (sqlite:disconnect *db*))))

(defvar *acceptor* nil)

(defclass my-acceptor (hunchentoot:acceptor)
  ((access-log-destination :initform nil
                           :accessor my-access-log-destination)
   (message-log-destination :initform nil
                            :accessor my-message-log-destination)))

(defmethod hunchentoot:acceptor-log-access ((acceptor my-acceptor) &key return-code)
  (declare (ignore return-code))
  nil)

;; NOTE that this would completely kill off the error logging
; (defmethod hunchentoot:acceptor-log-message ((acceptor my-acceptor) log-level format-string &rest format-args)
;   (declare (ignore log-level format-string format-args))
;   nil)

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor my-acceptor) request)
  (let* ((headers (hunchentoot:headers-in request))
         (env (list :path-info (hunchentoot:script-name request)
                    :query-string (hunchentoot:query-string request)
                    :raw-body (hunchentoot:raw-post-data :request request
                                                         :want-stream t)
                    :request-method (hunchentoot:request-method request)
                    :headers headers)))
    (let ((response (page-with-db-entry env)))
      (typecase response
        (null nil)
        (pathname
         (hunchentoot:handle-static-file response))
        (list
         (destructuring-bind (status headers &optional content) response
           (declare (type integer status)
                    (type list headers)
                    (type list content))
           (setf (hunchentoot:return-code*) status)
           (setf (hunchentoot:content-type*) (getf headers :content-type))
           ;; TODO figure out if cookies should be processed separately. The hunchentoot manual says so, but it still works(?)
           (loop for (header-name header-value) on headers by #'cddr
                 while header-name
                 do (setf (hunchentoot:header-out header-name) header-value))
           (car content)))))))

(defun start-server ()
  (when *acceptor*
    (hunchentoot:stop *acceptor*))
  ;; NOTE Can add :taskmaster (make-instance 'hunchentoot:single-threaded-taskmaster) to get single threading
  (setf *acceptor* (hunchentoot:start (make-instance 'my-acceptor :port *port*))))

(defun stop-server ()
  (hunchentoot:stop *acceptor*))

(unless *acceptor*
  (start-server))
