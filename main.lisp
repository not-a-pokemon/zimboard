(defpackage #:zimboard
  (:use
    #:cl
    #:clack #:lack #:cl-who
    #:sqlite
    #:flexi-streams
    #:md5
    ; #:ironclad
    #:magick-util))

(in-package #:zimboard)

(defvar *mem-db* (sqlite:connect "sqlite.db"))

(defvar *tables*
  '(("users" . "create table users (id integer primary key, name text, passwd text)")
    ;; A session is described by its identifier - a string of 32 alphanumeric characters
    ;; The expiration date is in standard UNIX time
    ;; Currently, session expiration isn't implemented
    ("sessions" . "create table sessions (id text primary key, expiration integer, user_id integer)")
    ;; If a comment's parent is a negative number, it's a post; otherwise, it's another comment it's attached to
    ;("comments" "create table comments (id integer primary key, parent integer, msg text)")
    ("posts" . "create table posts (id integer primary key, md5 text, complete integer, creation_date integer, posted_by integer)")
    ("tags" . "create table tags (id integer primary key, name text)")
    ("tags_to_posts" . "create table tags_to_posts (tag_id integer, post_id integer)")))

(defun table-exists-p (db name)
  (loop with stmt = (sqlite:prepare-statement db "select name from sqlite_master where type='table' and name=?")
        initially (sqlite:bind-parameter stmt 1 name)
        while (sqlite:step-statement stmt)
        thereis t))

(defun init-database (db) ;; TODO add filename, add force-overwrite flag
  (loop for i in *tables*
        unless (table-exists-p db (car i))
        do (sqlite:execute-non-query db (cdr i))))

; (defun post-text-message (txt &key db1 img) ;; TODO add filename
;   (let ((db (if db1 db1 (sqlite:connect ":memory:"))))
;     (sqlite:execute-non-query db "insert into text_line_posts (msg, imageid) values (?, ?)" txt (if img img 0))))

(defvar *mem-db-initialized* nil)
(defvar *max-session-age* 2592000)

;; TODO is this safe?
(defvar *z-random-state* (make-random-state t))

(defun startup-mem-db ()
  (init-database *mem-db*)
  ; (post-text-message "Welcome to the server" :db1 *mem-db*)
  ; (post-text-message "test test test" :db1 *mem-db*)
  ; (post-text-message "this text should be rendered on the page" :db1 *mem-db*)
  )

(unless *mem-db-initialized*
  (setf *mem-db-initialized* t)
  (startup-mem-db))

;; NOTE that this runs under the assumption a..z and likes are sequential in the codespace
(defun latin-char-p (c)
  (or (<= (char-code #\a) (char-code c) (char-code #\z))
      (<= (char-code #\A) (char-code c) (char-code #\Z))))

(defun numer-char-p (c)
  (<= (char-code #\0) (char-code c) (char-code #\9)))

(defun white-char-p (c)
  (or (eql c #\Tab)
      (eql c #\Newline)
      (eql c #\Space)))

(defun int-to-hex (n)
  "Returns the corresponding hex character for n in range 0-15 included"
  (if (< n 10)
    (code-char (+ 48 n))
    (code-char (+ 87 n))))

(defun array-to-hex (a)
  "Convert a byte array into a hex string"
  (loop with s = (make-string (* 2 (length a)))
        for i across a
        for ix from 0 do
        (progn
          (setf (elt s (+ ix ix)) (int-to-hex (logand 15 i)))
          (setf (elt s (+ ix ix 1)) (int-to-hex (ash i -4))))
        finally (return s)))

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
          (when (and (eql y 10) (>= l 2) (eql (aref a (- l 2)) 13))
            (return (adjust-array a l))))
        finally (return (adjust-array a l))))

(defun arr-int-equal-till-newline (a b &optional n)
  (let ((nx (if n n (length a))))
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

(defun parse-simple (request-body)
  (let ((l (typecase request-body
             (null nil)
             (string request-body)
             (t (read-line request-body nil)))))
    ;; NOTE that such a condition is redudant. (length nil) is zero
    (if (and l (not (zerop (length l))))
      (let ((r nil) (cur-name nil) (cpos 0))
        (labels ((finish
                   (pos)
                   ;; TODO is this use of intern correct and safe?
                   (if cur-name
                     (setf r `(,(subseq l cpos pos) ,(intern (string-upcase cur-name) :keyword) . ,r)
                           cur-name nil)
                     (setf r `(nil ,(intern (string-upcase (subseq l cpos pos)) :keyword) . ,r)))))
          (loop for i across l
                for ix from 0 do
                (case i
                  (#\& (finish ix) (setf cpos (1+ ix)))
                  (#\= (unless cur-name
                         (setf cur-name (subseq l cpos ix)
                               cpos (1+ ix))))
                  (otherwise nil))
                finally (progn (finish (length l)) (return (nreverse r)))))))))

(defun page-post-input (p)
  (cl-who:with-html-output (p)
    (:form :method "post" :enctype "multipart/form-data" ;; any other kind won't let me have file transmission
           :action "/id"
      ; (:input :type "text" :name "msg" :placeholder "enter text")
      (:input :type "file" :name "image")
      (:textarea :name "tags" :style "display: block"
                 :placeholder "post tags (separate with commas or whitespace)")
      (:input :type "submit" :value "Submit"))))

(defun page-navbar (p &optional logged-as)
  (cl-who:with-html-output
    (p)
    (:nav :id "page-navbar"
          ; (:img :id "lisp-logo"
          ;       :alt "May contain trace amounts of LISP"
          ;       :src "/imgs/lisplogo_warning_128.png")
          ; (:span :id "site-name" "ZImBoard")
          (:a :class "nav-link" :href "/" "Home")
          (:a :class "nav-link" :href "/search" "Search")
          (:a :class "nav-link" :href "/post" "Make a post")
          (if logged-as
            (cl-who:with-html-output
              (p)
              (:span :class "nav-span"
                     (format p "user/~A" logged-as))
              (:form :method "post" :action "/delete-session"
                     (:input :class "nav-button" :type "submit" :value "Log out")))
            (cl-who:with-html-output
              (p)
              (:a :class "nav-link" :href "/register" "Register")
              (:a :class "nav-link" :href "/login" "Login"))))))

(defun page-list-posts (args)
  (let ((start-time (get-internal-real-time)))
    (list
      200 '(:content-type "text/html")
      (list
        (cl-who:with-html-output-to-string
          (output nil :prologue t)
          (:html
            (:head (:title "zimboard")
                   (:link :rel "stylesheet"
                          :href "/static/style.css"))
            (:body
              (page-navbar output (getf args :deduced-user))
              (:div :id "main"
                    (:p "This is a simple imageboard, providing tag search. The 'search' page may be of interest.")
                    (:img :alt "May contain trace amounts of LISP"
                          :src "/imgs/lisplogo_warning_128.png")
                    (format
                      output "<p>Page generated in ~,5F</p>"
                      (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second))))))))))

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
               ((eql #\: i) (setf state 2))
               ((not (eql (elt compar ix) i))
                (return))))
          (2 (cond
               ((eql #\; i) (setf (fill-pointer this-name) 0))
               ((eql #\= i)
                (when (equalp this-name "name")
                  (setf state 3)))
               ((not (eql #\Space i)) (vector-push i this-name))))
          (3 (cond
               ((or (eql #\; i)
                    (eql #\Newline i)
                    (eql #\Return i))
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

(defun tagname-to-id (name db &key create-if-missing)
  (or
    (loop with stmt = (sqlite:prepare-statement db "select id from tags where name=?")
          initially (sqlite:bind-parameter stmt 1 name)
          while (sqlite:step-statement stmt)
          return (sqlite:statement-column-value stmt 0))
    (when create-if-missing
      (sqlite:execute-non-query
        db "insert into tags (name) values (?)" name)
      (sqlite:last-insert-rowid db))))

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
                       (eql i #\,))
               (finish ix))
            finally (progn (finish (length s))
                           (return (nreverse r)))))))

(defun page-post-create (args)
  (let ((parsed (parse-multipart (getf args :body)))
        ; (msg-text nil)
        (image-body nil)
        (tags nil)
        (cur-id nil))
    (dolist (i parsed)
      (let ((x (multipart-pull-out-name (getf i :headers))))
        (cond
          ; ((equalp "\"msg\"" x)
          ;  (setf msg-text (flexi-streams:octets-to-string
          ;                   (getf i :body)
          ;                   :external-format :utf-8)))
          ((equalp "\"image\"" x)
           (setf image-body (getf i :body)))
          ((equalp "\"tags\"" x)
           (setf tags (getf i :body))))))
    (when (and image-body
               (not (zerop (length image-body))))
      (let* ((clean-body (magick-util:make-clean-blob (copy-seq image-body)))
             (hash (array-to-hex (md5:md5sum-sequence clean-body))))
        ; (format t "calculated MD5: ~A~%" hash)
        ;; Was seemingly breaking on hashes starting with [digits]e... because of automatic type converison
        ;; FIXed by changing the type from non-existant 'string' to 'text'
        (sqlite:execute-non-query
          *mem-db* "insert into posts (complete, md5, creation_date, posted_by) values (0, ?, ?, ?)"
          hash (get-universal-time) (getf args :deduced-user))
        (setf cur-id (sqlite:last-insert-rowid *mem-db*))
        (let ((orig-name (format nil "./imgs/~A" (image-orig-path hash))))
          (ensure-directories-exist (directory-namestring orig-name))
          ;; TODO make some error checking
          (with-open-file (orig-f orig-name
                                  :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists nil)
            (cond
              (orig-f
                (loop for i across clean-body
                      do (write-byte i orig-f)))
              (t "Failed to write file: such hash already exists")))
          (let ((preview-name (format nil "./imgs/~A" (image-preview-path cur-id))))
            (ensure-directories-exist (directory-namestring preview-name))
            (unless (magick-util:make-thumbnail
                      clean-body
                      128 128
                      preview-name)
              "Failed to write the preview file")
            (loop for i in (parse-tags (flexi-streams:octets-to-string tags)) do
                  (sqlite:execute-non-query
                    *mem-db* "insert into tags_to_posts (tag_id, post_id) values (?, ?)"
                    (tagname-to-id i *mem-db* :create-if-missing t) cur-id)))
          (sqlite:execute-non-query
            *mem-db* "update posts set complete=1 where id=?" cur-id)))
      ;(post-text-message msg-text :img (if cur-id cur-id 0) :db1 *mem-db*)
      ))
  (list 303 '(:content-type "text/plain" :location "/")
        '("sending image")))

(defun page-post-form (args)
  (list
    200 '(:content-type "text/html")
    (list
      (cl-who:with-html-output-to-string
        (output nil :prologue t)
        (:html
          (:head (:title "Make a post")
                 (:link :rel "stylesheet"
                        :href "/static/style.css"))
          (:body
            (page-navbar output (getf args :deduced-user))
            (:div :id "main"
                  (page-post-input output))))))))

(defun page-error-div (output str)
  (cl-who:with-html-output
    (output)
    (:div :class "error"
          (:span (format output "~A" str)))))

(defun page-error-case (output error-kind)
  (cond
    ((not error-kind) nil)
    ((equalp error-kind "i")
     (page-error-div output "Invalid username"))
    ((equalp error-kind "x")
     (page-error-div output "User already exists"))
    ((equalp error-kind "n")
     (page-error-div output "Passwords do not match"))
    ((equalp error-kind "lx")
     (page-error-div output "User doesn't exist"))
    ((equalp error-kind "lp")
     (page-error-div output "Password incorrect"))))

(defun page-register (args)
  (let ((qp (parse-simple (getf args :query))))
    (list
      200 '(:content-type "text/html")
      (list
        (cl-who:with-html-output-to-string
          (output nil :prologue t)
          (:html
            (:head (:title "Register")
                   (:link :rel "stylesheet"
                          :href "/static/style.css"))
            (:body
              (page-navbar output (getf args :deduced-user))
              (:div :id "main"
                    (page-error-case output (getf qp :e))
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
                             (:tfoot (:td (:input :type "submit" :value "Register account")))))))))))))

(defun page-login (args)
  (let ((qp (parse-simple (getf args :query))))
    (list
      200 '(:content-type "text/html")
      (list
        (cl-who:with-html-output-to-string
          (output nil :prologue t)
          (:html
            (:head (:title "Login")
                   (:link :rel "stylesheet"
                          :href "/static/style.css"))
            (:body
              (page-navbar output (getf args :deduced-user))
              (:div :id "main"
                    (page-error-case output (getf qp :e))
                    (:form :method "post" :action "/session"
                           (:table
                             (:tr
                               (:td (:label :for "un" "Username"))
                               (:td (:input :name "username" :id "un" :type "text" :pattern "([A-Za-z0-9_-.])+")))
                             (:tr
                               (:td (:label :for "paswd" "Password"))
                               (:td (:input :name "Password" :id "paswd" :type "password")))
                             (:tfoot
                               (:td (:input :type "submit" :value "Log in")))))))))))))

(defun parse-search (s)
  (let ((r nil) (cpos 0))
    (labels ((finish
               (pos)
               (unless (= pos cpos)
                 (setf r (cons (subseq s cpos pos) r)))
               (setf cpos (1+ pos))))
      (loop for i across s
            for ix from 0 do
            (when (eql i #\+) (finish ix))
            finally (progn (finish (length s))
                           (return (nreverse r)))))))

(defun tag-count (tag-id)
  (sqlite:execute-single *mem-db* "select count(*) from tags_to_posts where post_id=?" tag-id))

(defun page-display-post (p post-id)
  (multiple-value-bind (id md5 date by)
    (sqlite:execute-one-row-m-v
      *mem-db* "select id, md5, creation_date, posted_by from posts where id=?" post-id)
    (when id
      (format p "<p>id:~A md5:~A date:~A posted_by:~A</p><img src=\"/imgs/~A\">"
              id md5 date
              (id-to-username by)
              (image-orig-path md5)))))

(defun post-matches-tag-p (post-id tag-id)
  (sqlite:execute-single "select post_id from tags_to_posts where post_id=? and tag_id=?" post-id tag-id))

(defun post-matches-tags-p (id x)
  (loop for i in x always (post-matches-tag-p id i)))

(defun page-search-list (p s)
  (let ((x (mapcar (lambda (name) (tagname-to-id name *mem-db*)) (parse-search s))))
    (let ((ctag nil))
      (loop with ccount = 100000000
            for i in x do
            (let ((y (tag-count i)))
              (when (< y ccount)
                (setf ccount y
                      ctag i))))
      (when ctag
        (loop with stmt = (sqlite:prepare-statement *mem-db* "select posts.id from posts inner join tags_to_posts where tags_to_posts.post_id=posts.id and tags_to_posts.tag_id=?")
              initially (sqlite:bind-parameter stmt 1 ctag)
              while (sqlite:step-statement stmt)
              when (post-matches-tags-p (sqlite:statement-column-value stmt 0) (cdr x))
              do (page-display-post p (sqlite:statement-column-value stmt 0)))))))

(defun page-search (args)
  (list
    200 '(:content-type "text/html")
    (list
      (cl-who:with-html-output-to-string 
        (output nil :prologue t)
        (:html
          (:head (:title "Search")
                 (:link :rel "stylesheet"
                        :href "/static/style.css"))
          (:body
            (page-navbar output (getf args :deduced-user))
            (:div :id "main"
                  (:form :action "/search"
                         (:label :for "s" "Search string: ")
                         (:input :id "s" :name "s" :type "text" :placeholder "empty"))
                  (:ul
                    (let ((s (getf (getf args :query-parsed) :s)))
                      (if s
                        (page-search-list output s)
                        (loop with stmt = (sqlite:prepare-statement *mem-db* "select id, md5 from posts order by creation_date desc limit 10")
                              while (sqlite:step-statement stmt)
                              do (format output "<li><img src=\"/imgs/~A\"><a href=\"/id?s=~A\">post #~A</li></li>"
                                         (image-orig-path (sqlite:statement-column-value stmt 1))
                                         (sqlite:statement-column-value stmt 0)
                                         (sqlite:statement-column-value stmt 0)))))))))))))

(defun valid-username-p (name)
  (loop for i across name
        ;; NOTE that arbitrary Unicode is not allowed in usernames.
        ;; only Latin script, numerics, and a few other symbols
        always (or (latin-char-p i)
                   (numer-char-p i)
                   (eql #\. i)
                   (eql #\_ i)
                   (eql #\- i))))

(defun invalid-username-p (name)
  (not (valid-username-p name)))

(defun username-exists-p (name)
  (loop with stmt = (sqlite:prepare-statement *mem-db* "select id from users where name=?")
        initially (sqlite:bind-parameter stmt 1 name)
        while (sqlite:step-statement stmt)
        thereis t))

(defun page-post-user (args)
  (let ((c (parse-simple (getf args :body))))
    (cond
      ((invalid-username-p (getf c :username))
       (list 303 '(:content-type "text/plain" :location "/register?e=i")
             '("Invalid username")))
      ((username-exists-p (getf c :username))
       (list 303 '(:content-type "text/plain" :location "/register?e=x")
             '("Username already exists")))
      ((not (equal (getf c :password)
                   (getf c :password1)))
       (list 303 '(:content-type "text/plain" :location "/register?e=n")
             '("Passwords not equal")))
      (t
        (sqlite:execute-non-query *mem-db* "insert into users (name, passwd) values (?, ?)"
                                  ;; TODO make this SHA256
                                  (getf c :username) (array-to-hex (md5:md5sum-sequence (getf c :password))))
        (list
          200 '(content-type "text/html")
          (list
            (cl-who:with-html-output-to-string
              (output nil :prologue t)
              (:html
                (:head (:title "Account registered")
                       (:link :rel "stylesheet"
                              :href "/static/style.css"))
                (:body
                  (page-navbar output (getf args :deduced-user))
                  (:div :id "main"
                        (:p (format output "Account (~A) succesfully registered" (getf c :username)))))))))))))

(defun password-match-p (username password)
  ;; TODO replace md5 with SHA256
  (loop with phash = (array-to-hex (md5:md5sum-sequence password))
        with stmt = (sqlite:prepare-statement *mem-db* "select 1 from users where name=? and passwd=?")
        initially (progn
                    (sqlite:bind-parameter stmt 1 username)
                    (sqlite:bind-parameter stmt 2 phash))
        while (sqlite:step-statement stmt)
        thereis t))

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

(defun username-to-id (name)
  (loop with stmt = (sqlite:prepare-statement
                      *mem-db*
                      "select id from users where name=?")
        initially (sqlite:bind-parameter stmt 1 name)
        while (sqlite:step-statement stmt)
        return (sqlite:statement-column-value stmt 0)))

(defun id-to-username (id)
  (when id
    (sqlite:execute-single *mem-db* "select name from users where id=?" id)))

(defun get-cookie-user (cookie-parsed)
  (loop with stmt = (sqlite:prepare-statement *mem-db* "select users.name, users.id from sessions inner join users where sessions.id=? and users.id=sessions.user_id")
        initially (sqlite:bind-parameter stmt 1 (getf cookie-parsed :session))
        while (sqlite:step-statement stmt)
        return (values (sqlite:statement-column-value stmt 0)
                       (sqlite:statement-column-value stmt 1))))

(defun delete-session-by-id (session)
  (sqlite:execute-non-query *mem-db* "delete from sessions where id=?" session))

(defun page-post-session (args)
  (when (getf args :cookie-session)
    (delete-session-by-id (getf args :cookie-session)))
  (let ((qp (parse-simple (getf args :body))))
    (cond
      ((not (username-exists-p (getf qp :username)))
       (list 303 '(:content-type "text/plain" :location "/login?e=lx")))
      ((not (password-match-p (getf qp :username) (getf qp :password)))
       (list 303 '(:content-type "text/plain" :location "/login?e=lp")))
      (t
        (list 303 `(:content-type "text/plain"
                    ;; Set cookies to expire after a month
                    :set-cookie ,(format nil "session=~A; Max-Age=~D" (generate-session-cookie *mem-db* (username-to-id (getf qp :username))) *max-session-age*)
                    :location "/"))))))

;; Logging out is simple, just set the cookie to die and redirect back
;; Also remove the database entry
(defun page-logout (args)
  (when (getf args :cookie-session)
    (delete-session-by-id (getf args :cookie-session)))
  `(303 (:content-type "text/plain"
         :set-cookie "session=; Max-Age=0"
         :location ,(or (getf (parse-simple (getf args :query)) :goto) "/"))))

(defun page-id (args)
  (list
    200 '(:content-type "text/html")
    (list
      (cl-who:with-html-output-to-string 
        (output nil :prologue t)
        (:html
          (:head (:title "Id")
                 (:link :rel "stylesheet"
                        :href "/static/style.css"))
          (:body
            (page-navbar output (getf args :deduced-user))
            (:div :id "main"
                  (let ((c (getf (getf args :query-parsed) :s)))
                    (when c
                      (page-display-post output c))))))))))

(defun page-notfound (args)
  (list 404 '(:content-type "text/plain")
        (list (format nil "Requested page not found ~S ~A"
                      (getf args :path)
                      (getf args :meth)))))

(defun route (meth path)
  (cond
    ((and (eql :get meth) (or (equal "/" path)
                              (equal "" path)))
     #'page-list-posts)
    ((and (eql :get meth) (equal "/search" path))
     #'page-search)
    ((and (eql :get meth) (equal "/post" path))
     #'page-post-form)
    ((and (eql :get meth) (equal "/register" path))
     #'page-register)
    ((and (eql :get meth) (equal "/login" path))
     #'page-login)
    ((and (eql :get meth) (equal "/id" path))
     #'page-id)
    ((and (eql :post meth) (equal "/id" path))
     #'page-post-create)
    ((and (eql :post meth) (equal "/user" path))
     #'page-post-user)
    ((and (eql :post meth) (equal "/session" path))
     #'page-post-session)
    ((and (eql :post meth) (equal "/delete-session" path))
     #'page-logout)
    (t #'page-notfound)))

(defun page-entry (env)
  (let* ((path (getf env :path-info))
         (query (getf env :query-string))
         (body (getf env :raw-body))
         (meth (getf env :request-method))
         (cookie (gethash "cookie" (getf env :headers)))
         (cookie-parsed (parse-simple cookie)))
    ; (declare (ignorable query))
    ; (format t "~A~%" (get-cookie-user cookie))
    (funcall (route meth path)
             (list :query query
                   :body body
                   :cookie cookie-parsed
                   :query-parsed (parse-simple query)
                   :path path
                   :meth meth
                   :cookie-session (getf cookie-parsed :session)
                   :deduced-user (get-cookie-user cookie-parsed)))))

(defvar *handler*
  (clack:clackup
    (lack:builder
      (:static :path "/imgs/" :root #P"./imgs/")
      (:static :path "/static/" :root #P"./static/")
      (lambda (env) (funcall 'page-entry env)))
    :port 8080))
