(defpackage #:magick-util
  (:use #:cl #:cffi)
  (:export :make-thumbnail
           :make-clean-blob))

(in-package #:magick-util)

(cffi:define-foreign-library magick-util-lib
  (t (:default "./magick_util")))

(cffi:use-foreign-library magick-util-lib)

; TODO run zi_init and zi_uninit where appropriate (Or I should rather not?)

(defun make-clean-blob (buf)
  (declare (type (simple-array (unsigned-byte 8)) buf))
  (cffi:with-foreign-objects
    ((new-len= :long 1)
     (new-buf= :pointer 1))
    (cffi:with-pointer-to-vector-data
      (buf= buf)
      (let ((status (cffi:foreign-funcall
                      "zi_make_clean_blob"
                      :pointer buf=
                      :long (length buf)
                      :pointer (cffi:mem-aptr new-len= :long)
                      :pointer (cffi:mem-aptr new-buf= :pointer)
                      :int)))
        (if (< status 0)
          nil
          (unwind-protect
            (loop with r = (make-array (cffi:mem-aref new-len= :long)
                                       :element-type '(unsigned-byte 8))
                  for i from 0 below (length r)
                  do (setf (aref r i) (cffi:mem-aref
                                        (cffi:mem-aref new-buf= :pointer)
                                        :unsigned-char i))
                  finally (return r))
            (cffi:foreign-funcall "zi_free_blob" :pointer (cffi:mem-aref new-buf= :pointer))))))))

(defun make-thumbnail (buf want-w want-h out-path)
  (declare (type (simple-array (unsigned-byte 8)) buf))
  (cffi:with-foreign-string
    (out-path= out-path)
    (cffi:with-pointer-to-vector-data
      (buf= buf)
      (let ((status (cffi:foreign-funcall
                      "zi_make_thumbnail"
                      :pointer buf=
                      :long (length buf)
                      :int want-w
                      :int want-h
                      :string out-path=
                      :int)))
        status))))
