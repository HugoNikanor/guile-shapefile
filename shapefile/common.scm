(define-module (shapefile common)
  :use-module ((rnrs records inspection)
               :select (record-type-name
                        record-type-parent
                        record-type-field-names))
  :use-module ((rnrs records procedural)
               :version (6)
               :select (record-accessor))
  :use-module ((rnrs bytevectors)
               :version (6)
               :select (bytevector-length
                        bytevector-copy!
                        make-bytevector
                        endianness))
  :use-module ((srfi srfi-9 gnu)
               :select (set-record-type-printer!))
  :export (big little create-printer bytevector-slice))

(define big (endianness big))
(define little (endianness little))



(define (create-printer record)
  (set-record-type-printer!
   record
   (lambda (r p)
     (format p "#<~a" (record-type-name record))
     (let self ((record record))
       (cond ((record-type-parent record) => self))
       (let ((v (record-type-field-names record)))
         (map
          (lambda (i)
            (format p " ~a=~a"
                    (vector-ref v i)
                    ((record-accessor record i) r)))
          (iota (vector-length v)))))
     (display ">" p))))


;; bv[from:to]. From inclusive, to exclusive
(define* (bytevector-slice bv from #:optional (to (1- (bytevector-length bv))))
  (define count (- to from))
  (let ((nbv (make-bytevector count)))
    (bytevector-copy! bv from nbv 0 count)
    nbv))
