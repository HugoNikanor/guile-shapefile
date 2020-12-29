;;; https://www.dbase.com/Knowledgebase/INT/db7_file_fmt.htm
;; http://web.archive.org/web/20150323061445/http://ulisse.elettra.trieste.it/services/doc/dbase/DBFstruct.htm

(define-module (shapefile dbase)
  :use-module (shapefile common)
  :use-module ((rnrs io ports)
               :select (bytevector->string
                        get-bytevector-n
                        native-transcoder))
  :use-module ((rnrs bytevectors)
               :select (bytevector-uint-ref
                        bytevector-u8-ref))
  :use-module (rnrs records syntactic)

  ;; (rnrs records inspection)
  :use-module ((rnrs records procedural)
               :version (6)
               :select (make-record-type-descriptor))

  :export (load-dbase-file)
  )








(define-record-type dbase-header
  (fields version date record-count
          bytes-in-header bytes-in-record
          ;; incomplete-dbase4-transaction
          ;; dbase4-encryption-flag
          ;; mdx-file-exists
          ;; language-driver-id
          ;; language-driver-name
          ))

(create-printer dbase-header)

(define (parse-header bv)
  (define version         (bytevector-uint-ref bv  0 little 1))
  (define date            (bytevector-uint-ref bv  1 big 3))
  (define record-count    (bytevector-uint-ref bv  4 little 4))
  (define bytes-in-header (bytevector-uint-ref bv  8 little 2))
  (define bytes-in-record (bytevector-uint-ref bv 10 little 2))


  #| v4 extensions
  (define incomplete-dbase4-transaction
  (bytevector-uint-ref bv 14 little 1))
  (define dbase4-encryption-flag
  (bytevector-uint-ref bv 15 little 1))
  (define mdx-file-exists
  (bytevector-uint-ref bv 28 little 1))
  (define language-driver-id
  (bytevector-uint-ref bv 29 little 1))
  (define language-driver-name
  ;; TODO handle null terminator
  (bytevector->string (bytevector-slice bv 32 64)
  (native-transcoder)))
  |#

  (make-dbase-header
   version date record-count bytes-in-header bytes-in-record
   )
  )

;; C: character
;; D: date (YYYYMMDD)
;; N: numeric
;; L: logical (? Y y N n T t F f)
;; M: ???


(define (decode-bcd bv)
  (string->number
   (string-trim-both
    (bytevector->string bv (native-transcoder)))))


(define (get-field-parser char)
  (lambda (bv transcoder)
    (case char
      ((#\C)
       (bytevector->string bv transcoder))
      ((#\D)
       (let ((str (bytevector->string bv (native-transcoder))))
         (list (substring str 0 4)
               (substring str 4 6)
               (substring str 6 8))))
      ((#\N)
       (decode-bcd bv))
      ((#\L)
       (case (integer->char (bytevector-u8-ref bv 0))
         ((Y y T t) #t)
         ((F f N n) #f)
         ((?) #f) (else #f)))
      ((#\M)
       (raise 'not-yet-implemented)))))


(define (build-record-type name descriptor-list)
  ;; record-constructor
  (make-record-type-descriptor
   name #f #f #f #f
   (list->vector (map (lambda (name) `(mutable ,(string->symbol name))) (map field-descriptor-v3-name descriptor-list)))))


(define (build-parser transcoder #|constructor|# descriptor-list)
  (lambda (bv)
    (cons
     (char=? #\* (integer->char (bytevector-uint-ref bv 0 little 1)))
     (let loop ((rem descriptor-list)
                (sum 1))
       (if (null? rem)
           '()
           (cons ((get-field-parser (field-descriptor-v3-type (car rem)))
                  (bytevector-slice bv sum (+ sum (field-descriptor-v3-length (car rem))))
                  transcoder)
                 (loop (cdr rem)
                       (+ sum (field-descriptor-v3-length (car rem))))))))))







(define-record-type field-descriptor-v3
  (fields name
          type
          length
          decimal-count
          work-area-id
          set-fields-flag
          ))

(export
 field-descriptor-v3-name
 field-descriptor-v3-type
 field-descriptor-v3-length
 field-descriptor-v3-decimal-count
 field-descriptor-v3-work-area-id
 field-descriptor-v3-set-fields-flag
 )

(create-printer field-descriptor-v3)

(define (parse-field-descriptor-v3 bv)
  (define name (string-trim-both (bytevector->string (bytevector-slice bv 0 11)
                                                     (native-transcoder))
                                 (char-set-complement char-set:graphic)))
  (define type (integer->char (bytevector-uint-ref bv 11 little 1)))
  (define length (bytevector-uint-ref bv 16 little 1))
  (define decimal-count (bytevector-uint-ref bv 17 little 1))
  (define work-area-id (bytevector-uint-ref bv 20 little 1))
  (define set-fields-flag (bytevector-uint-ref bv 23 little 1))
  (make-field-descriptor-v3 name type length
                            decimal-count work-area-id
                            set-fields-flag))

;; (define-record-type field-descriptor-v4
;;   (fields name
;;           type
;;           length
;;           decimal-count
;;           production?
;;           autoincrement))

;; (create-printer field-descriptor-v4)

;; (define (parse-field-descriptor-v4 bv)
;;   (define name (bytevector->string (bytevector-slice bv 0 32)
;;                                    (native-transcoder)))
;;   (define type (integer->char
;;                 (bytevector-uint-ref bv 32 little 1)))
;;   (define field-length (bytevector-uint-ref bv 33 little 1))
;;   (define decimal-count (bytevector-uint-ref bv 34 little 1))
;;   (define production-flag (bytevector-uint-ref bv 37 little 1))
;;   (define autoincrement (bytevector-uint-ref bv 40 little 4))
;;   (make-field-descriptor name type field-length decimal-count
;;                          production-flag autoincrement))



(define (load-dbase-file port transcoder)

 (define header (parse-header (get-bytevector-n port 32)))

 (define field-descriptor-count
   (/ (- (dbase-header-bytes-in-header header)
         32 1)
      32))

 (define bv (get-bytevector-n port (- (dbase-header-bytes-in-header header) 32)))

 (define field-descriptors
   (map (lambda (i) (parse-field-descriptor-v3
                (bytevector-slice bv (* 32 i))))
        (iota field-descriptor-count)))

 ;; (get-bytevector-n port 1) ; = 0x0D

 (define parser
   (build-parser
    transcoder
    ;; (build-record-type 'name field-descriptors)
    field-descriptors))


 (define v (make-vector (dbase-header-record-count header)))
 (for-each (lambda (i) (vector-set!
                   v i
                   (parser
                    (get-bytevector-n
                     port
                     (dbase-header-bytes-in-record header)))))
           (iota (dbase-header-record-count header)))
 (values field-descriptors v)


  )

