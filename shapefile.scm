(define-module (shapefile)
  :use-module (shapefile shp)
  :use-module (shapefile dbf)
  :use-module (shapefile cpg)
  :use-module (shapefile prj)

  :use-module (shapefile internal common)

  :use-module ((ice-9 ftw) :select (scandir))

  :use-module (rnrs records syntactic)
  :use-module ((srfi srfi-1) :select (last))
  :use-module ((ice-9 regex) :select (string-match))

  :export (read-shapefile
           create-shapefile
           shapefile?
           shapefile-transcoder
           shapefile-projection
           shapefile-records

           record?
           make-record
           record-shape
           record-data
           )
  )

(define-record-type shapefile
  (fields transcoder
          projection
          records
          ))
(create-printer shapefile)

;; TODO possibly give sensible befaults for transcoder and records.
(define* (create-shapefile
          #:key
          transcoder projection records)
  (make-shapefile transcoder projection records)
  )

(define-record-type record
  (fields shape data)
  )

(create-printer record)

(define (extension filename)
  (last (string-split filename #\.)))

(define shapefile-extensions
  '("shp" "shx" "dbf"
    "prj" "sbn" "sbx"
    "fbn" "fbx"
    "ain" "aih"
    "ixs" "mxz" "atx"
    "cpg" "qix"))

(define (read-shapefile path)
  (define filenames
    (scandir (dirname path)
             (lambda (s) (string-match
                     (format #f "^~a\\.(~a)$"
                             (basename path)
                             (string-join
                              shapefile-extensions
                              "|" 'infix))
                     s))))

  ;; Type := (list (cons extension-symbol filename))
  (define file-map
    (map (lambda (name)
           (cons (string->symbol (extension name))
                 (string-append path "." (extension name))))
         filenames))

  (define transcoder
    (cond [(assoc-ref file-map 'cpg)
           => (lambda (it) (call-with-input-file it parse-cpg-file))]
          [else ((@ (rnrs io ports) native-transcoder))]))


  ;; These three are REQUIRED to exist
  (define shp-file (assoc-ref file-map 'shp))
  (define shx-file (assoc-ref file-map 'shx))
  (define dbf-file (assoc-ref file-map 'dbf))

  (define dbf-records
    (call-with-values
        (lambda () (call-with-input-file dbf-file
                (lambda (port) (load-dbase-file port transcoder))))
      dbase-build-assoc-list))

  (define shape-data (call-with-input-file shp-file parse-shp-file))

  (define projection
    (cond [(assoc-ref file-map 'prj)
           => (lambda (it) (call-with-input-file it parse-prj-file))]
          [else #f]))

  ;; we completely ignore the shx file, since it's not needed.

  (create-shapefile
   #:transcoder transcoder
   #:projection projection
   #:records (map make-record
                  shape-data
                  dbf-records)))

