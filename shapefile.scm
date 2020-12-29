(define-module (shapefile)
  :use-module (shapefile shp)
  :use-module (shapefile dbase)
  :use-module (shapefile cpg)
  :use-module (shapefile prj)

  :use-module (shapefile internal common)

  :use-module ((ice-9 ftw) :select (scandir))

  :use-module (rnrs records syntactic)
  :use-module ((srfi srfi-1) :select (last))
  :use-module ((ice-9 regex) :select (string-match))

  :export (read-shapefile)
  )

(define-record-type shapefile
  (fields transcoder
          projection
          records
          ))
(create-printer shapefile)

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
           => (lambda (it) (call-with-input-file it parse-cgp-file))]
          [else ((@ (rnrs io ports) native-transcoder))]))


  ;; These three are REQUIRED to exist
  (define shp-file (assoc-ref file-map 'shp))
  (define shx-file (assoc-ref file-map 'shx))
  (define dbf-file (assoc-ref file-map 'dbf))

  ;; Type dbf-recods := (vector (list))
  ;; Type dbf-fields := (list field-descriptor-v3)
  (define-values (dbf-fields dbf-records*)
   (call-with-input-file dbf-file
     (lambda (port)
       ;; (display dbf-file) (newline)
       ;; (display port) (newline)
       ;; (display transcoder) (newline)
       (load-dbase-file port transcoder))))

  (define dbf-records
    (map (lambda (record)
           (map cons
                (map (compose string->symbol field-descriptor-v3-name)
                     dbf-fields)
                ;; remove leading bool (deletion indicator)
                (cdr record)))
         (vector->list dbf-records*)))

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

