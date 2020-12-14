(define-module (main)
  :export (main))

(use-modules (shp)
             (dbase)

             ((rnrs io ports)
              :select (make-transcoder
                       latin-1-codec
                       utf-8-codec
                       utf-16-codec))
             )

(define (resolve-codepage name)
  (make-transcoder
   (cond [(string-contains name "8859") (latin-1-codec)]
         [(string-contains name "16") (utf-16-codec)]
         [(string-contains name "8") (utf-8-codec)]
         [else #| warn here |# (latin-1-codec)])))


(define (main args)

  (define path "/home/hugo/code/lantmateriet/download-opendata.lantmateriet.se/GSD-Sverigekartor_vektor/1_milj/Sverige/Sweref_99_TM/shape/svk/riks/")

  (define basename "vl_riks")

  (define b-len (string-length basename))

  (define files
    ((@ (ice-9 ftw) scandir)
     path
     (lambda (path)
       (and (<= b-len (string-length path))
            (string=? basename (string-take path b-len))))))


  ;; Type := (list (cons extension-symbol filename))
  (define file-map
    (map (lambda (name)
           (cons (string->symbol
                  (car (reverse (string-split name #\.))))
                 name))
         files))

  (define transcoder
    (cond [(assoc-ref file-map 'cgp)
           => (lambda (it)
                (resolve-codepage
                 (call-with-input-file it
                   (@ (ice-9 rdelim) read-line))))]
          [else ((@ (rnrs io ports) native-transcoder))]))


  ;; These three are REQUIRED to exist
  (define shp-file (string-append path (assoc-ref file-map 'shp)))
  (define shx-file (string-append path (assoc-ref file-map 'shx)))
  (define dbf-file (string-append path (assoc-ref file-map 'dbf)))

  ;; vector of records
  (define dbf-records
   (call-with-input-file dbf-file
     (lambda (port) (load-dbase-file port transcoder))))

  (define data (call-with-input-file shp-file parse-shp-file))

  (cons dbf-records data)
  )
