#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path (dirname (current-filename)))

;; (define-module (main)
;;   :export (main))

(use-modules (shapefile shp)
             (shapefile dbase)
             (shapefile cpg)

             (ice-9 getopt-long)

             (srfi srfi-1)
             (srfi srfi-43)
             )




;; cgp - codepage
;; prj - projection
;; shp - shape data
;; shx - shape header
;; dbf - dbase (format|file)


(define option-spec
  '()
  )

(define (main args)
  (define options (getopt-long args option-spec))

  (define subopts (option-ref options '() '("ls")))
  (define mode (string->symbol (car subopts)))
  (define directory
    (if (>= 2 (length subopts))
        (cadr subopts)
        (getcwd)))

  (case mode
    ((ls)
     (shp-ls directory)
     )
    (else (format (current-error-port) "Unknown mode '~a'~%" mode)
          (exit 1)))

  ;; (set-port-encoding! (current-output-port) "ISO-8859-1")
  (let ((data (not-main args)))
    (vector-for-each
     (lambda (i r)
       (display (1+ i)) (display " ") (write r) (newline))
     (cdar data))
    (display (caar data))
    (newline)
    (display (length (cdr data)))
    (newline)
    (display (car (cdr data)))
    )
  (newline)

  (exit))

(define (not-main args)

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
    (cond [(assoc-ref file-map 'cpg)
           => (lambda (it)
                (resolve-codepage
                 (call-with-input-file
                     (string-append path it)
                   (@ (ice-9 rdelim) read-line))))]
          [else ((@ (rnrs io ports) native-transcoder))]))


  ;; These three are REQUIRED to exist
  (define shp-file (string-append path (assoc-ref file-map 'shp)))
  (define shx-file (string-append path (assoc-ref file-map 'shx)))
  (define dbf-file (string-append path (assoc-ref file-map 'dbf)))

  ;; vector of records
  (define-values (dbf-fields dbf-records)
   (call-with-input-file dbf-file
     (lambda (port) (load-dbase-file port transcoder))))

  (define data (call-with-input-file shp-file parse-shp-file))

  ;; TODO zip dbf-records and data
  (cons (cons dbf-fields dbf-records) data)
  )
