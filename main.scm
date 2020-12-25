#!/usr/bin/guile \
-e main -s
!#

(add-to-load-path (dirname (current-filename)))

;; (define-module (main)
;;   :export (main))

(use-modules (shp)
             (dbase)

             (ice-9 getopt-long)

             (srfi srfi-1)

             (srfi srfi-43)

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



;; cgp - codepage
;; prj - projection
;; shp - shape data
;; shx - shape header
;; dbf - dbase (format|file)


(define option-spec
  '()
  )

(define-once ls-colors #f)

(define (init-ls-colors)
  (define ht (make-hash-table))
  (for-each (lambda (lst) (hash-set! ht (car lst) (cadr lst)))
            (map (lambda (kv) (string-split kv #\=))
                 (remove string-null?
                         (string-split (getenv "LS_COLORS") #\:))))
  (set! ls-colors ht))

(unless ls-colors
  (init-ls-colors))

;; rs
;; di ; directory
;; ln ; link

(define (get-color stat-type)
  (hash-ref ls-colors
   (case stat-type
     ((regular       ) "rs")
     ((directory     ) "di")
     ((symlink       ) "ln")
     ((fifo          ) "pi")
     ((socket        ) "so")
     ((block-special ) "bd")
     ((char-special  ) "cd")
     ) ""))

(define (shp-ls path)
  (define found-extensions (make-hash-table))
  (define regulars '())
  (for-each (lambda (entry)
              (define s (stat (string-append path "/"  entry)))
              (case (stat:type s)
                ((regular)
                 (let* ((parts (string-split entry #\.))
                        (bname (basename entry (string-append "." (last parts))))
                        (extension (string->symbol (last parts)))
                        )
                   (if (and (<= 2 (length parts))
                            (memv extension '(cpg dbf prj shp shx)))
                       (hash-set! found-extensions
                                  bname (cons (cons extension stat) (hash-ref found-extensions bname '())))

                       (set! regulars (cons (cons entry s) regulars)))))
                (else
                 (set! regulars (cons (cons entry s) regulars)))))
            ((@ (ice-9 ftw) scandir) path))

  (for-each (lambda (pair)
              (define fname (car pair))
              (define stat (cdr pair))
              (format #t "\x1b[~am~a\x1b[m\t"
                      (get-color (stat:type stat))
                      fname))
            regulars)

  (format #t "~%")

  (hash-for-each
   (lambda (key value)
     (format #t "~a~20t~{~a~^, ~}~%"
             key (map car value)))
   found-extensions))

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
