(define-module (examples ls-dir))

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
