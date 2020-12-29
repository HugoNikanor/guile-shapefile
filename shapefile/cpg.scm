(define-module (shapefile cpg)
  :use-module ((rnrs io ports)
               :select (make-transcoder
                        latin-1-codec
                        utf-8-codec
                        utf-16-codec))
  :export (resolve-codepage parse-cpg-file))


(define (resolve-codepage name)
  (make-transcoder
   ;; TODO I have no idea if these are correct
   ;; All files I have are marked 'ISO 88591'
   (cond [(string-contains name "ISO 8859") (latin-1-codec)]
         [(string-contains name "UTF 16") (utf-16-codec)]
         [(string-contains name "UTF 8") (utf-8-codec)]
         [else #| warn here |#
          (format (current-error-port)
                  "Warning, couldn't read codepage from ~a~%"
                  name)
          (latin-1-codec)])))


(define (parse-cpg-file port)
  (resolve-codepage ((@ (ice-9 rdelim) read-line) port)))
