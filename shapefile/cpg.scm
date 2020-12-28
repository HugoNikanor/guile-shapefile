(define-module (shapefile cpg)
  :use-module ((rnrs io ports)
               :select (make-transcoder
                        latin-1-codec
                        utf-8-codec
                        utf-16-codec))
  :export (resolve-codepage))


(define (resolve-codepage name)
  (make-transcoder
   (cond [(string-contains name "8859") (latin-1-codec)]
         [(string-contains name "16") (utf-16-codec)]
         [(string-contains name "8") (utf-8-codec)]
         [else #| warn here |#
          (format (current-error-port)
                  "Warning, couldn't read codepage from ~a~%"
                  name)
          (latin-1-codec)])))
