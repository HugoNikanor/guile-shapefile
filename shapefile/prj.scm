(define-module (shapefile prj)
  :use-module (ice-9 peg)
  :use-module (ice-9 match)
  :export (parse-prj-string))

(define-peg-pattern string all
  (and (ignore "\"")
       (* (and (not-followed-by "\"") peg-any))
       (ignore "\"")))


(define-peg-pattern digit body
  (range #\0 #\9)
  )

(define-peg-pattern integer body
  (+ digit))

(define-peg-pattern number all
  (and integer "." integer))

(define-peg-pattern record all
  (and (+ (range #\A #\Z))
       (ignore "[")
       string
       (* (and (ignore ",") (or record string number)))
       (ignore "]")))

;; (peg:tree (match-pattern record str))

(define (parse-tree tree)
  (match tree
    (('string s) s)
    (('number s) (string->number s))
    (('record name body ...)
     `(,(string->symbol name)
       ,@(map parse-tree (keyword-flatten '(string record number)
                                          body))))
    (dflt
     (display dflt (current-error-port))
     (newline (current-error-port))
     )))



(define (parse-prj-string string)
  (parse-tree (peg:tree (match-pattern record string))))
