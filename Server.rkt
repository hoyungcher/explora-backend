#lang racket

(require web-server/servlet) 
(require web-server/servlet-env)
(require web-server/http/json)
(require json)


; definitions for datapoints
(define (missing) #hasheq((type . "missing")(value . "null")))
(define (corrected a b) `#hasheq((type . "corrected")(value . #hasheq((corrected . ,a)(original . ,b)))))
(define (uncertain a) `#hasheq((type . "uncertain")(value . #hasheq((value . ,a)(function . "std-deviation")))))

; functions for preparing data
(define (read-type datapoint) 
  (cond[(empty? (filter hash? datapoint)) "normal"]
       [(equal? (hash-ref (first (filter hash? datapoint)) 'type) "uncertain") "uncertain"]
       [(equal? (hash-ref (first (filter hash? datapoint)) 'type) "missing") "missing"]
       [(equal? (hash-ref (first (filter hash? datapoint)) 'type) "corrected") "corrected"]))

(define (extract-value value)
  (cond[(not(hash? value)) value]
       [else (hash-ref value 'value)]))

(define (prepare_tuple tuple)
  `#hash((id . ,(extract-value (first tuple)))
         (x . ,(extract-value (second tuple)))
         (y . ,(extract-value (third tuple)))
         (type . ,(read-type tuple))))

(define (data->json data)
  (map prepare_tuple data))


; sample data with different datapoints
(define combined-data (list
  (list 1 1 (uncertain 6))
  (list 2 2 (corrected 12 15))
  (list 3 3 (missing))
  (list 4 4 30)))

(data->json combined-data)

; create json response
(define (json-response jsexpr)  
  (response/full
    200                             ; HTTP response code.
    #"OK"                           ; HTTP response message.
    (current-seconds)               ; Timestamp.
    APPLICATION/JSON-MIME-TYPE      ; MIME type for content.
    '()                             ; Additional HTTP headers.
    (list (jsexpr->bytes jsexpr))))

(define (handle-query request)
   (json-response (data->json combined-data)))



;; URL routing table (URL dispatcher).
(define-values (dispatch generate-url)
  (dispatch-rules
    [("") #:method "post" handle-query]
    [else (error "There is no procedure to handle the url.")]))

(define (request-handler request)
  (dispatch request))

;; Start the server.
(serve/servlet
  request-handler
  #:launch-browser? #f
  #:quit? #f
  #:listen-ip "127.0.0.1"
  #:port 8000
  #:servlet-regexp #rx"")