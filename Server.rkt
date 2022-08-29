#lang racket

(require web-server/servlet) 
(require web-server/servlet-env)
(require web-server/http/json)
(require json)
;(require "normal.rkt")


; definitions for datapoints
(define (missing) #hasheq((type . "missing")(value . "null")))
(define (corrected a b) `#hasheq((type . "corrected")(value . #hasheq((corrected . ,a)(original . ,b)))))

(define (std-deviation a) `#hasheq((function . "std-deviation")(error . ,a)))
(define (percentage a) `#hasheq((function . "percentage")(error . ,a)))
(define (absolute a) `#hasheq((function . "absolute")(error . ,a)))

(define (uncertain a b) `#hasheq((type . "uncertain")(value . #hasheq((value . ,a)(function . ,(hash-ref b 'function))(error . ,(hash-ref b 'error))))))
         
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
  `#hash((x_label . "Time (hours)")(y_label . "Energy production (kWh)")(data . ,(map prepare_tuple data)))
  )


; Data for a 20MW solar farm
; Case 1: Normal visualisation
(define normal-data
  (list
   (list 1 6 0.5)
   (list 2 7 2.6)
   (list 3 8 4.2)
   (list 4 9 9.7)
   (list 5 10 12.0)
   (list 6 11 13.8)
   (list 7 12 14.6)
   (list 8 13 14.8)
   (list 9 14 14.2)
   (list 10 15 12.7)
   (list 11 16 10.5)
   (list 12 17 8.9)
   (list 13 18 6.5)
   (list 14 19 3.8)
   (list 15 20 1.6)))

; Case 2: Missing values
(define missing-data
  (list
   (list 1 6 0.5)
   (list 2 7 2.6)
   (list 3 8 4.2)
   (list 4 9 9.7)
   (list 5 10 12.0)
   (list 6 11 13.8)
   (list 7 12 (missing)) ; network error -> interpolate
   (list 8 13 14.8)
   (list 9 14 14.2)
   (list 10 15 12.7)
   (list 11 16 10.5)
   (list 12 17 (missing)) ; undergoing maintanance -> impute 0
   (list 13 18 6.5)
   (list 14 19 3.8)
   (list 15 20 (missing)))) ; cannot be interpolated

; Case 3: Uncertain values
(define uncertain-data
  (list
   (list 1 6 (uncertain 0.5 (percentage 5)))
   (list 2 7 (uncertain 2.4 (percentage 5)))
   (list 3 8 (uncertain 4.2 (percentage 5)))
   (list 4 9 (uncertain 9.7 (percentage 5)))
   (list 5 10 (uncertain 12.0 (percentage 5)))
   (list 6 11 (uncertain 13.8 (percentage 10)))
   (list 7 12 (uncertain 14.6 (percentage 10)))
   (list 8 13 (uncertain 14.8 (percentage 10)))
   (list 9 14 (uncertain 14.2 (percentage 10)))
   (list 10 15 (uncertain 12.7 (percentage 5)))
   (list 11 16 (uncertain 10.5 (percentage 5)))
   (list 12 17 (uncertain 8.9 (percentage 5)))
   (list 13 18 (uncertain 6.3 (percentage 5)))
   (list 14 19 (uncertain 3.2 (percentage 5)))
   (list 15 20 (uncertain 1.6 (percentage 5)))))

; Case 4: Corrected values
(define corrected-data
  (list
   (list 1 6 0.5)
   (list 2 7 2.6)
   (list 3 8 4.2)
   (list 4 9 9.7)
   (list 5 10 12.0)
   (list 6 11 13.8)
   (list 7 12 14.6)
   (list 8 13 14.8)
   (list 9 14 14.2)
   (list 10 15 12.7)
   (list 11 16 (corrected 10.5 1.8))
   (list 12 17 (corrected 8.9 1.8))
   (list 13 18 (corrected 6.5 1.8))
   (list 14 19 (corrected 3.7 1.8))
   (list 15 20 (corrected 1.6 1.8))))

; create json response
(define (json-response jsexpr)  
  (response/full
    200                             ; HTTP response code.
    #"OK"                           ; HTTP response message.
    (current-seconds)               ; Timestamp.
    APPLICATION/JSON-MIME-TYPE      ; MIME type for content.
    (list (header #"Access-Control-Allow-Origin" #"*"))                           ; Additional HTTP headers.
    (list (jsexpr->bytes jsexpr))))

(define (handle-normal-data request)
   (json-response (data->json normal-data)))

(define (handle-missing-data request)
   (json-response (data->json missing-data)))

(define (handle-uncertain-data request)
   (json-response (data->json uncertain-data)))

(define (handle-corrected-data request)
   (json-response (data->json corrected-data)))



;; URL routing table (URL dispatcher).
(define-values (dispatch generate-url)
  (dispatch-rules
    [("missing") #:method "get" handle-missing-data]
    [("uncertain") #:method "get" handle-uncertain-data]
    [("corrected") #:method "get" handle-corrected-data]
    [("") #:method "get" handle-normal-data]
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