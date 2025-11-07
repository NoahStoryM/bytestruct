#lang typed/racket/base

(require racket/case
         (only-in typed/racket/base/optional
                  [cast unsafe-cast]))

(provide current-big-endian?

         Byte<0>       byte<0>?
         Byte<1>       byte<1>?
         Byte<2>       byte<2>?
         Byte<4>       byte<4>?
         Byte<8>       byte<8>?
         Byte<1/2>     byte<1/2>?
         Byte<1/2/4>   byte<1/2/4>?
         Byte<1/2/4/8> byte<1/2/4/8>?)


(define current-big-endian? : (Parameter Boolean) (make-parameter #t))


(define-new-subtype Byte<0> (bytes->byte<0> Bytes))
(define-new-subtype Byte<1> (bytes->byte<1> Bytes))
(define-new-subtype Byte<2> (bytes->byte<2> Bytes))
(define-new-subtype Byte<4> (bytes->byte<4> Bytes))
(define-new-subtype Byte<8> (bytes->byte<8> Bytes))
(define-type Byte<1/2>     (∪ Byte<1>     Byte<2>))
(define-type Byte<1/2/4>   (∪ Byte<1/2>   Byte<4>))
(define-type Byte<1/2/4/8> (∪ Byte<1/2/4> Byte<8>))

(define byte<0>?       (unsafe-cast (λ (b*) (and (bytes? b*) (case/eq (bytes-length b*) [(0      ) #t] [else #f]))) (pred Byte<0>      )))
(define byte<1>?       (unsafe-cast (λ (b*) (and (bytes? b*) (case/eq (bytes-length b*) [(1      ) #t] [else #f]))) (pred Byte<1>      )))
(define byte<2>?       (unsafe-cast (λ (b*) (and (bytes? b*) (case/eq (bytes-length b*) [(2      ) #t] [else #f]))) (pred Byte<2>      )))
(define byte<4>?       (unsafe-cast (λ (b*) (and (bytes? b*) (case/eq (bytes-length b*) [(4      ) #t] [else #f]))) (pred Byte<4>      )))
(define byte<8>?       (unsafe-cast (λ (b*) (and (bytes? b*) (case/eq (bytes-length b*) [(8      ) #t] [else #f]))) (pred Byte<8>      )))
(define byte<1/2>?     (unsafe-cast (λ (b*) (and (bytes? b*) (case/eq (bytes-length b*) [(1 2    ) #t] [else #f]))) (pred Byte<1/2>    )))
(define byte<1/2/4>?   (unsafe-cast (λ (b*) (and (bytes? b*) (case/eq (bytes-length b*) [(1 2 4  ) #t] [else #f]))) (pred Byte<1/2/4>  )))
(define byte<1/2/4/8>? (unsafe-cast (λ (b*) (and (bytes? b*) (case/eq (bytes-length b*) [(1 2 4 8) #t] [else #f]))) (pred Byte<1/2/4/8>)))
