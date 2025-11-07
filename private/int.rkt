#lang typed/racket/base

(require "types.rkt"
         racket/case
         (only-in typed/racket/base/optional
                  [cast unsafe-cast]))

(provide UInt
         UInt8 UInt16 UInt32 UInt64
         uint8 uint16 uint32 uint64
         uint+ uint* uint-

         SInt
         SInt8 SInt16 SInt32 SInt64
         sint8 sint16 sint32 sint64
         sint+ sint* sint-

         Int
         int->natural
         int->integer)


(define-type UInt (∪ UInt8 UInt16 UInt32 UInt64))
(define-new-subtype UInt8  (byte<1>->uint8  Byte<1>))
(define-new-subtype UInt16 (byte<2>->uint16 Byte<2>))
(define-new-subtype UInt32 (byte<4>->uint32 Byte<4>))
(define-new-subtype UInt64 (byte<8>->uint64 Byte<8>))

(define-type SInt (∪ SInt8 SInt16 SInt32 SInt64))
(define-new-subtype SInt8  (byte<1>->sint8  Byte<1>))
(define-new-subtype SInt16 (byte<2>->sint16 Byte<2>))
(define-new-subtype SInt32 (byte<4>->sint32 Byte<4>))
(define-new-subtype SInt64 (byte<8>->sint64 Byte<8>))

(define-values (uint8 uint16 uint32 uint64 sint8 sint16 sint32 sint64)
  (let ()
    (define (make [size : Natural] [sign? : Boolean])
      (unsafe-cast
       (λ (n) (integer->integer-bytes n size sign? (current-big-endian?)))
       (∀ (Int) (→ Integer Int))))
    (values (inst (make 1 #f) UInt8 )
            (inst (make 2 #f) UInt16)
            (inst (make 4 #f) UInt32)
            (inst (make 8 #f) UInt64)
            (inst (make 1 #t) SInt8 )
            (inst (make 2 #t) SInt16)
            (inst (make 4 #t) SInt32)
            (inst (make 8 #t) SInt64))))

(: size->range (→ Natural (Values Positive-Integer Zero Positive-Integer Negative-Integer)))
(define (size->range size)
  (case/eq size
    [(1) (values #xff               0 #x7f               #x-80              )]
    [(2) (values #xffff             0 #x7fff             #x-8000            )]
    [(4) (values #xffffffff         0 #x7fffffff         #x-80000000        )]
    [(8) (values #xffffffffffffffff 0 #x7fffffffffffffff #x-8000000000000000)]
    [else (raise-argument-error 'size->range "(or/c 1 2 4 8)" size)]))

(: mod (→ Integer Integer Integer Integer))
(define (mod i min max)
  (cond
    [(< i min) (- (+ max 1) (bitwise-and (- max min) (- min i)))]
    [(> i max) (+ (- min 1) (bitwise-and (- max min) (- i max)))]
    [else i]))

(define zero8 (unsafe-cast #"\0" UInt8))
(define one8  (unsafe-cast #"\1" UInt8))

(define-values (_uint+ _uint* _sint+ _sint*)
  (let ()
    (define (make [op : (→ Natural * Natural)] [id8 : UInt8] [sign? : Boolean])
      (unsafe-cast
       (λ (n*)
         (cond
           [(null? n*) id8]
           [(null? (cdr n*)) (car n*)]
           [else
            (define big-endian? (current-big-endian?))
            (define-values (i size)
              (for/fold ([i (op)] [size 1]) ([n (in-list n*)])
                (values
                 (op i (integer-bytes->integer n sign? big-endian?))
                 (max size (bytes-length n)))))
            (define-values (umax umin smax smin) (size->range size))
            (integer->integer-bytes
             (if sign?
                 (mod i smin smax)
                 (mod i umin umax))
             size sign? big-endian?)]))
       (∀ (Int8 Int16 Int32 Int64)
          (case→
           (→ (Listof Byte<1>      ) Int8 )
           (→ (Listof Byte<1/2>    ) Int16)
           (→ (Listof Byte<1/2/4>  ) Int32)
           (→ (Listof Byte<1/2/4/8>) Int64)))))
    (values (inst (make + zero8 #f) UInt8 UInt16 UInt32 UInt64)
            (inst (make * one8  #f) UInt8 UInt16 UInt32 UInt64)
            (inst (make + zero8 #t) SInt8 SInt16 SInt32 SInt64)
            (inst (make * one8  #t) SInt8 SInt16 SInt32 SInt64))))

(define-values (uint+ uint* sint+ sint*)
  (let ()
    (define make
      (unsafe-cast
       (λ (_uint) (case-λ [(n) n] [n* (_uint n*)]))
       (∀ (Int8 Int16 Int32 Int64)
          (→ (case→
              (→ (Listof Byte<1>      ) Int8 )
              (→ (Listof Byte<1/2>    ) Int16)
              (→ (Listof Byte<1/2/4>  ) Int32)
              (→ (Listof Byte<1/2/4/8>) Int64))
             (case→
              (→ Byte<1>                     * Int8 )
              (→ Byte<1/2>     Byte<1/2>     * Int16)
              (→ Byte<1/2/4>   Byte<1/2/4>   * Int32)
              (→ Byte<1/2/4/8> Byte<1/2/4/8> * Int64))))))
    (values (make _uint+)
            (make _uint*)
            (make _sint+)
            (make _sint*))))

(define-values (uint- sint-)
  (let ()
    (define make
      (unsafe-cast
       (λ (name _int+ sign?)
         (define int-
           (case-λ
            [(n) (int- zero8 n)]
            [(n1 n2)
             (define big-endian? (current-big-endian?))
             (define i
               (+ (integer-bytes->integer n1 sign? big-endian?)
                  (bitwise-not (integer-bytes->integer n2 sign? big-endian?))
                  1))
             (define size (max (bytes-length n1) (bytes-length n2)))
             (define-values (umax umin smax smin) (size->range size))
             (integer->integer-bytes
              (if sign?
                  (mod i smin smax)
                  (mod i umin umax))
              size sign? big-endian?)]
            [(n . n*) (int- n (_int+ n*))]))
         (procedure-rename int- name))
       (∀ (Int8 Int16 Int32 Int64)
          (→ Symbol
             (case→
              (→ (Listof Byte<1>      ) Int8 )
              (→ (Listof Byte<1/2>    ) Int16)
              (→ (Listof Byte<1/2/4>  ) Int32)
              (→ (Listof Byte<1/2/4/8>) Int64))
             Boolean
             (case→
              (→ Byte<1>       Byte<1>       * Int8 )
              (→ Byte<1/2>     Byte<1/2>     * Int16)
              (→ Byte<1/2/4>   Byte<1/2/4>   * Int32)
              (→ Byte<1/2/4/8> Byte<1/2/4/8> * Int64))))))
    (values (make 'uint- _uint+ #f)
            (make 'sint- _sint+ #t))))


(define-type Int (∪ UInt SInt))

(: int->natural (→ Int Natural))
(: int->integer (→ Int Integer))
(define (int->natural i) (integer-bytes->integer i #f (current-big-endian?)))
(define (int->integer i) (integer-bytes->integer i #t (current-big-endian?)))
