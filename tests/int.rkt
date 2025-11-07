#lang typed/racket/base

(require "../main.rkt" typed/rackunit)


(for ([big-endian? '(#t #f)])
  (parameterize ([current-big-endian? big-endian?])
    (test-case "Test constructors"
      (check-equal? (uint8 #x000) #"\0"  )
      (check-equal? (uint8 #x+ff) #"\xff")

      (check-equal? (sint8 #x000) #"\0"  )
      (check-equal? (sint8 #x+7f) #"\x7f")
      (check-equal? (sint8 #x-01) #"\xff")
      (check-equal? (sint8 #x-80) #"\x80")

      (check-equal? (uint16 #xffff) #"\xff\xff")
      (check-equal? (uint16 #x102)
                    (if big-endian?
                        #"\x01\x02"
                        #"\x02\x01"))
      (check-equal? (sint16 -2)
                    (if big-endian?
                        #"\xff\xfe"
                        #"\xfe\xff"))

      (check-equal? (sint32 -1) #"\xff\xff\xff\xff")
      (check-equal? (uint32 #x12345678)
                    (if big-endian?
                        #"\x12\x34\x56\x78"
                        #"\x78\x56\x34\x12"))

      (check-equal? (sint64 -1) (bytes #xff #xff #xff #xff #xff #xff #xff #xff))
      (check-equal? (uint64 #x102030405060708)
                    (if big-endian?
                        #"\x01\x02\x03\x04\x05\x06\x07\x08"
                        #"\x08\x07\x06\x05\x04\x03\x02\x01")))

    (test-case "Test converters"
      (define u8-ff (uint8 #xff))       ; #"\xff"
      (define s8-neg1 (sint8 -1))       ; #"\xff"
      (define u16-102 (uint16 #x102))   ; #"\x01\x02" or #"\x02\x01"
      (define s16-neg2 (sint16 -2))     ; #"\xff\xfe" or #"\xfe\xff"

      (test-case "int->natural"
        (check-equal? (int->natural u8-ff) #xff)
        (check-equal? (int->natural s8-neg1) #xff)
        (check-equal? (int->natural u16-102) #x102)
        (check-equal? (int->natural s16-neg2) #xfffe))

      (test-case "int->integer"
        (check-equal? (int->integer u8-ff) -1)
        (check-equal? (int->integer s8-neg1) -1)
        (check-equal? (int->integer u16-102) #x102)
        (check-equal? (int->integer s16-neg2) -2)))

    (test-case "Unsigned Arithmetic"
      (define u8-1 (uint8 1))
      (define u8-2 (uint8 2))
      (define u8-100 (uint8 100))
      (define u8-102 (uint8 102))
      (define u8-255 (uint8 255))
      (define u16-3 (uint16 3))
      (define u16-1000 (uint16 1000))
      (define u16-65535 (uint16 65535))

      (test-case "uint+"
        (check-equal? (uint+) (uint8 0))
        (check-equal? (uint+ u8-2) u8-2)
        (check-equal? (uint+ u8-1 u8-2) (uint8 3))
        (check-equal? (uint+ u8-1 u16-3) (uint16 4))
        (check-pred byte<2>? (uint+ u8-1 u16-3))
        (check-equal? (uint+ u8-1 u8-2 u16-3) (uint16 6))
        (check-equal? (uint+ u8-255 u8-1) (uint8 0))
        (check-equal? (uint+ u16-65535 u8-1) (uint16 0)))

      (test-case "uint*"
        (check-equal? (uint*) (uint8 1))
        (check-equal? (uint* u8-2) u8-2)
        (check-equal? (uint* u8-2 u8-100) (uint8 200))
        (check-equal? (uint* u8-100 u8-100) (uint8 16))
        (check-equal? (uint* u16-1000 u8-2) (uint16 2000))
        (check-equal? (uint* (uint16 50000) (uint16 2)) (uint16 34464)))

      (test-case "uint-"
        (check-equal? (uint- u8-100) (uint8 156))
        (check-equal? (uint- u8-100 u8-2) (uint8 98))
        (check-equal? (uint- u8-2 u8-100) (uint8 158))
        (check-equal? (uint- u16-1000 u8-2) (uint16 998))
        (check-equal? (uint- u16-1000 u8-100 u8-2) (uint16 898))
        (check-equal? (uint- u16-1000 u8-102) (uint16 898))))

    (test-case "Signed Arithmetic"
      (define s8-1 (sint8 1))
      (define s8-2 (sint8 2))
      (define s8-neg1 (sint8 -1))
      (define s8-neg2 (sint8 -2))
      (define s8-127 (sint8 127))
      (define s8-neg127 (sint8 -127))
      (define s8-neg128 (sint8 -128))
      (define s16-100 (sint16 100))
      (define s16-neg200 (sint16 -200))

      (test-case "sint+"
        (check-equal? (sint+) (sint8 0))
        (check-equal? (sint+ s8-2) s8-2)
        (check-equal? (sint+ s8-1 s8-2) (sint8 3))
        (check-equal? (sint+ s8-1 s8-neg2) (sint8 -1))
        (check-equal? (sint+ s8-127 s8-1) s8-neg128)
        (check-equal? (sint+ s8-neg127 s8-neg1) s8-neg128)
        (check-equal? (sint+ s8-neg128 s8-neg1) (sint8 127))
        (check-equal? (sint+ s8-neg2 s16-neg200) (sint16 -202))
        (check-equal? (sint+ s8-1 s8-2 s8-neg1 s8-neg2) (sint8 0)))

      (test-case "sint*"
        (check-equal? (sint*) (sint8 1))
        (check-equal? (sint* s8-2) s8-2)
        (check-equal? (sint* (sint8 10) (sint8 5)) (sint8 50))
        (check-equal? (sint* (sint8 10) s8-neg2) (sint8 -20))
        (check-equal? (sint* s8-neg2 s8-neg2) (sint8 4))
        (check-equal? (sint* (sint8 64) s8-2) s8-neg128)
        (check-equal? (sint* (sint8 32) (sint8 8)) s8-neg128)
        (check-equal? (sint* s16-100 s8-neg2) (sint16 -200)))

      (test-case "sint-"
        (check-equal? (sint- s8-neg2) (sint8 2))
        (check-equal? (sint- s8-2 s8-1) (sint8 1))
        (check-equal? (sint- s8-1 s8-2) (sint8 -1))
        (check-equal? (sint- s8-neg1 s8-neg2) (sint8 1))
        (check-equal? (sint- s16-100 s8-neg2) (sint16 102))
        (check-equal? (sint- s8-neg128 s8-1) (sint8 127))
        (check-equal? (sint- s8-127 s8-neg1) (sint8 -128))))))
