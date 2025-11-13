#lang info

(define license 'MIT)
(define collection "bytestruct")
(define version "0.0")

(define pkg-desc "A library for structured data in bytes")

(define deps
  '("base"
    "typed-racket-lib"))
(define build-deps
  '("at-exp-lib"
    "scribble-lib"
    "rackunit-typed"
    "racket-doc"
    "typed-racket-doc"))
#;
(define scribblings '(("scribblings/bytestruct.scrbl")))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
