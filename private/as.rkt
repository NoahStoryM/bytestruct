#lang typed/racket/base/optional

(provide as)
(define-syntax-rule (as t v) (cast (ann (cast v t) Bytes) t))
