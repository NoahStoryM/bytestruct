#lang typed/racket/base

(require "../main.rkt" typed/rackunit)


(test-case "byte<0>?"
  (check-true (byte<0>? #""))
  (check-false (byte<0>? #"\0")))

(test-case "byte<1>?"
  (check-true (byte<1>? #"\1"))
  (check-false (byte<1>? #""))
  (check-false (byte<1>? #"\1\2")))

(test-case "byte<2>?"
  (check-true (byte<2>? #"\1\2"))
  (check-false (byte<2>? #"\1"))
  (check-false (byte<2>? #"\1\2\3")))

(test-case "byte<4>?"
  (check-true (byte<4>? #"\1\2\3\4"))
  (check-false (byte<4>? #"\1\2")))

(test-case "byte<8>?"
  (check-true (byte<8>? #"\1\2\3\4\5\6\7\a"))
  (check-false (byte<8>? #"\1\2\3\4")))

(test-case "byte<1/2>?"
  (check-true (byte<1/2>? #"\1"))
  (check-true (byte<1/2>? #"\1\2"))
  (check-false (byte<1/2>? #""))
  (check-false (byte<1/2>? #"\1\2\3")))

(test-case "byte<1/2/4>?"
  (check-true (byte<1/2/4>? #"\1"))
  (check-true (byte<1/2/4>? #"\1\2"))
  (check-true (byte<1/2/4>? #"\1\2\3\4"))
  (check-false (byte<1/2/4>? #"\1\2\3")))

(test-case "byte<1/2/4/8>?"
  (check-true (byte<1/2/4/8>? #"\1"))
  (check-true (byte<1/2/4/8>? #"\1\2"))
  (check-true (byte<1/2/4/8>? #"\1\2\3\4"))
  (check-true (byte<1/2/4/8>? #"\1\2\3\4\5\6\7\a"))
  (check-false (byte<1/2/4/8>? #"\1\2\3")))
