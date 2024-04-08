#lang racket
(require racket/string)
(require 2htdp/batch-io)
(require data/maybe)

;(string->number (first input))

(define input (read-lines "ACCOUNTS.TXT"))

; Get accounts
(define acct1 #f)
(define acct2 #f)
(define acct3 #f)

(match input
  [(list a b c)
   (set! acct1 a)
   (set! acct2 b)
   (set! acct3 c)])

(display acct1)
(newline)
(display acct2)
(newline)
(display acct3)
