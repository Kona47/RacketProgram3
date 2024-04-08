
#lang racket
(require racket/string)
(require 2htdp/batch-io)

;read data into a list
(define input (read-lines "scores.txt"))
;get team names from input
(define t1 (list-ref input 0))
(define t2 (list-ref input 16))
;get data of both teams
(define data (cdr input))

;These divide the team's data
(define team1info (append (take data 15)))
(define team2info  (append (take data 0) (drop data 16)))
 
;this converts to string 
(define (split str)
  (string-split str))

;drop player's name
(define (removeName lst)
  (drop lst 2))

;convert list to numbers
(define (toNum lst)
  (define (temp lst)
    (cond
      [(empty? lst) '()]
      [(equal? (first lst) "X")
       (cons 10 (temp (rest lst)))]
      [(and (not (empty? (rest lst))) (equal? (second lst) "/"))
       (cons (string->number (first lst))
             (cons (- 10 (string->number (first lst)))
                   (temp (rest (rest lst)))))]
      [(not (empty? (rest lst)))
       (cons (string->number (first lst))
                             (cons (string->number (second lst))
                                   (temp (rest (rest lst)))))]
      [else
       (cons (string->number (first lst))
             (temp (rest lst)))])) 
  (temp lst))

;this function converts numbers to frames. With help of ChatGPT 
(define (convertFrames lst)
  (define (helper lst)
    (cond
      [(empty? lst) '()]
      [(and (not (empty? lst)) (equal? (first lst) 10) (not (empty? (rest lst))) (not (empty? (rest (rest lst)))))
       (cons (+ 10 (second lst) (third lst)) (helper (rest lst)))]
      [(and (not (empty? lst)) (not (empty? (rest lst))) (not (empty? (rest (rest lst))))(equal? (+ (first lst) (second lst)) 10))
       (cons (+ 10 (third lst)) (helper (rest (rest lst))))]
      [(and (not (empty? lst)) (not (empty? (rest lst))))
            (cons (+ (first lst) (second lst)) (helper (rest (rest lst))))]
      [else helper(rest lst)]))

  (helper lst))

;This adds the frames
(define (gameSum lst)
  (define (temp lst n)
    (if (or (empty? lst) (= n 0))
        0
        (+ (first lst) (temp (rest lst) (- n 1)))))
  (temp lst 10))

;function from copilot.microsoft.com
;Does not add the scores together for each player, however
(define (outputInfo list)
  (let* ((result (foldl
                   (lambda (s acc)
                     (let* ((player (string-join (take (split s) 2) " "))
                            (numList (toNum (removeName (split s))))
                            (frameScores (convertFrames numList))
                            (score (gameSum frameScores)))
                       (display player)
                       (display ": ")
                       (display score)
                       (newline)
                       (if (> score (cdr acc))
                           (cons player score)
                           acc)))
                   (cons "" 0)
                   list))
         (highScorer (car result))
         (highScore (cdr result))
         (total (foldl
                        (lambda (s acc)
                          (let* ((num-list (toNum (removeName (split s))))
                                 (frameScores (convertFrames num-list))
                                 (score (gameSum frameScores)))
                            (+ acc score)))
                        0
                        list)))
    (newline)
    (display "Team Score: ")
    (display total)
    (newline)
    (newline)
    ;Outputs high scorer for each team. Could not figure out how to make it only 1 person out of the 2 teams
    (display "High Scorer: ")
    (display highScorer)
    (display ": ")
    (display highScore)
    (newline)
    (newline)
    total))

;display results
(define (displayStats team1 data1 team2 data2)
  (displayln team1)
  (let ((t1Total (outputInfo data1)))
    (displayln team2)
    (let ((t2Total (outputInfo data2)))
      (display "The winner is: ")
      (if (> t1Total t2Total)
          (displayln team1)
          (if (< t1Total t2Total)
              (displayln team2)
              (displayln "The game is a tie"))))))
;Call program
(displayStats t1 team1info t2 team2info)
