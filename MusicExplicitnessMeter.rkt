#lang racket
(require 2htdp/image)
(require csc151)
(require csc151/rex)
(require csc151www)

; project.rkt
;
; An amazing tool for measuring the explicitness of lyrics
;
; Project: Music Explicitness Meter
;
; Authors: Deepit Chandgothia, Aiden Klass, Andrea Suazo, & Kripa Bansal

#| References |#
;;; https://www.freewebheaders.com/full-list-of-bad-words-banned-by-google/#google_vignette
;;; ^^^ Used as a reference for bad/explicit words in the given lyrics
;;; https://docs.racket-lang.org/teachpack/2htdpimage.html
;;; ^^^ Referenced to create visualization
;;; Mini-project 4
;;; ^^^ Inspiration for project and drew code extract-words from it
;;; https://hotemoji.com/18-emoji.html
;;; ^^^ Inspiration for visualization
;;; songlyrics.com
;;; ^^^ drew our song lyrics from this page


#| Helpful Procedures |#
;;; Drawn from mini-project 4

;; (total-words str) -> integer?
;; lst: list?
;; Computes the total number of words in a list
(define total-words
  (lambda (lst)
    (length lst)))

;;; (extract-words str) -> listof? string?
;;; str : string?
;;; Make a list of all the words (sequences of letters) that appear in str.
;;; adapted from my miniproject 3
(define extract-words
  (let ([letters-together (rex-repeat (rex-any-of
            (rex-char-range #\a #\z)
            (rex-char-range #\' #\')
            (rex-char-range #\A #\Z)
            (rex-char-set "-")))])
    (lambda (str)
      (rex-find-matches letters-together str))))

;; (downcase str) -> str
;; str: string
;; Turns the string into an all downcase string
(define downcase
  (lambda (str)
    (list->string (map char-downcase (string->list str)))))

;;; (list-contains lst val)->bool?
;;; lst: list-of-vals?
;;; val: any type? val?
;; ;Returns true if val is in lst, returns false otherwise
(define list-contains?
  (lambda(lst val)
    (if (null? lst)
      #f
      (if (equal? (car lst) val)
        #t
        (list-contains? (cdr lst) val)))))


#| Part 1 |#
;;; This part of our project extracts lyrics as a list from an html file from this website: songlyrics.com 
;;; candidate for "A particularly nice procedure or piece of code" 

;;; (xml->text html) -> list?
;;; html: file?
;;; converts the html file into a list of song lyrics
(define xml->text
  (lambda (html)
    (sxpath-match "//p[contains(@class, 'songLyricsV14 iComment-text')]/text()" (file->xml html))))

;;; (remove-null lst)->lst
;;; lst: list-of lists?
;;; removes any null lists from a list of lists of lists
(define remove-null
  (lambda (lst)
    (if (null? lst)
      '()
      (if (null? (car lst))
        (remove-null (cdr lst))
        (cons (car lst) (remove-null (cdr lst)))))))

;;; (extract-text-html file)-> list-of-strings?
;;; (file: html-file?
  ;;; returns list of words from lyrics from 'songlyrics.com'
  (define extract-text-html
    (lambda (html)
      (reduce append(remove-null (map extract-words (xml->text html))))))
  
  
  #| Part 2 |#
  ;;; This part of our project categorizes song lyrics based on the amount of explicit words they
  ;;; contain
  
  ;;; (count-explicit lst)-> int
  ;;; lst: list-of-words
  ;;; returns the number of explicit words in lst, and explicit words 
  ;;; is from explicitwords.txt, which is a file adapted from https://www.freewebheaders.com/full-list-of-bad-words-banned-by-google/#google_vignette
  (define count-explicit
    (lambda (lst)
      (let ([ bad-words (extract-words (file->string "explicitwords.txt"))])
        (tally (section list-contains? bad-words <>) lst))))
  
  ;;; (percent-explicit-words lst) -> integer?
  ;;; lst: list?
  ;;; finds the percentage of words that are explicit in a list
  (define percent-explicit-words
    (lambda (song)
      (exact->inexact (- 100 (* (/ (- (total-words song) (count-explicit song))
              (total-words song))
            100)))))
  
  ;;; (%explicit lst) -> string?
  ;;; lst: list?
  ;;; categorizes the explicitness of a song based on the amount of explicit words it contains
  (define %explicit
    (lambda (lst)
      (if (> (percent-explicit-words lst) 10)
        "EXPLICIT: PARENTAL ADVISORY"
        'NOT-EXPLICIT)))
  
  
  #| Part 3 |#
  ;;; This part of our project creates a visualization of either explicit or non-explicit
  ;;; for the songs we categorized in previous code
  
  ;;; explicit-circle
  ;;; inspired by https://hotemoji.com/18-emoji.html
  (define explicit-circle
    (above (overlay (line 70 70 (make-pen "red" 6 "solid" "round" "round"))
        (text "18" 65 "white")
        (circle 50 'solid "black")
        (circle 55 'solid "red"))
      (above (text "Please use headphones" 25 "red")
        (text "while listening" 25 "red"))))
  
  ;;; normal-circle
  ;;; inspired by https://hotemoji.com/18-emoji.html
  (define normal-circle
    (above (overlay (text "18" 65 "white")
        (circle 50 'solid "lightblue")
        (circle 55 'solid (make-color 51 165 50)))
      (text "Headphones optional" 25 (make-color 51 165 50))))
  
  ;;; (rectangle color) -> image?
  ;;; color: string?
  ;;; produces a rectangle of the given color
  (define rectangle-img
    (lambda (color)
      (rectangle 80 30 'solid color)))
  
  ;;; (normal-bar) -> image?
  ;;; produces an image bar of normal explicitness 
  (define normal-bar
    (above (text "Explicitness Meter" 25 'darkgreen)
      (beside (rectangle-img 'lightyellow) (rectangle-img 'palegreen)
        (rectangle-img 'lightgreen) (rectangle-img'green)
        (rectangle-img 'darkgreen))))
  
  ;;; (explicit-bar) -> image?
  ;;; produces an image bar of high explicitness 
  (define explicit-bar
    (above (text "Explicitness Meter" 25 'darkred)
      (beside (rectangle-img 'lightyellow) (rectangle-img 'orangered)
        (rectangle-img 'red) (rectangle-img 'crimson)
        (rectangle-img 'firebrick))))
  
  
  #| Part 4 |#
  ;;; This image puts all our parts together and gives the explicitness rating of a song in an image
  ;;; This is the code you should input in the interactions pane
  
  ;;; (explicitness-rating song) -> image?
  ;;; song: file?
  ;;; produces an image representing the explicitness of a song lyrics file
  (define explicitness-rating
    (lambda (song)
      (if (equal? (%explicit (extract-text-html song)) "EXPLICIT: PARENTAL ADVISORY")
        (above (text (string-append song ":") 15 "black")
          (beside explicit-circle explicit-bar)
          (text (string-append "Percentage of explicit words: "
              (number->string (percent-explicit-words (extract-text-html song)))) 15 "black"))
        (above (text (string-append song ":") 15 "black")
          (beside normal-circle normal-bar)
          (text (string-append "Percentage of explicit words: "
              (number->string (percent-explicit-words (extract-text-html song)))) 15 "black")))))
  
  
  #| Sample Runs of Our Code |#
  ;;; Here we show our project works with two examples -> you should input these in the interactions pane
  
  ;;; explicit-song: 
  ; input: (explicitness-rating "explicit-song.html")
  ; output: explicit visualization
  
  ;;; non-explicit song: 
  ; input: (explicitness-rating "shape-of-you.html")
  ; output: normal visualization
  
  