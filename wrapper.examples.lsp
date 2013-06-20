#!/usr/bin/env newlisp
;; Creation Date: Thu, 20 Jun 2013 09:36:09 -0300
;; @author Fernando Canizo (aka conan) - http://conan.muriandre.com/
;; @description Show use of context-wrappers macros


(load "hin.lsp")
(load "wrappers.lsp")

(context 'WrapperExample)

(define (header)
	(wrap-in-context 'hin
		(html lang= "es"
			(head
				(title "Some title")))))


;; call it from other context, like MAIN for example
(context MAIN)

(println (WrapperExample:header))


;; now that wont work if you want to use variables, you must use
;; replace-this-context instead
(context 'AnotherWrapper)

(setq db:db '(
	("Title" "Thunderstruck")
	("Artist" "AC/DC")
	("Album"  "The Razors Edge")
	("Year" 1990)
	("Genre" "Hard Rock")))

(define (header)
	(replace-this-context 'AnotherWrapper 'hin
		(html lang= "es"
			(head
				(title ((assoc "Title" db:db) 1))))))


;; now we call it
(context MAIN)

(println (AnotherWrapper:header))
