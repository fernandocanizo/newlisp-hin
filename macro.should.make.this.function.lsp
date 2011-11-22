; Creation Date: 17 Nov 2011
; Author: Fernando Canizo (aka conan) - http://conan.muriandre.com/

; just a quick test to see how the function should look so I can program the macro which will create them

(define (html-tag innerCode betweenTagsCode)
	(append "<html" (if (> (length innerCode) 0) (append " " innerCode ">") ">") betweenTagsCode "</html>"))
