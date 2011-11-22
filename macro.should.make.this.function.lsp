; Creation Date: 17 Nov 2011
; Author: Fernando Canizo (aka conan) - http://conan.muriandre.com/

; just a quick test to see how the function should look so I can program the macro which will create them

(define (html-tag innerCode betweenTagsCode)
	; as a side effect, if innerCode is nil, the 'if' properly resolves so the 'append' don't complain
	(append "<html" (if (> (length innerCode) 0) (append " " innerCode ">") ">") (if (nil? betweenTagsCode) "" betweenTagsCode) "</html>"))
