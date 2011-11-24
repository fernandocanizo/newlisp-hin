; Creation Date: 17 Nov 2011
; Author: Fernando Canizo (aka conan) - http://conan.muriandre.com/

; just a quick test to see how the function should look so I can program the macro which will create them

(define (html-tag (innerCode ""))
	(append "<html"
		; as a side effect, if innerCode is nil, the 'if' properly resolves so the 'append' don't complain
		(if (> (length innerCode) 0)
			(append " " innerCode ">") ">")
		(let (betweenTagsContent "")
			(doargs (arg)
				(set 'betweenTagsContent
					(append betweenTagsContent
						(if (nil? arg) "" arg))))
			betweenTagsContent)
		"</html>"))


# test it

(println (html-tag))

(println (html-tag "just inner"))

(println (html-tag "inner" "one between"))

(println (html-tag "inner" "one" "two" "three"))

(println (html-tag "inner" "one" nil "three"))
