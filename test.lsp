; Creation Date: 15 Nov 2011
; Author: Fernando Canizo (aka conan) - http://conan.muriandre.com/

; test hin.lsp

(load "./hin.lsp")

(context 'hin) ; to avoid prepending all with "hin:"

(set 'result
	(html "lang=en"
		(head ""
			(title "Testing this awesome idea"))
		(body ""
			(h1 "id=title" "Awesome!")
			(time "datetime=2011-10-29 pubdate" "29 de octubre") ; time clashes with newlisp time, but we're in context hin now
			(p "class=story" "Lore ipsum..."))))

(println result)
(exit)
