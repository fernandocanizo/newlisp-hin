; Creation Date: 15 Nov 2011
; Author: Fernando Canizo (aka conan) - http://conan.muriandre.com/

; test hin.lsp

(load "hin.lsp")

(context 'hin) ; to avoid prepending all with "hin:"

(setq result
	(.html lang= "en"
		(.head
			(.title "Test"))
		(.body
			(.h1 class= "foo" id= "title" "Testing")
			(.time datetime= "2011-10-29" pubdate= "29 de octubre")
			(.p class= "story" id= "bar" "Lore ipsum..."))))

(println result)
(exit)
