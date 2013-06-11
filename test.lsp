; Creation Date: 15 Nov 2011
; Author: Fernando Canizo (aka conan) - http://conan.muriandre.com/

; test hin.lsp

(load "hin.lsp")

(context hin)

(setq buffer:buffer "")

(if (true? (catch
	(html lang= "en" KEEP_OPEN)
	'start))

	(setq buffer:buffer (append buffer start)))


(if (true? (catch
	(head
		(meta charset= "utf-8")
		(title "Test"))
		'middle))

	(setq buffer:buffer (append buffer middle)))


(if (true? (catch
	(html
		(body
			(h1 class= "foo" id= "title" "Testing")
			(time datetime= "2011-10-29" pubdate. "29 de octubre")
			(p class= "story" id= "bar" "Lore ipsum...")) CONTINUE)
	'end))

	(setq buffer:buffer (append buffer end)))


(println buffer:buffer)
(exit)
