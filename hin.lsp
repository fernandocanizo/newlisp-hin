; Creation Date: 15 Nov 2011
; Author: Fernando Canizo (aka conan) - http://conan.muriandre.com/

(context 'hin)

(constant 'doctype "<!DOCTYPE html>\n")

(define (comment aComment)
	(if (< (length aComment) 80)
		(join (list "\n<-- " aComment " -->\n"))
		; else
		(join (list "\n<--\n" aComment "\n-->\n"))))

; magic happens here
(define-macro (hin:hin tagName)
	(eval (list 'define (list (sym (if (symbol? tagName) (eval tagName) tagName)) 'innerCode 'betweenTagsContent)
		(list 'append "<" (eval tagName)
			'(if (> (length innerCode) 0) (append " " innerCode ">") ">")
			'(if (nil? betweenTagsContent) "" betweenTagsContent)
			"</" (eval tagName) ">"))))



(set 'htmlTags '(
	"a" "abbr" "address" "area" "article" "aside" "audio" 
	"b" "base" "bdo" "blockquote" "body" "br" "button"
	"canvas" "caption" "cite" "code" "col" "colgroup" "command" 
	"datalist" "dd" "del" "details" "dfn" "div" "dl" "dt" 
	"em" "embed" "eventsource" 
	"fieldset" "figcaption" "figure" "footer" "form" 
	"h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hgroup" "hr" "html" 
	"i" "iframe" "img" "input" "ins" 
	"kbd" "keygen" 
	"label" "legend" "li" "link" 
	"mark" "map" "menu" "meta" "meter" 
	"nav" "noscript" 
	"object" "ol" "optgroup" "option" "output" 
	"p" "param" "pre" "progress" 
	"q" 
	"ruby" "rp" "rt" 
	"samp" "script" "section" "select" "small" "source" "span" "strong" "style" "sub" "summary" "sup" 
	"table" "tbody" "td" "textarea" "tfoot" "th" "thead" "time" "title" "tr" 
	"ul" 
	"var" "video" 
	"wbr"))


; create the functions
(dolist (tag htmlTags)
	(hin tag))
