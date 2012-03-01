; Creation Date: 15 Nov 2011
; Author: Fernando Canizo (aka conan) - http://conan.muriandre.com/

(context 'hin)

(constant 'doctype "<!DOCTYPE html>\n")

(define (comment aComment)
	(if (< (length aComment) 80)
		(join (list "\n<-- " aComment " -->\n"))
		; else
		(join (list "\n<--\n" aComment "\n-->\n"))))


(define (quote-inner-code innerCode)
	; TODO: instead of 2048 and 512 should use PCRE_UTF8 and PCRE_UNGREEDY, but newlisp says to me:
	; ERR: value expected in function replace : PCRE_UTF8
	; find out why is not taking the constant
	(replace {=(\S+)(\s|$)} innerCode (append "=\"" $1 "\"" $2) (| 2048 512)))


; magic happens here
(define-macro (hin:hin tagName)
	(eval (list 'define (list (sym (if (symbol? tagName) (eval tagName) tagName)) (list 'innerCode ""))
		(list 'append "<" (eval tagName)
			'(if (> (length innerCode) 0) (append " " (quote-inner-code innerCode) ">") ">")
			(list 'let '(betweenTagsContent "")
				(list 'doargs '(arg)
					(list 'set ''betweenTagsContent
						(list 'append 'betweenTagsContent
							'(if (nil? arg) "" arg))))
				'betweenTagsContent)
			"</" (eval tagName) ">"))))


(setq htmlTags '(
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

(setq htmlAttributes '(
	"accept-charset" "accesskey" "action" "alt" "async" "autocomplete" "autofocus" "autoplay"
	"background"
	"challenge" "charset" "checked" "cite" "class" "classid" "codebase" "cols" "colspan" "content" "controls"
	"data" "datetime" "defer" "disabled"
	"enctype"
	"for" "form" "form" "formaction" "formenctype" "formmethod" "formnovalidate" "formt" "formtarget"
	"headers" "height" "high" "href" "hreflang" "http-equiv"
	"id" "ismap"
	"keytype"
	"label" "lang" "list" "loop" "low"
	"manifest" "max" "maxlength" "media" "method" "min" "multiple"
	"name" "novalidate"
	"open" "optimum"
	"pattern" "placeholder" "poster" "preload" "pubdate"
	"readonly" "rel" "required" "reversed" "rows" "rowspan"
	"sandbox" "scope" "scoped" "seamless" "selected" "shape" "size" "sizes" "span" "src" "start style" "step" "style" "styles" "summary" "syle"
	"tabindex" "target" "text" "title" "type"
	"usermap"
	"value"
	"width" "wrap"))

; export each attribute as a constant symbol
(dolist (a htmlAttributes)
	(constant (sym a) a))

; create the functions
(dolist (tag htmlTags)
	(hin tag))
