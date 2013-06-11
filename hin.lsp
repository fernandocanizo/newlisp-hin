#!/usr/bin/env newlisp
;; Creation Date: Wed, 08 May 2013 09:41:37 -0300
;; @author Fernando Canizo (aka conan) - http://conan.muriandre.com/
;; @module hin
;; @description DSL for HTML5
;; @version 2013.05.13

; TODO properly indented output would be nice

(context 'hin)

(constant 'KEEP_OPEN "KEEP_OPEN") ; given as final parameter allows element to be open ended, so it can receive more content
(constant 'CONTINUE "CONTINUE") ; given as final, or second to final parameter (if CONTINUE is also given) tells element is being continued from a previous snippet

(constant 'doctype "<!DOCTYPE html>\n")

(define (comment str-comment)
	(if (< (length str-comment) 80)
		(string "\n<-- " str-comment " -->\n")
		; else
		(string "\n<--\n" str-comment "\n-->\n")))


(setq iso-639-1-language-codes '(
	"aa" "ab" "ae" "af" "ak" "am" "an" "ar" "as" "av" "ay" "az"
	"ba" "be" "bg" "bh" "bi" "bm" "bn" "bo" "br" "bs"
	"ca" "ce" "ch" "co" "cr" "cs" "cu" "cv" "cy"
	"da" "de" "dv" "dz"
	"ee" "el" "en" "eo" "es" "et" "eu"
	"fa" "ff" "fi" "fj" "fo" "fr" "fy"
	"ga" "gd" "gl" "gn" "gu" "gv"
	"ha" "he" "hi" "ho" "hr" "ht" "hu" "hy" "hz"
	"ia" "id" "ie" "ig" "ii" "ik" "io" "is" "it" "iu"
	"ja" "jv"
	"ka" "kg" "ki" "kj" "kk" "kl" "km" "kn" "ko" "kr" "ks" "ku" "kv" "kw" "ky"
	"la" "lb" "lg" "li" "ln" "lo" "lt" "lu" "lv"
	"mg" "mh" "mi" "mk" "ml" "mn" "mr" "ms" "mt" "my"
	"na" "nb" "nd" "ne" "ng" "nl" "nn" "no" "nr" "nv" "ny"
	"oc" "oj" "om" "or" "os"
	"pa" "pi" "pl" "ps" "pt"
	"qu"
	"rm" "rn" "ro" "ru" "rw"
	"sa" "sc" "sd" "se" "sg" "si" "sk" "sl" "sm" "sn" "so" "sq" "sr" "ss" "st" "su" "sv" "sw"
	"ta" "te" "tg" "th" "ti" "tk" "tl" "tn" "to" "tr" "ts" "tt" "tw" "ty"
	"ug" "uk" "ur" "uz"
	"ve" "vi" "vo"
	"wa" "wo"
	"xh"
	"yi" "yo"
	"za" "zh" "zu"
	))


;; Note: all scripting attributes for events like "on<something>" are not included.
;; Use unobstrusive javascript to act upon those events. Mixing them in HTML is deprecated.
;; Format can be:
;; "<attribute>" ; just the attribute name
;; ("<attribute>" ("allowed-value-1" [...])) ; or a list with the attribute and a sublist with allowed values
(setq globalAttributes '(
	"accesskey"
	"class"
	("contenteditable"
		("false" "true"))
	"contextmenu"
	;; Note: data-* attributes are special in that user can make any name they want,
	;; the attribute just need to start with "data-".
	;; So the function 'data- will behave differently:
	;; it will use the first parameter as the finishing string for the attribute
	;; Example: (data- "id" 123) ; will give the string: data-id="123"
	"data-"
	("dir"
		("ltr" "rtl"))
	("draggable"
		("false" "true"))
	"dropzone"
	"hidden"
	"id"
	"inert"
	"itemid"
	"itemprop"
	"itemref"
	"itemscope"
	"itemtype"
	"lang" ; TODO search language codes and include in here as valid options (Use ISO 639-1 Codes)
	("spellcheck" ; implementation isn't defined by the specification, so browsers don't know/agree on what to do.
		("false" "true"))
	"style"
	"tabindex" ; TODO must be a number
	"title"
	("translate"
		("no" "yes"))
	))


;; Elements with no closing tag. All properties are coded using attributes
;; Taken from http://www.html-5.com/tags/html-tag-list.html "HTML Elements - Content Model: Void"
(setq standaloneTags '(
	"area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen" "link" "meta" "param" "source" "wbr"
	))


;; Note: boolean attributes symbols are defined without the equal sign at the end,
;; so interpreter will bark if one tries to use one with a value
(setq booleanAttributes '( ; taken from w3.org, with some minor additions by conan
	"allowfullscreen" "async" "autofocus" "autoplay" "checked" "controls"
	"default" "defer" "disabled" "formnovalidate" "hidden" "inert" "ismap"
	"itemscope" "loop" "multiple" "muted" "novalidate" "open" "readonly"
	"required" "reversed" "scoped" "seamless" "selected" "typemustmatch"
	"usemap" "pubdate"
	))
; other booleans that're handled different:
; autocomplete "on" / "off"
; contenteditable "true" / "false"
; draggable "true" / "false"
; spellcheck "true" / "false"
; translate "yes" / "no"


; TODO sort this once I finish with the book
;; Format can be one of these three:
;; "<tag>" ; if no special attributes
;; ("<tag>" (["attr1" [...]])) ; the tag and a list of attributes
;; ("<tag>" ("attr1" ("allowed-value-1" [...]))) ; the tag, a list of attributes, and some attributes with a list of allowed values
(setq elements '(
	"address"
	("base" ; TODO there can be only one
		("href" "target"))
	"body" ; TODO only one body is allowed per html document, it would be nice to check that
	"div"
	"h1"
	"h2"
	"h3"
	"h4"
	"h5"
	"h6"
	"hr"
	("html" ; TODO would be nice to set default lang="en", also there can be only one per document
		("manifest"))
	; TODO head may contain only these elements: base, link, meta, script, style, and title.
	; TODO is required and there can be only one.
	; TODO except for style, the other elements allowed in here can only appear inside head
	"head"
	("link" ; TODO hreflang takes the same values as lang
		("href" "hreflang" "media" "rel" "type" "sizes"))
	("meta"
		("content"
		("http-equiv" ; content-language and content-type are discouraged by HTML5, so I leave it out
			("default-style" "refresh"))
		("name" ; Not exhaustive, only HTML5 ones. There're others: http://wiki.whatwg.org/wiki/MetaExtensions
			("author" "description" "application-name" "generator"))
		("charset" ("utf-8")))) ; Not exhaustive, but will use this as default
	("style"
		("media" "title"
		("type" ("text/css"))
		"scoped"))
	"title" ; TODO there can be only one and goes inside <head>
	"dd"
	"dl"
	"dt"
	("li"
		("value")) ; value must be an integer and it's use is only valid when li's inside an ol element
	("ol" ("start" "reversed")) ; start must be an integer
	"ul"
	("a" ("href" "hreflang" "rel" "target" "type" "media" "ping"))
	"abbr"
	;; not officially deprecated by HTML5, but shouldn't be used because it's presentational not semantic
	;; "b"
	"bdo"
	("blockquote" ("cite"))
	"br"
	"cite"
	"code"
	("del" ("cite" "datetime"))
	"dfn"
	"em"
	;; "i" isn't officially deprecated by HTML5, but shouldn't be used because it's presentational not semantic
 	;; "i"
	("ins" ("cite" "datetime"))
	"kbd"
	"p"
	"pre"
	("q" ("cite"))
	"samp"
	"small" ; unlike "big", this one isn't deprecated, HTML5's redefined it's semantics
	"strong"
	"sub"
	"sup"
	"var"
	("area" ("alt" "coords" "download" "href" "shape" "target" "hreflang" "media" "ping" "rel" "type"))
	("img" ("alt" "height" "ismap" "src" "usemap" "width"))
	("map" ("name"))
	("object" ("data" "height" "name" "type" "usemap" "width" "form"))
	("param" ("name" "value"))
	("button"
		("disabled" "name"
		("type" ("button" "submit" "reset"))
		"value" "autofocus"
		"form" "formaction" "formenctype" "formmethod" "formnovalidate" "formtarget"))
	("fieldset"
		("disabled" "form" "name"))
	("form"
		("accept-charset" "action" "enctype"
		("method" ("post" "get" "delete" "put"))
		"name"
		("autocomplete" ("off" "on"))
		"novalidate" "target"))
		; target can be "_blank", "_parent", "_self", "_top", or a value you specify that could match the name of an iframe.
		; since allowed values aren't strict, I'll leave it like that for now
		; TODO see how can avoid the strict checking for this attribute
	("input"
		("alt" "checked" "disabled" "ismap"
		"maxlength" ; TODO must be an integer, how can I check it?
		"name" "readonly" "size" "src"
		("type" ("button" "checkbox" "file" "hidden" "image" "password" "radio" "reset" "submit" "text"
			"color" "date" "datetime" "datetime-local" "email" "month" "number" "range" "search" "tel" "time" "url" "week")) ; html5 new types
		"usemap" "value"
		("autocomplete" ("off" "on")) ; put it off for sensitive data, like credit card information
		"autofocus" "form" "formaction" "formenctype" "formmethod" "formnovalidate" "formtarget" "height"
		"list" "max" "min" "multiple" "pattern" "placeholder" "required" "step" "width"))
	("label"
		("for" "form"))
	"legend"
	("optgroup"
		("disabled" "label"))
	("option"
		("disabled" "label" "selected" "value"))
	("select"
		("disabled" "name" "multiple" "size" "autofocus" "form"))
	("textarea"
		("cols" "disabled" "name" "readonly" "rows" "autofocus" "form" "maxlength" "placeholder" "required"
		("wrap" ("hard" "soft"))))
	"caption"
	("col"
		("span"))
	("colgroup"
		("span"))
	("table"
		("summary"))
	"tbody"
	("td"
		("colspan" "headers" "rowspan"))
	"tfoot"
	("th"
		("colspan" "headers" "rowspan" "scope"))
	"thead"
	"tr"
	"noscript"
	("script"
		("charset" "defer" "type" "src" "async"))
	; Note: won't include frame, frameset and noframes. They're all obsolete in HTML5
	("iframe"
		("height" "name" "src" "width"
		("sandbox" ("allow-same-origin" "allow-top-navigation" "allow-forms" "allow-scripts"))
		"seamless" "srcdoc"))
	"header"
	"nav"
	"article"
	"aside"
	"section"
	"footer"
	"hgroup"
	"figure"
	"figcaption"
	"mark"
	("meter"
		("form" "high" "low" "min" "max" "optimum" "value"))
	("progress"
		("form" "max" "value"))
	"rp"
	"rt"
	"ruby"
	("time"
		("datetime" "pubdate"))
	"wbr"
	("audio"
		("autoplay" "controls" "loop" "preload" "src"))
	("canvas"
		("height" "width"))
	("embed"
		("height" "src" "type" "width"))
	("source"
		("media" "source" "type"))
	("video"
		("autoplay" "controls" "height" "loop" "poster"
		("preload" ("none" "metadata" "auto"))
		"src" "width"))
	"datalist"
	("keygen"
		("autofocus" "challenge" "disabled" "form" "keytype" "name"))
	("output"
		("for" "form" "name"))
	("command"
		("checked" "disabled" "icon" "label" "radiogroup"
		("type" ("checkbox" "command" "radio"))))
	("details"
		("open"))
	("menu"
		("label"
		("type" ("context" "toolbar"))))
	"summary"
	))


;; compose proper symbols for every possible attribute:
;; boolean attributes get names with a postfixed dot, example: hidden.
;; the rest get names with postfixed equal sign, example: class=
(define (build-attribute-symbols lst-attributes)
	(dolist (item lst-attributes)
		(if (list? item)
			(let (attr (item 0))
				(constant (sym (string attr "=")) (string attr "=")))
			; else item is an attribute
			(if (find item booleanAttributes)
				(constant (sym (string item ".")) (string item "."))
				; else
				(constant (sym (string item "=")) (string item "="))))))


;; append globalAttributes to every element defined in elements and build proper symbols
(dolist (e (copy elements))
	(if (list? e)
		(begin
			; build attribute symbols
			(build-attribute-symbols (e 1))
			; append global attributes
			(replace e elements (list (e 0) (append globalAttributes (e 1)))))
		; else
		(replace e elements (list e globalAttributes))))


;; build symbols for global attributes
(build-attribute-symbols globalAttributes)


(define (compare-list-or-atom attr lst-or-atom)
	(if (list? lst-or-atom)
		(= attr (lst-or-atom 0))
		; else
		(= attr lst-or-atom)))


(define (check-attributes element attribute lst-valid-attributes)
	(find attribute lst-valid-attributes compare-list-or-atom))


; macro which builds every element function
(define-macro (hin:hin lst-element-attributes)
	(let (element (nth 0 (eval lst-element-attributes)))

		;; Note: I can use constant to overwrite already defined symbols
		;; but for the time being I prefer to just throw an error
		;; will decide what to do when a conflict appears
		(if (protected? (sym element))
			(throw (string element " is already defined.")))

		(eval (list 'define (list (sym element))
			(list 'letn (list
				(list 'standAlone (list 'find element 'standaloneTags))
				(list 'validAttributes (list 'quote (nth 1 (eval lst-element-attributes))))
				(list 'innerCode (list 'string "<" element)) ; holds attributes and values
				(list 'betweenTags "") ; holds content
				;; if you need to provide both CONTINUE and KEEP_OPEN, they must be ordered like this:
				;; (... CONTINUE KEEP_OPEN)
				(list 'keepOpen (list 'and (list '>= (list 'length '$args) 1) (list '= "KEEP_OPEN" (list '$args -1))))
				(list 'continue (list 'or
					(list 'and (list '>= (list 'length '$args) 1) (list '= "CONTINUE" (list '$args -1)))
					(list 'and (list '>= (list 'length '$args) 2) (list '= "CONTINUE" (list '$args -2)))))
				; can't touch $idx, so to consume arbitrarily the arguments, I need to copy argument list and manage it myself
				(list 'myArgs
					(list 'if (list 'and (list 'true? 'keepOpen) (list 'true? 'continue))
						(list 0 -2 '$args)
						; else
						(list 'if (list 'or (list 'true? 'keepOpen) (list 'true? 'continue))
							(list 0 -1 '$args)
							; else
							'$args))))

				(list 'do-until (list 'null? 'myArgs)
					(list 'let (list (list 'currentArg (list 'pop 'myArgs)))
						(list 'if (list 'nil? 'continue) ; skip innerCode building if this is a continuation
							; currentArg comes evaluated, so gotta use sym, quoting is of no use
							(list 'if (list 'protected? (list 'sym 'currentArg))
								; check if it's valid for this element
								(list 'let (list 'attrIndex (list 'check-attributes element (list 0 -1 'currentArg) 'validAttributes))
									(list 'if (list 'nil? 'attrIndex)
										; we want valid HTML5, stop here Mr.
										(list 'throw (string 'currentArg " isn't a valid attribute for " element "."))
										; else check which kind of attribute (boolean or key=value)
										(list 'if (list 'ends-with 'currentArg "=")
											(list 'let (list
												(list 'attrValue (list 'pop 'myArgs))
												(list 'attrItem (list 'validAttributes 'attrIndex)))

												(list 'if (list 'list? 'attrItem) ; attribute has allowed values, check it's valid
													(list 'if (list 'find 'attrValue (list 'attrItem 1))
														(list 'setq 'innerCode (list 'string 'innerCode " " 'currentArg "\"" 'attrValue "\""))
														; else invalid value, stop here Mrs.
														(list 'throw (string 'currentArg " doesn't accepts " 'attrValue " as a valid value. $args:\n" '$args)))
												; else accept any value and append everything into innerCode
												(list 'setq 'innerCode (list 'string 'innerCode " " 'currentArg "\"" 'attrValue "\""))))
											; else it's a boolean attribute, strip last character (dot)
											(list 'setq 'innerCode (list 'string 'innerCode " " (list 0 -1 'currentArg))))))

									; TODO this code repetition might need refactoring
									; else it's just content
									(list 'if (list 'true? 'standAlone) ; there shouldn't be content
										(list 'throw (string element " is a standalone tag, it doesn't accepts content. You tried to put:\n" 'betweenTags))
										; else
										(list 'setq 'betweenTags (list 'string 'betweenTags 'currentArg))))
								; else is continued content
								(list 'if (list 'true? 'standAlone) ; there shouldn't be content
									(list 'throw (string element " is a standalone tag, it doesn't accepts content. You tried to put:\n" 'betweenTags))
									; else
									(list 'setq 'betweenTags (list 'string 'betweenTags 'currentArg))))

					(list 'if (list 'true? 'keepOpen)
						(list 'string (list 'if (list 'nil? 'continue) (list 'string 'innerCode ">") "") 'betweenTags)
						; else close tag
						(list 'string (list 'if (list 'nil? 'continue) (list 'string 'innerCode ">") "") 'betweenTags
							(list 'if (list 'true? 'standAlone)
								""
								(list 'string "</" element ">")))))))))))


;; create the element functions
(dolist (e elements)
	(hin e))

(context MAIN)
