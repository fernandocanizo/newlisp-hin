#!/usr/bin/env newlisp
;; Creation Date: Wed, 08 May 2013 09:41:37 -0300
;; @author Fernando Canizo (aka conan) - http://conan.muriandre.com/
;; @module hin
;; @description DSL for HTML tags
;; @version 2013.05.13

; TODO properly indented output would be nice

(context 'hin)

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
; TODO maybe I should build just one array with tags and attributes, need to solve the issue with automatically appending global attributes
;; Format can be one of these three:
;; "<tag>" ; if no special attributes
;; ("<tag>" (["attr1" [...]])) ; the tag and a list of attributes
;; ("<tag>" ("attr1" ("allowed-value-1" [...]))) ; the tag, a list of attributes, and some attributes with a list of allowed values
(setq tags '(
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
	"a" ; DEL
	("a" ("href" "hreflang" "rel" "target" "type" "media" "ping"))
	"abbr"
	;; not officially deprecated by HTML5, but shouldn't be used because it's presentational not semantic
	;; "b"
	"bdo"
	"blockquote" ; DEL
	("blockquote" ("cite"))
	"br"
	"cite"
	"code"
	"del" ; DEL
	("del" ("cite" "datetime"))
	"dfn"
	"em"
	;; "i" isn't officially deprecated by HTML5, but shouldn't be used because it's presentational not semantic
 	;; "i"
	"ins" ; DEL
	("ins" ("cite" "datetime"))
	"kbd"
	"p"
	"pre"
	"q" ; DEL
	("q" ("cite"))
	"samp"
	"small" ; unlike "big", this one isn't deprecated, HTML5's redefined it's semantics
	"strong"
	"sub"
	"sup"
	"var"
	"area" ; DEL
	("area" ("alt" "coords" "download" "href" "shape" "target" "hreflang" "media" "ping" "rel" "type"))
	"img" ; DEL
	("img" ("alt" "height" "ismap" "src" "usemap" "width"))
	"map" ; DEL
	("map" ("name"))
	"object" ; DEL
	("object" ("data" "height" "name" "type" "usemap" "width" "form"))
	"param" ; DEL
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


;; append globalAttributes to every element defined in tags
(dolist (element (copy tags))
	(if (list? element)
		(replace element tags (list (element 0) (append globalAttributes (element 1))))
		; else
		(replace element tags (list element globalAttributes))))


; TODO remove?
; (setq allAttributes (copy globalAttributes)) ; start with globalAttributes...
; (dolist (a specialAttributes) ; ... then add the rest
; 	(dolist (b (last a))
; 		(if (not (member b allAttributes))
; 			(push b hin:allAttributes -1))))

; TODO if allAttributes is not needed elsewhere, then build constants in one step instead of this
; of maybe I can use this one to separate booleans from key/value attributes, which is a planned feature
; (dolist (a allAttributes))

; (define (build-tag-attribute str-tag lst-attributes)
; 	(list str-tag lst-attributes))

; (dolist (t tags)
; 	(let (auxSA (assoc t specialAttributes))
; 		(if (true? auxSA)
; 			(push (build-tag-attribute t (flat (list globalAttributes (last auxSA)))) tags+attributes)
; 			; else put only global attributes
; 			(push (build-tag-attribute t globalAttributes) tags+attributes))))


; macro which builds every htmltag-function
; TODO maybe I don't need to test tagName with symbol, maybe just have to use the evaluated result
; TODO modify macro to build a function which test if attr is allowed for tag
; TODO don't consume another argument if attribute is boolean
; (define-macro (hin:hin tagName)
; 	(string (list 'define (list (sym (if (symbol? tagName) (string "." (eval tagName)) (string "." tagName))))
; 		(list 'let (list
; 			(list 'innerCode (list 'string "<" (eval tagName))) ; holds attributes and values
; 			(list 'betweenTags "") ; holds content
; 			(list 'myArgs '$args)) ; can't touch $idx, so to consume arbitrarily the arguments, I need to copy argument list and manage it myself
; 
; 			(list 'do-until (list 'null? 'myArgs)
; 				(list 'let (list (list 'currentArg (list 'pop 'myArgs)))
; 					(list 'if (list 'protected? (list 'sym 'currentArg))
; 						; get next arg (which is value for attribute) and append everything into innerCode
; 						(list 'setq 'innerCode (list 'string 'innerCode " " 'currentArg (list 'pop 'myArgs)))
; 						; else
; 						(list 'setq 'betweenTags (list 'string 'betweenTags 'currentArg)))))
; 
; 			(list 'string 'innerCode ">" 'betweenTags "</" (eval tagName) ">")))))


; create the htmltag-functions
; (dolist (tag tags)
; 	(hin tag))
