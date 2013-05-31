#!/usr/bin/env newlisp
;; Creation Date: Wed, 08 May 2013 09:41:37 -0300
;; @author Fernando Canizo (aka conan) - http://conan.muriandre.com/
;; @module hin
;; @description DSL for HTML tags
;; @version 2013.05.13

; TODO properly indent output would be nice

(context 'hin)

(constant 'doctype "<!DOCTYPE html>\n")

(define (comment str-comment)
	(if (< (length str-comment) 80)
		(string "\n<-- " str-comment " -->\n")
		; else
		(string "\n<--\n" str-comment "\n-->\n")))

;; Note: all scripting attributes "on<something>" are not included. Use unobstrusive javascript to act upon those events.

(setq globalAttributes '(
	"accesskey"
	"class"
	"contenteditable"
	"contextmenu"
	"dir"
	"draggable"
	"dropzone"
	"hidden"
	"id"
	"inert"
	"itemid"
	"itemprop"
	"itemref"
	"itemscope"
	"itemtype"
	"lang"
	"spellcheck" ; implementation isn't defined by the specification, so browsers don't know/agree on what to do.
	"style"
	"tabindex"
	"title"
	"translate"
	; TODO I left out data-* because they are created by user, don't know how to implement them yet
	; I left out event-kind attributes like onload, since it's use is deprecated
	))

(setq specialAttributes '(
	("html" ("manifest"))
	("base" ("href" "target"))
	("link" ("href" "hreflang" "media" "rel" "type" "sizes"))
	("meta" ("content" "http-equiv" "name" "charset"))
	("style" ("media" "title" "type" "scoped"))
	("li" ("value")) ; value must be an integer and it's only valid when li's inside an ol element
	("ol" ("start" "reversed")) ; start must be an integer
	("a" ("href" "hreflang" "rel" "target" "type" "media" "ping"))
	("blockquote" ("cite"))
	("del" ("cite" "datetime"))
	("ins" ("cite" "datetime"))
	("q" ("cite"))
	("area" ("alt" "coords" "download" "href" "shape" "target" "hreflang" "media" "ping" "rel" "type"))
	("img" ("alt" "height" "ismap" "src" "usemap" "width"))
	("map" ("name"))
	("object" ("data" "height" "name" "type" "usemap" "width" "form"))
	("param" ("name" "value"))
	))

; TODO sort this once I finish with the book
; TODO maybe I should build just one array with tags and attributes, need to solve the issue with automatically appending global attributes
;; Format:
;; (<tag> (["attr1" [...]])) or
;; (<tag> (attr1 ("allowed-value-1" [...])))
(setq tags '(
	"address"
	"body" ; TODO only one body is allowed per html document, it would be nice to check that
	"div"
	"h1"
	"h2"
	"h3"
	"h4"
	"h5"
	"h6"
	"hr"
	"html" ; TODO would be nice to set default lang="en", also there can be only one per document
	"base" ; TODO there can be only one
	"head" ; TODO head may contain only these elements: base, link, meta, script, style, and title. Required. Also base, link, meta, and title can only appear inside <head>
	"link"
	"meta"
	"style"
	"title" ; TODO there can be only one and goes inside <head>
	"dd"
	"dl"
	"dt"
	"li"
	"ol"
	"ul"
	"a"
	"abbr"
	"b" ; not officially deprecated by HTML5, but shouldn't be used because it's presentational not semantic
	"bdo"
	"blockquote"
	"br"
	"cite"
	"code"
	"del"
	"dfn"
	"em"
	"i" ; not officially deprecated by HTML5, but shouldn't be used because it's presentational not semantic
	"ins"
	"kbd"
	"p"
	"pre"
	"q"
	"samp"
	"small" ; unlike "big", this one isn't deprecated, HTML5's redefined it's semantics
	"strong"
	"sub"
	"sup"
	"var"
	"area"
	"img"
	"map"
	"object"
	"param"
	; TODO starting from here I decided to build an unique list with all the stuff and append later global attributes
	; so I have to work up from here including what I was writing separately in specialAttributes symbol
	; I I go with the book, then this started on page 157, Chapter 7
	; so this should change it's name to tags+attributes
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
		"name" "autocomplete" "novalidate" "target"))
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
		("autocomplete" ("on" "off")) ; put it off for sensitive data, like credit card information
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

;; Note: boolean attributes symbols are defined without the equal sign at the end,
;; so interpreter will bark if one tries to use one with a value
(setq boolean-attributes '( ; taken from w3.org, with some minor additions by conan
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
; spelcheck "true" / "false"
; translate "yes" / "no"

(setq allAttributes (copy globalAttributes)) ; start with globalAttributes...
(dolist (a specialAttributes) ; ... then add the rest
	(dolist (b (last a))
		(if (not (member b allAttributes))
			(push b hin:allAttributes -1))))

; TODO if allAttributes is not needed elsewhere, then build constants in one step instead of this
; of maybe I can use this one to separate booleans from key/value attributes, which is a planned feature
(dolist (a allAttributes)

; build association list with all tags and allowed attributes for each one
(setq tags+attributes '())

(define (build-tag-attribute str-tag lst-attributes)
	(list str-tag lst-attributes))

(dolist (t tags)
	(let (auxSA (assoc t specialAttributes))
		(if (true? auxSA)
			(push (build-tag-attribute t (flat (list globalAttributes (last auxSA)))) tags+attributes)
			; else put only global attributes
			(push (build-tag-attribute t globalAttributes) tags+attributes))))


; macro which builds every htmltag-function
; TODO maybe I don't need to test tagName with symbol, maybe just have to use the evaluated result
; TODO modify macro to build a function which test if attr is allowed for tag
; TODO don't consume another argument if attribute is boolean
(define-macro (hin:hin tagName)
	(eval (list 'define (list (sym (if (symbol? tagName) (string "." (eval tagName)) (string "." tagName))))
		(list 'let (list
			(list 'innerCode (list 'string "<" (eval tagName))) ; holds attributes and values
			(list 'betweenTags "") ; holds content
			(list 'myArgs '$args)) ; can't touch $idx, so to consume arbitrarily the arguments, I need to copy argument list and manage it myself

			(list 'do-until (list 'null? 'myArgs)
				(list 'let (list (list 'currentArg (list 'pop 'myArgs)))
					(list 'if (list 'protected? (list 'sym 'currentArg))
						; get next arg (which is value for attribute) and append everything into innerCode
						(list 'setq 'innerCode (list 'string 'innerCode " " 'currentArg (list 'pop 'myArgs)))
						; else
						(list 'setq 'betweenTags (list 'string 'betweenTags 'currentArg)))))

			(list 'string 'innerCode ">" 'betweenTags "</" (eval tagName) ">")))))


; create the htmltag-functions
(dolist (tag tags)
	(hin tag))
