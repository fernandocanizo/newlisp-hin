#!/usr/bin/env newlisp
;; Creation Date: Wed, 19 Jun 2013 11:51:56 -0300
;; @author Fernando Canizo (aka conan) - http://conan.muriandre.com/


;; switch evaluated context of all it's arguments with provided one
;; be careful: *any* context will be replaced. For more control over
;; what's replaced use replace-this-context

(context 'wrap-in-context)
(define-macro (wrap-in-context:wrap-in-context ctx-new)
	(eval
		(doargs (arg)
			(read-expr
				(replace {\w+:} (string arg) "" 0) (eval ctx-new)))))

(context MAIN)


(context 'replace-this-context)
(define-macro (replace-this-context:replace-this-context ctx-old ctx-new)
	(eval
		(doargs (arg)
			(read-expr
				(replace (string (eval ctx-old) ":") (string arg) "" 0) (eval ctx-new)))))

(context MAIN)

;; TODO have to find a way to distinguish parameters from auto-evaluated stuff.
;; So far I cannot define functions with arguments because they get the context
;; I'm replacing
