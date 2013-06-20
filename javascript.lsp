#!/usr/bin/env newlisp
;; Creation Date: Wed, 19 Jun 2013 09:25:16 -0300
;; @author Fernando Canizo (aka conan) - http://conan.muriandre.com/
;; @module Javascript
;; @description Holds a list of javascript files to be included at the very end of output


;; hin should be already loaded, but just in case...
(unless hin (load "hin.lsp"))

(context 'javascript)


(define (javascript:javascript)
	;; set container
	(if (not javascript:files)
		(setq javascript:files ""))

	(if $args
		;; append new files
		(doargs (arg)
			(setq javascript:files (append javascript:files
				(hin:script hin:src= arg) "\n")))

		;; else, spit built HTML
		javascript:files))
