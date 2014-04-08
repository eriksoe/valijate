all: compile

compile:
	./rebar compile skip_deps=true

clean:
	./rebar clean skip_deps=true

test:
	./rebar eunit skip_deps=true

doc:
	mkdir -p doc/html
	 emacs -Q --batch --visit=doc/API.org --eval "(progn \
	        (setq org-export-html-coding-system 'utf-8-unix) \
		(org-export-as-html 3 '() nil nil \"html/\"))"


.PHONY: all compile clean test doc
