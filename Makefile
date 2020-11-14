PAGES=index

.PHONY: all
all: $(patsubst %, %.html, $(PAGES))

%.html: %.md template.html
	pandoc --from=markdown+tex_math_single_backslash+tex_math_dollars $< --to=html5 --output=$@ --katex --template template.html --css api/odoc.css

.PHONY: rebuild
rebuild:
	while inotifywait -e close_write *.md; do make; done
