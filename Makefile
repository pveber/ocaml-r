PAGES=index

.PHONY: all
all: $(patsubst %, %.html, $(PAGES))

%.html: %.md
	pandoc --from=markdown+tex_math_single_backslash+tex_math_dollars $< --to=html5 --output=$@ --katex

.PHONY: rebuild
rebuild:
	while inotifywait -e close_write *.md; do make; done
