SOURCES=$(wildcard src/**/*.hs)

test.pdf: test.dot

%.pdf: %.dot
	dot -Tpdf -o$@ $<

test.dot: $(SOURCES)
	cabal run
