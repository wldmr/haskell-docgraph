SOURCES=$(shell find src -name '*.hs')

test.pdf: test.dot

%.pdf: %.dot
	dot -Tpdf -o$@ $<

test.dot: $(SOURCES)
	echo $(SOURCES)
	cabal run
