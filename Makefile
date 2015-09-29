SOURCES=$(shell find src -name '*.hs')
TEST_TEXT=$(shell find test-data -name '*.md')

test.pdf: test.dot

%.pdf: %.dot
	dot -Tpdf -o$@ $<

test.dot: $(SOURCES) $(TEST_TEXT)
	cabal run
