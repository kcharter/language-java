.PHONY: clean

GHC_OPTS=--make -O2 -funbox-strict-fields -i.. -outputdir build
LEXER=../Language/Java/Lexer
PROGS=cc cc-bench lex-bench

all: $(PROGS)

cc: cc.hs CharCount.hs
	ghc $(GHC_OPTS) cc.hs

cc-bench: cc-bench.hs CharCount.hs
	ghc $(GHC_OPTS) cc-bench.hs

lex-bench:: lex-bench.hs $(LEXER)/UnicodeEscapes.hs
	ghc $(GHC_OPTS) lex-bench.hs

clean:
	rm -f $(PROGS) *.o *.hi *~
	rm -rf build


