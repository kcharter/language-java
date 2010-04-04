.PHONY: clean

GHC_OPTS=--make -O2 -funbox-strict-fields

all: cc cc-bench

cc: cc.hs CharCount.hs
	ghc $(GHC_OPTS) cc.hs

cc-bench: cc-bench.hs CharCount.hs
	ghc $(GHC_OPTS) cc-bench.hs

clean:
	rm -f cc cc-bench *.o *.hi *~

