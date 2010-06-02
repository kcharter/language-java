.PHONY: clean runtests

GHC_OPTS=-O2 -funbox-strict-fields -i.. -outputdir build
PROGS=runtests

all: $(PROGS)

runtests:
	ghc --make $(GHC_OPTS) runtests.hs	
	./runtests

clean:
	rm -f $(PROGS) *.o *.hi *~
	rm -rf build


