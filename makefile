.PHONY: all ex clean run

EXE=_build/default/bin/main.exe

all:
	dune build bin/main.exe

ex: all
	$(EXE) run examples/fact.wort

repl:
	$(EXE) repl

clean:
	dune clean

run: 
	$(EXE) run $(filter-out $@, $(MAKECMDGOALS))

%: 
	@true
	
