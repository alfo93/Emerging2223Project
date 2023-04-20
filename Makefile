ERL = erl
ERLC = erlc

MODULES = ambient car render wellknown

all: compile run

compile:
	$(ERLC)	$(MODULES:%=%.erl)

run:
	$(ERL) -s ambient main -s init stop