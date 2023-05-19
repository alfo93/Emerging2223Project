ERL = erl
ERLC = erlc

MODULES = ambient car render wellknown utils

all: compile run

compile:
	$(ERLC) -I include $(MODULES:%=%.erl)

run:
	$(ERL) -s ambient main -s init stop

clean:
	rm -rf *.beam erl_crash.dump