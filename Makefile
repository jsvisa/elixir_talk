.PHONY: all test clean

all: compile

deps:
	mix deps.get

compile:
	mix compile

test:
	mix test

clean:
	mix clean

generate:
	mix relex.assemble

rel: deps compile generate

relclean:
	rm -rf rel/reverse
