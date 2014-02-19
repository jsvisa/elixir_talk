.PHONY: compile

all: compile

deps:
	mix deps.get

compile:
	mix compile

clean:
	mix clean

generate:
	mix relex.assemble

rel: deps compile generate

relclean:
	rm -rf rel/reverse
