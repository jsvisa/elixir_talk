.PHONY: all test

all: deps compile

deps:
	mix deps.get

compile:
	mix compile

test:
	mix test

clean:
	mix clean

