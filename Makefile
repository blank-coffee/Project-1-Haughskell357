.PHONY: build run test clean reset

build:
	stack build

run:
	stack run -- test-data 1000

test:
	stack test

reset:
	rm -rf test-data
	cp -r test-data-original test-data

clean:
	stack clean
	rm -rf .stack-work
