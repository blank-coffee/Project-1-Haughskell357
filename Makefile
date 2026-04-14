.PHONY: build run test clean

build:
stack build

run:
stack run -- test-data 1000

test:
stack test

clean:
stack clean
rm -rf .stack-work
