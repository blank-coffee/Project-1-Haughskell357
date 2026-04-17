.PHONY: build run tester clean reset

build:
	stack build

run:
	stack run file-organizer -- test-root

tester:
	stack run tester

clean:
	stack clean
	rm -rf .stack-work test-root tester-manifest.json