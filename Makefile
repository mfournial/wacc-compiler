# Make rules

all: build

build: 
	rm -f compile
	stack install --local-bin-path src-exe
	ln -s src-exe/compiler-exe compile

test:
	stack test --coverage

clean:
	rm -f compile
	rm -f src-exe/compiler-exe
	stack clean

.PHONY: all test clean
