# Make rules

all: build

build: 
	stack install --local-bin-path src-exe
	ln -s src-exe/compiler-exe compile

test:
	stack test --coverage

clean:
	stack clean

.PHONY: all test clean
