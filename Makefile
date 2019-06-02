.PHONY: clean version test

all: build

clean:
	rm -rf .cask

.cask:
	cask install
	cask update

version:
	emacs --version

build: version .cask
	cask build

test: version .cask
	cask exec buttercup -L .
