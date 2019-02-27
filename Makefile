version:
	emacs --version

test : version
	cask exec buttercup -L .

elpa:
	cask install
	cask update
