emacs:
	cask emacs -Q -l scripts/init.el simple.py

install:
	cask install

update:
	cask update

ci:
	eask clean all
	eask package
	eask install
	eask compile
