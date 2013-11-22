venv:
	virtualenv venv
	venv/bin/pip install -r requirements.txt

unittest: venv
	venv/bin/python3 -m unittest discover

.cask:
	cask install

ert: .cask
	cask exec emacs -Q --batch --directory $(CURDIR) --load test-company-jedi.el

check: unittest ert

default: venv

clean:
	rm -rf .cask venv
