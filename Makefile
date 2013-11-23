VENV = $(HOME)/.emacs.d/jedi_virtualenv

$(VENV):
	virtualenv $(VENV)
	$(VENV)/bin/pip install -r requirements.txt

unittest: $(VENV)
	$(VENV)/bin/python3 -m unittest discover

.cask:
	cask install

ert: .cask
	cask exec emacs -Q --batch --directory $(CURDIR) --load test-company-jedi.el

check: unittest ert

default: $(VENV)

clean:
	rm -rf .cask $(VENV)
