VENV = $(HOME)/.emacs.d/jedi/venv
BATCH = --batch

$(VENV):
	virtualenv $(VENV)
	$(VENV)/bin/pip install -r requirements.txt

unittest: $(VENV)
	$(VENV)/bin/python3 -m unittest discover

.cask:
	cask install

ert: .cask $(VENV)
	cask exec emacs -Q $(BATCH) --directory $(CURDIR) --load test-company-jedi.el

check: unittest ert

default: $(VENV)

clean:
	rm -rf .cask $(VENV)

emacs:
	cask exec emacs -Q -L $(CURDIR) -l script/init.el

start_jedi:
	$(VENV)/bin/python3 -m start_jedi -d
