VIRTUAL_ENV = $(HOME)/.emacs.d/jedi/venv

default: $(VIRTUAL_ENV)

install: $(VIRTUAL_ENV)

$(VIRTUAL_ENV):
	virtualenv $(VIRTUAL_ENV)
	$(VIRTUAL_ENV)/bin/pip install -r requirements.txt
