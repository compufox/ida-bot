LISPS = ros sbcl clisp cmucl ccl
CMDS = --eval "(ql:quickload :ida-bot)" --eval "(asdf:make :ida-bot)" --eval "(quit)"


ifeq ($(OS),Windows_NT)
	LISP := $(foreach lisp,$(LISPS), \
		$(shell where $(lisp)) \
		$(if $(.SHELLSTATUS),$(strip $(lisp)),))
else
	LISP := $(foreach lisp,$(LISPS), \
		$(if $(findstring $(lisp),"$(shell which $(lisp) 2>/dev/null)"), $(strip $(lisp)),))
endif

ifeq ($(LISP),)
	$(error "No lisps found")
endif

.PHONY: clean all extensions

all: ida-bot

ida-bot:
	$(LISP) $(CMDS)

clean: 
	rm -rf bin/

extensions:
	cp -r commands bin/
	cp -r services bin/
	cp -r handlers bin/
