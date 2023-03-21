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

all:
	$(LISP) $(CMDS)
	cp -r commands bin/

clean: 
	rm -rf bin/
