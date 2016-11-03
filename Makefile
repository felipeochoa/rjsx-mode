EMACS = emacs

BATCHFLAGS = -batch -Q

SRCS = rjsx-mode.el
OBJS = rjsx-mode.elc

%.elc: %.el
	${EMACS} $(BATCHFLAGS) -L . -f batch-byte-compile $^

all: $(OBJS)

clean:
	-rm -f $(OBJS)

test:
	${EMACS} $(BATCHFLAGS) -L . -l rjsx-mode.el -l js2-tests.el -l rjsx-tests.el\
	  -f ert-run-tests-batch-and-exit

install-deps:
	${EMACS} $(BATCHFLAGS) -l test-install-deps.el
