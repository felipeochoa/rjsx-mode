EMACS = emacs

BATCHFLAGS = -batch -Q

SRCS = rjsx-mode.el
OBJS = rjsx-mode.elc

JS2_LINK = https://raw.githubusercontent.com/mooz/js2-mode/master/js2-mode.el
JS2_OLD_INDENT_LINK = https://raw.githubusercontent.com/mooz/js2-mode/master/js2-old-indent.el


%.elc: %.el
	${EMACS} $(BATCHFLAGS) -L . -f batch-byte-compile $^

all: $(OBJS)

clean:
	-rm -f $(OBJS)

test:
	${EMACS} $(BATCHFLAGS) -L . -l rjsx-mode.el -l js2-tests.el -l rjsx-tests.el\
	  -f ert-run-tests-batch-and-exit

install-deps:
	curl --silent '$(JS2_LINK)' > js2-mode.el
	curl --silent '$(JS2_OLD_INDENT_LINK)' > js2-old-indent.el
