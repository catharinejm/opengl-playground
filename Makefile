# CFLAGS += -Wl,-rpath,/usr/lib/nvidia-331-updates -L/usr/lib/nvidia-331-updates -lGL -lglut
# CC ?= gcc
GUILE_OPTS = -C $(HOME)/local/lib/guile/2.0/ccache -C .

all:
	$(CC) -o main main.c $(CFLAGS)

clean:
	rm -f main
	rm -f *.o
	rm -f *.go

all:
	guile $(GUILE_OPTS) -c '(compile-file "main.scm" #:output-file "main.go")'

run:
	guile $(GUILE_OPTS) -c '(begin (use-modules (main)) (doit))'

optirun:
	optirun -b primus	guile $(GUILE_OPTS) -c '(begin (use-modules (main)) (doit))'
