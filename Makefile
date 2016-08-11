FC=		gfortran6
FFLAGS=		-Wall -O2
LDFLAGS+=	-Wl,-rpath="/usr/local/lib/gcc6"

MOD_SRC=	errfun.f90
MOD_MOD=	$(MOD_SRC:.f90=.mod)
MOD_OBJ=	$(MOD_SRC:.f90=.o)
OBJ+=		$(MOD_OBJ)
CLEAN+=		$(MOD_MOD) $(MOD_OBJ)

PROG_SRC=	errtest.f90
PROG_OBJ=	$(PROG_SRC:.f90=.o)
PROG_EXE=	$(PROG_SRC:.f90=.x)
OBJ+=		$(PROG_OBJ)
CLEAN+=		$(PROG_OBJ) $(PROG_EXE)

.SUFFIXES:
.SUFFIXES:	.f90 .o .mod .x

all: $(PROG_EXE)

$(PROG_OBJ): $(MOD_MOD) $(MOD_OBJ)

.f90.mod:
	$(FC) -c $< $(FFLAGS)
	touch $@

.f90.o:
	$(FC) -c $< $(FFLAGS)

.o.x:
	$(FC) -o $@ $(OBJ) $(LDFLAGS)

clean:
	rm $(CLEAN)
