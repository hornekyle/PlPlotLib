F90=gfortran
LINK=$(F90)
LN=ln
INCLUDE=$(shell pkg-config --cflags plplotd-f95)
LIBS=$(shell pkg-config --libs plplotd-f95)
#FLAGS=-O3 -march=native -mfpmath=sse
FLAGS=-g -Wall -Wtabs -fcheck=all -ffpe-trap=invalid,zero,overflow -fbacktrace -fdiagnostics-color=never
VPATH=src:bin
BPATH=bin
SPATH=src
EXE=main

all: $(EXE)

OBJS=main.o examples.o plplotlib.o utilities.o kinds.o
main.o: examples.o plplotlib.o kinds.o Makefile
examples.o: plplotlib.o kinds.o Makefile
plplotlib.o: utilities.o kinds.o Makefile
utilities.o: kinds.o Makefile
kinds.o: Makefile

$(EXE): $(OBJS) Makefile
	@echo 'Linking [$(EXE)] from [$(OBJS)] using [$(LINK)]'
	@$(LINK) $(FLAGS) -o $(BPATH)/$(EXE) $(addprefix $(BPATH)/,$(OBJS)) $(LIBS)
#	@$(LN) -sf $(BPATH)/$(EXE) $(EXE)

%.o: %.f90 Makefile
	@echo 'Compiling [$@] from [$<] using [$(F90)]'
	@$(F90) $(FLAGS) $(INCLUDE) -J $(BPATH) -c $< -o $(BPATH)/$@


clean:
	@-rm $(BPATH)/* $(EXE)
	@-touch $(BPATH)/empty

ford: project.md
	@echo 'Generating documentation'
#~ 	@echo "title: Changelog" > pages/changelog.md
#~ 	@echo "" >> pages/changelog.md
#~ 	@echo "Changelog" >> pages/changelog.md
#~ 	@echo "---------" >> pages/changelog.md
#~ 	@echo "~~~" >> pages/changelog.md
#~ 	@git log >> pages/changelog.md
#~ 	@echo "~~~" >> pages/changelog.md
	@ford project.md
 
clean-ford:
	@-rm -rf doc/* index.html
