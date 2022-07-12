#Makefile for building and running the jjm model on a Linux or Mac OSX
EXEC=jjms
# TPL=$(EXEC1).tpl

all: $(EXEC)

$(EXEC): $(EXEC).tpl
	admb -f $(EXEC)
	./jjms -nox -nohess -iprint 300 

bar: $(EXEC).tpl
	admb -f $(EXEC)
	./jjms -nox -nohess -iprint 300  -binp jjms.bar -phase 4

clean:
	rm -f $(EXEC1) $(EXEC2) *.eva *.log *.rpt *.htp *.cor *.par *.r* *.b* *.p* *.obj
	rm -f *.rep *.bar *.psv *.std $(EXEC2).cpp $(EXEC1).cpp admodel.* variance *.o *.ecm *.mc2 *.mcm
