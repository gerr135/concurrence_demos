.SUFFIXES:            # Delete the default suffixes
.SUFFIXES: .java .class   # Define our suffix listCC=gcc

#TGT = family_java.class

JC=javac
CLSDIR=cls
SRCDIR=src
vpath %.c src
vpath %.h src

sources = $(addprefix $(SRCDIR)/, MainGUISemaph.java  MainGUISynchro.java  MainCmdlineSemaph.java  MainCmdlineSynchro.java)
classes = $(addprefix $(CLSDIR)/, MainGUISemaph.class MainGUISynchro.class MainCmdlineSemaph.class MainCmdlineSynchro.class )

build:
	$(JC) -cp $(SRCDIR) -d $(CLSDIR) $(sources)
clean:
	rm $(classes)

.PHONY : clean
