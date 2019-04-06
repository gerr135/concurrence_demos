.SUFFIXES:            # Delete the default suffixes
.SUFFIXES: .java .class   # Define our suffix listCC=gcc

#TGT = family_java.class

JC=javac
CLSDIR=cls
SRCDIR=src

#sources = $(addprefix $(SRCDIR)/, MainGUISemaph.java  MainGUISynchro.java  MainCmdlineSemaph.java  MainCmdlineSynchro.java)
#classes = $(addprefix $(CLSDIR)/, MainGUISemaph.class MainGUISynchro.class MainCmdlineSemaph.class MainCmdlineSynchro.class )
sources = $(addprefix $(SRCDIR)/, MainCmdlineSemaph.java  MainCmdlineSynchro.java)
classes = $(addprefix $(CLSDIR)/, MainCmdlineSemaph.class MainCmdlineSynchro.class )

build:
	$(JC) -cp $(SRCDIR) -d $(CLSDIR) $(sources)
clean:
	rm $(CLSDIR)/*

.PHONY : clean
