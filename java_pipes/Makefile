.SUFFIXES:            # Delete the default suffixes
.SUFFIXES: .java .class   # Define our suffix listCC=gcc

#TGT = siple_threads_java.class

JC=javac
CLSDIR=cls
SRCDIR=src

sources = $(addprefix $(SRCDIR)/, pipe_demo.java)
#classes = $(addprefix $(CLSDIR)/, pipe_demo.class)

build:
	$(JC) -cp $(SRCDIR) -d $(CLSDIR) $(sources)
clean:
	rm $(CLSDIR)/*.class

.PHONY : clean
