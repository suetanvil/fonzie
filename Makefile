
# Why yes, I'm using make to build a Scala app.  Why do you ask?

.SUFFIXES: .scala .class .jar .html

SC=fsc
SCFLAGS=-g:vars

MYSRC=ListingSorter.scala RecordMatcher.scala ProductData.scala
TWITSRC=twitter/Json.scala twitter/extensions.scala
SRC= $(MYSRC) $(TWITSRC)

JARFILE=Fonzie.jar
MANIFEST=Fonzie.mf

all: $(JARFILE)

# Creates the JAR file but also creates symlinks to the source code
# next to the .class files so that jdb can find them.
$(JARFILE): $(SRC)
	$(SC) $(SCFLAGS) $(SRC)
	jar -cfm $(JARFILE) $(MANIFEST) ca/
	(for i in $(SRC); do ln -sf `pwd`/$$i ca/blit/Fonzie/`basename $$i`; done)

clean: tidy
	-rm $(JARFILE) index.html package.html index.js

tidy:
	-rm -rf ca/ com/ 

doc: index.html

index.html: $(JARFILE)
	scaladoc $(MYSRC)




