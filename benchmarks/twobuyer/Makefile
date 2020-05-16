################################################################################
# Customize these variables for your project
################################################################################
# The root files of your project, from which to begin scanning dependences
ML_FILES=_output/GeneratedTwoBuyerA.ml _output/GeneratedTwoBuyerB.ml _output/GeneratedTwoBuyerS.ml _output/Impl_A.ml _output/Impl_B.ml _output/Impl_S.ml
#/usr/local/Cellar/fstar/0.9.6.0/libexec

# The executable file you want to produce
PROGRAM=main

# A driver in ML to call into your program
TOP_LEVEL_FILE=ml/main.ml

# A place to put all the emitted .ml files
OUTPUT_DIRECTORY=_output

################################################################################
OCAML_EXE=$(PROGRAM).ocaml.exe

all: $(OCAML_EXE)

# The extracted ML files are precious, because you may want to examine them,
#     e.g., to see how type signatures were transformed from F*
.PRECIOUS: $(ML_FILES)

# _tags:
# 	echo "<ml>: traverse" > $@
# 	echo "<$(OUTPUT_DIRECTORY)>: traverse" >> $@
# 	echo "<$(OUTPUT_DIRECTORY)/c>: -traverse" >> $@

$(OCAML_EXE): $(ML_FILES) $(TOP_LEVEL_FILE)
	ocamlbuild -I $(OUTPUT_DIRECTORY) -use-ocamlfind -pkg threads -tag thread $(subst .ml,.native,$(TOP_LEVEL_FILE))
	mv $(subst .ml,.native,$(TOP_LEVEL_FILE:ml/%=%)) $@

test.ocaml: $(OCAML_EXE)
	./$< hello

clean:
	rm -rf _build *~ *.checked $(OCAML_EXE) .depend
