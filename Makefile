ifeq ($(OS),Windows_NT)
	MKDIR := mkdir
	DELETE := del /q /s
	DELETE_DIR := rmdir /q /s
	MOVE := move /y
	COPY := copy /y /b
	SEP :=\\
else
	MKDIR := mkdir -p
	DELETE := rm -fv
	DELETE_DIR := rm -rfv
	MOVE := mv
	COPY := cp
	SEP :=/
endif

LANG = Javalette

BNFC_DEST := src
BNFC_FILE := src$(SEP)$(LANG).cf
BNFC_DIR := $(BNFC_DEST)$(SEP)$(LANG)$(SEP)Lang
BNFC_GENS := $(addprefix $(BNFC_DIR)$(SEP), Abs.hs ErrM.hs Print.hs Skel.hs)
GENERATED_MODS := Abs.hs ErrM.hs Lex.hs Par.hs Print.hs Skel.hs
GENERATED := $(addprefix $(BNFC_DIR)$(SEP), $(GENERATED_MODS))
LEXER_TEMPLATE := Lex.x
LEXER_HS := $(BNFC_DIR)$(SEP)Lex.hs
PARSER_TEMPLATE := Par.y
PARSER_HS := $(BNFC_DIR)$(SEP)Par.hs

SRC_FILES := $(subst /,$(SEP),$(wildcard src/Javalette/*.hs) $(wildcard src/Javalette/Check/*.hs) $(wildcard src/Javalette/Gen/*.hs) $(wildcard src/Javalette/Gen/LLVM/*.hs))
DOC_DEST := doc$(SEP)haddock

HAPPY      := happy
HAPPY_OPTS := --array --info --ghc --coerce
ALEX       := alex
ALEX_OPTS  := --ghc
BNFC       := bnfc
BNFC_OPTS  := --haskell -d -p Javalette --text-token -o ${BNFC_DEST}
BNFC_TEMP_FILE := src$(SEP)Lang.cf

# JL_FILE must be set via command line
OUT_DIR := out
LL_FILE := $(OUT_DIR)$(SEP)$(notdir $(addsuffix .ll,$(basename $(JL_FILE))))
BC_FILE := $(LL_FILE:.ll=.bc)
OPT_BC_FILE := $(LL_FILE:.ll=.opt.bc)
LD_BC_FILE := $(LL_FILE:.ll=.ld.bc)
O_FILE := $(LL_FILE:.ll=.o)
ASM_FILE := $(LL_FILE:.ll=.s)
ifeq ($(OS),Windows_NT)
COMPILE_OUTPUT := $(LL_FILE:.ll=.exe)
else
COMPILE_OUTPUT := $(LL_FILE:.ll=)
endif
LLVM_RUNTIME := lib$(SEP)runtime.ll

ASSIGNMENT := C
TRY := 1
TAR_NAME := part$(ASSIGNMENT)-$(TRY).tar.gz

.DEFAULT_GOAL := build
.PHONY : all build test test-tc doc tar clean compile

all : build doc tar

build: $(GENERATED)
	stack install --local-bin-path ${CURDIR}
ifeq ($(OS),Windows_NT)
	if exist jlc $(DELETE) jlc
	$(COPY) jlc.exe jlc
endif

test: $(GENERATED) $(TAR_NAME)
	cd test && python3 testing.py ../$(TAR_NAME) --llvm -x arrays1 arrays2 objects1

test-tc: $(GENERATED) $(TAR_NAME)
	cd test && python3 testing.py ../$(TAR_NAME)

doc: $(GENERATED)
	stack exec -- haddock --html $(SRC_FILES) $(GENERATED) $(addprefix --hide=Javalette.Lang., $(basename $(GENERATED_MODS))) --package-name=jlc --hyperlinked-source --odir=$(DOC_DEST)

tar: $(TAR_NAME)

$(TAR_NAME):
	tar -cvzf $(TAR_NAME) Makefile *.hs package.yaml stack.yaml* $(SRC_FILES) lib doc test/*.hs README.md LICENCE $(BNFC_FILE)

# Rules for building the parser.

$(PARSER_HS): $(PARSER_TEMPLATE)
	${HAPPY} ${HAPPY_OPTS} -o $(PARSER_HS) $(PARSER_TEMPLATE)

$(LEXER_HS): $(LEXER_TEMPLATE)
	${ALEX} ${ALEX_OPTS} -o $(LEXER_HS) $(LEXER_TEMPLATE)

$(BNFC_GENS) $(PARSER_TEMPLATE) $(LEXER_TEMPLATE):
	$(COPY) $(BNFC_FILE) $(BNFC_TEMP_FILE)
	$(BNFC) $(BNFC_OPTS) $(BNFC_TEMP_FILE)
	$(DELETE) $(BNFC_TEMP_FILE)
	$(MOVE) $(patsubst %.hs,%.x, $(LEXER_HS)) .
	$(MOVE) $(patsubst %.hs,%.y, $(PARSER_HS)) .
	$(MOVE) $(BNFC_DIR)$(SEP)Doc.txt .
	$(MOVE) $(BNFC_DIR)$(SEP)Test.hs test

clean :
	$(DELETE) *.hi *.o *.log *.aux *.dvi jlc* jlc.cabal *.info test$(SEP)Test.hs *.tar.gz *.txt *.x *.y $(BNFC_TEMP_FILE)
ifeq ($(OS),Windows_NT)
	if exist $(BNFC_DIR) $(DELETE_DIR) $(BNFC_DIR)
	if exist $(DOC_DEST) $(DELETE_DIR) $(DOC_DEST)
	if exist $(OUT_DIR) $(DELETE_DIR) $(OUT_DIR)
else
	$(DELETE_DIR) $(BNFC_DIR) $(DOC_DEST) $(OUT_DIR)
endif
	stack purge
	cabal clean

# set JL_FILE via command line or environment variable
compile: build
ifeq ($(OS),Windows_NT)
	if not exist $(OUT_DIR) $(MKDIR) $(OUT_DIR)
else
	$(MKDIR) $(OUT_DIR)
endif
	.$(SEP)jlc -s --llvm -o$(LL_FILE) $(JL_FILE)
	llvm-as -o $(BC_FILE) $(LL_FILE)
	opt -O3 $(BC_FILE) -o=$(OPT_BC_FILE)
	llvm-link $(OPT_BC_FILE) $(LLVM_RUNTIME) -o $(LD_BC_FILE)
	clang $(LD_BC_FILE) -o $(COMPILE_OUTPUT)
#	llc $(LD_BC_FILE) --filetype=obj -o=$(O_FILE)

#	gcc $(O_FILE) -o $(COMPILE_OUTPUT)
