ifeq ($(OS),Windows_NT)
	DELETE := del /q /s
	DELETE_DIR := rmdir /q /s
	MOVE := move /y
	COPY := copy /y /b
	SEP :=\\
else
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

SRC_FILES := $(subst /,$(SEP),$(wildcard src/Javalette/*.hs) $(wildcard src/Javalette/Check/*.hs) $(wildcard src/Javalette/Gen/*.hs))
DOC_DEST := doc$(SEP)haddock

HAPPY      := happy
HAPPY_OPTS := --array --info --ghc --coerce
ALEX       := alex
ALEX_OPTS  := --ghc
BNFC       := bnfc
BNFC_OPTS  := --haskell -d -p Javalette -o ${BNFC_DEST}
BNFC_TEMP_FILE := src$(SEP)Lang.cf

ASSIGNMENT := B
TRY := 1
TAR_NAME := part$(ASSIGNMENT)-$(TRY).tar.gz

.DEFAULT_GOAL := build
.PHONY : all build test doc tar clean

all : build doc tar

ifeq ($(OS),Windows_NT)
build: $(GENERATED)
	stack install --local-bin-path ${CURDIR}
	$(DELETE) jlc
	$(COPY) jlc.exe jlc
else
build: $(GENERATED)
	stack install --local-bin-path ${CURDIR}
endif

test: $(GENERATED) $(TAR_NAME)
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
	$(DELETE) *.hi *.o *.log *.aux *.dvi jlc jlc.cabal *.info *.exe *.tar.gz *.txt *.x *.y $(BNFC_TEMP_FILE)
	$(DELETE_DIR) $(BNFC_DIR) $(DOC_DEST)
	stack purge
	cabal clean
