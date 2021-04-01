ifeq ($(OS),Windows_NT)
	DELETE := del /q
	MOVE := move /y
	COPY := copy /y /b
	SEP :=\\
else
	DELETE := rm -f
	MOVE := mv
	COPY := cp
	SEP :=/
endif

LANG = Javalette

BNFC_DEST := src
BNFC_FILE := src$(SEP)$(LANG).cf
BNFC_DIR := $(BNFC_DEST)$(SEP)$(LANG)
BNFC_GENS = $(addprefix $(BNFC_DIR)$(SEP), Abs.hs ErrM.hs Print.hs Skel.hs)
GENERATED = $(addprefix $(BNFC_DIR)$(SEP), Abs.hs ErrM.hs Lex.hs Par.hs Print.hs Skel.hs)
LEXER_TEMPLATE := Lex.x
LEXER_HS := $(BNFC_DIR)$(SEP)Lex.hs
PARSER_TEMPLATE := Par.y
PARSER_HS := $(BNFC_DIR)$(SEP)Par.hs

HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc
BNFC       = bnfc
BNFC_OPTS  = --haskell -d -o ${BNFC_DEST}

ASSIGNMENT := A
TRY := 1
TAR_NAME = part$(ASSIGNMENT)-$(TRY).tar.gz

.PHONY : all build test doc tar clean

all : build

build: $(GENERATED)
	stack install --local-bin-path ${CURDIR}
	$(DELETE) /q jlc
	$(COPY) jlc.exe jlc

test: $(GENERATED) $(TAR_NAME)
# stack test
	cd test && python3 testing.py ../partA-1.tar.gz

doc:
	stack build --haddock

tar: $(TAR_NAME)

$(TAR_NAME): $(GENERATED)
	tar -cvzf $(TAR_NAME) Makefile *.hs package.yaml stack.yaml* src/*.hs lib doc test/*.hs README.md LICENCE $(BNFC_FILE)

# Rules for building the parser.

$(PARSER_HS): $(PARSER_TEMPLATE)
	${HAPPY} ${HAPPY_OPTS} -o $(PARSER_HS) $(PARSER_TEMPLATE)

$(LEXER_HS): $(LEXER_TEMPLATE)
	${ALEX} ${ALEX_OPTS} -o $(LEXER_HS) $(LEXER_TEMPLATE)

$(BNFC_GENS) $(PARSER_TEMPLATE) $(LEXER_TEMPLATE):
	$(BNFC) $(BNFC_OPTS) $(BNFC_FILE)
	$(MOVE) $(patsubst %.hs,%.x, $(LEXER_HS)) .
	$(MOVE) $(patsubst %.hs,%.y, $(PARSER_HS)) .
	$(MOVE) $(BNFC_DIR)$(SEP)Doc.txt .
	$(MOVE) $(BNFC_DIR)$(SEP)Test.hs test

# broken on windows
clean :
	$(DELETE) *.hi *.o *.log *.aux *.dvi $(BNFC_DIR) jlc jlc.cabal *.info *.exe *.tar.gz
	stack purge
	cabal clean
