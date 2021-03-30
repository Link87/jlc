GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

SRC_DIR := $(abspath src/Javalette)
LEXER_TEMPLATE := Lex.x
LEXER_HS := $(SRC_DIR)/$(patsubst %.x,%.hs, $(LEXER_TEMPLATE))
PARSER_TEMPLATE := Par.y
PARSER_HS := $(SRC_DIR)/$(patsubst %.y,%.hs, $(PARSER_TEMPLATE))

ifeq ($(OS),Windows_NT)
	DELETE := del /q
	RENAME := ren
else
	DELETE := rm -f
	RENAME := mv
endif

.PHONY : all build test doc parser lexer bnfc clean

all : build

build: parser lexer
	stack install --local-bin-path ${CURDIR}
	$(DELETE) /q jlc
	$(RENAME) jlc.exe jlc

test: parser lexer
# stack test
	tar -cvzf partA-1.tar.gz Makefile *.hs *.yaml* *.x *.y src lib doc test/*.hs README.md LICENCE
	cd test && python3 testing.py ../partA-1.tar.gz
	
doc:
	stack build --haddock

# Rules for building the parser.

parser: $(PARSER_HS)

$(PARSER_HS):
	${HAPPY} ${HAPPY_OPTS} -o $(PARSER_HS) $(PARSER_TEMPLATE)

lexer: $(LEXER_HS)

$(LEXER_HS):
	${ALEX} ${ALEX_OPTS} -o $(LEXER_HS) $(LEXER_TEMPLATE)

bnfc:
	bnfc --haskell -d Javalette.cf

# broken on windows
clean :
	$(DELETE) *.hi *.o *.log *.aux *.dvi "$(PARSER_HS)" "$(LEXER_HS)" jlc jlc.cabal *.info *.exe *.tar.gz
	stack purge
	cabal clean
