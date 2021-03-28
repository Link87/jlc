GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

ROOT_DIR:=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))

SRC_DIR := $(abspath src)
LEXER_TEMPLATE := LexJavalette.x
LEXER_HS := $(SRC_DIR)/$(patsubst %.x,%.hs, $(LEXER_TEMPLATE))
PARSER_TEMPLATE := ParJavalette.y
PARSER_HS := $(SRC_DIR)/$(patsubst %.y,%.hs, $(PARSER_TEMPLATE))

.PHONY : all build test doc parser lexer clean

all : build

build: parser lexer
	stack install --local-bin-path ${CURDIR}

test: parser lexer
	stack test

doc:
	stack build --haddock

# Rules for building the parser.

parser: $(PARSER_HS)

$(PARSER_HS):
	${HAPPY} ${HAPPY_OPTS} -o $(PARSER_HS) $(PARSER_TEMPLATE)

lexer: $(LEXER_HS)

$(LEXER_HS):
	${ALEX} ${ALEX_OPTS} -o $(LEXER_HS) $(LEXER_TEMPLATE)

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi $(PARSER_HS) $(LEXER_HS) *.info *.exe
	stack purge
	cabal clean
