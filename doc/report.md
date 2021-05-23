# JLC

## Usage of JLC

If given no arguments, the compiler expects its input from stdin and writes its
output to stdout. The option `-o` can be used to specify an output file, where
jlc writes its output to instead. In addition, jlc can be supplied with the
path to a Javalette file. jlc will load that file instead of expecting input
from stdin.

The options `-p`, `-t`, `-i` and `-l` can be used to specify the type of output.
The default option is `-l`, on which the input is compiled to LLVM code.
If the flag `-t` is specified, the compiler only performs a type check.
On `-i`, the compiler prints its IR, i.e. an AST with type annotations.
With `-p`, the compiler stops after parsing and prints the parser result.

With `-s` the compiler produces more eye-pleasing output. It
can be used if the compiler is not in automated testing or does not pipe its
output into other programs (e.g. LLVM).

## Javalette Language

We make use of the default Javalette language as defined in `Javalette.cf` and
generate our code using BNFC.

## Shift/Reduce-Conflicts

Our parser has the dangling-else shift/reduce conflict. If two conditionals are
nested into each other, it is unclear to which one a following `else` belongs.

```java
if (x == 3) 
    if (y >= 5)
        doSomething();
    else
        doSomethingElse();
```

In the example above, the `else` could belong to either `if` statement. BNFC
automatically applies a shift in that case. This is presumably harmless, as the
same ambiguity is also present in Java and C and could be solved by just using
braces.

There exist additional shift/reduce conflicts that originate from the
introduction of arrays. More specifically, they affects the `new` expression,
which is defined in the grammar as follows:

```lbnf
ENew.       Expr8 ::= "new" Type [SizeItem] ;
SizeSpec.   SizeItem ::= "[" Expr "]" ;
separator   SizeItem "" ;
...
EArrIndex.  Expr7 ::= Expr7 "[" Expr "]" ;
```

The first conflict occurs, when a `[` is discovered after having parsed the type
in an `ENew` expression. The parser has two options at that point

- The parser can shift the `[` and try to parse a `SizeItem`. This is what BNFC
  applies as the default.
- The parser can reduce the `ENew` immediately. The following `[` would be
  parsed as an `EArrIndex`. Since we want the following expressions in brackets
  to be handled as size specifications instead of indices, we regard shifting as
  the correct operation. Moreover, indexing into a newly created anonymous array
  is presumably wrong and probably not what a programmer wants to do.

The second conflict is similar and occurs after having parsed the first
`SizeSpec`, i.e. while parsing the list of `SizeItem`s. Again, when discovering
a `[`, the parser has two choices. It can choose between shifting and parsing
the next `SizeItem`, or reducing and parsing the next brackets as an indexing
operation. As before, we do not want to apply indexing in a `new` expression.
Therefore, the BNFC-default of shifting in this case is correct.

## Implemented extensions

We implemented the following extensions:

- One-dimensional arrays
- Multi-dimensional arrays
- Structs and pointers
- Classes
- Overriding and dynamic dispatch
