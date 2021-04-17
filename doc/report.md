# JLC

## Usage of JLC

If given no arguments, the compiler expects its input from stdin and writes its
output to stdout. The option `-o` can be used to specify an output file, where
jlc writes its output to instead. In addition, jlc can be supplied with the
path to a Javalette file. jlc will load that file instead of expecting input
from stdin.

The options `-t`, `-i` and `-l` can be used to specify the type of output.
The default option is `-l`, on which the input is compiled to LLVM code.
If the flag `-t` is specified, the compiler only performs a type check.
On `-i`, the compiler prints its IR, i.e. an AST with type annotations.

With `-s` the compiler produces more eye-pleasing output. It
can be used if the compiler is not in automated testing or does not pipe its
output into other programs (e.g. LLVM).

## Javalette Language

We make use of the default Javalette language as defined in `Javalette.cf` and
generate our code using BNFC. Our changes to that file are all `internal`,
i.e. they do not change the semantics of the language.

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
