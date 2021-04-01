# JLC

## Usage of JLC

If given no arguments, the compiler expects its input from stdin and writes the
intermediate representation, i.e. the AST, into stdout. The option `-o` can be
used to specify a path where the Javalette file should be loaded from instead.

If the flag `-t` is specified, the compiler only performs a type check without
printing the IR. With `-s` the compiler produces more eye-pleasing output. It
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
