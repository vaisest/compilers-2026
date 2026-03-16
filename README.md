# Compiler for a programming language

This repository contains my solution to the HY 2026 compilers course. The compiler gets a grade 4, which means it passes all core functionality tests, but does not implement `break`, `continue`, or the function definition keyword. It should be noted that the course did not require us to make an assembler, and so I have opted to use the provided script `assembler.py` which links and assembles the program using GCC tools.

Normal usage includes: `cargo run --release -- compile test.prog --output=a.out`. Note that this requires a GCC installation which means it's probably easier to run it using WSL than Windows.

Here is a language example from the course docs [hy-compilers.github.io](https://hy-compilers.github.io/spring-2026/language-spec/):

```
var n: Int = read_int();
print_int(n);
while n > 1 do {
    if n % 2 == 0 then {
        n = n / 2;
    } else {
        n = 3*n + 1;
    }
    print_int(n);
}
```
