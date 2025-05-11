# Custom Language Lexer in Rust

This project is a lexer (tokenizer) written in Rust for a custom programming language. It reads source code and converts it into a stream of tokens, enabling parsing, interpretation, or compilation.

## ✨ Features

- Handwritten lexer without third-party libraries
- Clean token stream for further processing
- Recognizes:
  - Variable declarations using `::`
  - Type annotations using `:` (e.g., `bar: Int`)
  - Assignments using `=`
  - Function declarations with `fun` keyword
  - Argument destructuring and labeling (e.g., `name person: String`)
  - Return types specified after the parameter list (e.g., `fun (...) Int`)
  - Function calls using either positional or named arguments
  - String interpolation using `\(...)` syntax
  - Single-line comments using `#` Easy to extend with new token types or syntax rules
- Designed for use in future parsing and evaluation stages

## 🔤 Example Input

```
# constant declaration
foo :: 42;

# variable declaration
bar: Int = 4;

# function declaration
calc :: fun (~x, ~y: Int) Int {
    z :: x + y;
    z
};

calc(foo, bar);

hello :: fun (name person: String) {
    print("Hello, \(person)");
};

hello(name: "Firdaus");
```
