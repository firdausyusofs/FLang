# Custom Language Lexer in Rust

This project is a lexer (tokenizer) written in Rust for a custom programming language. It reads source code and converts it into a stream of tokens, enabling parsing, interpretation, or compilation.

## ✨ Features

- Handwritten lexer without third-party libraries
- Clean token stream for further processing
- Recognizes:
  - Immutable declarations with `val`
  - Mutable declarations with `mut`
  - Custom assignment operator `<-`
  - Optional type annotations (e.g. `x: Int`)
  - Function declarations with `fun`
  - Return types using `->`
  - Basic expression parsing (identifiers, arithmetic, function calls)
  - Block grouping with `{}` and function call arguments with `()`
- Easy to extend with new token types or syntax rules
- Designed for use in future parsing and evaluation stages

## 🔤 Example Input

```
-- variable declaration
foo :: 42
bar: Int = 4

-- function declaration
calc :: fun (~x, ~y: Int) Int {
    z :: x + y
    z
}

calc(foo, bar)

hello :: fun (name person: String) {
    print("Hello, \(person))
}

hello(name: "Firdaus")
```
