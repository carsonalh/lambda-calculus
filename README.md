# Lambda Calculus Interpreter

This is a simple program that offers a shell and a wasm library (through wasm bindgen) to evaluate lambda calculus
expressions.

See some example expressions evaluated below.

```
λ> (\x.x) y
y
λ> (\z. x z) y
x y
λ> (\x. x) ((\x. x x) y)
y y
```
