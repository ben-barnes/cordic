# CORDIC

This is an experimental implementation of the CORDIC algorithm in Haskell.

To build, use
```
git clone https://github.com/chronon-io/cordic.git
cd cordic
stack build
```
Currently only a library is implemented. To play around with it in the REPL, use `stack ghci`. You can then call
```
cordic angle iterations
```
to get an nth iteration approximation of `(cosine(angle), sine(angle))`
