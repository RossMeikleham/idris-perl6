# idris-perl6
Perl6 backend for Idris

[![Build Status](https://travis-ci.org/RossMeikleham/idris-perl6.svg?branch=master)](https://travis-ci.org/RossMeikleham/idris-perl6)

This repo is based on Edwin's PHP back end for Idris which can be found [here](https://github.com/edwinb/idris-php),
and converts Idris intermediate representation into Perl6 source code.

This currently only supports a very small set of Idris features, and performs absolutely no 
optimisations. 

See the examples folder for examples which can be compiled/run. 


# Example
```
$ cat fib.idr 
module Main

fibonacci : Nat -> Nat
fibonacci Z = Z
fibonacci (S n) = fibonacci' 0 1 n
  where
    fibonacci' : Nat -> Nat -> Nat -> Nat
    fibonacci' f1 f2 Z = f2
    fibonacci' f1 f2 (S n) = fibonacci' f2 (f1 + f2) n

main : IO ()
main = print (fibonacci 8)

$ idris fib.idr --codegen perl6 -o fib.p6
$ perl6 fib.p6
21
```


## Other Interesting Idris Backends 
- Ruby: idris-ruby(https://github.com/mrb/idris-ruby), 
- Python: idris-py(https://github.com/ziman/idris-py) 
- Java: idris-java(https://github.com/idris-hackers/idris-java)
- LLVM: idris-llvm(https://github.com/idris-hackers/idris-llvm)

