module Main

-- Calculate the nth fibonacci number using
-- the naive recursive algorithm
fibonacci : Nat -> Nat
fibonacci Z = Z
fibonacci (S n) = fibonacci' 0 1 n
  where
    fibonacci' : Nat -> Nat -> Nat -> Nat
    fibonacci' f1 f2 Z = f2 
    fibonacci' f1 f2 (S n) = fibonacci' f2 (f1 + f2) n

main : IO ()
main = do 
  print $ fibonacci 3
