module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined, Num (negate), read )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

-- addition
(+) :: Nat -> Nat -> Nat
n + m = case m of
        O -> n
        S m -> S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True 
--FINISHED
isZero :: Nat -> Nat
isZero O = S O
isZero _ = O

-- pred is the predecessor but we define zero's to be zero
--FINISHED
pred :: Nat -> Nat
pred O = O
pred (S x) = x

-- Output: O means False, S O means True
--FINISHED
even :: Nat -> Nat
even O = S O
even (S O) = O
even (S (S x)) = even x

--FINISHED
odd :: Nat -> Nat
odd O = O
odd (S O) = S O
odd (S (S x)) = odd x

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
--FINISHED
monus :: Nat -> Nat -> Nat
monus O _ = O --BASE (∀ n) 0-*n=0
monus n O = n
monus (S x) (S y) = monus x y

--FINISHED
(-*) :: Nat -> Nat -> Nat
(-*) = monus
infixl 6 -*

-- multiplication
--FINISHED
(*) :: Nat -> Nat -> Nat
n * O = O
n * (S O) = n
n * (S x) = n + (n * x)


infixl 7 *

-- exponentiation
--FINISHED
(^) :: Nat -> Nat -> Nat
_ ^ O = S O
n ^ (S O) = n
n ^ (S x) = n * (n ^ x)

-- decide: infix? ? ^

eucdiv :: Nat -> Nat -> (Nat, Nat) --    eucdiv(f,g) = (q,r) ⇔ f = g·q + r
eucdiv O _ = (O,O)    --zero é divisível a todos
eucdiv f (S O) = (f,O)  --o um divide a todos
eucdiv f g = undefined


--pelo algoritmo de euclides
  -- 4/2 ⇒ 4 = 2·q + r, q = 2 & r = 0
              

-- quotient
(/) :: Nat -> Nat -> Nat
f/g = q
      where 
        (q,_) = eucdiv f g

--ARGUMENTO:
-- S (S (S (S O))) / S (S O) four/two
-- = S (S O)
-- S (S (S O)) / S (S O) 
-- = S O
-- S (S (S (S (S (S O))))) / S (S (S O))
-- = S (S O)

-- remainder
(%) :: Nat -> Nat -> Nat
f%g = r
      where
        (_,r) = eucdiv f g

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
(|||) = undefined

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff = undefined

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial = undefined

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg _ = S O
--there's no -1 in our world of Nats

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined

