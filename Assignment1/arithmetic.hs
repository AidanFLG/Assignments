-----------------------
-- Data types of the VM
-----------------------

-- Natural numbers
data NN = O | S NN
  deriving (Eq, Show) -- for equality and printing

-- Integers
data II = II NN NN
  deriving Show -- for printing (define your own equality below)

-- Positive integers (to avoid dividing by 0)
data PP = I | T PP
  deriving (Show, Eq)

-- Rational numbers
data QQ = QQ II PP
  deriving Show

---------------
-- TYPECASTINGS
---------------

-- cast numbers of type PP as numbers of type NN
nn_pp :: PP -> NN
nn_pp I = O
nn_pp (T a) = S (nn_pp a)

-- cast numbers of type NN to numbers of type II
ii_nn :: NN -> II
ii_nn n = II n O

-- cast numbers of type PP to numbers of type II
ii_pp :: PP -> II
ii_pp p = II (nn_pp p) O

----------------
-- PP Arithmetic
----------------

-- add positive numbers
addP :: PP -> PP -> PP
addP I b = b
addP (T a) b = T (addP a b)

-- multiply positive numbers
multP :: PP -> PP -> PP
multP I _ = I
multP (T a) b = addP b (multP a b)

----------------
-- NN Arithmetic
----------------

-- add natural numbers
-- 0 + m = m, (n+1) + m = (n+m)+1
addN :: NN -> NN -> NN
addN O m = m
addN (S n) m = S (addN n m)

-- division, eg 13 divided by 5 is 2
divN :: NN -> PP -> NN
divN O _ = O  -- Division by zero: result is zero
divN n I = n  -- Dividing any number by 1 yields the original number
divN n (T p) = S (divN (subN n (nn_pp (T p))) (T p))

-- subtraction
subN :: NN -> NN -> NN
subN O _ = O  -- Subtracting any number from zero is zero
subN n O = n  -- Subtracting zero from any number is the number itself
subN (S n) (S m) = subN n m  -- Subtracting two successors

----------------------------------------------------
-- Converting between VM-numbers and Haskell-numbers
----------------------------------------------------

-- Precondition: Inputs are non-negative
nn_int :: Integer -> NN
nn_int n
  | n < 0 = error "Input is negative"
  | n == 0 = O
  | otherwise = S (nn_int (n - 1))

int_nn :: NN -> Integer
int_nn O = 0
int_nn (S n) = 1 + int_nn n

ii_int :: Integer -> II
ii_int n
  | n >= 0 = II (nn_int n) O
  | otherwise = II O (nn_int (-n))

int_ii :: II -> Integer
int_ii (II a b) = int_nn (addN a b)

-- Precondition: Inputs are positive
pp_int :: Integer -> PP
pp_int n
  | n <= 0 = error "Input is non-positive"
  | n == 1 = I
  | otherwise = T (pp_int (n - 1))

int_pp :: PP -> Integer
int_pp I = 1
int_pp (T p) = 1 + int_pp p

float_qq :: QQ -> Float
float_qq (QQ a b) = fromIntegral (int_ii a) / fromIntegral (int_pp b)

main :: IO ()
main = do
    -- Test addP
    let pp2 = pp_int 2
    let pp3 = pp_int 4
    putStrLn"addP: "; print $ int_pp (addP pp2 pp3)

    -- Test nn_pp
    let nn5 = nn_int 5
    putStr "nn_pp: "; print $ nn_pp pp2

    -- Test addN
    let nn7 = nn_int 7
    putStr "addN: "; print $ int_nn (addN nn5 nn7) 

    -- Test divN
    let pp5 = pp_int 5
    putStr "divN: "; print $ int_nn (divN nn7 pp5) 
