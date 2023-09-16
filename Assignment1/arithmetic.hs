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

------------------------
-- Arithmetic on the VM
------------------------

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
-- NN Arithmetic
----------------

-- add natural numbers
-- 0 + m = m, (n+1) + m = (n+m)+1
addN :: NN -> NN -> NN
addN O m = m
addN (S n) m = S (addN n m)

-- multiply natural numbers
-- 0 * m = 0, (n+1) * m = (n*m)+m
multN :: NN -> NN -> NN
multN O _ = O
multN (S n) m = addN (multN n m) m

-- division, eg 13 divided by 5 is 2
divN :: NN -> PP -> NN
divN _ I = O
divN n (T p) = S (divN (subN n (nn_pp p)) (T p))

modN :: NN -> PP -> NN
modN n p = subN n (multN (divN n p) n)







--gcdP :: PP -> PP -> PP
--gcdP I b = b
--gcdP a b = gcdP b (modN (ii_pp a) b)

-- subtraction
subN :: NN -> NN -> NN
subN n O = n
subN (S n) (S m) = subN n m

----------------
-- II Arithmetic
----------------

-- Addition
-- (a-b)+(c-d)=(a+c)-(b+d)
addI :: II -> II -> II
addI (II a b) (II c d) = II (addN a c) (addN b d)

-- Multiplication
multI :: II -> II -> II
multI (II a b) (II c d) = II (addN (multN a c) (multN b d)) (addN (multN a d) (multN b c))

-- Negation
negI :: II -> II
negI (II a b) = II b a

-- Equality of integers
instance Eq II where
  (II a b) == (II c d) = (addN a d) == (addN b c)

----------------
-- QQ Arithmetic
----------------

-- Addition
addQ :: QQ -> QQ -> QQ
addQ (QQ a b) (QQ c d) = QQ (addI (multI a (ii_pp d)) (multI c (ii_pp b))) (multP b d)

-- Multiplication:
multQ :: QQ -> QQ -> QQ
multQ (QQ a b) (QQ c d) = QQ (multI a c) (multP b d)

-- Equality of fractions
instance Eq QQ where
  (QQ a b) == (QQ c d) = (multI a (ii_pp d)) == (multI c (ii_pp b))

----------------
-- Normalisation
----------------

normalizeI :: II -> II
normalizeI (II a b) = II (addN a b) O

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

------------------------------
-- Normalisation by Evaluation
------------------------------

nbe :: II -> II
nbe a = normalizeI a

----------
-- Testing
----------

main :: IO ()
main = do
    -- Test addP
    let pp2 = pp_int 2
    let pp3 = pp_int 4
    putStrLn"addP: "; print $ int_pp (addP pp2 pp3) -- Expected: 6

    -- Test multP
    putStr "multP: "; print $ int_pp (multP pp2 pp3) -- Expected: 8

    -- Test nn_pp
    let nn5 = nn_int 5
    putStr "nn_pp: "; print $ nn_pp pp2 -- Expected: S (S O)

    -- Test ii_nn
    let ii5 = ii_nn nn5
    putStr "ii_nn: "; print $ int_ii ii5 -- Expected: 5

    -- Test ii_pp
    let ii_pp2 = ii_pp pp2
    putStr "ii_pp: "; print $ int_ii ii_pp2 -- Expected: 2

    -- Test addN
    let nn7 = nn_int 7
    putStr "addN: "; print $ int_nn (addN nn5 nn7) -- Expected: 12

    -- Test multN
    putStr "multN: "; print $ int_nn (multN nn5 nn7) -- Expected: 35

    -- Test divN
    let pp5 = pp_int 5
    putStr "divN: "; print $ int_nn (divN nn7 pp5) -- Expected: 1

    -- Test modN
    putStr "modN: "; print $ int_nn (modN nn7 pp5) -- Expected: 2

    -- Test gcdP
    let pp6 = pp_int 6
    --putStr "gcdP: "; print $ int_pp (gcdP pp6 pp2) -- Expected: 2

    -- Test addI
    let ii3 = ii_int 3
    let ii4 = ii_int 4
    putStr "addI: "; print $ int_ii (addI ii3 ii4) -- Expected: 7

    -- Test multI
    putStr "multI: "; print $ int_ii (multI ii3 ii4) -- Expected: 12

    -- Test negI
    putStr "negI: "; print $ int_ii (negI ii3) -- Expected: -3

    -- Test addQ
    let qq1_2 = QQ (ii_int 1) (pp_int 2)
    let qq3_4 = QQ (ii_int 3) (pp_int 4)
    putStr "addQ: "; print $ float_qq (addQ qq1_2 qq3_4) -- Expected: 1.25

    -- Test multQ
    putStr "multQ: "; print $ float_qq (multQ qq1_2 qq3_4) -- Expected: 0.75

    -- Test normalizeI
    let ii5_0 = II (nn_int 5) O
    let ii0_5 = II O (nn_int 5)
    putStr "normalizeI: "; print $ int_ii (normalizeI ii5_0) -- Expected: 5

    -- Test float_qq
    let qq7_2 = QQ (ii_int 7) (pp_int 2)
    putStr "float_qq: "; print $ float_qq qq7_2 -- Expected: 3.5

