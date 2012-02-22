module Data.Tuple.Enum (all2s, all3s, all4s, all5s, all6s, all7s, all8s, all9s, all10s )
where
import Data.Word
import Math.Combinat.Numbers

----------------------------------------------------------------------------------------------------
-- To understand better why this library could be useful for you, read the blog post: ...
-- and see the Monad Reader issue 20
----------------------------------------------------------------------------------------------------

-- | generate all 2-tuples (of enumerable values) so that the sum of the 2 fromEnum-values is monotonic increasing
--   fromEnum :: a -> Int
all2s :: (Enum a, Enum b, Eq a, Eq b, Bounded a, Bounded b) => [(a,b)]
all2s = enumFrom (minBound,minBound)

-- | generate all 3-tuples (of enumerable values) so that the sum of the 3 fromEnum-values is monotonic increasing
--   fromEnum :: a -> Int
all3s :: (Enum a, Enum b, Enum c, Eq a, Eq b, Eq c, Bounded a, Bounded b, Bounded c) => [(a,b,c)]
all3s = enumFrom (minBound,minBound,minBound)

-- | generate all 4-tuples (of enumerable values) so that the sum of the 4 fromEnum-values is monotonic increasing
--   fromEnum :: a -> Int
all4s :: (Enum a, Enum b, Enum c, Enum d, Eq a, Eq b, Eq c, Eq d,
          Bounded a, Bounded b, Bounded c, Bounded d) => [(a,b,c,d)]
all4s = enumFrom (minBound,minBound,minBound,minBound)

-- | generate all 5-tuples (of enumerable values) so that the sum of the 5 fromEnum-values is monotonic increasing
--   fromEnum :: a -> Int
all5s :: (Enum a, Enum b, Enum c, Enum d, Enum e, Eq a, Eq b, Eq c, Eq d, Eq e, 
          Bounded a, Bounded b, Bounded c, Bounded d, Bounded e) => [(a,b,c,d,e)]
all5s = enumFrom (minBound,minBound,minBound,minBound,minBound)

-- | generate all 6-tuples (of enumerable values) so that the sum of the 6 fromEnum-values is monotonic increasing
--   fromEnum :: a -> Int
all6s :: (Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Eq a, Eq b, Eq c, Eq d, Eq e, Eq f,
          Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f) => [(a,b,c,d,e,f)]
all6s = enumFrom (minBound,minBound,minBound,minBound,minBound,minBound)

-- | generate all 7-tuples (of enumerable values) so that the sum of the 7 fromEnum-values is monotonic increasing
--   fromEnum :: a -> Int
all7s :: (Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
          Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g) => [(a,b,c,d,e,f,g)]
all7s = enumFrom (minBound,minBound,minBound,minBound,minBound,minBound,minBound)

-- | generate all 8-tuples (of enumerable values) so that the sum of the 8 fromEnum-values is monotonic increasing
--   fromEnum :: a -> Int
all8s :: (Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h, Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h,
          Bounded a, Bounded b, Bounded c, Bounded d,
          Bounded e, Bounded f, Bounded g, Bounded h) => [(a,b,c,d,e,f,g,h)]
all8s = enumFrom (minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound)

-- | generate all 9-tuples (of enumerable values) so that the sum of the 9 fromEnum-values is monotonic increasing
--   fromEnum :: a -> Int
all9s :: (Enum a,Enum b,Enum c,Enum d,Enum e,Enum f,Enum g,Enum h,Enum i,
          Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
          Bounded a,Bounded b,Bounded c,Bounded d,
          Bounded e,Bounded f,Bounded g,Bounded h,Bounded i) => [(a,b,c,d,e,f,g,h,i)]
all9s = enumFrom (minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound)

-- | generate all 10-tuples (of enumerable values) so that the sum of the 10 fromEnum-values is monotonic increasing
--   fromEnum :: a -> Int
all10s :: (Enum a,Enum b,Enum c,Enum d,Enum e,Enum f,Enum g,Enum h,Enum i,Enum j,
           Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j,
           Bounded a,Bounded b,Bounded c,Bounded d,Bounded e,
           Bounded f,Bounded g,Bounded h,Bounded i,Bounded j) => [(a,b,c,d,e,f,g,h,i,j)]
all10s = enumFrom (minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound)

-- | generate all 11-tuples (of enumerable values) so that the sum of the 11 fromEnum-values is monotonic increasing
--   fromEnum :: a -> Int
all11s :: (Enum a,Enum b,Enum c,Enum d,Enum e,Enum f,Enum g,Enum h,Enum i,Enum j, Enum k,
           Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k,
           Bounded a,Bounded b,Bounded c,Bounded d,Bounded e,
           Bounded f,Bounded g,Bounded h,Bounded i,Bounded j,Bounded k) => [(a,b,c,d,e,f,g,h,i,j,k)]
all11s = enumFrom (minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound)

-- | generate all 12-tuples (of enumerable values) so that the sum of the 12 fromEnum-values is monotonic increasing
--   fromEnum :: a -> Int
all12s :: (Enum a,Enum b,Enum c,Enum d,Enum e,Enum f,Enum g,Enum h,Enum i,Enum j, Enum k, Enum l,
           Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l,
           Bounded a,Bounded b,Bounded c,Bounded d,Bounded e,Bounded f,
           Bounded g,Bounded h,Bounded i,Bounded j,Bounded k,Bounded l) => [(a,b,c,d,e,f,g,h,i,j,k,l)]
all12s = enumFrom (minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound)

-- | generate all 13-tuples (of enumerable values) so that the sum of the 13 fromEnum-values is monotonic increasing
--   fromEnum :: a -> Int
all13s :: (Enum a,Enum b,Enum c,Enum d,Enum e,Enum f,Enum g,Enum h,Enum i,Enum j, Enum k, Enum l, Enum m,
           Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m,
           Bounded a,Bounded b,Bounded c,Bounded d,Bounded e,Bounded f,
           Bounded g,Bounded h,Bounded i,Bounded j, Bounded k, Bounded l, Bounded m) => [(a,b,c,d,e,f,g,h,i,j,k,l,m)]
all13s = enumFrom (minBound,minBound,minBound,minBound,minBound,minBound,minBound,
                   minBound,minBound,minBound,minBound,minBound,minBound)

-- | generate all 14-tuples (of enumerable values) so that the sum of the 14 fromEnum-values is monotonic increasing
--   fromEnum :: a -> Int
all14s :: (Enum a,Enum b,Enum c,Enum d,Enum e,Enum f,Enum g,Enum h,Enum i,Enum j, Enum k, Enum l, Enum m, Enum n,
           Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n,
           Bounded a,Bounded b,Bounded c,Bounded d,Bounded e, Bounded f,Bounded g,
           Bounded h,Bounded i,Bounded j, Bounded k, Bounded l, Bounded m, Bounded n) => [(a,b,c,d,e,f,g,h,i,j,k,l,m,n)]
all14s = enumFrom (minBound,minBound,minBound,minBound,minBound,minBound,minBound,
                   minBound,minBound,minBound,minBound,minBound,minBound,minBound)

-- | generate all 15-tuples (of enumerable values) so that the sum of the 15 fromEnum-values is monotonic increasing
--   fromEnum :: a -> Int
all15s :: (Enum a,Enum b,Enum c,Enum d,Enum e,Enum f,Enum g,Enum h,Enum i,Enum j, Enum k, Enum l, Enum m, Enum n, Enum o,
           Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o,
           Bounded a,Bounded b,Bounded c,Bounded d,Bounded e, Bounded f,Bounded g,
           Bounded h,Bounded i,Bounded j, Bounded k, Bounded l, Bounded m, Bounded n, Bounded o) => [(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)]
all15s = enumFrom (minBound,minBound,minBound,minBound,minBound,minBound,minBound,
                   minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound)

------------------------------------------------------------------------------------------------------------
-- The size of enumeration hyperplanes
------------------------------------------------------------------------------------------------------------

polynomial :: Int -> [Rational] -> Rational
polynomial n coeffs = foldr (+) 0 (zipWith nPowerP coeffs [1..])
  where nPowerP a_j p = a_j * (fromIntegral (n^p))

sumOfPowers :: Int -> [Rational]
sumOfPowers p = reverse [ (bin j) * (ber j) / ((fromIntegral p)+1) | j <- [0..p] ]
  where bin j = fromIntegral (binomial (p+1) j)
        ber j | j == 1 = negate (bernoulli j) -- see wikipedia entry
              | otherwise = bernoulli j

hyperplaneSize :: Int -> Int -> Int
hyperplaneSize dim n | n == 0   = 0
                     | dim == 0 = 1
                     | otherwise = round (genPolynom 1 [1])
  where genPolynom :: Int -> [Rational] -> Rational
        genPolynom d coeffs | d == dim = polynomial n coeffs
                            | otherwise = genPolynom (d+1)
                              (merge coeffs (map sumOfPowers [1..(length coeffs)]))

merge coeffs ls = foldr myZip [] multiplied_ls
  where multiplied_ls = zipWith (\c l -> map (c*) l) coeffs ls
        myZip (l0:l0s) (l1:l1s) = (l0+l1) : (myZip l0s l1s)
        myZip a b = a ++ b

ssizes d = [ sum (take n sizes) | n <- [1..] ]
  where sizes = [ hyperplaneSize d i | i <- [0..] ]

summedSizes :: Int -> Int -> Int
summedSizes dim n = (ssizes dim) !! n

-- used in fromEnum
fe [x] = x
fe (x:xs) = ( summedSizes (length xs) (foldr (+) 0 (x:xs)) ) + (fe xs)

-- (summedSizes 4 (a1+b1+c1+d1+e1) ) +
-- (summedSizes 3    (b1+c1+d1+e1) ) +
-- (summedSizes 2       (c1+d1+e1) ) +
-- (summedSizes 1          (d1+e1) ) +
--                             e1

te :: Int -> Int -> [Int]
te dim n = differences $ reverse $ fst $ foldr hplanes ([],n) [1..dim]

differences :: [Int] -> [Int]
differences [x] = [x]
differences (x:y:ys) = (x-y) : (differences (y:ys))

hplanes :: Int -> ([Int],Int) -> ([Int],Int)
hplanes d (planes,rest) = ((fst hp):planes, snd hp)
  where hp = (hyperplane d rest)

hyperplane dim n = ( (length filtered) - 1, n - (if null filtered then 0 else last filtered) )
  where filtered = filterAndStop [ summedSizes (dim-1) i | i <- [0..] ]
        filterAndStop (x:xs) | x <= n     = x : (filterAndStop xs)
                             | otherwise = []

-----------------------------------------------------------------------------------------------

data J a = Jst a | I Int -- Just a value or an Int
           -- Usually a plain value is better than an Int because succ,pred is faster than doing toEnum.fromEnum
           -- If a boundary is reached there is no other way than to transform it into an Int.

instance Show a => Show (J a)
 where show (Jst x) = show x
       show (I i) = show i

--------------------------------------------------------
-- various helper functions
--------------------------------------------------------

-- Is it a minimum value
minB (Jst x) | x == minBound = True
             | otherwise     = False
minB (I i) = i == 0

-- predecessor with catching of errors
pre :: (Enum a, Eq a, Bounded a) => J a -> J a
pre (Jst x) | x == minBound = error "predecessor of minBound in enumeration"
            | otherwise     = Jst (pred x)

pre (I i)  | i == 0    = error "predecessor of 0 in enumeration"
           | otherwise = I (i-1)

-- successor, replacing everything that goes beyond a Border with an Int
suc :: (Enum a, Eq a, Bounded a) => J a -> J a
suc (Jst x) | x == maxBound = I ((fromEnum x)+1)
            | otherwise     = Jst (succ x)

suc (I i) | i == maxBound = error "successor of maxBound in enumeration"
          | otherwise     = I (i+1)

isJst (Jst a) = True
isJst   _     = False

fJ (Jst a) = a

getInt :: Enum a => J a -> Int
getInt (Jst a) = fromEnum a
getInt (I i)   = i

-- maximum boundary of value
mb :: Bounded a => (J a) -> a
mb (Jst x) = maxBound

-- is it below boudary? then toEnum else return an Int
ib :: (Enum a, Eq a, Bounded a) => J a -> Int -> J a
ib (Jst x) boundary = Jst x
ib (I i)   boundary | i <= boundary = Jst (toEnum i)
                    | otherwise     = I i

v :: (Enum a, Enum b, Eq a, Eq b, Bounded a, Bounded b) => Int -> J a -> J b
v fz z = if (isJst z) && (toEnum fz) /= (mb z) then Jst (toEnum fz) else I fz

------------------------------------------------------------------------------------------------------------
-- The following functions are explained in the Monad Reader No. 20
-- They basically build up a similar pattern like the pred-functions later, but with added support for reaching boundaries
-- (assuming that one usually enumerates beginning with 0)
-- example for reaching the boundary (True,True,True) :  enumFrom (False,False,False)
------------------------------------------------------------------------------------------------------------

succ2 :: ( Enum a, Enum b, Eq a, Eq b, Bounded a, Bounded b) =>
         Int -> Bool -> (J a,J b) -> (J a,J b)
succ2 fz s (y,z)
  | (minB y) && (minB z) = (Jst (succ minBound), Jst minBound) -- (1,0) starting with an asymmetry
  | (minB y) && (not (minB z)) = ( if ((isJst z) && (toEnum fz) == (mb z)) || not (isJst z)
                                   then I (fz+1) else Jst (toEnum (fz+1))             , Jst minBound)
  | otherwise = (pre y, suc z)


succ3 :: ( Enum a, Enum b, Enum c, Eq a, Eq b, Eq c, Bounded a, Bounded b, Bounded c) =>
         Int -> Bool -> ((J a,J b),J c) -> ((J a,J b),J c)
succ3 fz start ((x,y),z)
  | not (minB y) && (minB z) = ((x, (pre y)), suc z)
  |     (minB y) && (minB z) = (succ2 fz False (x,y), z)
  | not (minB y) && not (minB z) = ((x, (pre y)), if start then suc z else v (fz+1) z)
  | (minB y) && not (minB z) = (succ2 fz False (x, v fz z), Jst minBound)


succ4 :: ( Enum a, Enum b, Enum c, Enum d, Eq a, Eq b, Eq c, Eq d,
           Bounded a, Bounded b, Bounded c, Bounded d) =>
         Int -> Bool -> (((J a,J b),J c),J d) -> (((J a,J b),J c),J d)
succ4 fz start ((x,y),z)
  | not (minB y) && (minB z) = ((x, (pre y)), suc z)
  |     (minB y) && (minB z) = (succ3 fz False (x,y), z)
  | not (minB y) && not (minB z) = ((x, (pre y)), if start then suc z else v (fz+1) z)
  | (minB y) && not (minB z) = (succ3 fz False (x, v fz z), Jst minBound)


succ5 :: ( Enum a, Enum b, Enum c, Enum d,Enum e, Eq a, Eq b, Eq c, Eq d, Eq e,
           Bounded a, Bounded b, Bounded c, Bounded d, Bounded e) =>
         Int -> Bool -> ((((J a,J b),J c),J d),J e) -> ((((J a,J b),J c),J d),J e)
succ5 fz start ((x,y),z)
  | not (minB y) && (minB z) = ((x, (pre y)), suc z)
  |     (minB y) && (minB z) = (succ4 fz False (x,y), z)
  | not (minB y) && not (minB z) = ((x, (pre y)), if start then suc z else v (fz+1) z)
  | (minB y) && not (minB z) = (succ4 fz False (x, v fz z), Jst minBound)


succ6 :: ( Enum a, Enum b,Enum c, Enum d, Enum e, Enum f, Eq a, Eq b, Eq c, Eq d,
           Eq e, Eq f, Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f) =>
         Int -> Bool -> (((((J a,J b),J c),J d),J e),J f) -> (((((J a,J b),J c),J d),J e),J f)
succ6 fz start ((x,y),z)
  | not (minB y) && (minB z) = ((x, (pre y)), suc z)
  |     (minB y) && (minB z) = (succ5 fz False (x,y), z)
  | not (minB y) && not (minB z) = ((x, (pre y)), if start then suc z else v (fz+1) z)
  | (minB y) && not (minB z) = (succ5 fz False (x, v fz z), Jst minBound)


succ7 :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g,
            Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Bounded a, Bounded b,
            Bounded c, Bounded d, Bounded e, Bounded f, Bounded g ) =>
         Int -> Bool -> ((((((J a,J b),J c),J d),J e),J f),J g) -> ((((((J a,J b),J c),J d),J e),J f),J g)
succ7 fz start ((x,y),z)
  | not (minB y) && (minB z) = ((x, (pre y)), suc z)
  |     (minB y) && (minB z) = (succ6 fz False (x,y), z)
  | not (minB y) && not (minB z) = ((x, (pre y)), if start then suc z else v (fz+1) z)
  | (minB y) && not (minB z) = (succ6 fz False (x, v fz z), Jst minBound)


succ8 :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h,
            Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Bounded a, Bounded b,
            Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h ) =>
         Int -> Bool -> (((((((J a,J b),J c),J d),J e),J f),J g),J h)
                     -> (((((((J a,J b),J c),J d),J e),J f),J g),J h)
succ8 fz start ((x,y),z)
  | not (minB y) && (minB z) = ((x, (pre y)), suc z)
  |     (minB y) && (minB z) = (succ7 fz False (x,y), z)
  | not (minB y) && not (minB z) = ((x, (pre y)), if start then suc z else v (fz+1) z)
  | (minB y) && not (minB z) = (succ7 fz False (x, v fz z), Jst minBound)


succ9 :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h, Enum i,
            Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Bounded a, Bounded b,
            Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i ) =>
          Int -> Bool -> ((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i)
                      -> ((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i)
succ9 fz start ((x,y),z)
  | not (minB y) && (minB z) = ((x, (pre y)), suc z)
  |     (minB y) && (minB z) = (succ8 fz False (x,y), z)
  | not (minB y) && not (minB z) = ((x, (pre y)), if start then suc z else v (fz+1) z)
  | (minB y) && not (minB z) = (succ8 fz False (x, v fz z), Jst minBound)


succ10 :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h, Enum i, Enum j,
            Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Bounded a, Bounded b,
            Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j ) =>
          Int -> Bool -> (((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j)
                      -> (((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j)
succ10 fz start ((x,y),z)
  | not (minB y) && (minB z) = ((x, (pre y)), suc z)
  |     (minB y) && (minB z) = (succ9 fz False (x,y), z)
  | not (minB y) && not (minB z) = ((x, (pre y)), if start then suc z else v (fz+1) z)
  | (minB y) && not (minB z) = (succ9 fz False (x, v fz z), Jst minBound)


succ11 :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h, Enum i, Enum j, Enum k,
            Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Bounded a, Bounded b,
            Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k ) =>
          Int -> Bool -> ((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k)
                      -> ((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k)
succ11 fz start ((x,y),z)
  | not (minB y) && (minB z) = ((x, (pre y)), suc z)
  |     (minB y) && (minB z) = (succ10 fz False (x,y), z)
  | not (minB y) && not (minB z) = ((x, (pre y)), if start then suc z else v (fz+1) z)
  | (minB y) && not (minB z) = (succ10 fz False (x, v fz z), Jst minBound)


succ12 :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h, Enum i, Enum j, Enum k, Enum l,
            Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Bounded a, Bounded b,
            Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k, Bounded l ) =>
      Int -> Bool -> (((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k), J l)
                  -> (((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k), J l)
succ12 fz start ((x,y),z)
  | not (minB y) && (minB z) = ((x, (pre y)), suc z)
  |     (minB y) && (minB z) = (succ11 fz False (x,y), z)
  | not (minB y) && not (minB z) = ((x, (pre y)), if start then suc z else v (fz+1) z)
  | (minB y) && not (minB z) = (succ11 fz False (x, v fz z), Jst minBound)


succ13 :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h, Enum i, Enum j, Enum k, Enum l, Enum m,
            Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Bounded a, Bounded b, Bounded c,
            Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m) =>
      Int -> Bool -> ((((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k),J l),J m)
                  -> ((((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k),J l),J m)
succ13 fz start ((x,y),z)
  | not (minB y) && (minB z) = ((x, (pre y)), suc z)
  |     (minB y) && (minB z) = (succ12 fz False (x,y), z)
  | not (minB y) && not (minB z) = ((x, (pre y)), if start then suc z else v (fz+1) z)
  | (minB y) && not (minB z) = (succ12 fz False (x, v fz z), Jst minBound)


succ14 :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h, Enum i, Enum j, Enum k, Enum l, Enum m, Enum n,
            Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n,
            Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, 
            Bounded j, Bounded k, Bounded l, Bounded m, Bounded n) =>
      Int -> Bool -> (((((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k),J l),J m),J n)
                  -> (((((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k),J l),J m),J n)
succ14 fz start ((x,y),z)
  | not (minB y) && (minB z) = ((x, (pre y)), suc z)
  |     (minB y) && (minB z) = (succ13 fz False (x,y), z)
  | not (minB y) && not (minB z) = ((x, (pre y)), if start then suc z else v (fz+1) z)
  | (minB y) && not (minB z) = (succ13 fz False (x, v fz z), Jst minBound)


succ15 :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h, Enum i, Enum j, Enum k, Enum l, 
            Enum m, Enum n, Enum o,
            Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o,
            Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, 
            Bounded j, Bounded k, Bounded l, Bounded m, Bounded n, Bounded o) =>
      Int -> Bool -> ((((((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k),J l),J m),J n),J o)
                  -> ((((((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k),J l),J m),J n),J o)
succ15 fz start ((x,y),z)
  | not (minB y) && (minB z) = ((x, (pre y)), suc z)
  |     (minB y) && (minB z) = (succ14 fz False (x,y), z)
  | not (minB y) && not (minB z) = ((x, (pre y)), if start then suc z else v (fz+1) z)
  | (minB y) && not (minB z) = (succ14 fz False (x, v fz z), Jst minBound)


to2Tuple  (Jst a, Jst b) =
          (a,b)
to3Tuple  ((Jst a, Jst b), Jst c) =
          (a,b,c)
to4Tuple  (((Jst a,Jst b),Jst c),Jst d) =
          (a,b,c,d)
to5Tuple  ((((Jst a,Jst b),Jst c),Jst d),Jst e) =
          (a,b,c,d,e)
to6Tuple  (((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f) =
          (a,b,c,d,e,f)
to7Tuple  ((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g) =
          (a,b,c,d,e,f,g)
to8Tuple  (((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g),Jst h) =
          (a,b,c,d,e,f,g,h)
to9Tuple  ((((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g),Jst h),Jst i) =
          (a,b,c,d,e,f,g,h,i)
to10Tuple (((((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g),Jst h),Jst i),Jst j) =
          (a,b,c,d,e,f,g,h,i,j)
to11Tuple ((((((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g),Jst h),Jst i),Jst j),Jst k) =
          (a,b,c,d,e,f,g,h,i,j,k)
to12Tuple (((((((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g),Jst h),Jst i),Jst j),Jst k),Jst l) =
          (a,b,c,d,e,f,g,h,i,j,k,l)
to13Tuple ((((((((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g),Jst h),Jst i),Jst j),Jst k),Jst l),Jst m) =
          (a,b,c,d,e,f,g,h,i,j,k,l,m)
to14Tuple (((((((((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g),Jst h),Jst i),Jst j),Jst k),Jst l),Jst m),Jst n) =
          (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
to15Tuple
  ((((((((((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g),Jst h),Jst i),Jst j),Jst k),Jst l),Jst m),Jst n),Jst o) =
          (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)


from3Tuple (a,b,c) = ((Jst a, Jst b), Jst c)
from4Tuple (a,b,c,d) = (((Jst a,Jst b),Jst c),Jst d)
from5Tuple (a,b,c,d,e) = ((((Jst a,Jst b),Jst c),Jst d),Jst e)
from6Tuple (a,b,c,d,e,f) = (((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f)
from7Tuple (a,b,c,d,e,f,g) = ((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g)
from8Tuple (a,b,c,d,e,f,g,h) = (((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g),Jst h)
from9Tuple (a,b,c,d,e,f,g,h,i) = ((((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g),Jst h),Jst i)
from10Tuple (a,b,c,d,e,f,g,h,i,j) = (((((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g),Jst h),Jst i),Jst j)
from11Tuple (a,b,c,d,e,f,g,h,i,j,k) = ((((((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g),Jst h),Jst i),Jst j),Jst k)
from12Tuple (a,b,c,d,e,f,g,h,i,j,k,l) =
            (((((((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g),Jst h),Jst i),Jst j),Jst k),Jst l)
from13Tuple (a,b,c,d,e,f,g,h,i,j,k,l,m) =
            ((((((((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g),Jst h),Jst i),Jst j),Jst k),Jst l),Jst m)
from14Tuple (a,b,c,d,e,f,g,h,i,j,k,l,m,n) =
            (((((((((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g),Jst h),Jst i),Jst j),Jst k),Jst l),Jst m),Jst n)
from15Tuple (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) =
  ((((((((((((((Jst a,Jst b),Jst c),Jst d),Jst e),Jst f),Jst g),Jst h),Jst i),Jst j),Jst k),Jst l),Jst m),Jst n),Jst o)

--------------------------------------------------------------------------------
instance (Enum a, Enum b, Eq a, Eq b, Bounded a, Bounded b) => Enum (a, b) where 
--------------------------------------------------------------------------------
-- Enum instance for 2-tuples

  succ (x,y) | (x,y) == maxBound
                = error "Enum.succ{(a,b)}: tried to take `succ' of maxBound"
             | otherwise = to2Tuple $ succ2 (fromEnum y) True (Jst x,Jst y)


  pred (x,y) | (x,y) == (minBound,minBound) = error "Enum.pred{(a,b)}: tried to take `pred' of minBound"
             |    y  ==           minBound  = (minBound, toEnum (fx-1))
             | otherwise                    = (succ x  , pred y)
    where
      fx = fromEnum x

  toEnum n = (\[a,b] -> (toEnum a, toEnum b)) (te 2 n)

  fromEnum (a,b) = fe [fromEnum a, fromEnum b]

  enumFrom t2 | t2 == (maxBound,maxBound) = [(maxBound,maxBound)]
              | otherwise                 = t2 : (enumFrom (succ t2))

  enumFromTo t0 t1 = take l $ enumFrom t0
    where l = (fromEnum t1) - (fromEnum t0) + 1

------------------------------------------------------------------
instance (Enum a, Enum b, Enum c,
          Eq a, Eq b, Eq c,
          Bounded a, Bounded b, Bounded c) => Enum (a, b, c) where
------------------------------------------------------------------
-- 3
  succ (x,y,z) | (x,y,z) == maxBound
                 = error "Enum.succ{(x,y,z)}: tried to take `succ' of maxBound"
               | otherwise = to3Tuple $
                             findNext (fromEnum (mb (Jst x)), fromEnum (mb (Jst y)), fromEnum (mb (Jst z))) $
                             succ3 (fromEnum z) True (from3Tuple (x,y,z))
   where
    findNext :: ( Enum a, Enum b, Enum c, Eq a, Eq b, Eq c, Bounded a, Bounded b, Bounded c)
                => (Int,Int,Int) -> ((J a, J b), J c) -> ((J a, J b), J c)
    findNext (bx,by,bz) ((x,y),z) = if (not (isJst x)) || (not (isJst y)) || (not (isJst z))
                               then findNext (bx,by,bz) $ toBounded (bx,by,bz) $ succ3 (getInt z) True ((x,y),z)
                               else ((x,y),z)
    toBounded (bx,by,bz) ((jx,jy),jz) = ( ( ib jx bx, ib jy by ), ib jz bz )


  pred (x,y,z) = if z == minBound then
                   if y == minBound then
                     if x == minBound then error "Enum.pred{(x,y,z)}: tried to take `pred' of minBound"
                                      else (minBound, minBound, toEnum (fx-1)) -- (fy,fz) == (0,0)
                                    else   (succ x  , minBound, toEnum (fy-1)) --     fz  ==    0
                                  else     (x       , succ y  , pred z       )
    where
      fx = fromEnum x
      fy = fromEnum y

  toEnum n = (\[a,b,c] -> (toEnum a, toEnum b, toEnum c)) (te 3 n)

  fromEnum (a,b,c) = fe [fromEnum a, fromEnum b, fromEnum c]

  enumFrom t3 | t3 == (maxBound,maxBound,maxBound) = [(maxBound,maxBound,maxBound)]
              | otherwise                          = t3 : (enumFrom (succ t3))

  enumFromTo t0 t1 = take l $ enumFrom t0
    where l = (fromEnum t1) - (fromEnum t0) + 1

---------------------------------------------------------------------------------
instance (Enum a, Enum b, Enum c, Enum d,
          Eq a, Eq b, Eq c, Eq d,
          Bounded a, Bounded b, Bounded c, Bounded d) => Enum (a, b, c, d) where
--------------------------------------------------------------------------------
-- 4
  succ (a,b,c,d) | (a,b,c,d) == maxBound
                   = error "Enum.succ{(a,b,c,d)}: tried to take `succ' of maxBound"
                 | otherwise = to4Tuple $
                               findNext (fromEnum (mb (Jst a)), fromEnum (mb (Jst b)),
                                         fromEnum (mb (Jst c)), fromEnum (mb (Jst c))) $
                               succ4 (fromEnum d) True (from4Tuple (a,b,c,d))
   where
    findNext :: ( Enum a, Enum b, Enum c, Enum d, Eq a, Eq b, Eq c, Eq d, Bounded a, Bounded b, Bounded c, Bounded d)
                => (Int,Int,Int,Int) -> (((J a, J b), J c), J d) -> (((J a, J b), J c), J d)
    findNext (ba,bb,bc,bd) (((a,b),c),d) = if (not (isJst a)) || (not (isJst b)) || (not (isJst c)) || (not (isJst d))
                               then findNext (ba,bb,bc,bd) $ toBounded (ba,bb,bc,bd) $ succ4 (getInt d) True (((a,b),c),d)
                               else (((a,b),c),d)
    toBounded (ba,bb,bc,bd) (((ja,jb),jc),jd) = (((ib ja ba,ib jb bb),ib jc bc), ib jd bd)


  pred (a,b,c,d) =
      if d==minBound then
        if c==minBound then
          if fb==minBound then
            if fa==minBound then error "Enum.pred{(a,b,c,d)}: tried to take `pred' of minBound"
                            else (minBound, minBound, minBound, toEnum (fa-1)) -- (b,c,d) == (0,0,0)
                          else   (succ a  , minBound, minBound, toEnum (fb-1)) --   (c,d) ==   (0,0)
                        else     (a       , succ b  , minBound, toEnum (fc-1)) --       d ==      0
                      else       (a       , b       , succ c  , pred d       )
    where
      fa = fromEnum a
      fb = fromEnum b
      fc = fromEnum c

  toEnum n = (\[a,b,c,d] -> (toEnum a, toEnum b, toEnum c, toEnum d)) (te 4 n)

  fromEnum (a,b,c,d) = fe [fromEnum a, fromEnum b, fromEnum c, fromEnum d]

  enumFrom t4 | t4 == (maxBound,maxBound,maxBound,maxBound) = [(maxBound,maxBound,maxBound,maxBound)]
              | otherwise                                   = t4 : (enumFrom (succ t4))

  enumFromTo t0 t1 = take l $ enumFrom t0
    where l = (fromEnum t1) - (fromEnum t0) + 1

--------------------------------------------------------------------------------------------------------
instance (Enum a, Enum b, Enum c, Enum d, Enum e,
          Eq a, Eq b, Eq c, Eq d, Eq e,
          Bounded a, Bounded b, Bounded c, Bounded d, Bounded e) => Enum (a, b, c, d, e) where
--------------------------------------------------------------------------------------------------------
-- 5
  succ (a,b,c,d,e) | (a,b,c,d,e) == maxBound
                     = error "Enum.succ{(a,b,c,d,e)}: tried to take `succ' of maxBound"
                   | otherwise = to5Tuple $
                               findNext (fromEnum (mb (Jst a)), fromEnum (mb (Jst b)), fromEnum (mb (Jst c)),
                                         fromEnum (mb (Jst d)), fromEnum (mb (Jst e))) $
                               succ5 (fromEnum e) True (from5Tuple (a,b,c,d,e))
   where
    findNext :: ( Enum a, Enum b, Enum c, Enum d, Enum e,
                  Eq a, Eq b, Eq c, Eq d, Eq e, Bounded a, Bounded b, Bounded c, Bounded d, Bounded e)
                => (Int,Int,Int,Int,Int) -> ((((J a,J b),J c),J d),J e) -> ((((J a,J b),J c),J d),J e)
    findNext (ba,bb,bc,bd,be) ((((a,b),c),d),e) =
       if (not (isJst a)) || (not (isJst b)) || (not (isJst c)) || (not (isJst d)) || (not (isJst e))
       then findNext (ba,bb,bc,bd,be) $ toBounded (ba,bb,bc,bd,be) $ succ5 (getInt e) True ((((a,b),c),d),e)
       else ((((a,b),c),d),e)
    toBounded (ba,bb,bc,bd,be) ((((ja,jb),jc),jd),je) = ((((ib ja ba,ib jb bb),ib jc bc), ib jd bd), ib je be)


  pred (a,b,c,d,e) =
       if e == minBound then
         if d == minBound then
           if c == minBound then
             if b == minBound then
               if a == minBound then error "Enum.pred{(a,b,c,d,e)}: tried to take `pred' of minBound"
                                else (minBound, minBound, minBound, minBound, toEnum (fa-1))
                              else   (succ a  , minBound, minBound, minBound, toEnum (fb-1))
                            else     (a       , succ b  , minBound, minBound, toEnum (fc-1))
                          else       (a       , b       , succ c  , minBound, toEnum (fd-1))
                        else         (a       , b       , c       , succ d  , pred e)
    where

      fa = fromEnum a
      fb = fromEnum b
      fc = fromEnum c
      fd = fromEnum d

  toEnum n = (\[a,b,c,d,e] -> (toEnum a, toEnum b, toEnum c, toEnum d, toEnum e)) (te 5 n)

  fromEnum (a,b,c,d,e) = fe [fromEnum a, fromEnum b, fromEnum c, fromEnum d, fromEnum e]

  enumFrom t5 | t5 == (maxBound,maxBound,maxBound,maxBound,maxBound)
                   = [(maxBound,maxBound,maxBound,maxBound,maxBound)]
              | otherwise  = t5 : (enumFrom (succ t5))

  enumFromTo t0 t1 = take l $ enumFrom t0
    where l = (fromEnum t1) - (fromEnum t0) + 1

------------------------------------------------------------------------------------------------------------
instance (Enum a, Enum b, Enum c, Enum d, Enum e, Enum f,
          Eq a, Eq b, Eq c, Eq d, Eq e, Eq f,
          Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f) => Enum (a, b, c, d, e, f) where 
------------------------------------------------------------------------------------------------------------
-- 6
  succ (a,b,c,d,e,f) | (a,b,c,d,e,f) == maxBound
                       = error "Enum.succ{(a,b,c,d,e,f)}: tried to take `succ' of maxBound"
                     | otherwise = to6Tuple $
                               findNext (fromEnum (mb (Jst a)), fromEnum (mb (Jst b)), fromEnum (mb (Jst c)),
                                         fromEnum (mb (Jst d)), fromEnum (mb (Jst e)), fromEnum (mb (Jst f))) $
                               succ6 (fromEnum f) True (from6Tuple (a,b,c,d,e,f))
   where
    findNext :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f,
                  Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, 
                  Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f)
                => (Int,Int,Int,Int,Int,Int) -> (((((J a,J b),J c),J d),J e),J f) -> (((((J a,J b),J c),J d),J e),J f)
    findNext (ba,bb,bc,bd,be,bf) (((((a,b),c),d),e),f) =
       if (not (isJst a)) || (not (isJst b)) || (not (isJst c)) ||
          (not (isJst d)) || (not (isJst e)) || (not (isJst f))
       then findNext (ba,bb,bc,bd,be,bf) $ toBounded (ba,bb,bc,bd,be,bf) $ succ6 (getInt f) True (((((a,b),c),d),e),f)
       else (((((a,b),c),d),e),f)
    toBounded (ba,bb,bc,bd,be,bf) (((((ja,jb),jc),jd),je),jf) =
       (((((ib ja ba,ib jb bb),ib jc bc), ib jd bd), ib je be), ib jf bf)


  pred (a,b,c,d,e,f) =
       if f == minBound then
         if e == minBound then
           if d == minBound then
             if c == minBound then
               if b == minBound then
                 if a == minBound then error "Enum.pred{(a,b,c,d,e,f)}: tried to take `pred' of minBound"
                                  else (minBound, minBound, minBound, minBound, minBound, toEnum (fa-1))
                                else   (succ a  , minBound, minBound, minBound, minBound, toEnum (fb-1))
                              else     (a       , succ b  , minBound, minBound, minBound, toEnum (fc-1))
                            else       (a       , b       , succ c  , minBound, minBound, toEnum (fd-1))
                          else         (a       , b       , c       , succ d  , minBound, toEnum (fe-1))
                        else           (a       , b       , c       , d       , succ e  , pred f)
    where
      fa = fromEnum a
      fb = fromEnum b
      fc = fromEnum c

      fd = fromEnum d
      fe = fromEnum e

  toEnum n = (\[a,b,c,d,e,f] -> (toEnum a, toEnum b, toEnum c, toEnum d, toEnum e, toEnum f)) (te 6 n)

  fromEnum (a,b,c,d,e,f) = fe [fromEnum a, fromEnum b, fromEnum c,
                               fromEnum d, fromEnum e, fromEnum f]

  enumFrom t6 | t6 == (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound) =
                     [(maxBound,maxBound,maxBound,maxBound,maxBound,maxBound)]
              | otherwise                                                     = t6 : (enumFrom (succ t6))

  enumFromTo t0 t1 = take l $ enumFrom t0
    where l = (fromEnum t1) - (fromEnum t0) + 1

--------------------------------------------------------------------------------------------------------------------
instance (Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g,
          Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
          Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g) => Enum (a,b,c,d,e,f,g) where
--------------------------------------------------------------------------------------------------------------------
-- 7
  succ (a,b,c,d,e,f,g) | (a,b,c,d,e,f,g) == maxBound
                         = error "Enum.succ{(a,b,c,d,e,f,g)}: tried to take `succ' of maxBound"
                       | otherwise = to7Tuple $
                            findNext (fromEnum (mb (Jst a)), fromEnum (mb (Jst b)), fromEnum (mb (Jst c)),
                                      fromEnum (mb (Jst d)), fromEnum (mb (Jst d)), fromEnum (mb (Jst e)),
                                      fromEnum (mb (Jst f))) $
                           succ7 (fromEnum g) True (from7Tuple (a,b,c,d,e,f,g))
   where
    findNext :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f,Enum g,
                  Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
                  Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g)
                => (Int,Int,Int,Int,Int,Int,Int) -> ((((((J a,J b),J c),J d),J e),J f),J g) ->
                                                    ((((((J a,J b),J c),J d),J e),J f),J g)
    findNext (ba,bb,bc,bd,be,bf,bg) ((((((a,b),c),d),e),f),g) =
       if (not (isJst a)) || (not (isJst b)) || (not (isJst c)) ||
          (not (isJst d)) || (not (isJst e)) || (not (isJst f)) || (not (isJst g))
       then findNext (ba,bb,bc,bd,be,bf,bg) $
                     toBounded (ba,bb,bc,bd,be,bf,bg) $ succ7 (getInt g) True ((((((a,b),c),d),e),f),g)
       else ((((((a,b),c),d),e),f),g)
    toBounded (ba,bb,bc,bd,be,bf,bg) ((((((ja,jb),jc),jd),je),jf),jg) =
       ((((((ib ja ba,ib jb bb),ib jc bc), ib jd bd), ib je be), ib jf bf), ib jg bg)


  pred (a,b,c,d,e,f,g) =
    if g == minBound then
      if f == minBound then
        if e == minBound then
          if d == minBound then
            if c == minBound then
              if b == minBound then
                if a == minBound then error "Enum.pred{(a,b,c,d,e,f,g)}: tried to take `pred' of minBound"
                                 else (minBound, minBound, minBound, minBound, minBound, minBound, toEnum (fa-1))
                               else   (succ a  , minBound, minBound, minBound, minBound, minBound, toEnum (fb-1))
                             else     (a       , succ b  , minBound, minBound, minBound, minBound, toEnum (fc-1))
                           else       (a       , b       , succ c  , minBound, minBound, minBound, toEnum (fd-1))
                         else         (a       , b       , c       , succ d  , minBound, minBound, toEnum (fe-1))
                       else           (a       , b       , c       , d       , succ e  , minBound, toEnum (ff-1))
                     else             (a       , b       , c       , d       , e       , succ f  , pred g)

    where
      fa = fromEnum a
      fb = fromEnum b
      fc = fromEnum c
      fd = fromEnum d
      fe = fromEnum e
      ff = fromEnum f

  enumFrom t7 | t7 == (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound) =
                     [(maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound)]
              | otherwise = t7 : (enumFrom (succ t7))

  enumFromTo t0 t1 = take l $ enumFrom t0
    where l = (fromEnum t1) - (fromEnum t0) + 1

  toEnum n = (\[a,b,c,d,e,f,g] ->
              (toEnum a, toEnum b, toEnum c, toEnum d, toEnum e, toEnum f, toEnum g)) (te 7 n)

  fromEnum (a,b,c,d,e,f,g) = fe [fromEnum a, fromEnum b, fromEnum c,
                                 fromEnum d, fromEnum e, fromEnum f, fromEnum g]

-------------------------------------------------------------------------------------
instance (Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h,
          Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h,
          Bounded a, Bounded b, Bounded c, Bounded d,
          Bounded e, Bounded f, Bounded g, Bounded h) => Enum (a,b,c,d,e,f,g,h) where
-------------------------------------------------------------------------------------
-- 8
  succ (a,b,c,d,e,f,g,h) | (a,b,c,d,e,f,g,h) == maxBound
                           = error "Enum.succ{(a,b,c,d,e,f,g,h)}: tried to take `succ' of maxBound"
                         | otherwise = to8Tuple $
                            findNext (fromEnum (mb (Jst a)), fromEnum (mb (Jst b)), fromEnum (mb (Jst c)),
                                      fromEnum (mb (Jst d)), fromEnum (mb (Jst e)), fromEnum (mb (Jst f)),
                                      fromEnum (mb (Jst g)), fromEnum (mb (Jst h)) ) $
                           succ8 (fromEnum h) True (from8Tuple (a,b,c,d,e,f,g,h))
   where
    findNext :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h,
                  Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h,
                  Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h)
                => (Int,Int,Int,Int,Int,Int,Int,Int) -> (((((((J a,J b),J c),J d),J e),J f),J g),J h) ->
                                                        (((((((J a,J b),J c),J d),J e),J f),J g),J h)
    findNext (ba,bb,bc,bd,be,bf,bg,bh) (((((((a,b),c),d),e),f),g),h) =
       if (not (isJst a)) || (not (isJst b)) || (not (isJst c)) || (not (isJst d)) ||
          (not (isJst e)) || (not (isJst f)) || (not (isJst g)) || (not (isJst h))
       then findNext (ba,bb,bc,bd,be,bf,bg,bh) $
                     toBounded (ba,bb,bc,bd,be,bf,bg,bh) $ succ8 (getInt h) True (((((((a,b),c),d),e),f),g),h)
       else (((((((a,b),c),d),e),f),g),h)
    toBounded (ba,bb,bc,bd,be,bf,bg,bh) (((((((ja,jb),jc),jd),je),jf),jg),jh) =
       (((((((ib ja ba,ib jb bb),ib jc bc), ib jd bd), ib je be), ib jf bf), ib jg bg), ib jh bh)


  pred (a,b,c,d,e,f,g,h) =
   if f == minBound then
    if g == minBound then
     if f == minBound then
      if e == minBound then
       if d == minBound then
        if c == minBound then
         if b == minBound then
          if a == minBound then error "Enum.pred{(a,b,c,d,e,f,g,h)}: tried to take `pred' of minBound"
                           else (minBound, minBound, minBound, minBound, minBound, minBound, minBound, toEnum (fa-1))
                          else  (succ a  , minBound, minBound, minBound, minBound, minBound, minBound, toEnum (fb-1))
                         else   ( a      , succ b  , minBound, minBound, minBound, minBound, minBound, toEnum (fc-1))
                        else    ( a      , b       , succ c  , minBound, minBound, minBound, minBound, toEnum (fd-1))
                       else     ( a      , b       , c       , succ d  , minBound, minBound, minBound, toEnum (fe-1))
                      else      ( a      , b       , c       , d       , succ e  , minBound, minBound, toEnum (ff-1))
                     else       ( a      , b       , c       , d       , e       , succ f  , minBound, toEnum (fg-1))
                    else        ( a      , b       , c       , d       , e       , f       , succ g  , pred h)
    where
      fa = fromEnum a
      fb = fromEnum b
      fc = fromEnum c
      fd = fromEnum d
      fe = fromEnum e
      ff = fromEnum f
      fg = fromEnum g
      fh = fromEnum h

  enumFrom t8 | t8 == (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound) =
                     [(maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound)]
              | otherwise = t8 : (enumFrom (succ t8))

  enumFromTo t0 t1 = take l $ enumFrom t0
    where l = (fromEnum t1) - (fromEnum t0) + 1

  toEnum n = (\[a,b,c,d,e,f,g,h] ->
              (toEnum a, toEnum b, toEnum c, toEnum d,
               toEnum e, toEnum f, toEnum g, toEnum h)) (te 8 n)

  fromEnum (a,b,c,d,e,f,g,h) = fe [fromEnum a, fromEnum b, fromEnum c, fromEnum d,
                                   fromEnum e, fromEnum f, fromEnum g, fromEnum h]

---------------------------------------------------------------------------------------------------
instance (Enum a,Enum b,Enum c,Enum d,Enum e,Enum f,Enum g,Enum h,Enum i,
          Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
          Bounded a, Bounded b, Bounded c, Bounded d,
          Bounded e, Bounded f, Bounded g, Bounded h, Bounded i)  => Enum (a,b,c,d,e,f,g,h,i) where
---------------------------------------------------------------------------------------------------
-- 9
  succ (a,b,c,d,e,f,g,h,i) | (a,b,c,d,e,f,g,h,i) == maxBound
                             = error "Enum.succ{(a,b,c,d,e,f,g,h,i)}: tried to take `succ' of maxBound"
                           | otherwise = to9Tuple $
                            findNext (fromEnum (mb (Jst a)), fromEnum (mb (Jst b)), fromEnum (mb (Jst c)),
                                      fromEnum (mb (Jst d)), fromEnum (mb (Jst e)), fromEnum (mb (Jst f)),
                                      fromEnum (mb (Jst g)), fromEnum (mb (Jst h)), fromEnum (mb (Jst i))) $
                           succ9 (fromEnum i) True (from9Tuple (a,b,c,d,e,f,g,h,i))
   where
    findNext :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h, Enum i,
                  Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
                  Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i)
                => (Int,Int,Int,Int,Int,Int,Int,Int,Int) ->
                   ((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i) ->
                   ((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i)
    findNext (ba,bb,bc,bd,be,bf,bg,bh,bi) ((((((((a,b),c),d),e),f),g),h),i) =
       if (not (isJst a)) || (not (isJst b)) || (not (isJst c)) || (not (isJst d)) ||
          (not (isJst e)) || (not (isJst f)) || (not (isJst g)) || (not (isJst h)) || (not (isJst i))
       then findNext (ba,bb,bc,bd,be,bf,bg,bh,bi) $
                     toBounded (ba,bb,bc,bd,be,bf,bg,bh,bi) $ succ9 (getInt i) True ((((((((a,b),c),d),e),f),g),h),i)
       else ((((((((a,b),c),d),e),f),g),h),i)
    toBounded (ba,bb,bc,bd,be,bf,bg,bh,bi) ((((((((ja,jb),jc),jd),je),jf),jg),jh),ji) =
       ((((((((ib ja ba,ib jb bb),ib jc bc), ib jd bd), ib je be), ib jf bf), ib jg bg), ib jh bh), ib ji bi)


  pred (a,b,c,d,e,f,g,h,i) =
    if i==minBound then
    if h==minBound then
    if g==minBound then
    if f==minBound then
    if e==minBound then
    if d==minBound then
    if c==minBound then
    if b==minBound then
    if a==minBound then error "Enum.pred{(a,b,c,d,e,f,g,h,i)}: tried to take `pred' of minBound"
    else ( minBound, minBound, minBound, minBound, minBound, minBound, minBound, minBound, toEnum (fa-1))
    else ( succ a  , minBound, minBound, minBound, minBound, minBound, minBound, minBound, toEnum (fb-1))
    else ( a       , succ b  , minBound, minBound, minBound, minBound, minBound, minBound, toEnum (fc-1))
    else ( a       , b       , succ c  , minBound, minBound, minBound, minBound, minBound, toEnum (fd-1))
    else ( a       , b       , c       , succ d  , minBound, minBound, minBound, minBound, toEnum (fe-1))
    else ( a       , b       , c       , d       , succ e  , minBound, minBound, minBound, toEnum (ff-1))
    else ( a       , b       , c       , d       , e       , succ f  , minBound, minBound, toEnum (fg-1))
    else ( a       , b       , c       , d       , e       , f       , succ g  , minBound, toEnum (fh-1))
    else ( a       , b       , c       , d       , e       , f       ,      g  ,  succ h , pred i)
    where
      fa = fromEnum a
      fb = fromEnum b
      fc = fromEnum c
      fd = fromEnum d
      fe = fromEnum e
      ff = fromEnum f
      fg = fromEnum g
      fh = fromEnum h

  enumFrom t9 | t9 == (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound) =
                     [(maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound)]
              | otherwise = t9 : (enumFrom (succ t9))

  enumFromTo t0 t1 = take l $ enumFrom t0
    where l = (fromEnum t1) - (fromEnum t0) + 1

  toEnum n = (\[a,b,c,d,e,f,g,h,i] ->
              (toEnum a, toEnum b, toEnum c, toEnum d,
               toEnum e, toEnum f, toEnum g, toEnum h, toEnum i)) (te 9 n)

  fromEnum (a,b,c,d,e,f,g,h,i) = fe [fromEnum a, fromEnum b, fromEnum c, fromEnum d,
                                     fromEnum e, fromEnum f, fromEnum g, fromEnum h, fromEnum i]

----------------------------------------------------------------------------------------------------
instance (Enum a,Enum b,Enum c,Enum d,Enum e,Enum f,Enum g,Enum h,Enum i,Enum j,
          Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j,
          Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, 
          Bounded f, Bounded g, Bounded h, Bounded i, Bounded j) => Enum (a,b,c,d,e,f,g,h,i,j) where
----------------------------------------------------------------------------------------------------
-- 10
  succ (a,b,c,d,e,f,g,h,i,j)
     | (a,b,c,d,e,f,g,h,i,j) == maxBound
        = error "Enum.succ{(a,b,c,d,e,f,g,h,i,j)}: tried to take `succ' of maxBound"
     | otherwise = to10Tuple $
                      findNext (fromEnum (mb (Jst a)), fromEnum (mb (Jst b)), fromEnum (mb (Jst c)),
                                fromEnum (mb (Jst d)), fromEnum (mb (Jst e)), fromEnum (mb (Jst f)),
                                fromEnum (mb (Jst g)), fromEnum (mb (Jst h)), fromEnum (mb (Jst i)),
                                fromEnum (mb (Jst j)) ) $
                      succ10 (fromEnum j) True (from10Tuple (a,b,c,d,e,f,g,h,i,j))
   where
    findNext :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h, Enum i, Enum j,
                  Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j,
                  Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
                  Bounded h, Bounded i, Bounded j)
                => (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) ->
                   (((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j) ->
                   (((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j)
    findNext (ba,bb,bc,bd,be,bf,bg,bh,bi,bj) (((((((((a,b),c),d),e),f),g),h),i),j) =
       if (not (isJst a)) || (not (isJst b)) || (not (isJst c)) || (not (isJst d)) || (not (isJst e)) ||
          (not (isJst f)) || (not (isJst g)) || (not (isJst h)) || (not (isJst i)) || (not (isJst j))
       then findNext (ba,bb,bc,bd,be,bf,bg,bh,bi,bj) $
            toBounded (ba,bb,bc,bd,be,bf,bg,bh,bi,bj) $ succ10 (getInt j) True (((((((((a,b),c),d),e),f),g),h),i),j)
       else (((((((((a,b),c),d),e),f),g),h),i),j)
    toBounded (ba,bb,bc,bd,be,bf,bg,bh,bi,bj) (((((((((ja,jb),jc),jd),je),jf),jg),jh),ji),jj) =
       (((((((((ib ja ba,ib jb bb),ib jc bc), ib jd bd), ib je be), ib jf bf), ib jg bg), ib jh bh), ib ji bi), ib jj bj)


  pred (a,b,c,d,e,f,g,h,i,j) =
   if j == minBound then
   if i == minBound then
   if h == minBound then
   if g == minBound then
   if f == minBound then
   if e == minBound then
   if d == minBound then
   if c == minBound then
   if b == minBound then
   if a == minBound then error "Enum.pred{(a,b,c,d,e,f,g,h,i,j)}: tried to take `pred' of minBound"
   else (minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fa-1))
   else (succ a  ,minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fb-1))
   else ( a      , succ b, minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fc-1))
   else ( a      , b     , succ c  ,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fd-1))
   else ( a      , b     , c       , succ d ,minBound,minBound,minBound,minBound,minBound,toEnum (fe-1))
   else ( a      , b     , c       , d      , succ e ,minBound,minBound,minBound,minBound,toEnum (ff-1))
   else ( a      , b     , c       , d      , e      , succ f ,minBound,minBound,minBound,toEnum (fg-1))
   else ( a      , b     , c       , d      , e      , f      , succ g ,minBound,minBound,toEnum (fh-1))
   else ( a      , b     , c       , d      , e      , f      , g      , succ h ,minBound,toEnum (fi-1))
   else ( a      , b     , c       , d      , e      , f      , g      ,       h, succ i ,  pred j)
    where
      fa = fromEnum a
      fb = fromEnum b
      fc = fromEnum c
      fd = fromEnum d
      fe = fromEnum e
      ff = fromEnum f
      fg = fromEnum g
      fh = fromEnum h
      fi = fromEnum i

  enumFrom t10 | t10 == (maxBound,maxBound,maxBound,maxBound,maxBound,
                         maxBound,maxBound,maxBound,maxBound,maxBound) =
                       [(maxBound,maxBound,maxBound,maxBound,maxBound,
                         maxBound,maxBound,maxBound,maxBound,maxBound)]
               | otherwise = t10 : (enumFrom (succ t10))

  enumFromTo t0 t1 = take l $ enumFrom t0
    where l = (fromEnum t1) - (fromEnum t0) + 1

  toEnum n = (\[a,b,c,d,e,f,g,h,i,j] ->
              (toEnum a, toEnum b, toEnum c, toEnum d, toEnum e,
               toEnum f, toEnum g, toEnum h, toEnum i, toEnum j)) (te 10 n)

  fromEnum (a,b,c,d,e,f,g,h,i,j) = fe [fromEnum a, fromEnum b, fromEnum c, fromEnum d, fromEnum e,
                                       fromEnum f, fromEnum g, fromEnum h, fromEnum i, fromEnum j]

-------------------------------------------------------------------------------------------------------------
instance (Enum a,Enum b,Enum c,Enum d,Enum e,Enum f,Enum g,Enum h,Enum i,Enum j,Enum k,
          Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k,
          Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f,
          Bounded g, Bounded h, Bounded i, Bounded j, Bounded k)        => Enum (a,b,c,d,e,f,g,h,i,j,k) where
-------------------------------------------------------------------------------------------------------------
-- 11
  succ (a,b,c,d,e,f,g,h,i,j,k)
     | (a,b,c,d,e,f,g,h,i,j,k) == maxBound
        = error "Enum.succ{(a,b,c,d,e,f,g,h,i,j,k)}: tried to take `succ' of maxBound"
     | otherwise = to11Tuple $
                      findNext (fromEnum (mb (Jst a)), fromEnum (mb (Jst b)), fromEnum (mb (Jst c)),
                                fromEnum (mb (Jst d)), fromEnum (mb (Jst e)), fromEnum (mb (Jst f)),
                                fromEnum (mb (Jst g)), fromEnum (mb (Jst h)), fromEnum (mb (Jst i)),
                                fromEnum (mb (Jst j)), fromEnum (mb (Jst k))) $
                      succ11 (fromEnum k) True (from11Tuple (a,b,c,d,e,f,g,h,i,j,k))
   where
    findNext :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h, Enum i, Enum j, Enum k,
                  Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k,
                  Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
                  Bounded h, Bounded i, Bounded j, Bounded k)
                => (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) ->
                   ((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k) ->
                   ((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k)
    findNext (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk) ((((((((((a,b),c),d),e),f),g),h),i),j),k) =
       if (not (isJst a)) || (not (isJst b)) || (not (isJst c)) || (not (isJst d)) || (not (isJst e)) ||
          (not (isJst f)) || (not (isJst g)) || (not (isJst h)) || (not (isJst i)) || (not (isJst j)) ||
          (not (isJst k))
       then findNext (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk) $
            toBounded (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk) $
            succ11 (getInt k) True ((((((((((a,b),c),d),e),f),g),h),i),j),k)
       else ((((((((((a,b),c),d),e),f),g),h),i),j),k)
    toBounded (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk)
              ((((((((((ja,jb),jc),jd),je),jf),jg),jh),ji),jj),jk) =
              ((((((((((ib ja ba,ib jb bb),ib jc bc), ib jd bd), ib je be),ib jf bf), ib jg bg),
                       ib jh bh), ib ji bi), ib jj bj), ib jk bk)


  pred (a,b,c,d,e,f,g,h,i,j,k) =
   if k == minBound then
   if j == minBound then
   if i == minBound then
   if h == minBound then
   if g == minBound then
   if f == minBound then
   if e == minBound then
   if d == minBound then
   if c == minBound then
   if b == minBound then
   if a == minBound then error "Enum.pred{(a,b,c,d,e,f,g,h,i,j,k)}: tried to take `pred' of minBound"
   else (minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fa-1))
   else (succ a  ,minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fb-1))
   else ( a      , succ b, minBound,minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fc-1))
   else ( a      , b     , succ c  ,minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fd-1))
   else ( a      , b     , c       , succ d ,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fe-1))
   else ( a      , b     , c       , d      , succ e ,minBound,minBound,minBound,minBound,minBound,toEnum (ff-1))
   else ( a      , b     , c       , d      , e      , succ f ,minBound,minBound,minBound,minBound,toEnum (fg-1))
   else ( a      , b     , c       , d      , e      , f      , succ g ,minBound,minBound,minBound,toEnum (fh-1))
   else ( a      , b     , c       , d      , e      , f      , g      , succ h ,minBound,minBound,toEnum (fi-1))
   else ( a      , b     , c       , d      , e      , f      , g      , h      , succ i ,minBound,toEnum (fj-1))
   else ( a      , b     , c       , d      , e      , f      , g      , h      , i      , succ j ,  pred k)
    where
      fa = fromEnum a
      fb = fromEnum b
      fc = fromEnum c
      fd = fromEnum d
      fe = fromEnum e
      ff = fromEnum f
      fg = fromEnum g
      fh = fromEnum h
      fi = fromEnum i
      fj = fromEnum j

  enumFrom t11 | t11 == (maxBound,maxBound,maxBound,maxBound,maxBound,
                         maxBound,maxBound,maxBound,maxBound,maxBound,maxBound) =
                       [(maxBound,maxBound,maxBound,maxBound,maxBound,
                         maxBound,maxBound,maxBound,maxBound,maxBound,maxBound)]
               | otherwise = t11 : (enumFrom (succ t11))

  enumFromTo t0 t1 = take l $ enumFrom t0
    where l = (fromEnum t1) - (fromEnum t0) + 1

  toEnum n = (\[a,b,c,d,e,f,g,h,i,j,k] ->
              (toEnum a, toEnum b, toEnum c, toEnum d, toEnum e,
               toEnum f, toEnum g, toEnum h, toEnum i, toEnum j, toEnum k)) (te 11 n)

  fromEnum (a,b,c,d,e,f,g,h,i,j,k) = fe [fromEnum a, fromEnum b, fromEnum c, fromEnum d, fromEnum e,
                                         fromEnum f, fromEnum g, fromEnum h, fromEnum i, fromEnum j, fromEnum k]

------------------------------------------------------------------------------------------------------------
instance (Enum a,Enum b,Enum c,Enum d,Enum e,Enum f,Enum g,Enum h,Enum i,Enum j,Enum k,Enum l,
          Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l,
          Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i,
          Bounded j, Bounded k, Bounded l)
          => Enum (a,b,c,d,e,f,g,h,i,j,k,l) where
------------------------------------------------------------------------------------------------------------
-- 12
  succ (a,b,c,d,e,f,g,h,i,j,k,l)
     | (a,b,c,d,e,f,g,h,i,j,k,l) == maxBound
        = error "Enum.succ{(a,b,c,d,e,f,g,h,i,j,k,l)}: tried to take `succ' of maxBound"
     | otherwise = to12Tuple $
                      findNext (fromEnum (mb (Jst a)), fromEnum (mb (Jst b)), fromEnum (mb (Jst c)),
                                fromEnum (mb (Jst d)), fromEnum (mb (Jst e)), fromEnum (mb (Jst f)),
                                fromEnum (mb (Jst g)), fromEnum (mb (Jst h)), fromEnum (mb (Jst i)),
                                fromEnum (mb (Jst j)), fromEnum (mb (Jst k)), fromEnum (mb (Jst l))) $
                      succ12 (fromEnum l) True (from12Tuple (a,b,c,d,e,f,g,h,i,j,k,l))
   where
    findNext :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h, Enum i, Enum j, Enum k, Enum l,
                  Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l,
                  Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
                  Bounded h, Bounded i, Bounded j, Bounded k, Bounded l)
                => (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) ->
                   (((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k),J l) ->
                   (((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k),J l)
    findNext (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl) (((((((((((a,b),c),d),e),f),g),h),i),j),k),l) =
       if (not (isJst a)) || (not (isJst b)) || (not (isJst c)) || (not (isJst d)) || (not (isJst e)) ||
          (not (isJst f)) || (not (isJst g)) || (not (isJst h)) || (not (isJst i)) || (not (isJst j)) ||
          (not (isJst k)) || (not (isJst l))
       then findNext (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl) $
            toBounded (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl) $
            succ12 (getInt l) True (((((((((((a,b),c),d),e),f),g),h),i),j),k),l)
       else (((((((((((a,b),c),d),e),f),g),h),i),j),k),l)
    toBounded (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl)
              (((((((((((ja,jb),jc),jd),je),jf),jg),jh),ji),jj),jk),jl) =
              (((((((((((ib ja ba,ib jb bb),ib jc bc), ib jd bd), ib je be),ib jf bf), ib jg bg),
                       ib jh bh), ib ji bi), ib jj bj), ib jk bk), ib jl bl)


  pred (a,b,c,d,e,f,g,h,i,j,k,l) =
   if l == minBound then
   if k == minBound then
   if j == minBound then
   if i == minBound then
   if h == minBound then
   if g == minBound then
   if f == minBound then
   if e == minBound then
   if d == minBound then
   if c == minBound then
   if b == minBound then
   if a == minBound then error "Enum.pred{(a,b,c,d,e,f,g,h,i,j,k)}: tried to take `pred' of minBound"
               else (minBound,minBound,minBound,minBound,minBound,
                     minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fa-1))
               else (succ a  ,minBound,minBound,minBound,minBound,
                     minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fb-1))
               else ( a      , succ b, minBound,minBound,minBound,
                     minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fc-1))
               else ( a      , b     , succ c  ,minBound,minBound,
                     minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fd-1))
               else ( a      , b     , c       , succ d ,minBound,
                     minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fe-1))
               else ( a      , b     , c       , d      , succ e ,
                     minBound,minBound,minBound,minBound,minBound,minBound,toEnum (ff-1))
               else ( a      , b     , c       , d      , e      ,
                     succ f ,minBound,minBound,minBound,minBound,minBound,toEnum (fg-1))
               else ( a      , b     , c       , d      , e      ,
                     f      , succ g ,minBound,minBound,minBound,minBound,toEnum (fh-1))
               else ( a      , b     , c       , d      , e      ,
                     f      , g      , succ h ,minBound,minBound,minBound,toEnum (fi-1))
               else ( a      , b     , c       , d      , e      ,
                     f      , g      , h      , succ i ,minBound,minBound,toEnum (fj-1))
               else ( a      , b     , c       , d      , e      ,
                     f      , g      , h      , i      , succ j ,minBound,toEnum (fk-1))
               else ( a      , b     , c       , d      , e      ,
                     f      , g      , h      , i      , j      , succ k , pred l)
    where
      fa = fromEnum a
      fb = fromEnum b
      fc = fromEnum c
      fd = fromEnum d
      fe = fromEnum e
      ff = fromEnum f
      fg = fromEnum g
      fh = fromEnum h
      fi = fromEnum i
      fj = fromEnum j
      fk = fromEnum k

  enumFrom t12 | t12 == (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,
                         maxBound,maxBound,maxBound,maxBound,maxBound,maxBound) =
                       [(maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,
                         maxBound,maxBound,maxBound,maxBound,maxBound,maxBound)]
               | otherwise = t12 : (enumFrom (succ t12))

  enumFromTo t0 t1 = take l $ enumFrom t0
    where l = (fromEnum t1) - (fromEnum t0) + 1

  toEnum n = (\[a,b,c,d,e,f,g,h,i,j,k,l] ->
              (toEnum a, toEnum b, toEnum c, toEnum d, toEnum e, toEnum f,
               toEnum g, toEnum h, toEnum i, toEnum j, toEnum k, toEnum l)) (te 12 n)

  fromEnum (a,b,c,d,e,f,g,h,i,j,k,l) =
                     fe [fromEnum a, fromEnum b, fromEnum c, fromEnum d, fromEnum e, fromEnum f,
                         fromEnum g, fromEnum h, fromEnum i, fromEnum j, fromEnum k, fromEnum l]

------------------------------------------------------------------------------------------------------------
instance (Enum a,Enum b,Enum c,Enum d,Enum e,Enum f,Enum g,Enum h,Enum i,Enum j,Enum k,Enum l,Enum m,
          Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m,
          Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i,
          Bounded j, Bounded k, Bounded l, Bounded m)
          => Enum (a,b,c,d,e,f,g,h,i,j,k,l,m) where
------------------------------------------------------------------------------------------------------------
-- 13
  succ (a,b,c,d,e,f,g,h,i,j,k,l,m)
     | (a,b,c,d,e,f,g,h,i,j,k,l,m) == maxBound
        = error "Enum.succ{(a,b,c,d,e,f,g,h,i,j,k,l,m)}: tried to take `succ' of maxBound"
     | otherwise = to13Tuple $
                      findNext (fromEnum (mb (Jst a)), fromEnum (mb (Jst b)), fromEnum (mb (Jst c)),
                                fromEnum (mb (Jst d)), fromEnum (mb (Jst e)), fromEnum (mb (Jst f)),
                                fromEnum (mb (Jst g)), fromEnum (mb (Jst h)), fromEnum (mb (Jst i)),
                                fromEnum (mb (Jst j)), fromEnum (mb (Jst k)), fromEnum (mb (Jst l)),
                                fromEnum (mb (Jst m)) ) $
                      succ13 (fromEnum m) True (from13Tuple (a,b,c,d,e,f,g,h,i,j,k,l,m))
   where
    findNext :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h, Enum i, Enum j, Enum k, Enum l, Enum m,
                  Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m,
                  Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
                  Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m)
                => (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) ->
                   ((((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k),J l),J m) ->
                   ((((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k),J l),J m)
    findNext (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm) ((((((((((((a,b),c),d),e),f),g),h),i),j),k),l),m) =
       if (not (isJst a)) || (not (isJst b)) || (not (isJst c)) || (not (isJst d)) || (not (isJst e)) ||
          (not (isJst f)) || (not (isJst g)) || (not (isJst h)) || (not (isJst i)) || (not (isJst j)) ||
          (not (isJst k)) || (not (isJst l)) || (not (isJst m))
       then findNext (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm) $
            toBounded (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm) $
            succ13 (getInt m) True ((((((((((((a,b),c),d),e),f),g),h),i),j),k),l),m)
       else ((((((((((((a,b),c),d),e),f),g),h),i),j),k),l),m)
    toBounded (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm)
              ((((((((((((ja,jb),jc),jd),je),jf),jg),jh),ji),jj),jk),jl),jm) =
              ((((((((((((ib ja ba,ib jb bb),ib jc bc), ib jd bd), ib je be),ib jf bf), ib jg bg),
                       ib jh bh), ib ji bi), ib jj bj), ib jk bk), ib jl bl), ib jm bm)

  pred (a,b,c,d,e,f,g,h,i,j,k,l,m) =
   if m == minBound then
   if l == minBound then
   if k == minBound then
   if j == minBound then
   if i == minBound then
   if h == minBound then
   if g == minBound then
   if f == minBound then
   if e == minBound then
   if d == minBound then
   if c == minBound then
   if b == minBound then
   if a == minBound then error "Enum.pred{(a,b,c,d,e,f,g,h,i,j,k)}: tried to take `pred' of minBound"
      else (minBound,minBound,minBound,minBound,minBound,minBound,
            minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fa-1))
      else (succ a  ,minBound,minBound,minBound,minBound,minBound,
            minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fb-1))
      else ( a      , succ b, minBound,minBound,minBound,minBound,
            minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fc-1))
      else ( a      , b     , succ c  ,minBound,minBound,minBound,
            minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fd-1))
      else ( a      , b     , c       , succ d ,minBound,minBound,
            minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fe-1))
      else ( a      , b     , c       , d      , succ e ,minBound,
            minBound,minBound,minBound,minBound,minBound,minBound,toEnum (ff-1))
      else ( a      , b     , c       , d      , e      , succ f ,
            minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fg-1))
      else ( a      , b     , c       , d      , e      , f      ,
            succ g ,minBound,minBound,minBound,minBound,minBound,toEnum (fh-1))
      else ( a      , b     , c       , d      , e      , f      ,
             g      , succ h ,minBound,minBound,minBound,minBound,toEnum (fi-1))
      else ( a      , b     , c       , d      , e      , f      ,
             g      , h      , succ i ,minBound,minBound,minBound,toEnum (fj-1))
      else ( a      , b     , c       , d      , e      , f      ,
             g      , h      , i      , succ j ,minBound,minBound,toEnum (fk-1))
      else ( a      , b     , c       , d      , e      , f      ,
             g      , h      , i      , j      , succ k ,minBound,toEnum (fl-1))
      else ( a      , b     , c       , d      , e      , f      ,
             g      , h      , i      , j      , k      , succ l , pred m)
    where
      fa = fromEnum a
      fb = fromEnum b
      fc = fromEnum c
      fd = fromEnum d
      fe = fromEnum e
      ff = fromEnum f
      fg = fromEnum g
      fh = fromEnum h
      fi = fromEnum i
      fj = fromEnum j
      fk = fromEnum k
      fl = fromEnum l

  enumFrom t13
     | t13 == (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,
               maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound) =
             [(maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,
               maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound)]
     | otherwise = t13 : (enumFrom (succ t13))

  enumFromTo t0 t1 = take l $ enumFrom t0
    where l = (fromEnum t1) - (fromEnum t0) + 1

  toEnum n = (\[a,b,c,d,e,f,g,h,i,j,k,l,m] ->
              (toEnum a, toEnum b, toEnum c, toEnum d, toEnum e, toEnum f,
               toEnum g, toEnum h, toEnum i, toEnum j, toEnum k, toEnum l, toEnum m)) (te 13 n)

  fromEnum (a,b,c,d,e,f,g,h,i,j,k,l,m) =
                     fe [fromEnum a, fromEnum b, fromEnum c, fromEnum d, fromEnum e, fromEnum f,
                         fromEnum g, fromEnum h, fromEnum i, fromEnum j, fromEnum k, fromEnum l, fromEnum m]

------------------------------------------------------------------------------------------------------------
instance (Enum a,Enum b,Enum c,Enum d,Enum e,Enum f,Enum g,Enum h,Enum i,Enum j,Enum k,Enum l,Enum m,Enum n,
          Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n,
          Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i,
          Bounded j, Bounded k, Bounded l, Bounded m, Bounded n)
          => Enum (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
------------------------------------------------------------------------------------------------------------
-- 14
  succ (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
     | (a,b,c,d,e,f,g,h,i,j,k,l,m,n) == maxBound
        = error "Enum.succ{(a,b,c,d,e,f,g,h,i,j,k,l,m,n)}: tried to take `succ' of maxBound"
     | otherwise = to14Tuple $
                      findNext (fromEnum (mb (Jst a)), fromEnum (mb (Jst b)), fromEnum (mb (Jst c)),
                                fromEnum (mb (Jst d)), fromEnum (mb (Jst e)), fromEnum (mb (Jst f)),
                                fromEnum (mb (Jst g)), fromEnum (mb (Jst h)), fromEnum (mb (Jst i)),
                                fromEnum (mb (Jst j)), fromEnum (mb (Jst k)), fromEnum (mb (Jst l)),
                                fromEnum (mb (Jst m)), fromEnum (mb (Jst n)) ) $
                      succ14 (fromEnum m) True (from14Tuple (a,b,c,d,e,f,g,h,i,j,k,l,m,n))
   where
    findNext :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h, Enum i, Enum j,
                  Enum k, Enum l, Enum m, Enum n,
                  Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n,
                  Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
                  Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m, Bounded n)
                => (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) ->
                   (((((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k),J l),J m),J n) ->
                   (((((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k),J l),J m),J n)
    findNext (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn) (((((((((((((a,b),c),d),e),f),g),h),i),j),k),l),m),n) =
       if (not (isJst a)) || (not (isJst b)) || (not (isJst c)) || (not (isJst d)) || (not (isJst e)) ||
          (not (isJst f)) || (not (isJst g)) || (not (isJst h)) || (not (isJst i)) || (not (isJst j)) ||
          (not (isJst k)) || (not (isJst l)) || (not (isJst m)) || (not (isJst n))
       then findNext (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn) $
            toBounded (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn) $
            succ14 (getInt n) True (((((((((((((a,b),c),d),e),f),g),h),i),j),k),l),m),n)
       else (((((((((((((a,b),c),d),e),f),g),h),i),j),k),l),m),n)
    toBounded (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn)
              (((((((((((((ja,jb),jc),jd),je),jf),jg),jh),ji),jj),jk),jl),jm),jn) =
              (((((((((((((ib ja ba,ib jb bb),ib jc bc), ib jd bd), ib je be),ib jf bf), ib jg bg),
                       ib jh bh), ib ji bi), ib jj bj), ib jk bk), ib jl bl), ib jm bm), ib jn bn)


  pred (a,b,c,d,e,f,g,h,i,j,k,l,m,n) =
   if n == minBound then
   if m == minBound then
   if l == minBound then
   if k == minBound then
   if j == minBound then
   if i == minBound then
   if h == minBound then
   if g == minBound then
   if f == minBound then
   if e == minBound then
   if d == minBound then
   if c == minBound then
   if b == minBound then
   if a == minBound then error "Enum.pred{(a,b,c,d,e,f,g,h,i,j,k)}: tried to take `pred' of minBound"
   else (minBound,minBound,minBound,minBound,minBound,minBound,
         minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fa-1))
   else (succ a  ,minBound,minBound,minBound,minBound,minBound,
         minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fb-1))
   else ( a      , succ b, minBound,minBound,minBound,minBound,
         minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fc-1))
   else ( a      , b     , succ c  ,minBound,minBound,minBound,
         minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fd-1))
   else ( a      , b     , c       , succ d ,minBound,minBound,
         minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fe-1))
   else ( a      , b     , c       , d      , succ e ,minBound,
         minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (ff-1))
   else ( a      , b     , c       , d      , e      , succ f ,
         minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fg-1))
   else ( a      , b     , c       , d      , e      , f      ,
         succ g ,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fh-1))
   else ( a      , b     , c       , d      , e      , f      ,
          g      , succ h ,minBound,minBound,minBound,minBound,minBound,toEnum (fi-1))
   else ( a      , b     , c       , d      , e      , f      ,
          g      , h      , succ i ,minBound,minBound,minBound,minBound,toEnum (fj-1))
   else ( a      , b     , c       , d      , e      , f      ,
          g      , h      , i      , succ j ,minBound,minBound,minBound,toEnum (fk-1))
   else ( a      , b     , c       , d      , e      , f      ,
          g      , h      , i      , j      , succ k ,minBound,minBound,toEnum (fl-1))
   else ( a      , b     , c       , d      , e      , f      ,
          g      , h      , i      , j      , k      , succ l ,minBound,toEnum (fm-1))
   else ( a      , b     , c       , d      , e      , f      ,
          g      , h      , i      , j      , k      , l      , succ m , pred n)
    where
      fa = fromEnum a
      fb = fromEnum b
      fc = fromEnum c
      fd = fromEnum d
      fe = fromEnum e
      ff = fromEnum f
      fg = fromEnum g
      fh = fromEnum h
      fi = fromEnum i
      fj = fromEnum j
      fk = fromEnum k
      fl = fromEnum l
      fm = fromEnum m

  enumFrom t14
     | t14 == (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,
               maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound) =
             [(maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,
               maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound)]
     | otherwise = t14 : (enumFrom (succ t14))

  enumFromTo t0 t1 = take l $ enumFrom t0
    where l = (fromEnum t1) - (fromEnum t0) + 1

  toEnum n = (\[a,b,c,d,e,f,g,h,i,j,k,l,m,n] ->
              (toEnum a, toEnum b, toEnum c, toEnum d, toEnum e, toEnum f, toEnum g,
               toEnum h, toEnum i, toEnum j, toEnum k, toEnum l, toEnum m, toEnum n)) (te 14 n)

  fromEnum (a,b,c,d,e,f,g,h,i,j,k,l,m,n) =
                     fe [fromEnum a, fromEnum b, fromEnum c, fromEnum d, fromEnum e, fromEnum f, fromEnum g,
                         fromEnum h, fromEnum i, fromEnum j, fromEnum k, fromEnum l, fromEnum m, fromEnum m]

-------------------------------------------------------------------------------------------------------------------
instance (Enum a,Enum b,Enum c,Enum d,Enum e,Enum f,Enum g,Enum h,Enum i,Enum j,Enum k,Enum l,Enum m,Enum n,Enum o,
          Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o,
          Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i,
          Bounded j, Bounded k, Bounded l, Bounded m, Bounded n, Bounded o)
          => Enum (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
-------------------------------------------------------------------------------------------------------------------
-- 15  (we stop at this number beacause it is the official number of braces supported by the Prelude)
  succ (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
     | (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) == maxBound
        = error "Enum.succ{(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)}: tried to take `succ' of maxBound"
     | otherwise = to15Tuple $
                      findNext (fromEnum (mb (Jst a)), fromEnum (mb (Jst b)), fromEnum (mb (Jst c)),
                                fromEnum (mb (Jst d)), fromEnum (mb (Jst e)), fromEnum (mb (Jst f)),
                                fromEnum (mb (Jst g)), fromEnum (mb (Jst h)), fromEnum (mb (Jst i)),
                                fromEnum (mb (Jst j)), fromEnum (mb (Jst k)), fromEnum (mb (Jst l)),
                                fromEnum (mb (Jst m)), fromEnum (mb (Jst n)), fromEnum (mb (Jst o)) ) $
                      succ15 (fromEnum m) True (from15Tuple (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o))
   where
    findNext :: ( Enum a, Enum b, Enum c, Enum d, Enum e, Enum f, Enum g, Enum h, Enum i, Enum j,
                  Enum k, Enum l, Enum m, Enum n, Enum o,
                  Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o,
                  Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g,
                  Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m, Bounded n, Bounded o)
                => (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) ->
                   ((((((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k),J l),J m),J n),J o) ->
                   ((((((((((((((J a,J b),J c),J d),J e),J f),J g),J h),J i),J j),J k),J l),J m),J n),J o)
    findNext (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo) ((((((((((((((a,b),c),d),e),f),g),h),i),j),k),l),m),n),o) =
       if (not (isJst a)) || (not (isJst b)) || (not (isJst c)) || (not (isJst d)) || (not (isJst e)) ||
          (not (isJst f)) || (not (isJst g)) || (not (isJst h)) || (not (isJst i)) || (not (isJst j)) ||
          (not (isJst k)) || (not (isJst l)) || (not (isJst m)) || (not (isJst n)) || (not (isJst o))
       then findNext (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo) $
            toBounded (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo) $
            succ15 (getInt o) True ((((((((((((((a,b),c),d),e),f),g),h),i),j),k),l),m),n),o)
       else ((((((((((((((a,b),c),d),e),f),g),h),i),j),k),l),m),n),o)
    toBounded (ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo)
              ((((((((((((((ja,jb),jc),jd),je),jf),jg),jh),ji),jj),jk),jl),jm),jn),jo) =
              ((((((((((((((ib ja ba,ib jb bb),ib jc bc), ib jd bd), ib je be),ib jf bf), ib jg bg),
                       ib jh bh), ib ji bi), ib jj bj), ib jk bk), ib jl bl), ib jm bm), ib jn bn), ib jo bo)


  pred (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) =
   if o == minBound then
   if n == minBound then
   if m == minBound then
   if l == minBound then
   if k == minBound then
   if j == minBound then
   if i == minBound then
   if h == minBound then
   if g == minBound then
   if f == minBound then
   if e == minBound then
   if d == minBound then
   if c == minBound then
   if b == minBound then
   if a == minBound then error "Enum.pred{(a,b,c,d,e,f,g,h,i,j,k)}: tried to take `pred' of minBound"
   else (minBound,minBound,minBound,minBound,minBound,minBound,minBound,
         minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fa-1))
   else (succ a  ,minBound,minBound,minBound,minBound,minBound,minBound,
         minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fb-1))
   else ( a      , succ b, minBound,minBound,minBound,minBound,minBound,
         minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fc-1))
   else ( a      , b     , succ c  ,minBound,minBound,minBound,minBound,
         minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fd-1))
   else ( a      , b     , c       , succ d ,minBound,minBound,minBound,
         minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fe-1))
   else ( a      , b     , c       , d      , succ e ,minBound,minBound,
         minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (ff-1))
   else ( a      , b     , c       , d      , e      , succ f ,minBound,
         minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fg-1))
   else ( a      , b     , c       , d      , e      , f      , succ g ,
         minBound,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fh-1))
   else ( a      , b     , c       , d      , e      , f      , g      ,
          succ h ,minBound,minBound,minBound,minBound,minBound,minBound,toEnum (fi-1))
   else ( a      , b     , c       , d      , e      , f      , g      ,
          h      ,succ i  ,minBound,minBound,minBound,minBound,minBound,toEnum (fj-1))
   else ( a      , b     , c       , d      , e      , f      , g      ,
          h      ,i       , succ j ,minBound,minBound,minBound,minBound,toEnum (fk-1))
   else ( a      , b     , c       , d      , e      , f      , g      ,
          h      ,i      , j      , succ k ,minBound,minBound,minBound,toEnum (fl-1))
   else ( a      , b     , c       , d      , e      , f      , g      ,
          h      ,i      , j      , k       , succ l ,minBound,minBound,toEnum (fm-1))
   else ( a      , b     , c       , d      , e      , f      , g      ,
          h      ,i      , j      , k       , l      , succ m ,minBound,toEnum (fn-1))
   else ( a      , b     , c       , d      , e      , f      , g      ,
          h      ,i      , j      , k       , l      , m      , succ n , pred o)
    where
      fa = fromEnum a
      fb = fromEnum b
      fc = fromEnum c
      fd = fromEnum d
      fe = fromEnum e
      ff = fromEnum f
      fg = fromEnum g
      fh = fromEnum h
      fi = fromEnum i
      fj = fromEnum j
      fk = fromEnum k
      fl = fromEnum l
      fm = fromEnum m
      fn = fromEnum n

  enumFrom t15
     | t15 == (maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,
               maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound) =
             [(maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,
               maxBound,maxBound,maxBound,maxBound,maxBound,maxBound,maxBound)]
     | otherwise = t15 : (enumFrom (succ t15))

  enumFromTo t0 t1 = take l $ enumFrom t0
    where l = (fromEnum t1) - (fromEnum t0) + 1

  toEnum n = (\[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o] ->
              (toEnum a, toEnum b, toEnum c, toEnum d, toEnum e, toEnum f, toEnum g,
               toEnum h, toEnum i, toEnum j, toEnum k, toEnum l, toEnum m, toEnum n, toEnum o)) (te 15 n)

  fromEnum (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) =
         fe [fromEnum a, fromEnum b, fromEnum c, fromEnum d, fromEnum e, fromEnum f, fromEnum g,
             fromEnum h, fromEnum i, fromEnum j, fromEnum k, fromEnum l, fromEnum m, fromEnum n, fromEnum o]


-- data Strategy f g = Strategy f g
-- instance Enum g => Enum (Strategy f g) where

-- combinator library ?
-- stategies inside a plane?  tuple position that the tuple increases
-- fixing a digit / changing the speed
-- to make the upper enumeration into an ordering

