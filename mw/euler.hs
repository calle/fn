factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x*(factorial (x-1))

sum_digits :: Integer -> Integer
sum_digits x
    | x < 10 = x
    | otherwise = (mod x 10) + sum_digits (div x 10)


fib :: [Integer]
fib = fib'(1, 1)
    where fib' (x, y) = x : fib' (y, x+y)

count_digits :: Integer -> Integer
count_digits x
    | x < 10 = 1
    | otherwise = 1 + count_digits (div x 10)
 
count_list :: [a] -> Integer
count_list [] = 0
count_list (x:xs) = 1 + (count_list xs)

-- Euler 25: The first fibinacci term to contain 1000 digits
euler_25 = count_list (takeWhile (\n -> count_digits n < 1000) fib) + 1

pow :: Integer -> Integer -> Integer
pow x 1 = x
pow x y = x * pow x (y-1)


--pow_2 0 = 1
--pow_2 x = 2 * pow_2 (x-1)

pow_2d :: Integer -> Integer
pow_2d 0 = 1
pow_2d 1 = 2
pow_2d x = z*z
    where z = pow_2d (div x 2)

pow_2p :: (Integer, Integer) -> Integer
pow_2p (x, p)
    | x == 0 = 1
    | otherwise =  pow_2d ((mod x 2) * (pow2 p)) * pow_2p (div x 2, (p+1))

pow2 :: Integer -> Integer
pow2 x = pow_2p (x,0)

-- Euler 97: Find the 10 last digits of the non-Mersienne
-- prime 28433 * 2 ^ 7830457 + 1
--euler_97 = mod (pow_2p(7830457, 0) *  28433 + 1) 10000000000
euler_97 = mod (pow2 7830457 *  28433 + 1) 10000000000




--data ZZ = Z Integer Integer
--instance Show ZZ where
--    show (Z (a,b))  = (show a) ++ "+i" ++ (show b)


-- Integer complex number class
class Complex a where
    cadd :: (a,a) -> (a,a) -> (a,a)
    csub :: (a,a) -> (a,a) -> (a,a)


instance Complex Integer where
    cadd (a,b) (c,d) = (a+c,b+d)
    csub (a,b) (c,d) = (a-c,b-d)
