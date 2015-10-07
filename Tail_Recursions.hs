-- fibonacci finds Fibonacci numbers (including negative ones).
fibonacci :: Integer -> Integer

fibonacci n = helper 0 1 n
	where
		helper a b n | n == 0 =  a
	    	             | n > 0  = helper (b) (a + b) (n - 1)
        		     | n < 0  = helper (b) (a - b) (n + 1)


-- seqA finds the elements of the following recurrent sequence:             
-- a_0 = 1; a_1 = 2 ; a_2 = 3; a_{k+3} = a_{k+2} + a_{k+1} - 2 a_{k}.
seqA :: Integer -> Integer

seqA n = helper 1 2 3 n
	where
    		helper a b c n  | n == 0 = a
               			| n == 1 = b
                    		| n == 2 = c 
                    		| n > 2 = helper b c (c + b - 2 * a) (n - 1)
             

-- sum'n'count returns the sum and the number of digits of a given integer.
-- Two helper functions, getSum and getCount, are also defined 
-- in global scope and thus can be used separately.
sum'n'count :: Integer -> (Integer, Integer)

sum'n'count x = (getSum x, getCount x)

getSum x = helper x 0
	where 
		helper x n  	| x == 0 = n
         			| x > 0  = helper (x `div` 10) (n + x `mod` 10)
     				| x < 0  = helper ((-x) `div` 10) (n + (-x) `mod` 10)

getCount x = helper x 1 
	where
  		helper x 	| x `div` 10 == 0 || (-x) `div` 10 == 0 = n
             			| x > 0 = helper (x `div` 10) (n + 1) 
             			| x < 0 = helper ((-x) `div` 10) (n + 1) 
