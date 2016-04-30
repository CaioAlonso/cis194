main :: IO ()
main = undefined

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n >= 1    = hanoi (n-1) a c b ++ [(a,c)] ++ hanoi (n-1) b c a
hanoi _ _ _ _ = []
