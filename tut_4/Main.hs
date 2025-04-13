data Q = Q Int Int

instance Show Q where
  show (Q a b) = concat [show a, "/", show b]

simQ :: Q -> Q
simQ (Q a b) = Q (a `div` g) (b `div` g)
  where g = gcd a b

instance Eq Q where
  r1 == r2 = (a1 == a2) && (b1 == b2)
    where (Q a1 b1) = simQ r1 
          (Q a2 b2) = simQ  r2

