import Control.Monad
import Text.Printf

data Tree = Nil | Node Int Double Tree Tree deriving Show

look :: Int -> Double
look n = case n of
    1 -> 1/16
    2 -> 0
    3 -> 1/8
    4 -> 0
    5 -> 1/8
    6 -> 0
    7 -> 1/4
    8 -> 0
    9 -> 7/16

weight Nil = 0
weight (Node idx w l r) = w + weight l + weight r

range a b | b < a = [Nil]
range a b = [Node i (look i) l r | i <- [a..b], l <- range a (i-1), r <- range (i+1) b]

minidx Nil = 10
minidx (Node idx w l r) = minimum $ [idx, minidx l, minidx r]

maxidx Nil = 0
maxidx (Node idx w l r) = maximum $ [idx, maxidx l, maxidx r]

balanced Nil = True
balanced root@(Node idx w l r) = (all (\(Node _ _ l' r') -> abs (weight l' - weight r') >= abs (weight l - weight r))
                                   $ range (minidx root) (maxidx root)) && balanced l && balanced r

pprint Nil off = return ()
pprint (Node idx w l r) off = do
    putStr (take off $ repeat ' ')
    printf "%.3f\n" w
    pprint l (off + 4)
    pprint r (off + 4)
    when (off == 0) (putStrLn "")
  
testTree = Node 8 0 (
               Node 6 0 (
                   Node 4 0 (
                       Node 2 0 (
                           Node 1 (1/16) Nil Nil)
                           (Node 3 (1/8) Nil Nil))
                       (Node 5 (1/8) Nil Nil))
                   (Node 7 (1/4) Nil Nil))
               (Node 9 (7/16) Nil Nil)
                


main = do
    pprint testTree 0
    when (balanced testTree) (putStrLn "Test Passed!")
