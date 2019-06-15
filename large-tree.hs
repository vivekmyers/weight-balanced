import Control.Monad
import Text.Printf

data Tree = Nil | Node Int Double Tree Tree deriving Show

look :: Int -> Double
look n = case n of
    1 -> 1/32 
    2 -> eps
    3 -> 1/16
    4 -> eps
    5 -> 1/16+eps
    6 -> eps
    7 -> 1/8+2*eps
    8 -> eps
    9 -> 7/32+5*eps
    10 -> eps
    11 -> 25/64+8*eps

weight Nil = 0
weight (Node idx w l r) = w + weight l + weight r

range a b | b < a = [Nil]
range a b = [Node i (look i) l r | i <- [a..b], l <- range a (i-1), r <- range (i+1) b]

minidx Nil = 15
minidx (Node idx w l r) = minimum $ [idx, minidx l, minidx r]

maxidx Nil = 0
maxidx (Node idx w l r) = maximum $ [idx, maxidx l, maxidx r]

balanced Nil = True
balanced root@(Node idx w l r) = (all (\(Node _ _ l' r') -> abs (weight l' - weight r') >= abs (weight l - weight r))
                                   $ range (minidx root) (maxidx root)) && balanced l && balanced r

pprint Nil off = return ()
pprint (Node idx w l r) off = do
    putStr (take off $ repeat ' ')
    printf "%.5f\n" w
    pprint l (off + 4)
    pprint r (off + 4)
    when (off == 0) (putStrLn "")
 
eps = 1/192
 
testTree 1 = Node 1 (look 1) Nil Nil
testTree n = Node (n-1) (look $ n-1) (testTree $ n-2) (Node n (look n) Nil Nil)
                

main = do
    let tree = testTree 11
    pprint tree 0
    if balanced tree
        then putStrLn "Test Passed!"
        else putStrLn "Test Failed!"
