import Dist
import System.Random
import Data.List
import Data.Ord

---
--- QUESTION 1
---
dreidelList :: Double -> [(Double, Double)]
dreidelList bet = [(10 * bet, 1/4), (0.0, 3/4)]

dreidelDist :: Double -> Dist Double
dreidelDist bet = Dist (dreidelList bet)

dreidelDreidelDreidel :: Double -> Double -> Int -> Dist Double
dreidelDreidelDreidel y p n = dreidelCalc p n (Dist [(y, 1.0)])

dreidelCalc :: Double -> Int -> Dist Double -> Dist Double
dreidelCalc _ 0 distr = distr
dreidelCalc p n distr = dreidelCalc p (n - 1) newDistr
    where newDistr = applyRoundOfBetting p distr

applyRoundOfBetting :: Double -> Dist Double -> Dist Double
applyRoundOfBetting p (Dist xs) = normalize distAfterBet
    where distAfterBet = Dist (concatMap (listAfterBet p) xs)

listAfterBet :: Double -> (Double, Double) -> [(Double, Double)]
listAfterBet p (amount, prob) = scaleList prob theList
    where (remainder, bet) = splitMoney amount p
          theList = map (\(a, p) -> (a+remainder, p)) (dreidelList bet) 

splitMoney :: Double -> Double -> (Double, Double)
splitMoney amount proportion = (amount - bet, bet)
    where bet = amount * proportion

scaleList :: Double -> [(Double, Double)] -> [(Double, Double)]
scaleList scaleFactor xs = map scaleFunction xs
    where scaleFunction (amount, prob) = (amount, scaleFactor * prob)
    
---
--- QUESTION 2
---
mean :: Dist Double -> Double
mean (Dist amountProbPairs) = sum $ map (\(a, p) -> a * p) amountProbPairs

---
--- QUESTION 3
---

{-

*Main> main
Q3: (p, score) = (1.0,9536743.1640625)
...

-}

score :: Double -> Double
score p = mean $ dreidelDreidelDreidel 1000 p 10

diffEvolution ::(Show a) => [(a, Double)] -> (a -> Double) ->
    ([(a, Double)] -> (a -> Double) -> IO [(a, Double)]) -> 
    Int -> IO (a, Double)
diffEvolution candidates _       _        0 = do return (bestCandidate candidates)
diffEvolution candidates scoreFn evolveFn n = do
    newCandidates <- evolveFn candidates scoreFn
    diffEvolution newCandidates scoreFn evolveFn (n - 1)

bestCandidate :: [(a, Double)] -> (a, Double)
bestCandidate candidates = maximumBy (comparing snd) candidates

dreidelEvolve :: [(Double, Double)] -> (Double -> Double) -> IO [(Double, Double)]
dreidelEvolve candidates scoreFn = do
    [(base, _), (target, _)] <- sample 2 candidates
    let diffVector = 0.9 * (base - target) -- 0.9 is a constant scaling factor.
                                           -- adjust it if desired.
    
    [(candidate, _)] <- sample 1 candidates
    let mutant = candidate + diffVector
    
    return (crossover mutant candidates scoreFn)

crossover :: Double -> [(Double, Double)] -> (Double -> Double) -> [(Double, Double)]
crossover _ [] _ = []
crossover mutant (candidate@(val, score):cs) scoreFn = 
    best : crossover mutant cs scoreFn
    where   cross = (restrictToRange 0.0 1.0) $ val + mutant -- how to do crossover?
            crossScore = scoreFn cross
            best = if crossScore > score 
                    then (cross, crossScore)
                    else candidate

sample :: Int -> [a] -> IO [a]
sample n xs = do
    sample' n xs []

sample' :: Int -> [a] -> [a] -> IO [a]
sample' _ [] sampleList = do return sampleList
sample' 0 _  sampleList = do return sampleList
sample' n xa@(x:xs) sampleList = do
    p <- randomRIO (0.0, 1.0) :: IO Double
    if p <= fromIntegral n / (fromIntegral $ length xa)
        then sample' (n - 1) xs (x:sampleList)
        else sample' n       xs sampleList

restrictToRange :: (Ord a) => a -> a -> a -> a
-- Assumes that lBound <= uBound.
restrictToRange lBound uBound x
    | x < lBound = lBound
    | x > uBound = uBound
    | otherwise  = x

---
--- QUESTION 4
---

{-

*Main> main
...
Q4: (p, score) = (0.41903035648259357,0.4744071960449219)
Q4: Mean amount = 131219.7061936526

What is the value of p?
5.853095859361834e-2

What is the probability of ending up with over 4000 using that value of p?
0.4744071960449219

What is the expected (i.e., mean) amount ended up with?
131219.7061936526

-}

prExceeds :: Double -> Dist Double -> Double
prExceeds target dist = mean $ fmap (fromIntegral . fromEnum . (>= target)) dist

score2 :: Double -> Double
score2 p = prExceeds 2000 $ dreidelDreidelDreidel 1000 p 10

---
--- MAIN
---
getInitCandidates n scoreFn = do
    g <- getStdGen
    let startVals = take n (randomRs (0.0, 1.0) g) :: [Double]
    let initCandidates = zip startVals (map scoreFn startVals)
    
    newStdGen
    
    return initCandidates

main :: IO ()
main = do
    initCandidates <- getInitCandidates 20 score
    soln1 <- diffEvolution initCandidates score dreidelEvolve 30
    putStrLn $ "Q3: (p, score) = " ++ (show soln1)
    
    initCandidates <- getInitCandidates 20 score2
    soln2 <- diffEvolution initCandidates score2 dreidelEvolve 30
    putStrLn $ "Q4: (p, score) = " ++ (show soln2)
    putStrLn $ "Q4: Mean amount = " ++ (show (score (fst soln2)))