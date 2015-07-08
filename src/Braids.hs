{-# LANGUAGE FlexibleInstances, DataKinds #-}
{-|
Module      : Braids
-}
module Braids 
where
import Data.Graph as Graph
import Data.List
import Data.Tree
import Data.List.Split (splitOneOf)
import Data.Char (isNumber)

-- | A 'Braid' is a width and word.
-- Integers in the word represent Artin generators and their invereses.  E.g. @[1,3,-2]@ represents the word \sigma_1\sigma_3\sigma_2^{-1}.
data Braid = Braid { braidWidth :: Int
				   , braidWord :: [Int]}
				   deriving (Eq, Show, Read)

-- | A braid can also be written as a collection of 'Node's.  See knotatlas for more info on 'Cross' and 'Join'.
data Node = Cross Int Int Int Int | Join Int Int
	deriving (Eq, Show, Read, Ord)

-- | A 'PD' (Planar Diagram) is a collection of 'Node's.
type PD = [Node]

-- | A 'Resolution' is a collection of integers.  These should all be 0 or 1 and should probably be represented by [Bool].
type Resolution = [Int]

-- | A resolved 'Diagram' has a 'Resolution' and a set of components.
data Diagram = Diagram { resolution' :: Resolution
					   , components' :: [[Int]]}
					   deriving (Eq, Show)

-- | Parse command line input into a braid.
parse :: [String] -> (Braid, Int)
parse input = (braid, mark) where
	braid = Braid {braidWord = parseWord word, braidWidth = width}
	parseWord = map (read :: String -> Int) . filter (not . null) . splitOneOf ",]["
	(mark, word, width) = if all isNumber (head input) 
		then (read $ input !! 0 :: Int, input !! 1, read $ input !! 2 :: Int)
		else (1, input !! 0, read $ input !! 	1 :: Int)

-- | Construct a family of braids from <Ng and Khandawit's paper http://www.math.duke.edu/~ng/math/papers/nonsimple.pdf>.  This will fail if either input is negative.
ngkLeft :: (Int, Int) -> Braid
ngkRight (a, b) = Braid {braidWord = [3,-2,-2] ++ replicate (2 + 2*a) 3 ++ [2,-3,-1,2] ++ replicate (2 + 2*b) 1
                        , braidWidth = 4}

ngkRight :: (Int, Int) -> Braid
ngkLeft (a, b) = Braid {braidWord = [3,-2,-2] ++ replicate (2 + 2*a) 3 ++ [2,-3] ++ replicate (2 + 2*b) 1 ++ [2,-1]
                        , braidWidth = 4}


-- | Parse a 'Braid' into a 'PD'.
braidToPD :: Braid -> PD
braidToPD braid = concat [crossingToNodes c (braidWidth braid) d | (d,c) <- zip [1..] word] ++ [Join a (a + len * braidWidth braid) | a<-[1..(braidWidth braid)]] where
	word = braidWord braid
	len = length . braidWord $ braid

-- | Parse a crossing into a 'Node'.
crossingToNodes :: Int -> Int -> Int -> [Node]
crossingToNodes crossing width level = concatMap toNode [initial..(initial + width - 1)] where
	initial = 1 + (level - 1)*width
	--range = [initial..(initial + width - 1)]
	cAfter = abs crossing + initial - 1
	toNode :: Int -> [Node]
	toNode k | k == cAfter && crossing < 0 		= [Cross (k+1) k (k + width) (k + width + 1)]
	toNode k | k == cAfter && crossing > 0		= [Cross k (k + width) (k + width + 1) (k+1)]
	toNode k | k == cAfter + 1					= []
	toNode k | otherwise 						= [Join k (k + width)]

isCross :: Node -> Bool
isCross n = case n of
	Cross{} -> True
	Join{}  -> False

allRes :: PD -> [Resolution]
allRes pd = sequence binary where
	binary = replicate (length $ filter isCross pd)  [0,1]

-- | Take a 'PD' and a 'Resolution' and returns the resolved 'PD'.  
-- Note that the output is always a list of 'Join's.
resolve :: PD -> Resolution -> PD
resolve (Join a b : ns ) res = Join a b : resolve ns res
resolve (Cross a b c d:ns) (r:res) | r == 0		= [Join a b, Join c d] ++ resolve ns res
								   | r == 1		= [Join a d, Join b c] ++ resolve ns res
resolve [] _ = []								  

-- | Compute all resolutions of a 'PD'.
resolutions :: PD -> [PD]
resolutions pd = map (($ pd) . flip resolve) (allRes pd)

-- | This is the only use for "Data.Graph".
resolutionToComponents ::  PD -> [[Int]]
resolutionToComponents pd = map (sort . flatten) $ Graph.components resAsGraph where
	graphAsList = [(v,vs) | v <- allArcs pd, vs <- delete v $ allArcs $ connectedTo v pd]
	resAsGraph = Graph.buildG (minArc pd, maxArc pd) graphAsList

	maxArc :: PD -> Int
	maxArc [] = 0
	maxArc pd' = maximum $ map maxNode pd' where
		maxNode :: Node -> Int
		maxNode (Cross a' b' c' d') = maximum [a',b',c',d']
		maxNode (Join a' b') = maximum [a',b']

	minArc :: PD -> Int
	minArc [] = 0
	minArc pd' = minimum $ map minNode pd' where
		minNode :: Node -> Int -- rewrite
		minNode (Cross a' b' c' d') = minimum [a',b',c',d']
		minNode (Join a' b') = minimum [a',b']

	allArcs :: [Node] -> [Int]
	allArcs [] = []
	allArcs (Join a b : ns) = nub (a:b: allArcs ns)
	allArcs (Cross a b c d :ns) = nub (a:b:c:d: allArcs ns)

	connectedTo :: Int -> [Node] -> [Node]
	connectedTo a' = filter (has a') where
		has :: Int -> Node -> Bool
		has x' (Join b' c') = x' `elem` [b',c']
		has x' (Cross b' c' d' e') = x' `elem` [b',c',d',e']


-- | Take a 'PD' and returns a list of all the 'Diagram's of its resolutions.
cubeOfResolutions :: PD -> [Diagram]
cubeOfResolutions pd = [Diagram {resolution' = res, components' = comps res} | res <- allRes pd] where
	comps res = resolutionToComponents $ resolve pd res 

-- | The total cube of resolutions for a braid.
braidCube :: Braid -> [Diagram]
braidCube b = do
	res <- allRes . braidToPD $ b
	let diagram res' = resolutionToComponents . resolve (braidToPD b) $ res' 
	return Diagram {resolution' = res, components' = diagram res} 
