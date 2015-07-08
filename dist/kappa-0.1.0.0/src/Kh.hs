module Kh
where
import Util
import Complex
import Braids
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.List 
import Control.Monad
import Control.Arrow (second)

-- Returns the diagram underlying a generator.
gToD :: Generator -> Diagram
gToD g = Diagram {resolution' = resolution g, components' = components g}
	
-- compute the homological grading of a generator
homGrading :: Generator -> Int
homGrading gen = sum . resolution $ gen

-- compute the q-grading of a generator.  In this convention, the Khovanov differential *lowers* the q-grading by 1.
qGrading :: Generator -> Int
qGrading gen = sum . signs $ gen

-- Takes a component and a braid width and determines if the component is non-trivial.
-- To compute the mod 2 winding number about the braid axis, check how many of the 'top arcs' live in a component.  -- this blows
nonTrivialCircle :: [Int] -> Int -> Bool
nonTrivialCircle arcs width = odd $ length (filter (`elem` [1..width]) arcs) -- rewrite

nonTrivialCircle' :: [Int] -> Int -> Int -- combine?
nonTrivialCircle' arcs width | nonTrivialCircle arcs width		= 1
                             | not(nonTrivialCircle arcs width)	= 0

-- Compute the kgrading of a generator.
kGrading :: (Resolution, [[Int]],[Int]) -> Int -> Int
kGrading (_,cs,ss) width = sum ks where
	nonTriv = [nonTrivialCircle' c width | c <- cs]
	ks = zipWith (*) ss nonTriv

-- "Applies the filtered Khovanov functor to a diagram."
-- We still need the braid width as input to get the k-grading.
khovanov :: Int -> Diagram -> [Generator]
khovanov width d = do
	ss <- generateSigns . components' $ d
	return Generator {resolution = resolution' d,
					  components = components' d, 
					  signs = ss, 
					  kgrade = kGrading (resolution' d, components' d, ss) width} 
    where
        generateSigns cs = Control.Monad.replicateM (length cs) [-1,1] -- this is a slick and dumb way of making sequences of (-1) and 1

-- returns the sign at mark
markedSign :: Int -> Generator -> Int
markedSign mark gen = case findIndex (elem mark) (components gen) of
	Just x -> (!!) (signs gen) x
	Nothing -> -1 -- should thrown a exception

-- "Applies the reduced Khovanov functor to a diagram"
reducedKhovanov :: Int -> Int -> Diagram -> [Generator]
reducedKhovanov mark width cube  = filter (\g -> (-1) == markedSign mark g) (khovanov width cube)

-- "Applies the quotient Khovanov functor to a diagram"
quotientKhovanov :: Int -> Int -> Diagram -> [Generator]
quotientKhovanov mark width cube = filter (\g -> (1) == markedSign mark g) (khovanov width cube)

-- The next three functions apply the Khovanov functor to a cube of resolutions
khovanovComplex :: Int -> [Diagram] -> Map Int (Set Generator)
khovanovComplex width cube = M.fromListWith S.union [(homGrading g, S.singleton g) | g <- concatMap (khovanov width) cube]

reducedKhovanovComplex :: Int -> Int -> [Diagram] -> Map Int (Set Generator)
reducedKhovanovComplex mark width cube = M.fromListWith S.union [(homGrading g, S.singleton g) | g <- concatMap (reducedKhovanov mark width) cube]

quotientKhovanovComplex :: Int -> Int -> [Diagram] -> Map Int (Set Generator)
quotientKhovanovComplex mark width cube = M.fromListWith S.union [(homGrading g, S.singleton g) | g <- concatMap (quotientKhovanov mark width) cube]

-- A morphism is determined by three components, but merges and splits are different.
data ElMo -- elementary morphisms, terminology stolen from Milatz.
	= Merge [Int] [Int] [Int] -- merges a b to c
	| Split [Int] [Int] [Int] -- splits a to b c
	deriving (Show, Eq)

-- Takes two diagrams and returns the morphisms between them, if there is one.  
whichMorphism :: Diagram -> Diagram -> Maybe ElMo
whichMorphism d d'     | not (succRes d d') 				  = Nothing
                       | length cs2' == 2 && length cs1' == 1 = Just (Split (head cs1') (head cs2') (head $ tail cs2'))
                       | length cs1' == 2 && length cs2' == 1 = Just (Merge (head cs1') (head $ tail cs1') (head cs2'))
                       | otherwise 						 	  =  Nothing
                       where
                         cs2' = components' d' \\ components' d
                         cs1' = components' d \\ components' d'
                         succRes :: Diagram -> Diagram -> Bool
                         succRes e e' = all (>=0) diff && (sum diff == 1) where 
	                       diff = zipWith subtract (resolution' e)  (resolution' e')

-- Takes a morphism and two generators and returns True if there should be an edge from one to the other.
morphismAction :: Maybe ElMo -> Generator -> Generator -> Bool
morphismAction (Just (Merge c1 c2 c3)) g g' | (c1 `notElem` cs1) || (c2 `notElem` cs1) || (c3 `notElem` cs2) 		= False
                                            | (cs2 \\ [c3]) /= (cs1 \\ [c1,c2])                                     = False
                                            | (deleteAt a2' . deleteAt a1) ss1 /= deleteAt a3 ss2 					= False
                                            | ((!!) ss1 a1 == 1) && ((!!) ss1 a2 == 1) && ((!!) ss2 a3 == 1)		= True
                                            | ((!!) ss1 a1 == 1) && ((!!) ss1 a2 == (-1)) && ((!!) ss2 a3 == (-1))	= True
                                            | ((!!) ss1 a1 == (-1)) && ((!!) ss1 a2 == 1) && ((!!) ss2 a3 == (-1))	= True
                                            | otherwise = False
											where
                                                    cs1 = components g 
                                                    cs2 = components g'
                                                    ss1 = signs g 
                                                    ss2 = signs g'
                                                    Just a1 =  elemIndex c1 cs1
                                                    Just a2 =  elemIndex c2 cs1
                                                    Just a3 =  elemIndex c3 cs2
                                                    Just a2' = elemIndex c2 (deleteAt a1 cs1)

morphismAction (Just (Split c1 c2 c3)) g g' | (c1 `notElem` cs1) || (c2 `notElem` cs2) || (c3 `notElem` cs2)    		= False
                                            | (cs2 \\ [c2,c3])  /= (cs1 \\ [c1])                                        = False
                                            | deleteAt a1 ss1 /= (deleteAt a3' . deleteAt a2) ss2					    = False
                                            | ((!!) ss1 a1 == 1)    && ((!!) ss2 a2 == 1)    && ((!!) ss2 a3 == (-1))   = True
                                            | ((!!) ss1 a1 == 1)    && ((!!) ss2 a2 == (-1)) && ((!!) ss2 a3 == 1) 	    = True
                                            | ((!!) ss1 a1 == (-1)) && ((!!) ss2 a2 == (-1)) && ((!!) ss2 a3 == (-1))	= True
                                            | otherwise = False
                                            where
                                                    cs1 = components g 
                                                    cs2 = components g'
                                                    ss1 = signs g 
                                                    ss2 = signs g'
                                                    Just a1 =  elemIndex c1 cs1
                                                    Just a2 =  elemIndex c2 cs2
                                                    Just a3 =  elemIndex c3 cs2
                                                    Just a3' = elemIndex c3 (deleteAt a2 cs2)

morphismAction (Nothing) _ _  = False

kDrop :: Generator -> Generator -> Int
kDrop g g' = kgrade g - kgrade g'

-- Like morphismAction, but only connects two generators if the drop in kgrading from one to the other is less than or equal to a.
filteredMorphismAction :: Int -> Maybe ElMo -> Generator -> Generator -> Bool
filteredMorphismAction k e g g' | kDrop g g' <= k = morphismAction e g g'
                                | otherwise	= False

-- returns Morphisms from the generator into the set with kDrop less than or equal to k
filteredMorphismsFrom :: Int -> Generator -> Set Generator -> Morphisms
filteredMorphismsFrom k g gs = M.singleton g gs' where
								 gs' = S.filter (\g' -> filteredMorphismAction k (mor g') g g') gs
								 mor g' = whichMorphism (gToD g) (gToD g')

-- Applies filteredSubMorphismsFrom to every generator in a list into the same list.
filteredComplexLevel :: Int -> Map Int (Set Generator) -> Int -> Morphisms
filteredComplexLevel k gs i = case M.lookup (i+1) gs of
	Nothing -> M.empty
	otherwise -> M.unionsWith S.union . S.toList . S.map (\g -> filteredMorphismsFrom k g gsi1) $ gsi where
		gsi = gs ! i
		gsi1 = gs ! (i+1)

-- Produces the generator corresponding to the transverse invariant of a braid.
psi :: Braid -> Generator
psi b = Generator {resolution = res, components = comps, signs = ss, kgrade = k} where
	res = fmap (\x -> if x >= 0 then 0 else 1) (braidWord b)
	comps = resolutionToComponents . flip resolve res . braidToPD $ b 
	ss = replicate (length comps) (-1)
	k = (-1)* braidWidth b

-- Produces the generator corresponding to the QUOTIENT transverse invariant of a braid
quotPsi :: Braid -> Int -> Generator
quotPsi b p = Generator {resolution = res, components = comps, signs = ss, kgrade = k} where
    res = fmap (\x -> if x >= 0 then 0 else 1) (braidWord b)
    comps = resolutionToComponents . flip resolve res . braidToPD $ b 
    Just c = findIndex (elem p) comps
    ss = uncurry (++) . second (\xs -> 1:xs) . second tail . splitAt c $ replicate (length comps)(-1)
    k = kGrading (res, comps, ss) (braidWidth b)

-- Produces the portion of the cube of resolutions of a braid which is relevant for computing kappa.
-- This means only using resolutions whose weights are less than or equal to psi's.
-- note that this only uses the homological grading of psi, so we don't need a separate function for the quotient.
psiCube :: Braid -> [Diagram]
psiCube b =  do
	res <- allRes (braidToPD b)
	let diagram = resolutionToComponents . resolve (braidToPD b) 
	guard (sum res <= (sum . resolution . psi $ b)) 
	return Diagram {resolution' = res, components' = diagram res}
