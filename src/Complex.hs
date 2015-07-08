module Complex
where
import Braids
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map, keys, (!))
import qualified Data.Map as M
import Data.Monoid

-- | A Generator has a resolution, a list of components, a list of signs (all 1 or -1), and a k-grading.  The k-grading is special in that it is determined by the other three parts, but this is not indicated in the data structure.
data Generator = Generator { resolution :: Resolution
			   , components :: [[Int]]
			   , signs :: [Int]
			   , kgrade :: Int}
			   deriving (Eq, Ord, Show)

-- | Morphisms is a map from generators to sets of generators
type Morphisms = Map Generator (Set Generator)

-- | Cancellations are Morphisms, but they have a special composition
data Cancellations = Cancellations (Map Generator (Set Generator))

-- | The next three items allow us to record cancellation
instance Monoid Cancellations where
	mempty = Cancellations M.empty
	mappend (Cancellations c) (Cancellations c') = Cancellations (c `consume` c')

consume :: Ord k => Map k (Set k) -> Map k (Set k) -> Map k (Set k) -- c consumes c'
consume c c' = foldr (flip consumeKey) c (M.toList c')

consumeKey :: Ord k => Map k (Set k) -> (k, Set k) -> Map k (Set k) -- assumes k `member` m
consumeKey m (k, ks) = if M.null . M.filter (S.member k) $ m then m `M.union` M.singleton k ks
					   else foldr (M.adjust (addMod2Set ks)) m changeKeys where
						 changeKeys = keys (M.filter (S.member k) m)

-- | The next three items implement mod 2 addition at the level of sets and Morphisms
addMod2 :: (Eq a, Ord a) => a -> Set a -> Set a
addMod2 b set = if S.member b set then S.delete b set else S.insert b set

addMod2Set :: (Eq a, Ord a) => Set a -> Set a -> Set a
addMod2Set bs set = S.foldr addMod2 set bs 

addMod2Map :: Map Generator (Set Generator) -> Map Generator (Set Generator) -> Map Generator (Set Generator)
addMod2Map = M.unionWith addMod2Set

-- | Return the complex simplified at (g,g')
simplifyEdgeGraph :: (Generator, Generator) -> Morphisms -> Morphisms
simplifyEdgeGraph (g,g') mors = addMod2Map newArrows (deleteEdge (g,g') mors) where
				fromG = S.toList . S.delete g' $ (mors ! g)
				toG'  = keys . M.delete g . M.filter (S.member g') $ mors
				newArrows = M.fromListWith S.union [(a, S.singleton b) | a <- toG', b <- fromG]
				deleteEdge (h,h') mors' = (fmap (S.delete h) . M.delete h) .
										   fmap (S.delete h') . M.delete h' 
										   $ mors'
