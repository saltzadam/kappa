module Cancellation
where
import Braids
import Complex
import Util
import Kh
import Data.List (find, (\\))
import Data.Set (member)
import qualified Data.Set as S
import Data.Map.Strict ((!), mapWithKey, keys)
import qualified Data.Map as M
import Control.Monad.Writer

-- returns all morphisms which change the kgrading by less than k
kFilteredMorphisms :: Int -> Morphisms -> Morphisms
kFilteredMorphisms k mors = M.filter (not . S.null) . mapWithKey (\g x -> S.filter (\y -> kDrop g y <= k) x) $ mors

-- returns all generators which map to psi'
whoHasPsi :: Generator -> Morphisms -> [Generator]
whoHasPsi psi' mors = filter (\g -> psi' `member` (mors ! g)) (keys mors)

kWhoHasPsi :: Int -> Generator -> Morphisms -> [Generator]
kWhoHasPsi k psi' = filter (\g -> kgrade g <= kgrade psi' + k) . whoHasPsi psi'

-- does g have a single arrow from it?
soloArrow :: Generator -> Morphisms -> Bool
soloArrow g mors = S.size (mors ! g) == 1

-- what kills psi?
psiKillers :: Generator -> Morphisms -> [Generator]
psiKillers psi' mors = filter (`soloArrow` mors) . whoHasPsi psi' $ mors

-- what kills psi and has kDrop less than or equal to k?
kPsiKillers :: Int -> Generator -> Morphisms -> [Generator]
kPsiKillers k psi' = filter (\g -> kgrade g <= kgrade psi' + k) . psiKillers psi'

-- cancels g in mors while dodging psi
-- records what it canceled
cancelKey :: Generator -> Morphisms -> Generator -> Writer Cancellations Morphisms
cancelKey psi' mors g = let targets' = M.lookup g mors in 
  case targets' of 
    Nothing -> return mors
    Just targets -> if S.null targets || targets == S.singleton psi'
      then return mors
      else do 
          let g' = head . S.toList . S.delete psi' $ targets
          let toG' = keys . M.delete g . M.filter (S.member g') $ mors
          let stored = M.fromList [(a, S.singleton g) | a <- toG'] -- note that a <- toG' does not repeat so we don't need fromListWith
          tell (Cancellations stored)
          return (simplifyEdgeGraph (g, head . S.toList . S.delete psi' $ targets) mors)
        
-- simplify the complex at filtration k while dodging psi'
-- Uses the writer monad to keep track of what's canceled (but that information isn't used, presently)
kSimplify :: Int -> Generator -> Morphisms -> Writer Cancellations Morphisms
kSimplify k psi' mors | null . kWhoHasPsi k psi' $ mors                   = return mors
                      | kWhoHasPsi k psi' mors == kPsiKillers k psi' mors = return mors
                      | otherwise                                         = do 
                                    let g = head (kWhoHasPsi k psi' mors \\ kPsiKillers k psi' mors)
                                    mors' <- cancelKey psi' mors g
                                    kSimplify k psi' mors'

-- simplifies the complex up to filtration k while dodging psi'
-- just composes kSimplify for ascending k
kSimplifyComplex :: Int -> Generator -> Morphisms -> Writer Cancellations Morphisms
kSimplifyComplex k psi' mors = foldM (\mor k' -> kSimplify k' psi' mor) mors [0,2..k] where
  
-- test whether psi' dies at filtration k
kDoesPsiVanish :: Int -> Generator -> Morphisms -> Bool
kDoesPsiVanish k psi' mors = any (`soloArrow` mors) . filter rightK . whoHasPsi psi' $ mors where 
  rightK g = kgrade g - kgrade psi' <= k

-- same as above?  oops
isKappaK :: Int -> Generator -> Morphisms -> Bool
isKappaK k psi' mors = any (`soloArrow` mors) . filter rightK . whoHasPsi psi' $ mors where 
  rightK g = kgrade g - kgrade psi' <= k
  
-- returns (Maybe kappa, Maybe the simplified complex with cancellation information)
computeKappa :: Braid -> (Maybe Int, Maybe (Writer Cancellations Morphisms))
computeKappa braid = maybeTuple
                  . find (\(k, c) -> isKappaK k psi' . fst . runWriter $ c)
                  $ map (\k -> (k, kSimplifyComplex k psi' morphisms )) [0,2..2*braidWidth braid] where
                      morphisms = M.unionsWith S.union . fmap (filteredComplexLevel (2*braidWidth braid) gens) $ [0..(1 + length (braidWord braid))]
                      gens = reducedKhovanovComplex 0 (braidWidth braid) (psiCube braid)
                      psi' = psi braid

computeReducedKappa :: Braid -> Int -> (Maybe Int, Maybe (Writer Cancellations Morphisms))
computeReducedKappa braid m = maybeTuple
                  . find (\(k, c) -> isKappaK k psi' . fst . runWriter $ c)
                  $ map (\k -> (k, kSimplifyComplex k psi' morphisms )) [0,2..2*braidWidth braid] where
                      morphisms = M.unionsWith S.union . fmap (filteredComplexLevel (2*braidWidth braid) gens) $ [0..(1 + length (braidWord braid))]
                      gens = reducedKhovanovComplex m (braidWidth braid) (psiCube braid)
                      psi' = psi braid

computeQuotientKappa :: Braid -> Int -> (Maybe Int, Maybe (Writer Cancellations Morphisms))
computeQuotientKappa braid m = maybeTuple
                  . find (\(k, c) -> isKappaK k psi' . fst . runWriter $ c)
                  $ map (\k -> (k, kSimplifyComplex k psi' morphisms )) [0,2..2*braidWidth braid] where
                      morphisms = M.unionsWith S.union . fmap (filteredComplexLevel (2*braidWidth braid) gens) $ [0..(1 + length (braidWord braid))]
                      gens = quotientKhovanovComplex m (braidWidth braid) (psiCube braid)
                      psi' = quotPsi braid m

computeKappaNum :: Braid -> Maybe Int
computeKappaNum =  fst . computeKappa

computeReducedKappaNum :: Braid -> Int -> Maybe Int
computeReducedKappaNum b m  = fst $ computeReducedKappa b m 

computeQuotientKappaNum :: Braid -> Int -> Maybe Int
computeQuotientKappaNum b m = fst $ computeQuotientKappa b m

computeKappaComplex :: Braid -> Maybe (Writer Cancellations Morphisms)
computeKappaComplex = snd . computeKappa