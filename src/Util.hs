module Util
where

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = ys ++ tail zs where
	(ys,zs) = splitAt i xs

maybeTuple :: Maybe (a,b) -> (Maybe a, Maybe b)
maybeTuple (Just (a,b)) = (Just a, Just b)
maybeTuple Nothing = (Nothing, Nothing)
