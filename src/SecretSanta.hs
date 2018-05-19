module SecretSanta where

import           Data.List (delete)
import           Data.Tuple (swap)
import           Control.Monad (guard)
import           System.Random (getStdRandom, randomR)


match :: Eq a => [a] -> [a] -> [[(a,a)]]
match [] [] = [[]]
match [] _  = []
match xs ys = do
        let x = head xs
        y <- ys
        guard $ x /= y
        map ((x,y):) $ match (delete x xs) (delete y ys)


shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle lst = do
        (e, rest) <- pickElem <$> getIx
        (e:) <$> shuffle rest
        where
        getIx = getStdRandom $ randomR (1, length lst)
        pickElem n = case splitAt n lst of
                ([], s) -> error $ "failed at index " ++ show n -- should never match
                (r, s)  -> (last r, init r ++ s)


randomMatch :: Eq a => [a] -> IO [(a,a)]
randomMatch xs = head <$> match xs <$> shuffle xs >>= shuffle
