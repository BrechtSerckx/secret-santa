module SecretSanta.Match
where

import           Control.Monad (guard)
import           Data.List (delete)
import           System.Random (getStdRandom, randomR)


match :: Eq a => [a] -> [a] -> [[(a,a)]]
match [] [] = [[]]
match [] _  = []
match (x:xs) ys = do
        y <- ys
        guard $ x /= y
        map ((x,y):) $ match xs (delete y ys)


shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
        i <- getStdRandom $ randomR (0, length xs - 1)
        let 
                x   = xs !! i
                xs' = take i xs ++ drop (i+1) xs
        (x:) <$> shuffle xs'


randomMatch :: (Eq a) => [a] -> IO [(a,a)]
randomMatch xs = head <$> match xs <$> shuffle xs >>= shuffle


