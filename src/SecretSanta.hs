{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
module SecretSanta where

import           Control.Monad (guard)
import           Data.List (delete)
import           Data.Text (Text)
import           System.Random (getStdRandom, randomR)

import           Import (Day)
import           Mail (Address)

type Participant = (Text,Address)

data SantaInfo = SantaInfo
        { santaDescr :: Maybe Text
        , santaDate  :: Maybe Day
        , santaPrice :: Maybe Double
        } deriving (Show,Eq)
        
        

data SantaData = SantaData
        { santaInfo    :: SantaInfo
        , participants :: [Participant]
        } deriving (Show,Eq)


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
        i <- getStdRandom $ randomR (1, length xs)
        let 
                x   = xs !! i
                xs' = take i xs ++ drop (i+1) xs
        (x:) <$> shuffle xs'


randomMatch :: Eq a => [a] -> IO [(a,a)]
randomMatch xs = head <$> match xs <$> shuffle xs >>= shuffle


