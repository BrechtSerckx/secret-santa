module SecretSanta where

import           Data.Text (Text)

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
