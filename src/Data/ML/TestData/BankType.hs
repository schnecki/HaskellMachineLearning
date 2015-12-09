

-- | TODO: comment this module
module Data.ML.TestData.BankType where

import Data.ML.DecisionTree

data Bank = Bank {
  age :: Int
  , job :: Job
  , marital :: Marital
  , education :: Education
  , def :: Bool
  , balance :: Int
  , housing :: Bool
  , loan :: Bool
  , contact :: Contact
  , day :: Int
  , month :: Month
  , duration :: Int
  , campaign :: Int
  , pdays :: Int
  , previous :: Int
  , poutcome :: POutcome
  , y :: Bool
                 } deriving (Show, Eq, Ord)


data Job = Management
         | Technician
         | Entrepreneur
         | BlueCollar
         | JobUnknown
         | Retired
         | Admin
         | Services
         | Student
         | SelfEmployed
         | Unemployed
         | Housemaid
         deriving (Show, Eq, Enum, Ord, Bounded)

data Marital = Married
             | Single
             | Divorced
               deriving (Show, Eq, Enum, Ord, Bounded)

data Education = Primary
               | Secondary
               | Tertiary
               | EduUnknown
               deriving (Show, Eq, Enum, Ord, Bounded)


data Month = Jan
           | Feb
           | Mar
           | Apr
           | May
           | Jun
           | Jul
           | Aug
           | Sep
           | Oct
           | Nov
           | Dec
           deriving (Show, Eq, Enum, Ord, Bounded)

data Contact = ConUnknown
             | Telephone
             | Cellular
             deriving (Show, Eq, Enum, Ord, Bounded)

data POutcome = OutUnknown
              | Success
              | Failure
              | Other
              deriving (Show, Eq, Enum, Ord, Bounded)


attrBank :: [Attr Bank]
attrBank = [
  attrNr age "age"
  , attr job "job"
  , attr marital "marital"
  , attr education "education"
  , attr def "def"
  , attrNr balance "balance"
  , attr housing "housing"
  , attr loan "loan"
  , attr contact "contact"
  , attrNr day "day"
  , attr month "month"
  , attrNr duration "duration"
  , attrNr campaign "campaign"
  , attrNr pdays "pdays"
  , attrNr previous "previous"
  , attr poutcome "poutcome"
  , attr y "y"
           ]


