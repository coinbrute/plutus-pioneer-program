{-# LANGUAGE TemplateHaskell #-}

module Week8.Lens where

import Control.Lens

-- straightforward type of company consisting of a list of Persons
newtype Company = Company {_staff :: [Person]} deriving Show

data Person  = Person
    { _name    :: String
    , _address :: Address
    } deriving Show

-- address is just a city 
newtype Address = Address {_city :: String} deriving Show

-- heres some people
alejandro, lars :: Person
alejandro = Person
  {  _name    = "Alejandro"
  ,  _address = Address {_city = "Zacateca"}
  }
lars = Person
  {  _name    = "Lars"
  ,  _address = Address {_city = "Regensburg"}
  }

iohk :: Company
iohk = Company { _staff = [alejandro, lars] }

goTo :: String -> Company -> Company
goTo there c = c {_staff = map movePerson (_staff c)}
  where
    movePerson p = p {_address = (_address p) {_city = there}}

-- template haskell to make lens out of Company Person and Address
  -- assuming the fields in record syntax definitions have been underscored 
makeLenses ''Company
makeLenses ''Person
makeLenses ''Address

goTo' :: String -> Company -> Company
-- these fields now can be grabbed via their optics 
  -- (.~) binds the value on the right to the record on the left
goTo' there c = c & staff . each . address . city .~ there
