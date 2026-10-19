module LensPerson

public export
record Address where
    constructor MkAddress
    street : String
    houseNumber : Int
    place : String
    country : String

public export
record Born where
    constructor MkBorn
    bornAt : Address
    bornOn : String

public export
record Person where
    constructor MkPerson
    name : String
    born : Born
    address : Address

renameStreet : String -> Address -> Address
renameStreet s a = {street := s} a

export
bornStreet : Born -> String
bornStreet born = born.bornAt.street

export
setCurrentStreet : String -> Person -> Person
setCurrentStreet s p = {address.street := s} p

export
renameStreets : (String -> String) -> Person -> Person
renameStreets f p =
  {born.bornAt.street $= f, address.street $= f} p