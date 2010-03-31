has
===

This is a library which provides generic accessors for records.

    -- This example is based on fclabel's one: http://hackage.haskell.org/package
    import Data.Has
    import Control.Arrow
    
    data Age = Age; data Name = Name
    data City = City; data Country = Country
    
    data Sex = Male | Female deriving (Show)
    
    type Person = Place :&: Info
    
    type Info   = Age :> Int :&: Name :> String :&: Row Sex
    type Place  = City :> String :&: Country :> String
    
    nonowarn :: Person
    nonowarn = nonowarnPlace & nonowarnInfo
      where
        nonowarnPlace = City .> "Yokohama" & Country .> "Japan"
        nonowarnInfo  = Age .> 17 & Name .> "nonowarn" & row Male
    
    -- nonowarn :: Person
    -- nonowarn = injl Name "nonowarn"
    --          . injl Age 17
    --          . inj Male
    --          . injl City "Yokohama"
    --          . injl Country "Japan"
    --          $ error "unknown record"
    
    getAge :: (Knows Age Int a) => a -> Int
    getAge = prjl Age
    
    moveToKyoto :: (Knows City String a) => a -> a
    moveToKyoto = injl City "Kyoto"
    
    getCity :: (Knows City String a) => a -> String
    getCity = prjl City
    
    spendFourYears :: (Knows Age Int a) => a -> a
    spendFourYears = updl Age (+4)
    
    test = (21,"Kyoto") == ((getAge &&& getCity) . spendFourYears . moveToKyoto $ nonowarn)
