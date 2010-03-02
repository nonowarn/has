has
===

This is a library which provides generic accessors for records.

    -- This example is based on fclabel's one: http://hackage.haskell.org/package/fclabels
    
    import Data.Has
    
    import Control.Arrow
    
    type Person = Name :*: Age :*: Sex :*: Place
    type Place  = City :*: Country
    
    newtype Name = Name { unName :: String }
    newtype Age  = Age  { unAge  :: Int    }
    data    Sex  = Male | Female
    newtype City = City { unCity :: String }
    newtype Country = Country { unCountry :: String }
    
    nonowarn :: Person
    nonowarn =   Name "nonowarn"
             :*: Age 17
             :*: Male
             :*: City "Yokohama"
             :*: Country "Japan"
    
    getAge :: (Has Age a) => a -> Int
    getAge = unAge . prj
    
    moveToKyoto :: (Has City a) => a -> a
    moveToKyoto = inj (City "Kyoto")
    
    getCity :: (Has City a) => a -> String
    getCity = unCity . prj
    
    spendFourYears :: (Has Age a) => a -> a
    spendFourYears = upd (Age . (+4) . unAge)
    
    test = (21,"Kyoto") == ((getAge &&& getCity) . spendFourYears . moveToKyoto $ nonowarn)
