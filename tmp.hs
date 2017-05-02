import Data.Monoid ((<>))

data Values = Values { onGround :: Double
                     , underWater :: Double
                     , inSky :: Double
                     , antiGravity :: Double
                     }
                     deriving (Show)

data Object = Object { name         :: String
                     , description  :: String
                     , speed        :: Values
                     , acceleration :: Double
                     , weight       :: Double
                     , handling     :: Values
                     , traction     :: Double
                     , miniTurbo    :: Double
                     }
                     deriving (Show)

instance Monoid Values where
    mempty
        = Values 0 0 0 0
    mappend (Values g w s a) (Values g' w' s' a')
        = Values (g + g') (w + w') (s + s') (a + a')

instance Monoid Object where
    mempty
        = Object { name         = "<No name> "
                 , description  = mempty
                 , speed        = mempty
                 , acceleration = 0
                 , weight       = 0
                 , handling     = mempty
                 , traction     = 0
                 , miniTurbo    = 0
                 }
    mappend (Object n d s a w h t m) (Object n' d' s' a' w' h' t' m')
        = Object (n ++ " + " ++ n') (d <> d') (s <> s') (a + a') (w + w') (h <> h') (t + t') (m + m')

{------------------------------- Data --}

metalMario = Object { name = "Metal Mario"
                    , description = "https://www.mariowiki.com/Metal_Mario_(character)"
                    , speed = Values 4.25  4.5  4.75  4
                    , acceleration = 3.25
                    , weight = 4.5
                    , handling = Values 3.25  2.75  3.25  3.25
                    , traction = 3.25
                    , miniTurbo = 3
                    }

pinkGoldPeach = Object { name = "Pink Gold Peach"
                       , description = "https://www.mariowiki.com/Pink_Gold_Peach"
                       , speed = Values 4.25  4.5  4.75  4
                       , acceleration = 3.25
                       , weight = 4.5
                       , handling = Values 3.25  2.75  3.25  3.25
                       , traction = 3.25
                       , miniTurbo = 3
                       }

drivers = [metalMario, pinkGoldPeach]

standardKart = Object { name = "Standard Kart"
                      , description = "https://www.mariowiki.com/Standard_Kart"
                      , speed = Values 0 0 0 0
                      , acceleration = 0
                      , weight = 0
                      , handling = Values 0 0 0 0
                      , traction = 0
                      , miniTurbo = 0
                      }

bodies = [standardKart]

standardTire = Object { name = "Standard Tire"
                      , description = "https://www.mariowiki.com/Standard_(tire)"
                      , speed = Values 0 0 0 0
                      , acceleration = 0
                      , weight = 0
                      , handling = Values 0 0 0 0
                      , traction = 0
                      , miniTurbo = 0
                      }

tires = [standardTire]

superGlider = Object { name = "Super Glider"
                     , description = "https://www.mariowiki.com/Super_Glider"
                     , speed = Values 0 0 0 0
                     , acceleration = 0
                     , weight = 0
                     , handling = Values 0 0 0 0
                     , traction = 0
                     , miniTurbo = 0
                     }

glider = [superGlider]

type OpDouble = Maybe Double

search :: OpDouble -> OpDouble -> OpDouble -> OpDouble -> OpDouble -> [Object]
search spd acc wgt hdl grip =
    let candidates = [c <> b <> t <> g | c <- drivers, b <- bodies, t <- tires, g <- glider]
    in filter conditions candidates
        where
            convert :: OpDouble -> Double 
            convert Nothing = 0
            convert (Just x) = x

            spd' = convert spd
            acc' = convert acc
            wgt' = convert wgt
            hdl' = convert hdl
            grp' = convert grip

            conditions :: Object -> Bool
            conditions obj = if (onGround (speed obj) > spd'
                              && acceleration obj > acc')
                             then True
                             else False
