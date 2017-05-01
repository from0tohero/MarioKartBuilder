import Data.Monoid

type MDouble = Sum Double

type NullableMDouble = Maybe MDouble

data Values = Values { onGround :: MDouble
                     , underWater :: NullableMDouble
                     , inSky :: NullableMDouble
                     , antiGravity :: NullableMDouble
                     }
                     deriving (Show)

data Object = Object { name         :: String
                     , description  :: String
                     , speed        :: Values
                     , acceleration :: MDouble
                     , weight       :: MDouble
                     , handling     :: Values
                     , traction     :: MDouble
                     , miniTurbo    :: NullableMDouble
                     }
                     deriving (Show)

instance Monoid Values where
    mempty
        = Values 0 Nothing Nothing Nothing
    mappend (Values g w s a) (Values g' w' s' a')
        = Values (g <> g') (w <> w') (s <> s') (a <> a')

instance Monoid Object where
    mempty
        = Object { name         = "<No name> "
                 , description  = mempty
                 , speed        = mempty
                 , acceleration = mempty
                 , weight       = mempty
                 , handling     = mempty
                 , traction     = mempty
                 , miniTurbo    = mempty
                 }
    mappend (Object n d s a w h t m) (Object n' d' s' a' w' h' t' m')
        = Object (n <> n') (d <> d') (s <> s') (a <> a') (w <> w') (h <> h') (t <> t') (m <> m')
