{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Grammar where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic (..))
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

data Tune = Tune Line Line
  deriving (Show, Eq, Generic)
data Line = Line Tonic Bar Bar Tonic
  deriving (Show, Eq, Generic)
data Bar
  = TonicBar Tonic
  | SubBar Subdominant
  | DomBar Dominant
  | LowBar Low
  deriving (Show, Eq, Generic)

data Tonic = Tonic Ton ByTon Ton Ton ByTon Ton
  deriving (Show, Eq, Generic)
data Dominant = Dominant Dom ByDom Dom Dom ByDom Dom
  deriving (Show, Eq, Generic)
data Subdominant = Subdominant Subd BySubd Subd Subd BySubd Subd
  deriving (Show, Eq, Generic)
data Low = Low Lw Lw Lw Lw Lw Lw
  deriving (Show, Eq, Generic)

data Ton = At | Dt | Ft | Aht deriving (Show, Eq, Generic)
data ByTon = Bt | Gt | ByTon Ton deriving (Show, Eq, Generic)
data Dom = Ad | Cd | Ed | Ahd deriving (Show, Eq, Generic)
data ByDom = Fd | Gd | ByDom Dom deriving (Show, Eq, Generic)
data Subd = Bs | Ds | Gs | Bhs deriving (Show, Eq, Generic)
data BySubd = Es | BySubd Subd deriving (Show, Eq, Generic)
data Lw = Fl | Bl | Dl | Flt deriving (Show, Eq, Generic)

class ShowTune a where
  showTune :: a -> Text

instance ShowTune Tune where
  showTune (Tune line1 line2) =
    showTune line1 <> "\n" <> showTune line2

instance ShowTune Line where
  showTune (Line b1 b2 b3 b4) =
    T.concat $ map (\b -> showTune b <> "|") [TonicBar b1, b2, b3, TonicBar b4]

instance ShowTune Bar where
  showTune (TonicBar t) = showTune t
  showTune (SubBar s)   = showTune s
  showTune (DomBar d)   = showTune d
  showTune (LowBar l)   = showTune l

instance ShowTune Tonic where
  showTune (Tonic t1 t2 t3 t4 t5 t6) =
    showTune t1 <> showTune t2 <> showTune t3 <> " " <>
    showTune t4 <> showTune t5 <> showTune t6

instance ShowTune Dominant where
  showTune (Dominant t1 t2 t3 t4 t5 t6) =
    showTune t1 <> showTune t2 <> showTune t3 <> " " <>
    showTune t4 <> showTune t5 <> showTune t6

instance ShowTune Subdominant where
  showTune (Subdominant t1 t2 t3 t4 t5 t6) =
    showTune t1 <> showTune t2 <> showTune t3 <> " " <>
    showTune t4 <> showTune t5 <> showTune t6

instance ShowTune Low where
  showTune (Low t1 t2 t3 t4 t5 t6) =
    showTune t1 <> showTune t2 <> showTune t3 <> " " <>
    showTune t4 <> showTune t5 <> showTune t6

instance ShowTune Ton where
  showTune At  = "a"
  showTune Dt  = "d"
  showTune Ft  = "f"
  showTune Aht = "A"

instance ShowTune ByTon where
  showTune Bt          = "b"
  showTune Gt          = "g"
  showTune (ByTon ton) = showTune ton

instance ShowTune Dom where
  showTune Ad  = "a"
  showTune Cd  = "c"
  showTune Ed  = "e"
  showTune Ahd = "A"

instance ShowTune ByDom where
  showTune Fd          = "f"
  showTune Gd          = "g"
  showTune (ByDom dom) = showTune dom

instance ShowTune Subd where
  showTune Bs  = "b"
  showTune Ds  = "d"
  showTune Gs  = "g"
  showTune Bhs = "B"

instance ShowTune BySubd where
  showTune Es            = "e"
  showTune (BySubd subd) = showTune subd

instance ShowTune Lw where
  showTune Fl  = "f"
  showTune Bl  = "b"
  showTune Dl  = "d"
  showTune Flt = "F"

#define GENARB(T) instance Arbitrary T where\
  arbitrary = genericArbitrary;\
  shrink = genericShrink

GENARB(Tune)
GENARB(Line)
GENARB(Bar)
GENARB(Tonic)
GENARB(Dominant)
GENARB(Subdominant)
GENARB(Low)
GENARB(Ton)
GENARB(ByTon)
GENARB(Dom)
GENARB(ByDom)
GENARB(Subd)
GENARB(BySubd)
GENARB(Lw)
