module XMonad.Config.DescriptiveKeys
(
  Category(..)
, Tag(..)
, Tags
, Description(..)
, DescriptiveKey(..)
, defaultDescriptiveKey
, DescriptiveKeys
, descriptiveKeys
, wKeys
, setDescriptiveKeys
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Foldable as F
import XMonad

newtype Category =
  Category (Maybe String)
  deriving (Eq, Ord, Show)

newtype Tag =
  Tag String
  deriving (Eq, Ord, Show)

type Tags =
  S.Set Tag

newtype Description =
  Description (Maybe String)
  deriving (Eq, Ord, Show)

data DescriptiveKey =
  DescriptiveKey {
    mask        :: ButtonMask
  , sym         :: KeySym
  , action      :: X ()
  , description :: Description
  , tags        :: Tags
  , category    :: Category
  }

defaultDescriptiveKey ::
  ButtonMask
  -> KeySym
  -> X ()
  -> DescriptiveKey
defaultDescriptiveKey m s a =
  DescriptiveKey m s a (Description Nothing) S.empty (Category Nothing)

newtype DescriptiveKeys =
  DescriptiveKeys (XConfig Layout -> [DescriptiveKey])

descriptiveKeys ::
  (XConfig Layout -> [DescriptiveKey])
  -> DescriptiveKeys
descriptiveKeys =
  DescriptiveKeys

wKeys ::
  (XConfig Layout -> [(String, [(KeyMask, KeySym, X(), String, [String])])])
  -> DescriptiveKeys
wKeys z =
  descriptiveKeys (\l -> let h = M.fromList (z l)
                         in do c               <- M.keys h
                               (m, s, a, d, t) <- h M.! c
                               return (DescriptiveKey m s a (Description (Just d)) (S.fromList $ map Tag t) (Category (Just c))))

setDescriptiveKeys ::
  DescriptiveKeys
  -> XConfig l
  -> XConfig l
setDescriptiveKeys k l =
  let rawKeys (DescriptiveKeys d) = F.foldl' (\p (DescriptiveKey m s a _ _ _) -> M.insert (m, s) a p) M.empty . d
  in l { keys = rawKeys k }
