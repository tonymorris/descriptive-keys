module XMonad.Config.DescriptiveKeys
(
  Tag(..)
, Tags
, Description(..)
, DescriptiveKey(..)
, defaultDescriptiveKey
, DescriptiveKeys
, descriptiveKeys
, wKeys
, setDescriptiveKeys
, allTags
, SearchTags(..)
, defaultSearchTags
, filterTags
, DescriptiveKeysPP(..)
, defaultDescriptiveKeysPP
, DescribeKeys(..)
, defaultDescribeKeys
, describeTags
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Foldable as F
import XMonad
import Data.Bits

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
  }

defaultDescriptiveKey ::
  ButtonMask
  -> KeySym
  -> X ()
  -> DescriptiveKey
defaultDescriptiveKey m s a =
  DescriptiveKey m s a (Description Nothing) S.empty

newtype DescriptiveKeys =
  DescriptiveKeys (XConfig Layout -> [DescriptiveKey])

descriptiveKeys ::
  (XConfig Layout -> [DescriptiveKey])
  -> DescriptiveKeys
descriptiveKeys =
  DescriptiveKeys

wKeys ::
  (XConfig Layout -> [(KeyMask, KeySym, X(), String, [String])])
  -> DescriptiveKeys
wKeys z =
  descriptiveKeys (fmap (\(m, s, a, d, t) -> DescriptiveKey m s a (Description (Just d)) (S.fromList (fmap Tag t))) . z)

setDescriptiveKeys ::
  DescriptiveKeys
  -> XConfig l
  -> XConfig l
setDescriptiveKeys k l =
  let rawKeys (DescriptiveKeys d) = F.foldl' (\p (DescriptiveKey m s a _ _) -> M.insert (m, s) a p) M.empty . d
  in l { keys = rawKeys k }

allTags ::
  XConfig Layout
  -> DescriptiveKeys
  -> Tags
allTags l (DescriptiveKeys k) =
  S.unions (fmap tags (k l))

newtype SearchTags =
  SearchTags {
    searchTags :: String -> Tags
  }

defaultSearchTags ::
  SearchTags
defaultSearchTags =
  SearchTags (S.fromList . fmap Tag . words)

filterTags ::
  Tags
  -> DescriptiveKeys
  -> DescriptiveKeys
filterTags t z@(DescriptiveKeys k) =
  if S.null t
    then z
    else DescriptiveKeys (\l -> filter (\(DescriptiveKey _ _ _ _ u) -> not (S.null (S.intersection t u))) $ k l)

data DescriptiveKeysPP =
  DescriptiveKeysPP {
    descriptiveKeysPP :: [DescriptiveKey] -> String
  , searchText :: String
  }

defaultDescriptiveKeysPP ::
  DescriptiveKeysPP
defaultDescriptiveKeysPP =
  DescriptiveKeysPP (unlines . fmap (\(DescriptiveKey m s _ d _) ->
                       let pick n str = if n .&. complement m == 0 then str else ""
                           mk = concatMap (++"-") . filter (not . null) . map (uncurry pick) $
                               [
                                 (mod1Mask, "mod")
                               , (mod2Mask, "mod")
                               , (mod3Mask, "mod")
                               , (mod4Mask, "mod")
                               , (mod5Mask, "mod")
                               , (controlMask, "cntrl")
                               , (shiftMask,"shift")
                               ]
                           space g = g ++ replicate (16 - length g) ' '
                       in space (mk ++ keysymToString s) ++ case d of
                                                              Description Nothing  -> ""
                                                              Description (Just e) -> "    " ++ e)) "Search key-bindings"

newtype DescribeKeys =
  DescribeKeys {
    describeKeys :: [DescriptiveKey] -> X ()
  }

defaultDescribeKeys ::
  DescriptiveKeysPP
  -> DescribeKeys
defaultDescribeKeys pp =
  DescribeKeys (\k -> spawn ("xmessage \"" ++ descriptiveKeysPP pp k ++ "\""))

describeTags ::
  SearchTags
  -> DescribeKeys
  -> DescriptiveKeys
  -> XConfig Layout
  -> String
  -> X ()
describeTags (SearchTags s) (DescribeKeys k) d l e =
  let DescriptiveKeys y = filterTags (s e) d
  in k (y l)
