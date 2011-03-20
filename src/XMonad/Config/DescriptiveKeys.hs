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
, SearchTextPrompt(..)
, defaultSearchTextPrompt
, DescribeKeys(..)
, defaultDescribeKeys
, HelpPromptConfig(..)
, helpPrompt
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Foldable as F
import XMonad
import XMonad.Prompt
import XMonad.Prompt.Input
import Data.Bits
import Data.List

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
  DescriptiveKeysPP ([DescriptiveKey] -> String)

defaultDescriptiveKeysPP ::
  DescriptiveKeysPP
defaultDescriptiveKeysPP =
  DescriptiveKeysPP (unlines . fmap (\(DescriptiveKey m s _ d _) ->
                       let pick n str = if n .&. complement m == 0 then str else ""
                           mk = concatMap (++"-") . filter (not . null) . map (uncurry pick) $
                               [
                                 (mod1Mask,    "mod")
                               , (mod2Mask,    "mod")
                               , (mod3Mask,    "mod")
                               , (mod4Mask,    "mod")
                               , (mod5Mask,    "mod")
                               , (controlMask, "cntrl")
                               , (shiftMask,   "shift")
                               ]
                           space g = g ++ replicate (16 - length g) ' '
                       in space (mk ++ keysymToString s) ++ case d of
                                                              Description Nothing  -> ""
                                                              Description (Just e) -> "    " ++ e))

newtype SearchTextPrompt =
  SearchTextPrompt String
  deriving (Eq, Ord, Show)

defaultSearchTextPrompt ::
  SearchTextPrompt
defaultSearchTextPrompt =
  SearchTextPrompt "Search key-bindings"

newtype DescribeKeys =
  DescribeKeys {
    describeKeys :: String -> X ()
  }

defaultDescribeKeys ::
  DescriptiveKeys
  -> XConfig Layout
  -> DescribeKeys
defaultDescribeKeys k l =
  let dk (DescriptiveKeys g) = g
      pp (DescriptiveKeysPP p) = p
      j s = dk (filterTags (searchTags defaultSearchTags s) k) l
  in DescribeKeys (\z -> spawn ("xmessage \"" ++ pp defaultDescriptiveKeysPP (j z) ++ "\""))

data HelpPromptConfig =
  HelpPromptConfig {
    descriptiveHelp :: DescriptiveKeys
  , xpConfigHelp    :: XPConfig
  , keyHelp         :: (ButtonMask, KeySym)
  , searchTextHelp  :: SearchTextPrompt
  , describeHelp    :: DescribeKeys
  }

helpPrompt ::
  (XConfig Layout -> HelpPromptConfig)
  -> XConfig l
  -> XConfig l
helpPrompt f c =
  c {
    keys = \d -> let HelpPromptConfig ks xpc ms (SearchTextPrompt stp) (DescribeKeys describek) = f d
                     compl s = return $ filter (isPrefixOf s) . fmap (\(Tag t) -> t) $ S.toList (allTags d ks)
                 in M.insert ms (inputPromptWithCompl xpc stp compl ?+ describek) (keys c d)
   }

