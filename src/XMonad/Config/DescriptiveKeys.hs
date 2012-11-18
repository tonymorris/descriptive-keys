-- | Specify your key-bindings with a description and zero or more tags,
-- then add a key-binding to search through them.
module XMonad.Config.DescriptiveKeys
(
-- * Usage
-- $usage
-- * Description
  Description(..)
, DescriptiveKey(..)
, defaultDescriptiveKey
-- * `DescriptiveKeys` data structure
, DescriptiveKeys
, descriptiveKeys
, wKeys
, setDescriptiveKeys
-- * Tags
, Tag(..)
, Tags
, allTags
, SearchTags(..)
, defaultSearchTags
, filterTags
-- * Pretty-printing the key description
, DescriptiveKeysPP(..)
, defaultDescriptiveKeysPP
-- * The prompt text when searching
, SearchTextPrompt(..)
, defaultSearchTextPrompt
-- * The action to take to describe the keys
, DescribeKeys(..)
, defaultDescribeKeys
-- * Configuration
, HelpPromptConfig(..)
, helpPrompt
, helpPromptAndSet
, defaultHelpPromptAndSet
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Foldable as F
import XMonad
import XMonad.Prompt
import XMonad.Prompt.Input
import Data.Bits
import Data.List

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Config.DescriptiveKeys
--
--
-- Create an instance of `DescriptiveKeys` by annotating key-bindings with a description and list of tags.
--
-- >     myDescriptiveKeys = wKeys $ \c -> [(modMask c, xk_z, spawn "xclock"), "opens xclock", ["open", "xclock"]
--
-- Modify your `XConfig` with a configuration combinator. Following is the simplest.
--
-- >     myXConfig = defaultHelpPromptAndSet myDescriptiveKeys myXPConfig xConfig
--
-- This will set the keys property of the `XConfig` and a key-binding of mod-F1 to search.

-- | Wraps a string to create a tag.
newtype Tag =
  Tag String
  deriving (Eq, Ord, Show)

-- | A set of tags.
type Tags =
  S.Set Tag

-- | Wraps an optional string to denote a description for a key-binding.
newtype Description =
  Description (Maybe String)
  deriving (Eq, Ord, Show)

-- | The data structure that denotes an annotated key-binding.
data DescriptiveKey =
  DescriptiveKey {
    mask        :: ButtonMask
  , sym         :: KeySym
  , action      :: X ()
  , description :: Description
  , tags        :: Tags
  }

-- | A default key-binding that has no description or tags.
defaultDescriptiveKey ::
  ButtonMask
  -> KeySym
  -> X ()
  -> DescriptiveKey
defaultDescriptiveKey m s a =
  DescriptiveKey m s a (Description Nothing) S.empty

-- | A list of descriptive key-bindings that have access to the `XConfig`.
newtype DescriptiveKeys =
  DescriptiveKeys (XConfig Layout -> [DescriptiveKey])

-- | Construct a list of descriptive key-bindings.
descriptiveKeys ::
  (XConfig Layout -> [DescriptiveKey])
  -> DescriptiveKeys
descriptiveKeys =
  DescriptiveKeys

-- | Construct a list of descriptive key-bindings by specifying the description as a string
-- and the tags as a list of strings.
wKeys ::
  (XConfig Layout -> [(KeyMask, KeySym, X(), String, [String])])
  -> DescriptiveKeys
wKeys z =
  descriptiveKeys (fmap (\(m, s, a, d, t) -> DescriptiveKey m s a (Description (Just d)) (S.fromList (fmap Tag t))) . z)

-- | Sets the `keys` property of the given `XConfig` with the given descriptive key-bindings.
setDescriptiveKeys ::
  DescriptiveKeys
  -> XConfig l
  -> XConfig l
setDescriptiveKeys k l =
  let rawKeys (DescriptiveKeys d) = F.foldl' (\p (DescriptiveKey m s a _ _) -> M.insert (m, s) a p) M.empty . d
  in l { keys = rawKeys k }

-- | Returns all the tags for a given list of key-bindings.
allTags ::
  XConfig Layout
  -> DescriptiveKeys
  -> Tags
allTags l (DescriptiveKeys k) =
  S.unions (fmap tags (k l))

-- | How to produce a set of tags from a string, which will likely come from user-input.
newtype SearchTags =
  SearchTags {
    searchTags :: String -> Tags
  }

-- | Splits a string by spaces to produce a set of tags.
defaultSearchTags ::
  SearchTags
defaultSearchTags =
  SearchTags (S.fromList . fmap Tag . words)

-- | Removes all descriptive key-bindings that are not in the given set of tags.
filterTags ::
  Tags
  -> DescriptiveKeys
  -> DescriptiveKeys
filterTags t z@(DescriptiveKeys k) =
  if S.null t
    then z
    else DescriptiveKeys (\l -> filter (\(DescriptiveKey _ _ _ _ u) -> not (S.null (S.intersection t u))) $ k l)

-- | A pretty-printer for descriptive key-bindings.
data DescriptiveKeysPP =
  DescriptiveKeysPP ([DescriptiveKey] -> String)

-- | A plain-text pretty-printer that takes particular care of mod/mask keys and spacing.
defaultDescriptiveKeysPP ::
  DescriptiveKeysPP
defaultDescriptiveKeysPP =
  DescriptiveKeysPP (unlines . fmap (\(DescriptiveKey m s _ d _) ->
                       let pick n str = if n .&. complement m == 0 then str else ""
                           mk = concatMap (++"-") . filter (not . null) . map (uncurry pick) $
                               [
                                 (mod1Mask,    "alt")
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

-- | The prompt text when searching
newtype SearchTextPrompt =
  SearchTextPrompt String
  deriving (Eq, Ord, Show)

-- | The default search prompt, @Search key-bindings@
defaultSearchTextPrompt ::
  SearchTextPrompt
defaultSearchTextPrompt =
  SearchTextPrompt "Search key-bindings"

-- | The action to take to describe key-bindings from a string user-input.
newtype DescribeKeys =
  DescribeKeys {
    describeKeys :: String -> X ()
  }

-- | A default that opens @xmessage@ and uses the default pretty-printer.
defaultDescribeKeys ::
  DescriptiveKeys
  -> XConfig Layout
  -> DescribeKeys
defaultDescribeKeys k l =
  let dk (DescriptiveKeys g) = g
      pp (DescriptiveKeysPP p) = p
      j s = dk (filterTags (searchTags defaultSearchTags s) k) l
  in DescribeKeys (\z -> spawn ("xmessage \"" ++ pp defaultDescriptiveKeysPP (j z) ++ "\""))

-- | The attributes required to do the final configuration of the descriptive key-bindings.
data HelpPromptConfig =
  HelpPromptConfig {
    descriptiveHelp :: DescriptiveKeys      -- ^ The descriptive key-bindings.
  , xpConfigHelp    :: XPConfig             -- ^ The `XPConfig` that is used.
  , keyHelp         :: (ButtonMask, KeySym) -- ^ The key-binding to prompt the user to search.
  , searchTextHelp  :: SearchTextPrompt     -- ^ The search text prompt.
  , describeHelp    :: DescribeKeys         -- ^ The action to take after string user-input.
  }

-- | Sets the help prompt on the given `XPConfig`.
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

-- | Sets the help prompt on the given `XPConfig` and sets the `keys` attribute.
helpPromptAndSet ::
  DescriptiveKeys
  -> XPConfig
  -> (ButtonMask, KeySym)
  -> SearchTextPrompt
  -> (XConfig Layout -> DescribeKeys)
  -> XConfig l
  -> XConfig l
helpPromptAndSet k c m s d =
  helpPrompt (\l -> HelpPromptConfig {
    descriptiveHelp = k
  , xpConfigHelp    = c
  , keyHelp         = m
  , searchTextHelp  = s
  , describeHelp    = d l
  }) .
  setDescriptiveKeys k

-- | Sets the help prompt on the given `XPConfig` and sets the `keys` attribute with a default
-- key-binding of mod-F1, default search text prompt and using @xmessage@ to provide the descriptive response.
defaultHelpPromptAndSet ::
  DescriptiveKeys
  -> XPConfig
  -> XConfig l
  -> XConfig l
defaultHelpPromptAndSet k c =
  helpPromptAndSet k c (mod4Mask, xK_F1) defaultSearchTextPrompt (defaultDescribeKeys k)
