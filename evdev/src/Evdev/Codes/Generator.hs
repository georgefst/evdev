-- TODO pure vibes

{-# LANGUAGE TemplateHaskellQuotes #-}

module Evdev.Codes.Generator (generateCodes) where

import Data.Char
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Language.Haskell.TH
import Numeric

-- | A raw define from the header file.
data RawDefine
    = RawPrimary String Int -- ^ Name and numeric value
    | RawAlias String String -- ^ Alias name and target name
    deriving (Show)

-- | A processed define, after deduplication.
data Define
    = Primary String -- ^ A name that should become a constructor
    | Alias String String -- ^ An alias name pointing to a target name
    deriving (Show)

-- | Configuration for a group of defines that map to a single Haskell type.
data Group = Group
    { groupTypeName :: String
    , groupPrefixes :: [String]
    , groupDoc :: String
    }

groups :: [Group]
groups =
    [ Group "EventType" ["EV_"] "Each of these corresponds to one of the constructors of 'Evdev.EventData'. So you're unlikely to need to use these directly (C doesn't have ADTs - we do)."
    , Group "SyncEvent" ["SYN_"] "Synchronization events"
    , Group "Key" ["KEY_", "BTN_"] "Keys and buttons"
    , Group "RelativeAxis" ["REL_"] "Relative changes"
    , Group "AbsoluteAxis" ["ABS_"] "Absolute changes"
    , Group "SwitchEvent" ["SW_"] "Stateful binary switches"
    , Group "MiscEvent" ["MSC_"] "Miscellaneous"
    , Group "LEDEvent" ["LED_"] "LEDs"
    , Group "RepeatEvent" ["REP_"] "Specifying autorepeating events"
    , Group "SoundEvent" ["SND_"] "For simple sound output devices"
    , Group "DeviceProperty" ["INPUT_PROP_"] "Device properties"
    ]

-- | Names to skip when parsing the header.
skippedNames :: [String]
skippedNames = ["KEY_MIN_INTERESTING"]

-- | Parse a single @#define@ line.
parseLine :: String -> Maybe RawDefine
parseLine line = case words line of
    ("#define" : name : value : _)
        | any (`isSuffixOf'` name) ["_MAX", "_CNT"] -> Nothing
        | name `elem` skippedNames -> Nothing
        | name == "_INPUT_EVENT_CODES_H" -> Nothing
        | Just n <- parseNumericValue value -> Just (RawPrimary name n)
        | "(" `isPrefixOf` value -> Nothing
        | all (\c -> isAlphaNum c || c == '_') value -> Just (RawAlias name value)
        | otherwise -> Nothing
    _ -> Nothing
  where
    isSuffixOf' suffix str = drop (length str - length suffix) str == suffix

-- | Parse a numeric value (decimal or hex).
parseNumericValue :: String -> Maybe Int
parseNumericValue ('0' : 'x' : rest)
    | all isHexDigit rest, [(n, "")] <- readHex rest = Just n
parseNumericValue ('0' : 'X' : rest)
    | all isHexDigit rest, [(n, "")] <- readHex rest = Just n
parseNumericValue s
    | all isDigit s = Just (read s)
parseNumericValue _ = Nothing

-- | Parse the header file, returning all raw defines.
parseHeader :: String -> [RawDefine]
parseHeader = mapMaybe parseLine . lines

-- | Get the C name from a 'RawDefine'.
rawDefineName :: RawDefine -> String
rawDefineName (RawPrimary n _) = n
rawDefineName (RawAlias n _) = n

-- | Check if a define belongs to a group.
defInGroup :: Group -> RawDefine -> Bool
defInGroup grp def = any (`isPrefixOf` rawDefineName def) (groupPrefixes grp)

-- | Deduplicate primaries: when multiple primaries share a numeric value,
-- keep the last one as the constructor and turn earlier ones into aliases.
-- This handles cases like @BTN_GAMEPAD 0x130@ followed by @BTN_SOUTH 0x130@,
-- where @BTN_SOUTH@ becomes the constructor and @BTN_GAMEPAD@ becomes an alias.
dedup :: [RawDefine] -> [Define]
dedup rawDefs =
    let -- First pass: find which name is the "winner" for each value (last one wins)
        valueToName :: Map Int String
        valueToName = foldl'
            (\m rd -> case rd of
                RawPrimary name val -> Map.insert val name m
                RawAlias _ _ -> m
            )
            Map.empty
            rawDefs

        -- Second pass: convert, turning losers into aliases
        convert :: RawDefine -> Define
        convert (RawPrimary name val) =
            let winner = valueToName Map.! val
            in if name == winner
                then Primary name
                else Alias name winner
        convert (RawAlias name target) = Alias name target
    in map convert rawDefs

-- | Transform a C name like @KEY_LEFT_SHIFT@ into a Haskell constructor name like @KeyLeftShift@.
toCamelCase :: [String] -> String -> String
toCamelCase prefixes cName =
    let (haskPrefix, rest) = stripPrefix' prefixes cName
        segments = splitOn '_' rest
    in haskPrefix ++ concatMap titleCase segments
  where
    stripPrefix' [] n = ("", n)
    stripPrefix' (p : ps) n
        | p `isPrefixOf` n = (prefixToCamel p, drop (length p) n)
        | otherwise = stripPrefix' ps n

    prefixToCamel p =
        let segs = filter (not . null) $ splitOn '_' p
        in concatMap titleCase segs

    titleCase [] = []
    titleCase (c : cs) = toUpper c : map toLower cs

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn sep s =
    let (w, rest) = break (== sep) s
    in w : case rest of
        [] -> []
        (_ : rest') -> splitOn sep rest'

-- | Transform a C name like @KEY_ESC@ into the hs-bindgen generated name like @kEY_ESC@.
toRawName :: String -> String
toRawName [] = []
toRawName (c : cs) = toLower c : cs

-- | Generate all declarations for all groups.
generateCodes :: Q [Dec]
generateCodes = do
    contents <- runIO $ readFile "/nix/store/7iwv8dcgsjmkrnn752hnfdxh3f7wahmd-linux-headers-6.16.7/include/linux/input-event-codes.h"
    let rawDefs = parseHeader contents
    concat <$> mapM (generateGroup rawDefs) groups

-- | Generate declarations for a single group: data type, SimpleEnum instance, and pattern synonyms.
generateGroup :: [RawDefine] -> Group -> Q [Dec]
generateGroup allRawDefs grp = do
    let myRawDefs = filter (defInGroup grp) allRawDefs
        myDefs = dedup myRawDefs
        primaries = [n | Primary n <- myDefs]
        aliases = [(a, t) | Alias a t <- myDefs]
        prefixes = groupPrefixes grp
        tyName = mkName (groupTypeName grp)
        conNames = map (\n -> mkName (toCamelCase prefixes n)) primaries

    dataDec <- generateDataDec tyName conNames (groupDoc grp)
    enumInst <- generateSimpleEnumInst tyName primaries prefixes
    patSyns <- concat <$> mapM (generatePatSyn tyName prefixes) aliases
    pure $ dataDec ++ enumInst ++ patSyns

-- | Generate: @data TypeName = Con1 | Con2 | ... deriving (Bounded, Eq, Ord, Read, Show)@
generateDataDec :: Name -> [Name] -> String -> Q [Dec]
generateDataDec tyName conNames _doc = do
    let cons = map (\n -> NormalC n []) conNames
        derivs = [DerivClause Nothing (map ConT [''Bounded, ''Eq, ''Ord, ''Read, ''Show])]
    pure [DataD [] tyName [] Nothing cons derivs]

-- | Generate a @SimpleEnum@ instance for the given type.
generateSimpleEnumInst :: Name -> [String] -> [String] -> Q [Dec]
generateSimpleEnumInst tyName primaries prefixes = do
    let simpleEnumName = mkName "SimpleEnum"
        enumerateBody =
            ListE [ConE (mkName (toCamelCase prefixes p)) | p <- primaries]

        nName = mkName "n"
        toEnumClauses =
            let guardedBody = map
                    (\p ->
                        let rawN = mkName (toRawName p)
                            conN = mkName (toCamelCase prefixes p)
                        in ( NormalG (InfixE (Just (VarE nName))
                                            (VarE '(==))
                                            (Just (AppE (VarE 'fromIntegral) (VarE rawN))))
                           , AppE (ConE 'Just) (ConE conN)
                           )
                    )
                    primaries
                otherwiseGuard =
                    ( NormalG (VarE 'otherwise)
                    , ConE 'Nothing
                    )
            in [Clause [VarP nName] (GuardedB (guardedBody ++ [otherwiseGuard])) []]

        fromEnumMatches = map
            (\p ->
                let rawN = mkName (toRawName p)
                    conN = mkName (toCamelCase prefixes p)
                in Match (ConP conN [] []) (NormalB (AppE (VarE 'fromIntegral) (VarE rawN))) []
            )
            primaries
        fromEnumBody = LamCaseE fromEnumMatches

    pure
        [ InstanceD Nothing []
            (AppT (ConT simpleEnumName) (ConT tyName))
            [ FunD (mkName "enumerate'") [Clause [] (NormalB enumerateBody) []]
            , FunD (mkName "toEnum'") toEnumClauses
            , FunD (mkName "fromEnum'") [Clause [] (NormalB fromEnumBody) []]
            ]
        ]

-- | Generate a pattern synonym for an alias.
generatePatSyn :: Name -> [String] -> (String, String) -> Q [Dec]
generatePatSyn tyName prefixes (aliasName, targetName) = do
    let aliasConName = mkName (toCamelCase prefixes aliasName)
        targetConName = mkName (toCamelCase prefixes targetName)
        patSynSig = PatSynSigD aliasConName (ConT tyName)
        patSynDec = PatSynD aliasConName (PrefixPatSyn []) ImplBidir (ConP targetConName [] [])
    pure [patSynSig, patSynDec]
