{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Evdev.Codes.Generator (generateCodes) where

import Data.Char
import Data.Either
import Data.Foldable
import Data.Functor
import Data.List
import Data.List.Extra
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Language.Haskell.TH
import Text.Read

groups :: [TypeInfo]
groups =
    [ TypeInfo
        { name = TypeName $ mkName "EventType"
        , prefixes = ["EV"]
        , doc =
            """
            Each of these corresponds to one of the constructors of 'Evdev.EventData'.
            So you're unlikely to need to use these directly (C doesn't have ADTs - we do).
            """
        }
    , TypeInfo
        { name = TypeName $ mkName "SyncEvent"
        , prefixes = ["SYN"]
        , doc =
            """
            Synchronization events
            """
        }
    , TypeInfo
        { name = TypeName $ mkName "Key"
        , prefixes = ["KEY", "BTN"]
        , doc =
            """
            Keys and buttons
            """
        }
    , TypeInfo
        { name = TypeName $ mkName "RelativeAxis"
        , prefixes = ["REL"]
        , doc =
            """
            Relative changes
            """
        }
    , TypeInfo
        { name = TypeName $ mkName "AbsoluteAxis"
        , prefixes = ["ABS"]
        , doc =
            """
            Absolute changes
            """
        }
    , TypeInfo
        { name = TypeName $ mkName "SwitchEvent"
        , prefixes = ["SW"]
        , doc =
            """
            Stateful binary switches
            """
        }
    , TypeInfo
        { name = TypeName $ mkName "MiscEvent"
        , prefixes = ["MSC"]
        , doc =
            """
            Miscellaneous
            """
        }
    , TypeInfo
        { name = TypeName $ mkName "LEDEvent"
        , prefixes = ["LED"]
        , doc =
            """
            LEDs
            """
        }
    , TypeInfo
        { name = TypeName $ mkName "RepeatEvent"
        , prefixes = ["REP"]
        , doc =
            """
            Specifying autorepeating events
            """
        }
    , TypeInfo
        { name = TypeName $ mkName "SoundEvent"
        , prefixes = ["SND"]
        , doc =
            """
            For simple sound output devices
            """
        }
    , TypeInfo
        { name = TypeName $ mkName "DeviceProperty"
        , prefixes = ["INPUT_PROP"]
        , doc =
            """
            Device properties
            """
        }
    ]

data TypeInfo = TypeInfo
    { name :: TypeName
    , prefixes :: [String]
    , doc :: String
    }

data Define
    = Primary {name :: MacroName, value :: Int}
    | Alias {name :: MacroName, target :: MacroName}
    deriving (Show)

-- | Parse a single @#define@ line.
parseLine :: String -> Maybe Define
parseLine line = case words line of
    ("#define" : k@(MacroName -> name) : v : _)
        | any (`isSuffixOf` k) metaSuffices -> Nothing
        | Just value <- readMaybe v -> Just Primary{name, value}
        | otherwise -> Just Alias{name, target = MacroName v}
    _ -> Nothing
  where
    metaSuffices =
        [ "_MAX"
        , "_CNT"
        , "_MIN_INTERESTING"
        ]

parseHeader :: String -> [(TypeInfo, [Define])]
parseHeader input =
    map snd
        . Map.toList
        . Map.fromListWith (\(t, a) (_, b) -> (t, a <> b))
        . map
            ( \d ->
                let
                    MacroName s = d.name
                    ty =
                        snd
                            . fromMaybe (error $ "no prefix matched: " <> show d.name)
                            $ find (\(p, _) -> p `isPrefixOf` s) prefixes
                 in
                    (ty.name, (ty, [d]))
            )
        . mapMaybe parseLine
        $ lines input
  where
    prefixes = concatMap (\g -> (,g) <$> g.prefixes) groups

{- | Deduplicate primaries: when multiple primaries share a value string,
keep the last one as the constructor and turn earlier ones into aliases.
This handles cases like @BTN_GAMEPAD 0x130@ followed by @BTN_SOUTH 0x130@,
where @BTN_SOUTH@ becomes the constructor and @BTN_GAMEPAD@ becomes an alias.
-}
dedup :: [Define] -> [Define]
dedup defs =
    let
        -- First pass: find which name is the "winner" for each value (last one wins)
        valueToName :: Map Int MacroName
        valueToName =
            foldl'
                ( \m d -> case d of
                    Primary name val -> Map.insert val name m
                    Alias _ _ -> m
                )
                Map.empty
                defs
        -- Second pass: convert losers into aliases pointing to the winner
        convert :: Define -> Define
        convert (Primary name val) =
            let winner = valueToName Map.! val
             in if name == winner
                    then Primary name val
                    else Alias name winner
        convert a@Alias{} = a
     in
        map convert defs

newtype MacroName = MacroName String deriving newtype (Eq, Ord, Show)
newtype TypeName = TypeName Name deriving newtype (Eq, Ord, Show)
newtype BindgenName = BindgenName Name deriving newtype (Eq, Ord, Show)
newtype ConstructorName = ConstructorName Name deriving newtype (Eq, Ord, Show)
newtype PatternName = PatternName Name deriving newtype (Eq, Ord, Show)

generateCodes :: Q [Dec]
generateCodes = do
    -- oh yeah, this doesn't do anything unless it's in the cabal file
    -- addDependentFile file
    contents <- runIO $ readFile file
    -- for_ groups \TypeInfo{name = TypeName name, doc} -> putDoc (DeclDoc name) doc
    pure $ concatMap (uncurry generateType) $ parseHeader contents
  where
    -- cp /nix/store/7iwv8dcgsjmkrnn752hnfdxh3f7wahmd-linux-headers-6.16.7/include/linux/input-event-codes.h codes.h
    -- file = "codes.h"
    file = "/nix/store/7iwv8dcgsjmkrnn752hnfdxh3f7wahmd-linux-headers-6.16.7/include/linux/input-event-codes.h"

generateType :: TypeInfo -> [Define] -> [Dec]
generateType ty defs =
    [ dataType ty.name $ map snd primaries
    , simpleEnumInstance ty.name primaries
    ]
        <> concatMap (patternSynonym ty.name) aliases
  where
    (primaries, aliases) =
        partitionEithers $
            dedup defs <&> \case
                Primary n _ -> Left (toBindgenName n, toConstructorName n)
                Alias a t -> Right (toPatternName a, toConstructorName t)

dataType :: TypeName -> [ConstructorName] -> Dec
dataType (TypeName tyName) conNames =
    DataD
        []
        tyName
        []
        Nothing
        (conNames <&> \(ConstructorName s) -> NormalC s [])
        [DerivClause Nothing (map ConT [''Eq, ''Ord, ''Read, ''Show])]

simpleEnumInstance :: TypeName -> [(BindgenName, ConstructorName)] -> Dec
simpleEnumInstance (TypeName tyName) conNames =
    InstanceD
        Nothing
        []
        (AppT (ConT (mkName "SimpleEnum")) (ConT tyName))
        [ FunD
            (mkName "enumerate'")
            [ Clause
                []
                (NormalB (ListE $ conNames <&> \(_, ConstructorName s) -> ConE s))
                []
            ]
        , FunD
            (mkName "toEnum'")
            [ let n = mkName "n"
               in Clause
                    [VarP n]
                    ( GuardedB
                        ( map
                            ( \(BindgenName val, ConstructorName con) ->
                                ( NormalG
                                    ( InfixE
                                        (Just (VarE n))
                                        (VarE '(==))
                                        (Just (AppE (VarE 'fromIntegral) (VarE val)))
                                    )
                                , AppE (ConE 'Just) (ConE con)
                                )
                            )
                            conNames
                            <> [(NormalG (VarE 'otherwise), ConE 'Nothing)]
                        )
                    )
                    []
            ]
        , FunD
            (mkName "fromEnum'")
            [ Clause
                []
                ( NormalB
                    ( LamCaseE
                        ( map
                            ( \(BindgenName val, ConstructorName con) ->
                                Match
                                    (ConP con [] [])
                                    (NormalB (AppE (VarE 'fromIntegral) (VarE val)))
                                    []
                            )
                            conNames
                        )
                    )
                )
                []
            ]
        ]

patternSynonym :: TypeName -> (PatternName, ConstructorName) -> [Dec]
patternSynonym (TypeName tyName) (PatternName pat, ConstructorName con) =
    [ PatSynSigD pat (ConT tyName)
    , PatSynD pat (PrefixPatSyn []) ImplBidir (ConP con [] [])
    ]

-- KEY_LEFT_SHIFT -> KeyLeftShift
toConstructorName :: MacroName -> ConstructorName
toPatternName :: MacroName -> PatternName
(toConstructorName, toPatternName) = (f ConstructorName, f PatternName)
  where
    f c (MacroName s) = c . mkName . concatMap titleCase . splitOn "_" $ s
    titleCase = \case
        [] -> []
        c : cs -> toUpper c : map toLower cs

-- KEY_LEFT_SHIFT -> kEY_LEFT_SHIFT
toBindgenName :: MacroName -> BindgenName
toBindgenName (MacroName s) = BindgenName $ mkName case s of
    [] -> []
    (c : cs) -> toLower c : cs
