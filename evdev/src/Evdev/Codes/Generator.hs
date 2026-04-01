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
import Data.Tuple.Extra
import Language.Haskell.TH

groups :: [Group]
groups =
    [ Group
        { typeName = mkName "EventType"
        , prefixes = ["EV"]
        , doc =
            """
            Each of these corresponds to one of the constructors of 'Evdev.EventData'.
            So you're unlikely to need to use these directly (C doesn't have ADTs - we do).
            """
        }
    , Group
        { typeName = mkName "SyncEvent"
        , prefixes = ["SYN"]
        , doc =
            """
            Synchronization events
            """
        }
    , Group
        { typeName = mkName "Key"
        , prefixes = ["KEY", "BTN"]
        , doc =
            """
            Keys and buttons
            """
        }
    , Group
        { typeName = mkName "RelativeAxis"
        , prefixes = ["REL"]
        , doc =
            """
            Relative changes
            """
        }
    , Group
        { typeName = mkName "AbsoluteAxis"
        , prefixes = ["ABS"]
        , doc =
            """
            Absolute changes
            """
        }
    , Group
        { typeName = mkName "SwitchEvent"
        , prefixes = ["SW"]
        , doc =
            """
            Stateful binary switches
            """
        }
    , Group
        { typeName = mkName "MiscEvent"
        , prefixes = ["MSC"]
        , doc =
            """
            Miscellaneous
            """
        }
    , Group
        { typeName = mkName "LEDEvent"
        , prefixes = ["LED"]
        , doc =
            """
            LEDs
            """
        }
    , Group
        { typeName = mkName "RepeatEvent"
        , prefixes = ["REP"]
        , doc =
            """
            Specifying autorepeating events
            """
        }
    , Group
        { typeName = mkName "SoundEvent"
        , prefixes = ["SND"]
        , doc =
            """
            For simple sound output devices
            """
        }
    , Group
        { typeName = mkName "DeviceProperty"
        , prefixes = ["INPUT_PROP"]
        , doc =
            """
            Device properties
            """
        }
    ]

-- | A define from the header file: either a primary (value is a number) or an alias (value is another name).
data Define
    = Primary {grp :: Name, name :: String, val :: String}
    | Alias {grp :: Name, name :: String, target :: String}
    deriving (Show)

-- | Configuration for a group of defines that map to a single Haskell type.
data Group = Group
    { typeName :: Name
    , prefixes :: [String]
    , doc :: String
    }

groupMap :: Map Name Group
groupMap = Map.fromList $ ((.typeName) &&& id) <$> groups

-- | Names to skip when parsing the header.
skippedNames :: [String]
skippedNames = ["KEY_MIN_INTERESTING"]

-- | Parse a single @#define@ line.
parseLine :: String -> Maybe Define
parseLine line = case words line of
    ("#define" : name : value@(v : _) : _)
        | any (`isSuffixOf` name) ["_MAX", "_CNT"] -> Nothing
        | name `elem` skippedNames -> Nothing
        | name == "_INPUT_EVENT_CODES_H" -> Nothing
        | isDigit v -> Just (Primary (getGroup name) name value)
        | isAlpha v -> Just (Alias (getGroup name) name value)
        | otherwise -> Nothing
    _ -> Nothing
  where
    getGroup s =
        snd
            . fromMaybe (error $ "no prefix matched: " <> s)
            . find ((`isPrefixOf` s) . fst)
            $ concatMap (\g -> (,g.typeName) <$> g.prefixes) groups

-- | Parse the header file, returning all defines.
parseHeader :: String -> [(Name, (Group, [Define]))]
parseHeader input =
    Map.toList
        . Map.fromListWith (\(g, a) (_, b) -> (g, a <> b))
        . map (\d -> (d.grp, (fromMaybe (error "group not found") $ Map.lookup d.grp groupMap, [d])))
        . mapMaybe parseLine
        $ lines input

{- | Deduplicate primaries: when multiple primaries share a value string,
keep the last one as the constructor and turn earlier ones into aliases.
This handles cases like @BTN_GAMEPAD 0x130@ followed by @BTN_SOUTH 0x130@,
where @BTN_SOUTH@ becomes the constructor and @BTN_GAMEPAD@ becomes an alias.
-}
dedup :: [Define] -> [Define]
dedup defs =
    let
        -- First pass: find which name is the "winner" for each value (last one wins)
        valueToName :: Map String String
        valueToName =
            foldl'
                ( \m d -> case d of
                    Primary _ name val -> Map.insert val name m
                    Alias _ _ _ -> m
                )
                Map.empty
                defs
        -- Second pass: convert losers into aliases pointing to the winner
        convert :: Define -> Define
        convert (Primary grp name val) =
            let winner = valueToName Map.! val
             in if name == winner
                    then Primary grp name val
                    else Alias grp name winner
        convert a@Alias{} = a
     in
        map convert defs

generateCodes :: Q [Dec]
generateCodes = do
    -- oh yeah, this doesn't do anything unless it's in the cabal file
    -- addDependentFile file
    contents <- runIO $ readFile file
    -- for_ groups \grp -> putDoc (DeclDoc grp.typeName) grp.doc
    pure $ concatMap (uncurry generateGroup . snd) $ parseHeader contents
  where
    -- cp /nix/store/7iwv8dcgsjmkrnn752hnfdxh3f7wahmd-linux-headers-6.16.7/include/linux/input-event-codes.h codes.h
    -- file = "codes.h"
    file = "/nix/store/7iwv8dcgsjmkrnn752hnfdxh3f7wahmd-linux-headers-6.16.7/include/linux/input-event-codes.h"

generateGroup :: Group -> [Define] -> [Dec]
generateGroup grp defs =
    [ dataType grp.typeName primaries
    , simpleEnumInstance grp.typeName primaries
    ]
        <> concatMap (aliasPatternSynonym grp.typeName) aliases
  where
    (primaries, aliases) =
        partitionEithers $
            dedup defs <&> \case
                Primary _ n _ ->
                    Left (mkName $ toBindgenName n, mkName $ toConstructorName n)
                Alias _ a t ->
                    Right (mkName $ toConstructorName a, mkName $ toConstructorName t)

dataType :: Name -> [(Name, Name)] -> Dec
dataType tyName conNames =
    DataD
        []
        tyName
        []
        Nothing
        (map (flip NormalC [] . snd) conNames)
        [DerivClause Nothing (map ConT [''Eq, ''Ord, ''Read, ''Show])]

simpleEnumInstance :: Name -> [(Name, Name)] -> Dec
simpleEnumInstance tyName conNames =
    InstanceD
        Nothing
        []
        (AppT (ConT (mkName "SimpleEnum")) (ConT tyName))
        [ FunD
            (mkName "enumerate'")
            [ Clause
                []
                (NormalB (ListE $ map (ConE . snd) conNames))
                []
            ]
        , FunD
            (mkName "toEnum'")
            [ let nName = mkName "n"
               in Clause
                    [VarP nName]
                    ( GuardedB
                        ( map
                            ( \(raw, camel) ->
                                ( NormalG
                                    ( InfixE
                                        (Just (VarE nName))
                                        (VarE '(==))
                                        (Just (AppE (VarE 'fromIntegral) (VarE raw)))
                                    )
                                , AppE (ConE 'Just) (ConE camel)
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
                            ( \(raw, camel) ->
                                Match
                                    (ConP camel [] [])
                                    (NormalB (AppE (VarE 'fromIntegral) (VarE raw)))
                                    []
                            )
                            conNames
                        )
                    )
                )
                []
            ]
        ]

aliasPatternSynonym :: Name -> (Name, Name) -> [Dec]
aliasPatternSynonym tyName (aliasName, targetName) =
    [ PatSynSigD aliasName (ConT tyName)
    , PatSynD aliasName (PrefixPatSyn []) ImplBidir (ConP targetName [] [])
    ]

-- KEY_LEFT_SHIFT -> KeyLeftShift
toConstructorName :: String -> String
toConstructorName = concatMap titleCase . splitOn "_"
  where
    titleCase = \case
        [] -> []
        c : cs -> toUpper c : map toLower cs

-- KEY_LEFT_SHIFT -> kEY_LEFT_SHIFT
toBindgenName :: String -> String
toBindgenName = \case
    [] -> []
    (c : cs) -> toLower c : cs
