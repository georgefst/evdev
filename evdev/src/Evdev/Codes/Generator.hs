{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Evdev.Codes.Generator (generateCodes) where

import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Foldable
import Data.Functor
import Data.List
import Data.List.Extra
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Ordered qualified as OMap
import Data.Map.Strict qualified as Map
import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Read

data CodeType
    = EventType
    | SyncEvent
    | Key
    | RelativeAxis
    | AbsoluteAxis
    | SwitchEvent
    | MiscEvent
    | LEDEvent
    | RepeatEvent
    | SoundEvent
    | DeviceProperty
    deriving (Eq, Ord, Show, Enum, Bounded)
codeTypePrefixes :: CodeType -> [String]
codeTypePrefixes = \case
    EventType -> ["EV"]
    SyncEvent -> ["SYN"]
    Key -> ["KEY", "BTN"]
    RelativeAxis -> ["REL"]
    AbsoluteAxis -> ["ABS"]
    SwitchEvent -> ["SW"]
    MiscEvent -> ["MSC"]
    LEDEvent -> ["LED"]
    RepeatEvent -> ["REP"]
    SoundEvent -> ["SND"]
    DeviceProperty -> ["INPUT_PROP"]
codeTypeDoc :: CodeType -> String
codeTypeDoc = \case
    EventType ->
        """
        Each of these corresponds to one of the constructors of 'Evdev.EventData'.
        So you're unlikely to need to use these directly (C doesn't have ADTs - we do).
        """
    SyncEvent -> "Synchronization events"
    Key -> "Keys and buttons"
    RelativeAxis -> "Relative changes"
    AbsoluteAxis -> "Absolute changes"
    SwitchEvent -> "Stateful binary switches"
    MiscEvent -> "Miscellaneous"
    LEDEvent -> "LEDs"
    RepeatEvent -> "Specifying autorepeating events"
    SoundEvent -> "For simple sound output devices"
    DeviceProperty -> "Device properties"

data Define = Define
    { name :: MacroName
    , value :: Either Integer MacroName
    }

parseDefineLine :: String -> Maybe Define
parseDefineLine line = case words line of
    ("#define" : k@(MacroName -> name) : v : _)
        | any (`isSuffixOf` k) metaSuffices -> Nothing
        | Just n <- readMaybe v -> Just Define{name, value = Left n}
        | otherwise -> Just Define{name, value = Right $ MacroName v}
    _ -> Nothing
  where
    metaSuffices =
        [ "_MAX"
        , "_CNT"
        , "_MIN_INTERESTING"
        ]

parseHeader :: String -> [(CodeType, [Define])]
parseHeader input =
    Map.toList
        . foldr (uncurry $ Map.adjust . (:)) (Map.fromList $ map (,[]) enumerate)
        . map (\d@Define{name = MacroName name} -> (d, snd . unwrap name $ find ((`isPrefixOf` name) . fst) prefixes))
        . mapMaybe parseDefineLine
        $ lines input
  where
    unwrap name = fromMaybe (error $ "no prefix matched: " <> show name)
    prefixes = concatMap (\t -> (,t) <$> codeTypePrefixes t) enumerate

processType :: [Define] -> [(ConstructorName, (Integer, [PatternName]))]
processType defs =
    map (first toConstructorName) . OMap.assocs $
        foldl'
            (flip \(alias, target) -> OMap.alter (fmap $ second (toPatternName alias :)) target)
            litsByPrimary
            aliasMacros
  where
    (litMacros, aliasMacros) = partitionEithers $ defs <&> \Define{name, value} -> bimap (name,) (name,) value
    litsByValue = foldl' (flip \(name, value) -> Map.insertWith ((<>)) value (pure name)) Map.empty litMacros
    -- when multiple literal macros point to the same value, turn all but the first in to pattern synonyms
    litsByPrimary = OMap.fromList . map (\(n, k :| as) -> (k, (n, map toPatternName as))) $ Map.toList litsByValue

newtype MacroName = MacroName String deriving newtype (Eq, Ord, Show)
newtype TypeName = TypeName Name deriving newtype (Eq, Ord, Show)
newtype ConstructorName = ConstructorName Name deriving newtype (Eq, Ord, Show)
newtype PatternName = PatternName Name deriving newtype (Eq, Ord, Show)

generateCodes :: FilePath -> Q [Dec]
generateCodes path = do
    contents <- runIO $ readFile path
    addModFinalizer $ for_ enumerate \ct -> putDoc (DeclDoc $ mkName (show ct)) $ codeTypeDoc ct
    pure
        . concatMap
            ( uncurry (uncurry . generateType)
                . bimap
                    (TypeName . mkName . show)
                    (foldMap (\(k, (n, as)) -> (([(k, n)], map (,k) as))) . processType)
            )
        $ parseHeader contents

generateType :: TypeName -> [(ConstructorName, Integer)] -> [(PatternName, ConstructorName)] -> [Dec]
generateType name constructors patterns =
    [ dataType name $ map fst constructors
    , simpleEnumInstance name constructors
    ]
        <> concatMap (uncurry $ patternSynonym name) patterns

dataType :: TypeName -> [ConstructorName] -> Dec
dataType (TypeName tyName) conNames =
    DataD
        []
        tyName
        []
        Nothing
        (conNames <&> \(ConstructorName s) -> NormalC s [])
        [DerivClause Nothing (map ConT [''Eq, ''Ord, ''Read, ''Show])]

simpleEnumInstance :: TypeName -> [(ConstructorName, Integer)] -> Dec
simpleEnumInstance (TypeName tyName) conNames =
    InstanceD
        Nothing
        []
        (AppT (ConT (mkName "SimpleEnum")) (ConT tyName))
        [ FunD
            (mkName "enumerate'")
            [ Clause
                []
                (NormalB (ListE $ conNames <&> \(ConstructorName s, _) -> ConE s))
                []
            ]
        , FunD
            (mkName "toEnum'")
            [ let n = mkName "n"
               in Clause
                    [VarP n]
                    ( GuardedB
                        ( map
                            ( \(ConstructorName con, val) ->
                                ( NormalG
                                    ( InfixE
                                        (Just (VarE n))
                                        (VarE '(==))
                                        (Just (LitE (IntegerL val)))
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
                            ( \(ConstructorName con, val) ->
                                Match
                                    (ConP con [] [])
                                    (NormalB (LitE (IntegerL val)))
                                    []
                            )
                            conNames
                        )
                    )
                )
                []
            ]
        ]

patternSynonym :: TypeName -> PatternName -> ConstructorName -> [Dec]
patternSynonym (TypeName tyName) (PatternName pat) (ConstructorName con) =
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
