{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Floskell.Config
    ( Indent(..)
    , LayoutContext(..)
    , WithinDeclaration(..)
    , Location(..)
    , WsLoc(..)
    , Whitespace(..)
    , Layout(..)
    , WithinLayout(..)
    , ConfigMapKey(..)
    , ConfigMap(..)
    , PenaltyConfig(..)
    , AlignConfig(..)
    , IndentConfig(..)
    , LayoutConfig(..)
    , OpConfig(..)
    , GroupConfig(..)
    , ImportsGroupOrder(..)
    , ImportsGroup(..)
    , SortImportsRule(..)
    , DeclarationConstruct(..)
    , OptionConfig(..)
    , Config(..)
    , defaultConfig
    , safeConfig
    , cfgMapFind
    , cfgOpWs
    , cfgGroupWs
    , cfgOpWs'
    , cfgGroupWs'
    , inWs
    , wsSpace
    , wsLinebreak
    , textToKey
    ) where

import           Data.Aeson
                 ( FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON )
import qualified Data.Aeson         as JSON
import           Data.Aeson.Types   as JSON
                 ( Options(..), camelTo2, typeMismatch )
import           Data.ByteString    ( ByteString )
import           Data.Default       ( Default(..) )
import qualified Data.HashMap.Lazy  as HashMap
import           Data.Map.Strict    ( Map )
import qualified Data.Map.Strict    as Map
import           Data.Set           ( Set )
import qualified Data.Set           as Set
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T ( decodeUtf8, encodeUtf8 )

import           GHC.Generics

data Indent = Align | IndentBy !Int | AlignOrIndentBy !Int
    deriving ( Eq, Ord, Show, Generic )

data LayoutContext = Declaration | Type | Pattern | Expression | Other
    deriving ( Eq, Ord, Bounded, Enum, Show, Generic )

data WithinDeclaration = ModuleDeclaration
                       | RecordDeclaration
                       | TypeDeclaration
                       | ComprehensionDeclaration
                       | SpecialDeclaration
                       | OtherDeclaration
    deriving ( Eq, Ord, Bounded, Enum, Show, Generic )

data Location = Before | After
    deriving ( Eq, Ord, Bounded, Enum, Show, Generic )

data WsLoc = WsNone | WsBefore | WsAfter | WsBoth
    deriving ( Eq, Ord, Bounded, Enum, Show, Generic )

data Whitespace = Whitespace { wsSpaces         :: !WsLoc
                             , wsLinebreaks     :: !WsLoc
                             , wsForceLinebreak :: !Bool
                             }
    deriving ( Show, Generic )

data Layout = Flex | Vertical | TryOneline
    deriving ( Eq, Ord, Bounded, Enum, Show, Generic )

data ConfigMapKey = ConfigMapKey !(Maybe ByteString) !(Maybe LayoutContext) !(Maybe WithinDeclaration)
    deriving ( Eq, Ord, Show )

data ConfigMap a =
    ConfigMap { cfgMapDefault :: !a, cfgMapOverrides :: !(Map ConfigMapKey a) }
    deriving ( Generic )

data PenaltyConfig = PenaltyConfig { penaltyMaxLineLength :: !Int
                                   , penaltyLinebreak     :: !Int
                                   , penaltyIndent        :: !Int
                                   , penaltyOverfull      :: !Int
                                   , penaltyOverfullOnce  :: !Int
                                   }
    deriving ( Generic )

instance Default PenaltyConfig where
    def = PenaltyConfig { penaltyMaxLineLength = 80
                        , penaltyLinebreak     = 100
                        , penaltyIndent        = 1
                        , penaltyOverfull      = 10
                        , penaltyOverfullOnce  = 200
                        }

data AlignConfig =
    AlignConfig { cfgAlignLimits       :: !(Int, Int)
                , cfgAlignCase         :: !Bool
                , cfgAlignClass        :: !Bool
                , cfgAlignImportModule :: !Bool
                , cfgAlignImportSpec   :: !Bool
                , cfgAlignLetBinds     :: !Bool
                , cfgAlignMatches      :: !Bool
                , cfgAlignRecordFields :: !Bool
                , cfgAlignWhere        :: !Bool
                }
    deriving ( Generic )

instance Default AlignConfig where
    def = AlignConfig { cfgAlignLimits       = (10, 25)
                      , cfgAlignCase         = False
                      , cfgAlignClass        = False
                      , cfgAlignImportModule = False
                      , cfgAlignImportSpec   = False
                      , cfgAlignLetBinds     = False
                      , cfgAlignMatches      = False
                      , cfgAlignRecordFields = False
                      , cfgAlignWhere        = False
                      }

data IndentConfig =
    IndentConfig { cfgIndentOnside :: !Int
                 , cfgIndentDeriving :: !Int
                 , cfgIndentWhere :: !Int
                 , cfgIndentApp :: !Indent
                 , cfgIndentCase :: !Indent
                 , cfgIndentClass :: !Indent
                 , cfgIndentDo :: !Indent
                 , cfgIndentExportSpecList :: !Indent
                 , cfgIndentIf :: !Indent
                 , cfgIndentImportSpecList :: !Indent
                 , cfgIndentLet :: !Indent
                 , cfgIndentLetBinds :: !Indent
                 , cfgIndentLetIn :: !Indent
                 , cfgIndentMultiIf :: !Indent
                 , cfgIndentTypesig :: !Indent
                 , cfgIndentWhereBinds :: !Indent
                 }
    deriving ( Generic )

instance Default IndentConfig where
    def = IndentConfig { cfgIndentOnside = 4
                       , cfgIndentDeriving = 4
                       , cfgIndentWhere = 2
                       , cfgIndentApp = IndentBy 4
                       , cfgIndentCase = IndentBy 4
                       , cfgIndentClass = IndentBy 4
                       , cfgIndentDo = IndentBy 4
                       , cfgIndentExportSpecList = IndentBy 4
                       , cfgIndentIf = IndentBy 4
                       , cfgIndentImportSpecList = IndentBy 4
                       , cfgIndentLet = IndentBy 4
                       , cfgIndentLetBinds = IndentBy 4
                       , cfgIndentLetIn = IndentBy 4
                       , cfgIndentMultiIf = IndentBy 4
                       , cfgIndentTypesig = IndentBy 4
                       , cfgIndentWhereBinds = IndentBy 2
                       }

data WithinLayout
  = WithinLayout { wlModuleLayout :: !Layout
                 , wlRecordLayout :: !Layout
                 , wlTypeLayout :: !Layout
                 , wlOtherLayout :: !Layout
                 }
    deriving ( Generic )

data LayoutConfig =
    LayoutConfig { cfgLayoutApp :: !Layout
                 , cfgLayoutConDecls :: !Layout
                 , cfgLayoutDeclaration :: !Layout
                 , cfgLayoutExportSpecList :: !Layout
                 , cfgLayoutIf :: !Layout
                 , cfgLayoutImportSpecList :: !Layout
                 , cfgLayoutInfixApp :: !Layout
                 , cfgLayoutLet :: !Layout
                 , cfgLayoutListComp :: !Layout
                 , cfgLayoutRecord :: !Layout
                 , cfgLayoutType :: !WithinLayout
                 }
    deriving ( Generic )

instance Default LayoutConfig where
    def = LayoutConfig { cfgLayoutApp = Flex
                       , cfgLayoutConDecls = Flex
                       , cfgLayoutDeclaration = Flex
                       , cfgLayoutExportSpecList = Flex
                       , cfgLayoutIf = Flex
                       , cfgLayoutImportSpecList = Flex
                       , cfgLayoutInfixApp = Flex
                       , cfgLayoutLet = Flex
                       , cfgLayoutListComp = Flex
                       , cfgLayoutRecord = Flex
                       , cfgLayoutType = WithinLayout Flex Flex Flex Flex
                       }

newtype OpConfig = OpConfig { unOpConfig :: ConfigMap Whitespace }
    deriving ( Generic )

instance Default OpConfig where
    def =
        OpConfig ConfigMap { cfgMapDefault   = Whitespace WsBoth WsBefore False
                           , cfgMapOverrides = Map.empty
                           }

newtype GroupConfig = GroupConfig { unGroupConfig :: ConfigMap Whitespace }
    deriving ( Generic )

instance Default GroupConfig where
    def = GroupConfig ConfigMap { cfgMapDefault   =
                                      Whitespace WsBoth WsAfter False
                                , cfgMapOverrides = Map.empty
                                }

data ImportsGroupOrder =
    ImportsGroupKeep | ImportsGroupSorted | ImportsGroupGrouped
    deriving ( Generic )

data ImportsGroup = ImportsGroup { importsPrefixes :: ![String]
                                 , importsOrder    :: !ImportsGroupOrder
                                 }
    deriving ( Generic )

data SortImportsRule =
    NoImportSort | SortImportsByPrefix | SortImportsByGroups ![ImportsGroup]

data DeclarationConstruct = DeclModule | DeclClass | DeclInstance | DeclWhere
    deriving ( Eq, Ord, Generic )

data OptionConfig =
    OptionConfig { cfgOptionSortPragmas           :: !Bool
                 , cfgOptionSplitLanguagePragmas  :: !Bool
                 , cfgOptionSortImports           :: !SortImportsRule
                 , cfgOptionSortImportLists       :: !Bool
                 , cfgOptionAlignSumTypeDecl      :: !Bool
                 , cfgOptionFlexibleOneline       :: !Bool
                 , cfgOptionPreserveVerticalSpace :: !Bool
                 , cfgOptionDeclNoBlankLines      :: !(Set DeclarationConstruct)
                 }
    deriving ( Generic )

instance Default OptionConfig where
    def = OptionConfig { cfgOptionSortPragmas           = False
                       , cfgOptionSplitLanguagePragmas  = False
                       , cfgOptionSortImports           = NoImportSort
                       , cfgOptionSortImportLists       = False
                       , cfgOptionAlignSumTypeDecl      = False
                       , cfgOptionFlexibleOneline       = False
                       , cfgOptionPreserveVerticalSpace = False
                       , cfgOptionDeclNoBlankLines      = Set.empty
                       }

data Config = Config { cfgPenalty :: !PenaltyConfig
                     , cfgAlign   :: !AlignConfig
                     , cfgIndent  :: !IndentConfig
                     , cfgLayout  :: !LayoutConfig
                     , cfgOp      :: !OpConfig
                     , cfgGroup   :: !GroupConfig
                     , cfgOptions :: !OptionConfig
                     }
    deriving ( Generic )

instance Default Config where
    def = Config { cfgPenalty = def
                 , cfgAlign   = def
                 , cfgIndent  = def
                 , cfgLayout  = def
                 , cfgOp      = def
                 , cfgGroup   = def
                 , cfgOptions = def
                 }

defaultConfig :: Config
defaultConfig =
    def { cfgOp    = OpConfig ((unOpConfig def) { cfgMapOverrides =
                                                      Map.fromList opWsOverrides
                                                })
        , cfgGroup = GroupConfig ((unGroupConfig def) { cfgMapOverrides = Map.fromList groupWsOverrides
                                                      })
        }
  where
    opWsOverrides =
        [ (ConfigMapKey (Just ",") Nothing Nothing, Whitespace WsAfter WsBefore False)
        , ( ConfigMapKey (Just "record") Nothing Nothing
          , Whitespace WsAfter WsAfter False
          )
        , ( ConfigMapKey (Just ".") (Just Type) Nothing
          , Whitespace WsAfter WsAfter False
          )
        ]

    groupWsOverrides =
        [ (ConfigMapKey (Just "[") (Just Type) Nothing, Whitespace WsBoth WsNone False)
        ]

safeConfig :: Config -> Config
safeConfig cfg = cfg { cfgGroup = group, cfgOp = op }
  where
    group = GroupConfig $
        updateOverrides (unGroupConfig $ cfgGroup cfg)
                        [ ("(#", Expression, WsBoth, Nothing), ("(#", Pattern, WsBoth, Nothing) ]

    op = OpConfig $
        updateOverrides (unOpConfig $ cfgOp cfg) [ (".", Expression, WsBoth, Nothing), ("@", Pattern, WsNone, Nothing)  ]

    updateOverrides config overrides =
        config { cfgMapOverrides =
                     foldl (updateWs config) (cfgMapOverrides config) overrides
               }

    updateWs config m (key, ctx, ws, within) =
        Map.insertWith (flip const)
                       (ConfigMapKey (Just key) (Just ctx) within)
                       (cfgMapFind ctx key config) { wsSpaces = ws }
                       m

cfgMapFind' :: LayoutContext -> Maybe WithinDeclaration -> ByteString -> ConfigMap a -> a
cfgMapFind' ctx within key ConfigMap{..} =
    let value = cfgMapDefault
        -- Find default for context
        value' = Map.findWithDefault value
                                     (ConfigMapKey Nothing (Just ctx) Nothing)
                                     cfgMapOverrides
        -- Override with default for that op or group
        value'' = Map.findWithDefault value'
                                      (ConfigMapKey (Just key) Nothing Nothing)
                                      cfgMapOverrides
        -- Override with specific default for that op or group
        value''' = Map.findWithDefault value''
                                       (ConfigMapKey (Just key) (Just ctx) Nothing)
                                       cfgMapOverrides
        -- Override with specific for that op or group in within
        value'''' = Map.findWithDefault value'''
                                       (ConfigMapKey (Just key) (Just ctx) within)
                                       cfgMapOverrides
    in
        value''''

cfgMapFind :: LayoutContext -> ByteString -> ConfigMap a -> a
cfgMapFind ctx key c = cfgMapFind' ctx Nothing key c

cfgOpWs' :: LayoutContext -> Maybe WithinDeclaration -> ByteString -> OpConfig -> Whitespace
cfgOpWs' ctx within op = cfgMapFind' ctx within op . unOpConfig

cfgOpWs :: LayoutContext -> ByteString -> OpConfig -> Whitespace
cfgOpWs ctx op = cfgOpWs' ctx Nothing op

cfgGroupWs' :: LayoutContext -> Maybe WithinDeclaration -> ByteString -> GroupConfig -> Whitespace
cfgGroupWs' ctx within op = cfgMapFind' ctx within op . unGroupConfig

cfgGroupWs :: LayoutContext -> ByteString -> GroupConfig -> Whitespace
cfgGroupWs ctx op = cfgGroupWs' ctx Nothing op

inWs :: Location -> WsLoc -> Bool
inWs _ WsBoth = True
inWs Before WsBefore = True
inWs After WsAfter = True
inWs _ _ = False

wsSpace :: Location -> Whitespace -> Bool
wsSpace loc ws = loc `inWs` wsSpaces ws

wsLinebreak :: Location -> Whitespace -> Bool
wsLinebreak loc ws = loc `inWs` wsLinebreaks ws

------------------------------------------------------------------------
readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
    [ (x, "") ] -> Just x
    _ -> Nothing

enumOptions :: Int -> Options
enumOptions n =
    JSON.defaultOptions { constructorTagModifier = JSON.camelTo2 '-' . drop n }

recordOptions :: Int -> Options
recordOptions n =
    JSON.defaultOptions { fieldLabelModifier = JSON.camelTo2 '-' . drop n
                        , unwrapUnaryRecords = True
                        }

instance ToJSON Indent where
    toJSON i = JSON.String $ case i of
        Align -> "align"
        IndentBy x -> "indent-by " `T.append` T.pack (show x)
        AlignOrIndentBy x -> "align-or-indent-by " `T.append` T.pack (show x)

instance FromJSON Indent where
    parseJSON v@(JSON.String t) = maybe (JSON.typeMismatch "Indent" v) return $
        if t == "align"
        then Just Align
        else if "indent-by " `T.isPrefixOf` t
             then IndentBy <$> readMaybe (T.unpack $ T.drop 10 t)
             else if "align-or-indent-by " `T.isPrefixOf` t
                  then AlignOrIndentBy <$> readMaybe (T.unpack $ T.drop 19 t)
                  else Nothing

    parseJSON v = JSON.typeMismatch "Indent" v

instance ToJSON LayoutContext where
    toJSON = genericToJSON (enumOptions 0)

instance FromJSON LayoutContext where
    parseJSON = genericParseJSON (enumOptions 0)

instance ToJSON WithinDeclaration where
    toJSON = genericToJSON (enumOptions 0)

instance FromJSON WithinDeclaration where
    parseJSON = genericParseJSON (enumOptions 0)

instance ToJSON WsLoc where
    toJSON = genericToJSON (enumOptions 2)

instance FromJSON WsLoc where
    parseJSON = genericParseJSON (enumOptions 2)

instance ToJSON Whitespace where
    toJSON = genericToJSON (recordOptions 2)

instance FromJSON Whitespace where
    parseJSON = genericParseJSON (recordOptions 2)

instance ToJSON Layout where
    toJSON = genericToJSON (enumOptions 0)

instance FromJSON Layout where
    parseJSON = genericParseJSON (enumOptions 0)

layoutToText :: LayoutContext -> T.Text
layoutToText Declaration = "declaration"
layoutToText Type = "type"
layoutToText Pattern = "pattern"
layoutToText Expression = "expression"
layoutToText Other = "other"

textToLayout :: T.Text -> Maybe LayoutContext
textToLayout "declaration" = Just Declaration
textToLayout "type" = Just Type
textToLayout "pattern" = Just Pattern
textToLayout "expression" = Just Expression
textToLayout "other" = Just Other
textToLayout _ = Nothing

withinToText :: WithinDeclaration -> T.Text
withinToText ModuleDeclaration = "module"
withinToText RecordDeclaration = "record"
withinToText TypeDeclaration = "type"
withinToText ComprehensionDeclaration = "comprehension"
withinToText SpecialDeclaration = "special"
withinToText OtherDeclaration = "other"

textToWithin :: T.Text -> Maybe WithinDeclaration
textToWithin "module" = Just ModuleDeclaration
textToWithin "record" = Just RecordDeclaration
textToWithin "type" = Just TypeDeclaration
textToWithin "comprehension" = Just ComprehensionDeclaration
textToWithin "special" = Just SpecialDeclaration
textToWithin "other" = Just OtherDeclaration
textToWithin _ = Nothing

keyToText :: ConfigMapKey -> T.Text
keyToText (ConfigMapKey Nothing Nothing Nothing) = "default"
keyToText (ConfigMapKey (Just n) Nothing Nothing) = T.decodeUtf8 n
keyToText (ConfigMapKey Nothing (Just l) Nothing) = "* in " `T.append` layoutToText l
keyToText (ConfigMapKey Nothing Nothing (Just w)) = "* within " `T.append` withinToText w
keyToText (ConfigMapKey Nothing (Just l) (Just w)) =
  "* in " `T.append` layoutToText l `T.append` " within " `T.append` withinToText w
keyToText (ConfigMapKey (Just n) (Just l) Nothing) =
    T.decodeUtf8 n `T.append` " in " `T.append` layoutToText l
keyToText (ConfigMapKey (Just n) Nothing (Just w)) =
    T.decodeUtf8 n `T.append` " within " `T.append` withinToText w
keyToText (ConfigMapKey (Just n) (Just l) (Just w)) =
    T.decodeUtf8 n `T.append` " in " `T.append` layoutToText l `T.append` " within " `T.append` withinToText w

-- attoparsec for within parsing
textToKey :: T.Text -> Maybe ConfigMapKey
textToKey t = 
  let
    hasWithin = " within " `T.isInfixOf` t
    hasIn = " in " `T.isInfixOf` t
  in case (hasIn, hasWithin) of
    (False, False) -> case t of
      "default" -> Just (ConfigMapKey Nothing Nothing Nothing)
      name -> Just (ConfigMapKey (Just (T.encodeUtf8 name)) Nothing Nothing)
      _ -> Nothing
    (True, False) -> case T.splitOn " in " t of
      [ "*", "*" ] -> Just (ConfigMapKey Nothing Nothing Nothing)
      [ name, "*" ] -> Just (ConfigMapKey (Just (T.encodeUtf8 name)) Nothing Nothing)
      [ "*", layout ] -> noWithin Nothing . Just <$> textToLayout layout
      [ name, layout ] -> noWithin (Just (T.encodeUtf8 name)) . Just
          <$> textToLayout layout
      _ -> Nothing
    (False, True) -> case T.splitOn " within " t of
      [ "*", "*" ] -> Just (ConfigMapKey Nothing Nothing Nothing)
      [ name, "*" ] -> Just (ConfigMapKey (Just (T.encodeUtf8 name)) Nothing Nothing)
      [ "*", within ] -> noLayout Nothing . Just <$> textToWithin within
      [ name, within ] -> noLayout (Just (T.encodeUtf8 name)) . Just
          <$> textToWithin within
      _ -> Nothing
    (True, True) -> case T.splitOn " in " t of
      [ name', rest ] -> case T.splitOn " within" rest of
          [ layout', within' ] -> case (name', layout', within') of
              ( "*", "*", "*" ) -> Just (ConfigMapKey Nothing Nothing Nothing)
              ( "*", layout, "*" ) -> Just (ConfigMapKey Nothing (textToLayout layout) Nothing)
              ( "*", "*", within ) -> Just (ConfigMapKey Nothing Nothing (textToWithin within))
              ( name, "*", "*" ) -> Just (ConfigMapKey (Just (T.encodeUtf8 name)) Nothing Nothing)
              ( name, layout, "*" ) -> Just (ConfigMapKey (Just (T.encodeUtf8 name)) (textToLayout layout) Nothing)
              ( name, "*", within ) -> Just (ConfigMapKey (Just (T.encodeUtf8 name)) Nothing (textToWithin within))
              ( name, layout, within ) -> Just (ConfigMapKey (Just (T.encodeUtf8 name)) (textToLayout layout) (textToWithin within))
              _ -> Nothing
      _ -> Nothing
  where
    noWithin o l = ConfigMapKey o l Nothing
    noLayout o w = ConfigMapKey o Nothing w

instance ToJSON a => ToJSON (ConfigMap a) where
    toJSON ConfigMap{..} = toJSON $ Map.insert "default" cfgMapDefault $
        Map.mapKeys keyToText cfgMapOverrides

instance FromJSON a => FromJSON (ConfigMap a) where
    parseJSON value = do
        o <- parseJSON value
        cfgMapDefault <- maybe (fail "Missing key: default") return $
            HashMap.lookup "default" o
        cfgMapOverrides <- either fail (return . Map.fromList) $ mapM toKey $
            HashMap.toList $ HashMap.delete "default" o
        return ConfigMap { .. }
      where
        toKey (k, v) = case textToKey k of
            Just k' -> Right (k', v)
            Nothing -> Left ("Invalid key: " ++ T.unpack k)

instance ToJSON PenaltyConfig where
    toJSON = genericToJSON (recordOptions 7)

instance FromJSON PenaltyConfig where
    parseJSON = genericParseJSON (recordOptions 7)

instance ToJSON AlignConfig where
    toJSON = genericToJSON (recordOptions 8)

instance FromJSON AlignConfig where
    parseJSON = genericParseJSON (recordOptions 8)

instance ToJSON IndentConfig where
    toJSON = genericToJSON (recordOptions 9)

instance FromJSON IndentConfig where
    parseJSON = genericParseJSON (recordOptions 9)

instance ToJSON LayoutConfig where
    toJSON = genericToJSON (recordOptions 9)

instance FromJSON LayoutConfig where
    parseJSON = genericParseJSON (recordOptions 9)

instance ToJSON WithinLayout where
    toJSON = genericToJSON (recordOptions 2)

instance FromJSON WithinLayout where
    parseJSON v@(JSON.String t) = do
        layout <- parseJSON v
        pure WithinLayout { wlModuleLayout = layout
                          , wlRecordLayout = layout
                          , wlTypeLayout = layout
                          , wlOtherLayout = layout
                          }

    parseJSON o = genericParseJSON (recordOptions 2) o

instance ToJSON OpConfig where
    toJSON = genericToJSON (recordOptions 0)

instance FromJSON OpConfig where
    parseJSON = genericParseJSON (recordOptions 0)

instance ToJSON GroupConfig where
    toJSON = genericToJSON (recordOptions 0)

instance FromJSON GroupConfig where
    parseJSON = genericParseJSON (recordOptions 0)

instance ToJSON ImportsGroupOrder where
    toJSON = genericToJSON (enumOptions 12)

instance FromJSON ImportsGroupOrder where
    parseJSON = genericParseJSON (enumOptions 12)

instance ToJSON ImportsGroup where
    toJSON = genericToJSON (recordOptions 7)

instance FromJSON ImportsGroup where
    parseJSON x@JSON.Array{} =
        ImportsGroup <$> parseJSON x <*> pure ImportsGroupKeep
    parseJSON x = genericParseJSON (recordOptions 7) x

instance ToJSON SortImportsRule where
    toJSON NoImportSort = toJSON False
    toJSON SortImportsByPrefix = toJSON True
    toJSON (SortImportsByGroups xs) = toJSON xs

instance FromJSON SortImportsRule where
    parseJSON (JSON.Bool False) = return NoImportSort
    parseJSON (JSON.Bool True) = return SortImportsByPrefix
    parseJSON v = SortImportsByGroups <$> parseJSON v

instance ToJSON DeclarationConstruct where
    toJSON = genericToJSON (enumOptions 4)

instance FromJSON DeclarationConstruct where
    parseJSON = genericParseJSON (enumOptions 4)

instance ToJSON OptionConfig where
    toJSON = genericToJSON (recordOptions 9)

instance FromJSON OptionConfig where
    parseJSON = genericParseJSON (recordOptions 9)

instance ToJSON Config where
    toJSON = genericToJSON (recordOptions 3)

instance FromJSON Config where
    parseJSON = genericParseJSON (recordOptions 3)
