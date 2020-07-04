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
    , simpleWithinLayout
    , cfgMapFind
    , cfgOpWs
    , cfgGroupWs
    , cfgOpWs'
    , cfgGroupWs'
    , inWs
    , wsSpace
    , wsLinebreak
    , wsSpaceHOverride
    , wsSpaceH
    , simpleWhitespace
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
import           Data.Maybe         ( fromMaybe )
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
                       | GADTDeclaration
                       | GADTFieldDeclaration
                       | GADTFieldTypeDeclaration
                       | TypeDeclaration
                       | ComprehensionDeclaration
                       | SpecialDeclaration
                       | PatternDeclaration
                       | GuardDeclaration
                       | ExportDeclaration
                       | ClassDeclaration
                       | CaseDeclaration
                       | OtherDeclaration
    deriving ( Eq, Ord, Bounded, Enum, Show, Generic )

data Location = Before | After
    deriving ( Eq, Ord, Bounded, Enum, Show, Generic )

data WsLoc = WsNone | WsBefore | WsAfter | WsBoth
    deriving ( Eq, Ord, Bounded, Enum, Show, Generic )

data Whitespace = Whitespace { wsSpaces          :: !WsLoc
                             , wsLinebreaks      :: !WsLoc
                             , wsForceLinebreak  :: !Bool
                             , wsSpacesHOverride :: !(Maybe WsLoc)
                             }
    deriving ( Show, Generic )

simpleWhitespace :: WsLoc -> WsLoc -> Bool -> Whitespace
simpleWhitespace wsSpaces wsLinebreaks wsForceLinebreak = Whitespace {wsSpacesHOverride = Nothing, ..}

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
    AlignConfig { cfgAlignLimits            :: !(Int, Int)
                , cfgAlignCase              :: !Bool
                , cfgAlignClass             :: !Bool
                , cfgAlignDoLeftArrow       :: !Bool
                , cfgAlignImportModule      :: !Bool
                , cfgAlignImportSpec        :: !Bool
                , cfgAlignImportAsMin       :: !(Maybe Int)
                , cfgAlignLetBinds          :: !Bool
                , cfgAlignMatches           :: !Bool
                , cfgAlignModulePragmaEnds  :: !Bool
                , cfgAlignMultiIfRhs        :: !Bool
                , cfgAlignRecordFields      :: !Bool
                , cfgAlignWhere             :: !Bool
                }
    deriving ( Generic )

instance Default AlignConfig where
    def = AlignConfig { cfgAlignLimits            = (10, 25)
                      , cfgAlignCase              = False
                      , cfgAlignClass             = False
                      , cfgAlignDoLeftArrow       = False
                      , cfgAlignImportModule      = False
                      , cfgAlignImportSpec        = False
                      , cfgAlignImportAsMin       = Nothing
                      , cfgAlignLetBinds          = False
                      , cfgAlignMatches           = False
                      , cfgAlignModulePragmaEnds  = False
                      , cfgAlignMultiIfRhs        = False
                      , cfgAlignRecordFields      = False
                      , cfgAlignWhere             = False
                      }

data IndentConfig =
    IndentConfig { cfgIndentOnside :: !Int
                 , cfgIndentDeriving :: !Int
                 , cfgIndentWhere :: !Int
                 , cfgIndentModuleWhere :: !(Maybe Int)
                 , cfgIndentApp :: !Indent
                 , cfgIndentCase :: !Indent
                 , cfgIndentClass :: !Indent
                 , cfgIndentDo :: !Indent
                 , cfgIndentExportSpecList :: !Indent
                 , cfgIndentExportSpecInnerList :: !Indent
                 , cfgIndentIf :: !Indent
                 , cfgIndentImportSpecList :: !Indent
                 , cfgIndentImportSpecInnerList :: !Indent
                 , cfgIndentLet :: !Indent
                 , cfgIndentLetBinds :: !Indent
                 , cfgIndentLetIn :: !Indent
                 , cfgIndentMultiIf :: !Indent
                 , cfgIndentPatternApp :: !Indent
                 , cfgIndentPatternsig :: !Indent
                 , cfgIndentSimpleDeclaration :: !Indent
                 , cfgIndentTypeApp :: !Indent
                 , cfgIndentTypesig :: !Indent
                 , cfgIndentWhereBinds :: !Indent
                 }
    deriving ( Generic )

instance Default IndentConfig where
    def = IndentConfig { cfgIndentOnside = 4
                       , cfgIndentDeriving = 4
                       , cfgIndentWhere = 2
                       , cfgIndentModuleWhere = Nothing
                       , cfgIndentApp = IndentBy 4
                       , cfgIndentCase = IndentBy 4
                       , cfgIndentClass = IndentBy 4
                       , cfgIndentDo = IndentBy 4
                       , cfgIndentExportSpecList = IndentBy 4
                       , cfgIndentExportSpecInnerList = Align
                       , cfgIndentIf = IndentBy 4
                       , cfgIndentImportSpecList = IndentBy 4
                       , cfgIndentImportSpecInnerList = Align
                       , cfgIndentLet = IndentBy 4
                       , cfgIndentLetBinds = IndentBy 4
                       , cfgIndentLetIn = IndentBy 4
                       , cfgIndentMultiIf = IndentBy 4
                       , cfgIndentPatternApp = IndentBy 4
                       , cfgIndentPatternsig = IndentBy 4
                       , cfgIndentSimpleDeclaration = AlignOrIndentBy 4
                       , cfgIndentTypeApp = Align
                       , cfgIndentTypesig = IndentBy 4
                       , cfgIndentWhereBinds = IndentBy 2
                       }

data WithinLayout
  = WithinLayout { wlModuleLayout :: !Layout
                 , wlRecordLayout :: !Layout
                 , wlGADTLayout :: !Layout
                 , wlGADTFieldLayout :: !Layout
                 , wlGADTFieldTypeLayout :: !Layout
                 , wlTypeLayout :: !Layout
                 , wlSpecialLayout :: !Layout
                 , wlPatternLayout :: !Layout
                 , wlGuardLayout :: !Layout
                 , wlComprehensionLayout :: !Layout
                 , wlExportLayout :: !Layout
                 , wlClassLayout :: !Layout
                 , wlCaseLayout :: !Layout
                 , wlOtherLayout :: !Layout
                 }
    deriving ( Generic )

instance Default WithinLayout where
    def = simpleWithinLayout Flex

simpleWithinLayout :: Layout -> WithinLayout
simpleWithinLayout layout = WithinLayout { wlModuleLayout = layout
                                         , wlRecordLayout = layout
                                         , wlGADTLayout = layout
                                         , wlGADTFieldLayout = layout
                                         , wlGADTFieldTypeLayout = layout
                                         , wlTypeLayout = layout
                                         , wlSpecialLayout = layout
                                         , wlPatternLayout = layout
                                         , wlComprehensionLayout = layout
                                         , wlGuardLayout = layout
                                         , wlExportLayout = layout
                                         , wlClassLayout = layout
                                         , wlCaseLayout = layout
                                         , wlOtherLayout = layout
                                         }

data LayoutConfig =
    LayoutConfig { cfgLayoutApp :: !Layout
                 , cfgLayoutConDecls :: !Layout
                 , cfgLayoutDeclaration :: !Layout
                 , cfgLayoutExportSpecList :: !Layout
                 , cfgLayoutExportSpecInnerList :: !Layout
                 , cfgLayoutIf :: !Layout
                 , cfgLayoutImportSpecList :: !Layout
                 , cfgLayoutImportSpecInnerList :: !Layout
                 , cfgLayoutInfixApp :: !Layout
                 , cfgLayoutLet :: !Layout
                 , cfgLayoutListComp :: !Layout
                 , cfgLayoutList :: !Layout
                 , cfgLayoutRecord :: !Layout
                 , cfgLayoutPatternApp :: !Layout
                 , cfgLayoutPatternSynonym :: !Layout
                 , cfgLayoutConstraints :: !Layout
                 , cfgLayoutType :: !WithinLayout
                 , cfgLayoutTypeApp :: !Layout
                 , cfgLayoutUnboxedSum :: !Layout
                 }
    deriving ( Generic )

instance Default LayoutConfig where
    def = LayoutConfig { cfgLayoutApp = Flex
                       , cfgLayoutConDecls = Flex
                       , cfgLayoutDeclaration = Flex
                       , cfgLayoutExportSpecList = Flex
                       , cfgLayoutExportSpecInnerList = Flex
                       , cfgLayoutIf = Flex
                       , cfgLayoutImportSpecList = Flex
                       , cfgLayoutImportSpecInnerList = Flex
                       , cfgLayoutInfixApp = Flex
                       , cfgLayoutLet = Flex
                       , cfgLayoutListComp = Flex
                       , cfgLayoutList = Flex
                       , cfgLayoutRecord = Flex
                       , cfgLayoutPatternApp = Flex
                       , cfgLayoutPatternSynonym = Flex
                       , cfgLayoutConstraints = Flex
                       , cfgLayoutType = def
                       , cfgLayoutTypeApp = Flex
                       , cfgLayoutUnboxedSum = Flex
                       }

newtype OpConfig = OpConfig { unOpConfig :: ConfigMap Whitespace }
    deriving ( Generic )

instance Default OpConfig where
    def =
        OpConfig ConfigMap { cfgMapDefault   = Whitespace WsBoth WsBefore False Nothing
                           , cfgMapOverrides = Map.empty
                           }

newtype GroupConfig = GroupConfig { unGroupConfig :: ConfigMap Whitespace }
    deriving ( Generic )

instance Default GroupConfig where
    def = GroupConfig ConfigMap { cfgMapDefault   =
                                      Whitespace WsBoth WsAfter False Nothing
                                , cfgMapOverrides = Map.empty
                                }

data ImportsGroupOrder =
    ImportsGroupKeep | ImportsGroupSorted | ImportsGroupGrouped
    deriving ( Generic, Show )

data ImportsGroup = ImportsGroup { importsPrefixes :: ![String]
                                 , importsOrder    :: !ImportsGroupOrder
                                 }
    deriving ( Generic, Show )

data SortImportsRule =
    NoImportSort | SortImportsByPrefix | SortImportsByGroups ![ImportsGroup]
    deriving ( Show )

data DeclarationConstruct = DeclModule | DeclClass | DeclInstance | DeclWhere
    deriving ( Eq, Ord, Generic )

data OptionConfig =
    OptionConfig { cfgOptionSortPragmas                 :: !Bool
                 , cfgOptionSplitLanguagePragmas        :: !Bool
                 , cfgOptionSortImports                 :: !SortImportsRule
                 , cfgOptionSortImportLists             :: !Bool
                 , cfgOptionSortImportSpecInnerLists    :: !Bool
                 , cfgOptionImportCompactSpecialization :: !(Bool, Bool)
                 , cfgOptionAlignSumTypeDecl            :: !Bool
                 , cfgOptionFlexibleOneline             :: !Bool
                 , cfgOptionPreserveVerticalSpace       :: !Bool
                 , cfgOptionMultiIfPadding              :: !Bool
                 , cfgOptionLetSpecialization           :: !Bool
                 , cfgOptionLetDoSpecialization         :: !Bool
                 , cfgOptionLetInPadding                :: !(Bool, Bool)
                 , cfgOptionListCompSpecialization      :: !Bool
                 , cfgOptionCompactVerticalList         :: !(Maybe (Bool, Bool))
                 , cfgOptionDeclNoBlankLines            :: !(Set DeclarationConstruct)
                 }
    deriving ( Generic )

instance Default OptionConfig where
    def = OptionConfig { cfgOptionSortPragmas                 = False
                       , cfgOptionSplitLanguagePragmas        = False
                       , cfgOptionSortImports                 = NoImportSort
                       , cfgOptionSortImportLists             = False
                       , cfgOptionSortImportSpecInnerLists    = False
                       , cfgOptionImportCompactSpecialization = (False, False)
                       , cfgOptionAlignSumTypeDecl            = False
                       , cfgOptionFlexibleOneline             = False
                       , cfgOptionPreserveVerticalSpace       = False
                       , cfgOptionMultiIfPadding              = False
                       , cfgOptionLetSpecialization           = False
                       , cfgOptionLetDoSpecialization         = False
                       , cfgOptionLetInPadding                = (True, True)
                       , cfgOptionListCompSpecialization      = False
                       , cfgOptionCompactVerticalList         = Nothing
                       , cfgOptionDeclNoBlankLines            = Set.empty
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
        , cfgGroup = GroupConfig ((unGroupConfig def) { cfgMapOverrides =
                                                            Map.fromList groupWsOverrides
                                                      })
        }
  where
    opWsOverrides =
        [ (ConfigMapKey (Just ",") Nothing Nothing, Whitespace WsAfter WsBefore False Nothing)
        , ( ConfigMapKey (Just "record") Nothing Nothing
          , Whitespace WsAfter WsAfter False Nothing
          )
        , ( ConfigMapKey (Just ".") (Just Type) Nothing
          , Whitespace WsAfter WsAfter False Nothing
          )
        , ( ConfigMapKey (Just "module_where") (Just Declaration) Nothing
          , Whitespace WsBefore WsNone False Nothing
          )
        , ( ConfigMapKey (Just "if") (Just Expression) Nothing
          , Whitespace WsNone WsAfter False (Just WsAfter)
          )
        , ( ConfigMapKey (Just "then") (Just Expression) Nothing
          , Whitespace WsNone WsAfter False (Just WsAfter)
          )
        , ( ConfigMapKey (Just "else") (Just Expression) Nothing
          , Whitespace WsNone WsAfter False (Just WsAfter)
          )
        ]

    groupWsOverrides =
        [ (ConfigMapKey (Just "[") (Just Type) Nothing, Whitespace WsBoth WsNone False Nothing)
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
cfgMapFind ctx = cfgMapFind' ctx Nothing

cfgOpWs' :: LayoutContext -> Maybe WithinDeclaration -> ByteString -> OpConfig -> Whitespace
cfgOpWs' ctx within op = cfgMapFind' ctx within op . unOpConfig

cfgOpWs :: LayoutContext -> ByteString -> OpConfig -> Whitespace
cfgOpWs ctx = cfgOpWs' ctx Nothing

cfgGroupWs' :: LayoutContext -> Maybe WithinDeclaration -> ByteString -> GroupConfig -> Whitespace
cfgGroupWs' ctx within op = cfgMapFind' ctx within op . unGroupConfig

cfgGroupWs :: LayoutContext -> ByteString -> GroupConfig -> Whitespace
cfgGroupWs ctx = cfgGroupWs' ctx Nothing

inWs :: Location -> WsLoc -> Bool
inWs _ WsBoth = True
inWs Before WsBefore = True
inWs After WsAfter = True
inWs _ _ = False

wsSpace :: Location -> Whitespace -> Bool
wsSpace loc ws = loc `inWs` wsSpaces ws

wsLinebreak :: Location -> Whitespace -> Bool
wsLinebreak loc ws = loc `inWs` wsLinebreaks ws

wsSpaceHOverride :: Location -> Whitespace -> Bool
wsSpaceHOverride loc ws = loc `inWs` fromMaybe WsNone (wsSpacesHOverride ws)

wsSpaceH :: Location -> Whitespace -> Bool
wsSpaceH loc ws = loc `inWs` fromMaybe (wsSpaces ws) (wsSpacesHOverride ws)

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
withinToText GADTDeclaration = "gadt"
withinToText GADTFieldDeclaration = "gadt_field"
withinToText GADTFieldTypeDeclaration = "gadt_field_type"
withinToText TypeDeclaration = "type"
withinToText ComprehensionDeclaration = "comprehension"
withinToText SpecialDeclaration = "special"
withinToText PatternDeclaration = "pattern"
withinToText GuardDeclaration = "guard"
withinToText ExportDeclaration = "export"
withinToText ClassDeclaration = "class"
withinToText CaseDeclaration = "case"
withinToText OtherDeclaration = "other"

textToWithin :: T.Text -> Maybe WithinDeclaration
textToWithin "module" = Just ModuleDeclaration
textToWithin "record" = Just RecordDeclaration
textToWithin "gadt" = Just GADTDeclaration
textToWithin "gadt_field" = Just GADTFieldDeclaration
textToWithin "gadt_field_type" = Just GADTFieldDeclaration
textToWithin "type" = Just TypeDeclaration
textToWithin "comprehension" = Just ComprehensionDeclaration
textToWithin "special" = Just SpecialDeclaration
textToWithin "pattern" = Just PatternDeclaration
textToWithin "guard" = Just GuardDeclaration
textToWithin "export" = Just ExportDeclaration
textToWithin "class" = Just ClassDeclaration
textToWithin "case" = Just CaseDeclaration
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
      [ name', rest ] -> case T.splitOn " within " rest of
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
    noLayout o = ConfigMapKey o Nothing

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
    parseJSON v@JSON.String {} = do
        layout <- parseJSON v
        pure $ simpleWithinLayout layout

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
