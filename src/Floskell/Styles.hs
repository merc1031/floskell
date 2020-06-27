{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Floskell.Styles ( Style(..), styles ) where

import qualified Data.Map        as Map
import qualified Data.Set        as Set
import           Data.Text       ( Text )

import           Floskell.Config

-- | A printer style.
data Style =
    Style { styleName        :: !Text   -- ^ Name of the style, used in the commandline interface.
          , styleAuthor      :: !Text   -- ^ Author of the style definition.
          , styleDescription :: !Text   -- ^ Description of the style.
          , styleConfig      :: !Config -- ^ Style definition.
          }

chrisDoneCfg :: Config
chrisDoneCfg =
    defaultConfig { cfgIndent, cfgLayout, cfgOp, cfgGroup, cfgOptions }
  where
    cfgIndent =
        IndentConfig { cfgIndentOnside = 2
                     , cfgIndentDeriving = 2
                     , cfgIndentWhere = 2
                     , cfgIndentModuleWhere = Nothing
                     , cfgIndentApp = Align
                     , cfgIndentCase = IndentBy 2
                     , cfgIndentClass = IndentBy 2
                     , cfgIndentDo = Align
                     , cfgIndentIf = IndentBy 3
                     , cfgIndentLet = Align
                     , cfgIndentLetBinds = Align
                     , cfgIndentLetIn = Align
                     , cfgIndentMultiIf = IndentBy 2
                     , cfgIndentPatternApp = Align
                     , cfgIndentPatternsig = Align
                     , cfgIndentSimpleDeclaration = AlignOrIndentBy 4
                     , cfgIndentTypesig = Align
                     , cfgIndentTypeApp = Align
                     , cfgIndentWhereBinds = Align
                     , cfgIndentExportSpecList = IndentBy 2
                     , cfgIndentExportSpecInnerList = Align
                     , cfgIndentImportSpecList = AlignOrIndentBy 7
                     , cfgIndentImportSpecInnerList = Align
                     }

    cfgLayout = LayoutConfig { cfgLayoutApp = TryOneline
                             , cfgLayoutConDecls = Vertical
                             , cfgLayoutDeclaration = TryOneline
                             , cfgLayoutExportSpecList = TryOneline
                             , cfgLayoutExportSpecInnerList = TryOneline
                             , cfgLayoutIf = Vertical
                             , cfgLayoutImportSpecList = Flex
                             , cfgLayoutImportSpecInnerList = Flex
                             , cfgLayoutInfixApp = TryOneline
                             , cfgLayoutLet = Vertical
                             , cfgLayoutListComp = Flex
                             , cfgLayoutList = Flex
                             , cfgLayoutRecord = Vertical
                             , cfgLayoutPatternApp = TryOneline
                             , cfgLayoutPatternSynonym = TryOneline
                             , cfgLayoutConstraints = Flex
                             , cfgLayoutType = simpleWithinLayout TryOneline
                             , cfgLayoutTypeApp = Flex
                             , cfgLayoutUnboxedSum = Flex
                             }

    cfgOp =
        OpConfig ConfigMap { cfgMapDefault   = simpleWhitespace WsBoth WsBefore False
                           , cfgMapOverrides = Map.fromList opWsOverrides
                           }

    opWsOverrides =
        [ (ConfigMapKey (Just ",") Nothing Nothing, simpleWhitespace WsNone WsBefore False)
        , ( ConfigMapKey (Just "record") Nothing Nothing
          , simpleWhitespace WsAfter WsNone False
          )
        , ( ConfigMapKey (Just ".") (Just Type) Nothing
          , simpleWhitespace WsAfter WsAfter False
          )
        , (ConfigMapKey (Just "=") Nothing Nothing, simpleWhitespace WsBoth WsAfter False)
        , (ConfigMapKey (Just "<-") Nothing Nothing, simpleWhitespace WsBoth WsAfter False)
        , (ConfigMapKey (Just ":") Nothing Nothing, simpleWhitespace WsNone WsBefore False)
        ]

    cfgGroup =
        GroupConfig ConfigMap { cfgMapDefault   =
                                    simpleWhitespace WsNone WsNone False
                              , cfgMapOverrides = Map.fromList groupWsOverrides
                              }

    groupWsOverrides = []

    cfgOptions = OptionConfig { cfgOptionSortPragmas                  = False
                              , cfgOptionSplitLanguagePragmas         = False
                              , cfgOptionSortImports                  = NoImportSort
                              , cfgOptionSortImportLists              = False
                              , cfgOptionSortImportSpecInnerLists     = False
                              , cfgOptionImportCompactSpecialization  = (False, False)
                              , cfgOptionAlignSumTypeDecl             = True
                              , cfgOptionFlexibleOneline              = False
                              , cfgOptionPreserveVerticalSpace        = False
                              , cfgOptionMultiIfPadding               = True
                              , cfgOptionLetSpecialization            = False
                              , cfgOptionLetDoSpecialization          = False
                              , cfgOptionLetInPadding                 = (True, True)
                              , cfgOptionListCompSpecialization       = False
                              , cfgOptionCompactVerticalList          = Nothing
                              , cfgOptionSimpleTypeApp                = False
                              , cfgOptionAltPadding                   = False
                              , cfgOptionDeclNoBlankLines             = Set.empty
                              }

cramerCfg :: Config
cramerCfg = defaultConfig { cfgAlign
                          , cfgIndent
                          , cfgLayout
                          , cfgOp
                          , cfgGroup
                          , cfgOptions
                          }
  where
    cfgAlign = AlignConfig { cfgAlignLimits           = (10, 25)
                           , cfgAlignCase             = False
                           , cfgAlignClass            = False
                           , cfgAlignDoLeftArrow      = False
                           , cfgAlignImportModule     = True
                           , cfgAlignImportSpec       = True
                           , cfgAlignImportAsMin      = Nothing
                           , cfgAlignLetBinds         = False
                           , cfgAlignMatches          = False
                           , cfgAlignModulePragmaEnds = False
                           , cfgAlignMultiIfRhs       = False
                           , cfgAlignRecordFields     = True
                           , cfgAlignWhere            = False
                           }

    cfgIndent =
        IndentConfig { cfgIndentOnside = 4
                     , cfgIndentDeriving = 4
                     , cfgIndentWhere = 2
                     , cfgIndentModuleWhere = Nothing
                     , cfgIndentApp = Align
                     , cfgIndentCase = IndentBy 4
                     , cfgIndentClass = IndentBy 4
                     , cfgIndentDo = IndentBy 4
                     , cfgIndentIf = Align
                     , cfgIndentLet = Align
                     , cfgIndentLetBinds = Align
                     , cfgIndentLetIn = IndentBy 4
                     , cfgIndentMultiIf = IndentBy 4
                     , cfgIndentPatternApp = Align
                     , cfgIndentPatternsig = Align
                     , cfgIndentSimpleDeclaration = AlignOrIndentBy 4
                     , cfgIndentTypesig = Align
                     , cfgIndentTypeApp = Align
                     , cfgIndentWhereBinds = IndentBy 2
                     , cfgIndentExportSpecList = IndentBy 4
                     , cfgIndentExportSpecInnerList = Align
                     , cfgIndentImportSpecList = AlignOrIndentBy 17
                     , cfgIndentImportSpecInnerList = Align
                     }

    cfgLayout = LayoutConfig { cfgLayoutApp = TryOneline
                             , cfgLayoutConDecls = TryOneline
                             , cfgLayoutDeclaration = Flex
                             , cfgLayoutExportSpecList = TryOneline
                             , cfgLayoutExportSpecInnerList = TryOneline
                             , cfgLayoutIf = TryOneline
                             , cfgLayoutImportSpecList = Flex
                             , cfgLayoutImportSpecInnerList = Flex
                             , cfgLayoutInfixApp = Flex
                             , cfgLayoutLet = TryOneline
                             , cfgLayoutListComp = TryOneline
                             , cfgLayoutList = Flex
                             , cfgLayoutRecord = TryOneline
                             , cfgLayoutPatternApp = TryOneline
                             , cfgLayoutPatternSynonym = Flex
                             , cfgLayoutConstraints = Flex
                             , cfgLayoutType = simpleWithinLayout TryOneline
                             , cfgLayoutTypeApp = Flex
                             , cfgLayoutUnboxedSum = Flex
                             }

    cfgOp =
        OpConfig ConfigMap { cfgMapDefault   = simpleWhitespace WsBoth WsBefore False
                           , cfgMapOverrides = Map.fromList opWsOverrides
                           }

    opWsOverrides =
        [ (ConfigMapKey (Just ",") Nothing Nothing, simpleWhitespace WsAfter WsBefore False)
        , ( ConfigMapKey (Just "record") Nothing Nothing
          , simpleWhitespace WsAfter WsNone False
          )
        , ( ConfigMapKey (Just ".") (Just Type) Nothing
          , simpleWhitespace WsAfter WsAfter False
          )
        , (ConfigMapKey (Just "=") Nothing Nothing, simpleWhitespace WsBoth WsAfter False)
        , (ConfigMapKey (Just "$") Nothing Nothing, simpleWhitespace WsBoth WsAfter False)
        , (ConfigMapKey (Just "@") Nothing Nothing, simpleWhitespace WsNone WsNone False)
        , ( ConfigMapKey (Just "->") (Just Expression) Nothing
          , simpleWhitespace WsBoth WsAfter False
          )
        , ( ConfigMapKey (Just "record") (Just Pattern) Nothing
          , simpleWhitespace WsNone WsNone False
          )
        ]

    cfgGroup =
        GroupConfig ConfigMap { cfgMapDefault   =
                                    simpleWhitespace WsBoth WsAfter False
                              , cfgMapOverrides = Map.fromList groupWsOverrides
                              }

    groupWsOverrides =
        [ (ConfigMapKey Nothing (Just Type) Nothing, simpleWhitespace WsNone WsAfter False)
        , ( ConfigMapKey Nothing (Just Pattern) Nothing
          , simpleWhitespace WsNone WsAfter False
          )
        , (ConfigMapKey (Just "$(") Nothing Nothing, simpleWhitespace WsNone WsNone False)
        , (ConfigMapKey (Just "[|") Nothing Nothing, simpleWhitespace WsNone WsNone False)
        , (ConfigMapKey (Just "[d|") Nothing Nothing, simpleWhitespace WsNone WsNone False)
        , (ConfigMapKey (Just "[p|") Nothing Nothing, simpleWhitespace WsNone WsNone False)
        , (ConfigMapKey (Just "[t|") Nothing Nothing, simpleWhitespace WsNone WsNone False)
        , (ConfigMapKey (Just "(") Nothing Nothing, simpleWhitespace WsNone WsAfter False)
        , ( ConfigMapKey (Just "(") (Just Other) Nothing
          , simpleWhitespace WsBoth WsAfter False
          )
        , ( ConfigMapKey (Just "[") (Just Pattern) Nothing
          , simpleWhitespace WsBoth WsAfter False
          )
        , ( ConfigMapKey (Just "[") (Just Type) Nothing
          , simpleWhitespace WsNone WsNone False
          )
        ]

    cfgOptions =
        OptionConfig { cfgOptionSortPragmas                   = True
                     , cfgOptionSplitLanguagePragmas          = True
                     , cfgOptionSortImports                   = SortImportsByPrefix
                     , cfgOptionSortImportLists               = True
                     , cfgOptionSortImportSpecInnerLists      = False
                     , cfgOptionImportCompactSpecialization  = (False, False)
                     , cfgOptionAlignSumTypeDecl              = False
                     , cfgOptionFlexibleOneline               = False
                     , cfgOptionPreserveVerticalSpace         = True
                     , cfgOptionMultiIfPadding                = True
                     , cfgOptionLetSpecialization             = False
                     , cfgOptionLetDoSpecialization           = False
                     , cfgOptionLetInPadding                  = (True, True)
                     , cfgOptionListCompSpecialization        = False
                     , cfgOptionCompactVerticalList           = Nothing
                     , cfgOptionSimpleTypeApp                 = False
                     , cfgOptionAltPadding                    = False
                     , cfgOptionDeclNoBlankLines              = Set.empty
                     }

gibianskyCfg :: Config
gibianskyCfg = defaultConfig { cfgAlign
                             , cfgIndent
                             , cfgLayout
                             , cfgOp
                             , cfgGroup
                             , cfgOptions
                             }
  where
    cfgAlign = AlignConfig { cfgAlignLimits           = (10, 25)
                           , cfgAlignCase             = True
                           , cfgAlignClass            = False
                           , cfgAlignDoLeftArrow      = False
                           , cfgAlignImportModule     = True
                           , cfgAlignImportSpec       = False
                           , cfgAlignImportAsMin      = Nothing
                           , cfgAlignLetBinds         = False
                           , cfgAlignMatches          = False
                           , cfgAlignModulePragmaEnds = False
                           , cfgAlignMultiIfRhs       = False
                           , cfgAlignRecordFields     = False
                           , cfgAlignWhere            = False
                           }

    cfgIndent =
        IndentConfig { cfgIndentOnside = 2
                     , cfgIndentDeriving = 2
                     , cfgIndentWhere = 2
                     , cfgIndentModuleWhere = Nothing
                     , cfgIndentApp = IndentBy 2
                     , cfgIndentCase = IndentBy 2
                     , cfgIndentClass = IndentBy 2
                     , cfgIndentDo = IndentBy 2
                     , cfgIndentIf = Align
                     , cfgIndentLet = Align
                     , cfgIndentLetBinds = Align
                     , cfgIndentLetIn = Align
                     , cfgIndentMultiIf = IndentBy 2
                     , cfgIndentPatternApp = IndentBy 2
                     , cfgIndentPatternsig = Align
                     , cfgIndentSimpleDeclaration = AlignOrIndentBy 4
                     , cfgIndentTypesig = Align
                     , cfgIndentTypeApp = Align
                     , cfgIndentWhereBinds = IndentBy 2
                     , cfgIndentExportSpecList = IndentBy 4
                     , cfgIndentExportSpecInnerList = Align
                     , cfgIndentImportSpecList = Align
                     , cfgIndentImportSpecInnerList = Align
                     }

    cfgLayout = LayoutConfig { cfgLayoutApp = TryOneline
                             , cfgLayoutConDecls = Vertical
                             , cfgLayoutDeclaration = Flex
                             , cfgLayoutExportSpecList = TryOneline
                             , cfgLayoutExportSpecInnerList = TryOneline
                             , cfgLayoutIf = Vertical
                             , cfgLayoutImportSpecList = Flex
                             , cfgLayoutImportSpecInnerList = Flex
                             , cfgLayoutInfixApp = TryOneline
                             , cfgLayoutLet = Vertical
                             , cfgLayoutListComp = TryOneline
                             , cfgLayoutList = Flex
                             , cfgLayoutRecord = TryOneline
                             , cfgLayoutPatternApp = TryOneline
                             , cfgLayoutPatternSynonym = Flex
                             , cfgLayoutConstraints = Flex
                             , cfgLayoutType = simpleWithinLayout TryOneline
                             , cfgLayoutTypeApp = Flex
                             , cfgLayoutUnboxedSum = TryOneline
                             }

    cfgOp =
        OpConfig ConfigMap { cfgMapDefault   = simpleWhitespace WsBoth WsBefore False
                           , cfgMapOverrides = Map.fromList opWsOverrides
                           }

    opWsOverrides =
        [ (ConfigMapKey (Just ",") Nothing Nothing, simpleWhitespace WsAfter WsBefore False)
        , ( ConfigMapKey (Just "record") Nothing Nothing
          , simpleWhitespace WsAfter WsNone False
          )
        , ( ConfigMapKey (Just ".") (Just Type) Nothing
          , simpleWhitespace WsAfter WsAfter False
          )
        , (ConfigMapKey (Just "=") Nothing Nothing, simpleWhitespace WsBoth WsAfter False)
        , (ConfigMapKey (Just ":") Nothing Nothing, simpleWhitespace WsNone WsBefore False)
        ]

    cfgGroup =
        GroupConfig ConfigMap { cfgMapDefault   =
                                    simpleWhitespace WsNone WsNone False
                              , cfgMapOverrides = Map.fromList groupWsOverrides
                              }

    groupWsOverrides =
        [ (ConfigMapKey (Just "{") Nothing Nothing, simpleWhitespace WsBoth WsAfter False) ]

    cfgOptions = OptionConfig { cfgOptionSortPragmas                    = False
                              , cfgOptionSplitLanguagePragmas           = False
                              , cfgOptionSortImports                    = NoImportSort
                              , cfgOptionSortImportLists                = False
                              , cfgOptionSortImportSpecInnerLists       = False
                              , cfgOptionImportCompactSpecialization    = (False, False)
                              , cfgOptionAlignSumTypeDecl               = False
                              , cfgOptionFlexibleOneline                = False
                              , cfgOptionPreserveVerticalSpace          = False
                              , cfgOptionMultiIfPadding                 = True
                              , cfgOptionLetSpecialization              = False
                              , cfgOptionLetDoSpecialization            = False
                              , cfgOptionLetInPadding                   = (True, True)
                              , cfgOptionListCompSpecialization         = False
                              , cfgOptionCompactVerticalList            = Nothing
                              , cfgOptionSimpleTypeApp                  = False
                              , cfgOptionAltPadding                     = False
                              , cfgOptionDeclNoBlankLines               = Set.empty
                              }

johanTibellCfg :: Config
johanTibellCfg =
    defaultConfig { cfgIndent, cfgLayout, cfgOp, cfgGroup, cfgOptions }
  where
    cfgIndent =
        IndentConfig { cfgIndentOnside = 4
                     , cfgIndentDeriving = 4
                     , cfgIndentWhere = 2
                     , cfgIndentModuleWhere = Nothing
                     , cfgIndentApp = IndentBy 4
                     , cfgIndentCase = IndentBy 4
                     , cfgIndentClass = IndentBy 4
                     , cfgIndentDo = IndentBy 4
                     , cfgIndentIf = IndentBy 4
                     , cfgIndentLet = Align
                     , cfgIndentLetBinds = Align
                     , cfgIndentLetIn = Align
                     , cfgIndentMultiIf = IndentBy 2
                     , cfgIndentPatternApp = IndentBy 4
                     , cfgIndentPatternsig = Align
                     , cfgIndentSimpleDeclaration = AlignOrIndentBy 4
                     , cfgIndentTypesig = Align
                     , cfgIndentTypeApp = Align
                     , cfgIndentWhereBinds = IndentBy 2
                     , cfgIndentExportSpecList = IndentBy 2
                     , cfgIndentExportSpecInnerList = Align
                     , cfgIndentImportSpecList = AlignOrIndentBy 7
                     , cfgIndentImportSpecInnerList = Align
                     }

    cfgLayout = LayoutConfig { cfgLayoutApp = TryOneline
                             , cfgLayoutConDecls = Vertical
                             , cfgLayoutDeclaration = TryOneline
                             , cfgLayoutExportSpecList = TryOneline
                             , cfgLayoutExportSpecInnerList = TryOneline
                             , cfgLayoutIf = Vertical
                             , cfgLayoutImportSpecList = Flex
                             , cfgLayoutImportSpecInnerList = Flex
                             , cfgLayoutInfixApp = TryOneline
                             , cfgLayoutLet = Vertical
                             , cfgLayoutListComp = Flex
                             , cfgLayoutList = Flex
                             , cfgLayoutRecord = Vertical
                             , cfgLayoutPatternApp = TryOneline
                             , cfgLayoutPatternSynonym = TryOneline
                             , cfgLayoutConstraints = Flex
                             , cfgLayoutType = simpleWithinLayout TryOneline
                             , cfgLayoutTypeApp = Flex
                             , cfgLayoutUnboxedSum = Flex
                             }

    cfgOp =
        OpConfig ConfigMap { cfgMapDefault   = simpleWhitespace WsBoth WsBefore False
                           , cfgMapOverrides = Map.fromList opWsOverrides
                           }

    opWsOverrides =
        [ (ConfigMapKey (Just ",") Nothing Nothing, simpleWhitespace WsAfter WsBefore False)
        , ( ConfigMapKey (Just "record") Nothing Nothing
          , simpleWhitespace WsAfter WsAfter True
          )
        , ( ConfigMapKey (Just ".") (Just Type) Nothing
          , simpleWhitespace WsAfter WsAfter False
          )
        , (ConfigMapKey (Just "=") Nothing Nothing, simpleWhitespace WsBoth WsAfter False)
        , ( ConfigMapKey (Just ":") (Just Pattern) Nothing
          , simpleWhitespace WsNone WsBefore False
          )
        , ( ConfigMapKey (Just ",") (Just Pattern) Nothing
          , simpleWhitespace WsNone WsBefore False
          )
        , ( ConfigMapKey (Just ",") (Just Other) Nothing
          , simpleWhitespace WsNone WsBefore False
          )
        , ( ConfigMapKey (Just "record") (Just Pattern) Nothing
          , simpleWhitespace WsAfter WsAfter False
          )
        ]

    cfgGroup =
        GroupConfig ConfigMap { cfgMapDefault   =
                                    simpleWhitespace WsNone WsNone False
                              , cfgMapOverrides = Map.fromList groupWsOverrides
                              }

    groupWsOverrides =
        [ (ConfigMapKey (Just "{") Nothing Nothing, simpleWhitespace WsBoth WsAfter False)
        , ( ConfigMapKey (Just "{") (Just Pattern) Nothing
          , simpleWhitespace WsNone WsNone False
          )
        ]

    cfgOptions = OptionConfig { cfgOptionSortPragmas                    = False
                              , cfgOptionSplitLanguagePragmas           = False
                              , cfgOptionSortImports                    = NoImportSort
                              , cfgOptionSortImportLists                = False
                              , cfgOptionSortImportSpecInnerLists       = False
                              , cfgOptionImportCompactSpecialization    = (False, False)
                              , cfgOptionAlignSumTypeDecl               = True
                              , cfgOptionFlexibleOneline                = True
                              , cfgOptionPreserveVerticalSpace          = False
                              , cfgOptionMultiIfPadding                 = True
                              , cfgOptionLetSpecialization              = False
                              , cfgOptionLetDoSpecialization            = False
                              , cfgOptionLetInPadding                   = (True, True)
                              , cfgOptionListCompSpecialization         = False
                              , cfgOptionCompactVerticalList            = Nothing
                              , cfgOptionSimpleTypeApp                  = False
                              , cfgOptionAltPadding                     = False
                              , cfgOptionDeclNoBlankLines               = Set.empty
                              }

pureCITICfg :: Config
pureCITICfg =
    defaultConfig { cfgAlign
                  , cfgIndent
                  , cfgLayout
                  , cfgOp
                  , cfgGroup
                  , cfgOptions
                  , cfgPenalty
                  }
  where
    cfgAlign = AlignConfig { cfgAlignLimits           = (60, 75)
                           , cfgAlignCase             = True
                           , cfgAlignClass            = True
                           , cfgAlignDoLeftArrow      = True
                           , cfgAlignImportModule     = True
                           , cfgAlignImportSpec       = True
                           , cfgAlignImportAsMin      = Just 50
                           , cfgAlignLetBinds         = True
                           , cfgAlignMatches          = True
                           , cfgAlignModulePragmaEnds = True
                           , cfgAlignMultiIfRhs       = True
                           , cfgAlignRecordFields     = True
                           , cfgAlignWhere            = True
                           }

    cfgIndent =
        IndentConfig { cfgIndentOnside = 2
                     , cfgIndentDeriving = 2
                     , cfgIndentWhere = 2
                     , cfgIndentModuleWhere = Just 2
                     , cfgIndentApp = IndentBy 0
                     , cfgIndentCase = IndentBy 0
                     , cfgIndentClass = IndentBy 2
                     , cfgIndentDo = IndentBy 0
                     , cfgIndentIf = IndentBy 0
                     , cfgIndentLet = Align
                     , cfgIndentLetBinds = IndentBy 2
                     , cfgIndentLetIn = IndentBy 2
                     , cfgIndentMultiIf = IndentBy 0
                     , cfgIndentPatternApp = IndentBy 0
                     , cfgIndentPatternsig = Align
                     , cfgIndentSimpleDeclaration = Align
                     , cfgIndentTypesig = Align
                     , cfgIndentTypeApp = Align
                     , cfgIndentWhereBinds = IndentBy 2
                     , cfgIndentExportSpecList = IndentBy 2
                     , cfgIndentExportSpecInnerList = IndentBy (-2)
                     , cfgIndentImportSpecList = IndentBy 2
                     , cfgIndentImportSpecInnerList = IndentBy (-2)
                     }

    cfgLayout = LayoutConfig { cfgLayoutApp = Flex -- Vertical -- TryOneline
                             , cfgLayoutConDecls = Vertical
                             , cfgLayoutDeclaration = Vertical
                             , cfgLayoutExportSpecList = Vertical
                             , cfgLayoutExportSpecInnerList = Vertical
                             , cfgLayoutIf = Vertical
                             , cfgLayoutImportSpecList = Vertical
                             , cfgLayoutImportSpecInnerList = Vertical
                             , cfgLayoutInfixApp = Flex
                             , cfgLayoutLet = Vertical
                             , cfgLayoutListComp = Vertical
                             , cfgLayoutList = Vertical
                             , cfgLayoutRecord = Vertical
                             , cfgLayoutPatternApp = TryOneline
                             , cfgLayoutPatternSynonym = Flex
                             , cfgLayoutConstraints = Vertical
                             , cfgLayoutType = WithinLayout { wlModuleLayout = Vertical
                                                            , wlRecordLayout = TryOneline
                                                            , wlGADTLayout = TryOneline
                                                            , wlGADTFieldLayout = Vertical
                                                            , wlGADTFieldTypeLayout = Vertical
                                                            , wlTypeLayout = Vertical
                                                            , wlComprehensionLayout = TryOneline
                                                            , wlSpecialLayout = TryOneline
                                                            , wlPatternLayout = TryOneline
                                                            , wlGuardLayout = TryOneline
                                                            , wlExportLayout = TryOneline
                                                            , wlClassLayout = TryOneline
                                                            , wlCaseLayout = TryOneline
                                                            , wlOtherLayout = TryOneline
                                                            }
                             , cfgLayoutTypeApp = TryOneline
                             , cfgLayoutUnboxedSum = Vertical
                             }

    cfgOp =
        OpConfig ConfigMap { cfgMapDefault   = simpleWhitespace WsBoth WsBefore False
                           , cfgMapOverrides = Map.fromList opWsOverrides
                           }

    opWsOverrides =
        [ ( ConfigMapKey (Just ",") Nothing Nothing
          , simpleWhitespace WsAfter WsBefore False
          )
        , ( ConfigMapKey (Just "=") Nothing Nothing
          , simpleWhitespace WsBoth WsBefore False
          )
        , ( ConfigMapKey (Just "@") (Just Pattern) Nothing
          , simpleWhitespace WsNone WsNone False
          )
        , ( ConfigMapKey (Just "default") Nothing Nothing
          , simpleWhitespace WsBoth WsBefore False
          )
        , ( ConfigMapKey (Just "->") (Just Other) (Just TypeDeclaration)
          , simpleWhitespace WsBoth WsNone False
          )
        , ( ConfigMapKey (Just "->") (Just Type) (Just OtherDeclaration)
          , simpleWhitespace WsAfter WsBefore False
          )
        , ( ConfigMapKey (Just "=") (Just Declaration) (Just OtherDeclaration)
          , simpleWhitespace WsAfter WsBefore False
          )
        -- Function signatures
        , ( ConfigMapKey (Just "type-fun-outer") (Just Type) (Just OtherDeclaration)
          , simpleWhitespace WsAfter WsBefore True
          )
        , ( ConfigMapKey (Just "type-fun-inner") (Just Type) (Just OtherDeclaration)
          , simpleWhitespace WsBoth WsNone False
          )
        , ( ConfigMapKey (Just "=>") (Just Type) (Just OtherDeclaration)
          , simpleWhitespace WsBoth WsNone False
          )
        --- Inner forall
        , ( ConfigMapKey (Just ".") (Just Type) (Just OtherDeclaration)
          , simpleWhitespace WsAfter WsNone False
          )
        , ( ConfigMapKey (Just "type-fun-outer") (Just Type) (Just TypeDeclaration)
          , simpleWhitespace WsAfter WsBefore True
          )
        , ( ConfigMapKey (Just "type-fun-inner") (Just Type) (Just TypeDeclaration)
          , simpleWhitespace WsBoth WsNone False
          )
        , ( ConfigMapKey (Just "=>") (Just Type) (Just TypeDeclaration)
          , simpleWhitespace WsAfter WsBefore False
          )
        --- Outer forall
        , ( ConfigMapKey (Just ".") (Just Type) (Just TypeDeclaration)
          , Whitespace WsBefore WsBefore False (Just WsAfter)
          )
        -- Patern Synonmyns
        , ( ConfigMapKey (Just "<-") (Just Declaration) (Just PatternDeclaration)
          , simpleWhitespace WsBoth WsNone False
          )
        , ( ConfigMapKey (Just "=") (Just Declaration) (Just PatternDeclaration)
          , simpleWhitespace WsAfter WsBefore True
          )
        -- Specialize special comma sig
        , ( ConfigMapKey (Just "specialize_comma") (Just Declaration) Nothing
          , simpleWhitespace WsAfter WsBefore True
          )
        -- Format simple data adts correctly
        , ( ConfigMapKey (Just "|") (Just Declaration) (Just RecordDeclaration)
          , simpleWhitespace WsAfter WsBefore True
          )
        -- Format let in with sane defaults
        , ( ConfigMapKey (Just "let") (Just Expression) Nothing
          , simpleWhitespace WsNone WsAfter True
          )
        , ( ConfigMapKey (Just "in") (Just Expression) Nothing
          , simpleWhitespace WsNone WsBoth True
          )
        , ( ConfigMapKey (Just "in") (Just Expression) (Just SpecialDeclaration)
          , simpleWhitespace WsAfter WsBefore True
          )
        -- Format case of with | and -> guards correctly
        , ( ConfigMapKey (Just "|") (Just Pattern) Nothing
          , simpleWhitespace WsBoth WsNone False
          )
        , ( ConfigMapKey (Just "->") (Just Expression) Nothing
          , simpleWhitespace WsBoth WsNone False
          )
        --- Pattern Guard Commas
        , ( ConfigMapKey (Just ",") (Just Pattern) (Just GuardDeclaration)
          , Whitespace WsAfter WsBefore True (Just WsBoth)
          )
        -- Helps "lists" not have too much padding in pattern matches
        , ( ConfigMapKey (Just ":") (Just Pattern) Nothing
          , simpleWhitespace WsNone WsNone False
          )
        -- Format simple data adts correctly
        , ( ConfigMapKey (Just "|") (Just Expression) (Just ComprehensionDeclaration)
          , simpleWhitespace WsAfter WsBefore True
          )
        , ( ConfigMapKey (Just ",") (Just Expression) (Just ComprehensionDeclaration)
          , simpleWhitespace WsAfter WsBefore True
          )
        -- Record in pattern match
        , ( ConfigMapKey (Just "record") (Just Pattern) Nothing
          , simpleWhitespace WsAfter WsNone False
          )
        , ( ConfigMapKey (Just ".") (Just Declaration) Nothing
          , simpleWhitespace WsBoth WsNone False
          )
        , ( ConfigMapKey (Just "::") (Just Declaration) (Just TypeDeclaration)
          , simpleWhitespace WsAfter WsBefore True
          )
        , ( ConfigMapKey (Just "<>") (Just Expression) Nothing
          , simpleWhitespace WsAfter WsBefore True
          )
        , ( ConfigMapKey (Just "$") (Just Expression) Nothing
          , simpleWhitespace WsAfter WsBefore True
          )
        , ( ConfigMapKey (Just "record") Nothing Nothing
          , simpleWhitespace WsAfter WsNone False
          )
        , ( ConfigMapKey (Just "deriving") (Just Other) Nothing
          , simpleWhitespace WsNone WsAfter True
          )
        , ( ConfigMapKey (Just ",") (Just Other) (Just TypeDeclaration)
          , simpleWhitespace WsAfter WsBefore True
          )
        , ( ConfigMapKey (Just ",") (Just Other) (Just RecordDeclaration)
          , simpleWhitespace WsAfter WsBefore True
          )
        -- Record declaration
        , ( ConfigMapKey (Just "record") (Just Declaration) Nothing
          , simpleWhitespace WsNone WsAfter False
          )
        , ( ConfigMapKey (Just "=") (Just Declaration) (Just RecordDeclaration)
          , simpleWhitespace WsAfter WsBefore False
          )
        , ( ConfigMapKey (Just "::") (Just Declaration) (Just RecordDeclaration)
          , simpleWhitespace WsBoth WsNone False
          )
        , ( ConfigMapKey (Just ".") (Just Type) (Just RecordDeclaration)
          , Whitespace WsAfter WsBefore False (Just WsAfter)
          )
        -- Record expression
        , ( ConfigMapKey (Just "record") (Just Expression) Nothing
          , simpleWhitespace WsNone WsAfter False
          )
        , ( ConfigMapKey (Just "=") (Just Expression) (Just RecordDeclaration)
          , simpleWhitespace WsBoth WsBefore False
          )
        -- Lists in expressions
        , ( ConfigMapKey (Just ",") (Just Expression) Nothing
          , simpleWhitespace WsAfter WsBefore False
          )
        -- Hang module where
        , ( ConfigMapKey (Just "module_where") (Just Declaration) Nothing
          , simpleWhitespace WsNone WsBefore True
          )
        -- Module lists
        , ( ConfigMapKey (Just ",") (Just Other) (Just ModuleDeclaration)
          , simpleWhitespace WsAfter WsBefore True
          )
        -- = in simple declarations
        , ( ConfigMapKey (Just "=") (Just Declaration) Nothing
          , simpleWhitespace WsAfter WsBefore True
          )
        -- GADT fun
        , ( ConfigMapKey (Just "::") (Just Declaration) (Just GADTDeclaration)
          , simpleWhitespace WsBoth WsNone False
          )
        , ( ConfigMapKey (Just "::") (Just Declaration) (Just GADTFieldDeclaration)
          , simpleWhitespace WsAfter WsBefore True
          )
        , ( ConfigMapKey (Just "->") (Just Type) (Just GADTFieldDeclaration)
          , simpleWhitespace WsAfter WsNone False
          )
        --- GADT Deriving bullshit
        , ( ConfigMapKey (Just ",") (Just Other) (Just GADTDeclaration)
          , simpleWhitespace WsAfter WsBefore True
          )
        -- GADT Arrow at end
        , ( ConfigMapKey (Just "type-fun-outer") (Just Type) (Just GADTFieldTypeDeclaration)
          , simpleWhitespace WsAfter WsBefore True
          )
        , ( ConfigMapKey (Just "type-fun-inner") (Just Type) (Just GADTFieldTypeDeclaration)
          , simpleWhitespace WsBoth WsNone False
          )
        --  record ->
        , ( ConfigMapKey (Just "->") (Just Type) (Just RecordDeclaration)
          , simpleWhitespace WsBoth WsNone False
          )
        --  Unboxed sums
        , ( ConfigMapKey (Just "unboxed-alt") (Just Expression) Nothing
          , simpleWhitespace WsBefore WsBefore False
          )
        , ( ConfigMapKey (Just "unboxed-alt-present") (Just Expression) Nothing
          , simpleWhitespace WsBoth WsBefore False
          )
        -- In | guards
        , ( ConfigMapKey (Just "=") (Just Declaration) (Just GuardDeclaration)
          , simpleWhitespace WsBoth WsNone False
          )
        -- Export lists
        , ( ConfigMapKey (Just ",") (Just Other) (Just ExportDeclaration)
          , simpleWhitespace WsBoth WsBefore True
          )
        -- Class and instances
        , ( ConfigMapKey (Just "=>") (Just Type) (Just ClassDeclaration)
          , simpleWhitespace WsBoth WsBefore False
          )
        , ( ConfigMapKey (Just "no-ctx-instance") (Just Declaration) (Just ClassDeclaration)
          , simpleWhitespace WsAfter WsNone False
          )
        , ( ConfigMapKey (Just "instance") (Just Declaration) (Just ClassDeclaration)
          , simpleWhitespace WsNone WsAfter True
          )
        , ( ConfigMapKey (Just "1-ctx-instance") (Just Declaration) (Just ClassDeclaration)
          , simpleWhitespace WsAfter WsNone False
          )
        , ( ConfigMapKey (Just "0-ctx-instance") (Just Declaration) (Just ClassDeclaration)
          , simpleWhitespace WsAfter WsNone False
          )
        , ( ConfigMapKey (Just "no-ctx-class") (Just Declaration) (Just ClassDeclaration)
          , simpleWhitespace WsAfter WsNone False
          )
        , ( ConfigMapKey (Just "class") (Just Declaration) (Just ClassDeclaration)
          , simpleWhitespace WsNone WsAfter True
          )
        , ( ConfigMapKey (Just "1-ctx-class") (Just Declaration) (Just ClassDeclaration)
          , simpleWhitespace WsAfter WsNone False
          )
        , ( ConfigMapKey (Just "0-ctx-class") (Just Declaration) (Just ClassDeclaration)
          , simpleWhitespace WsAfter WsNone False
          )
        , ( ConfigMapKey (Just ",") (Just Type) (Just ClassDeclaration)
          , simpleWhitespace WsAfter WsBefore True
          )
        , ( ConfigMapKey (Just "where") (Just Declaration) (Just ClassDeclaration)
          , simpleWhitespace WsBefore WsAfter True
          )
        -- If operator bull
        , ( ConfigMapKey (Just "if") (Just Expression) Nothing
          , Whitespace WsAfter WsAfter False Nothing
          )
        , ( ConfigMapKey (Just "then") (Just Expression) Nothing
          , Whitespace WsAfter WsAfter False Nothing
          )
        , ( ConfigMapKey (Just "else") (Just Expression) Nothing
          , Whitespace WsAfter WsAfter False Nothing
          )
        -- Applicative
        , ( ConfigMapKey (Just "<$>") (Just Expression) Nothing
          , Whitespace WsAfter WsBefore True (Just WsBoth)
          )
        , ( ConfigMapKey (Just "<*>") (Just Expression) Nothing
          , Whitespace WsAfter WsBefore True (Just WsBoth)
          )
        ]

    cfgGroup =
        GroupConfig ConfigMap { cfgMapDefault   =
                                    simpleWhitespace WsBoth WsAfter False
                              , cfgMapOverrides = Map.fromList groupWsOverrides
                              }

    groupWsOverrides =
        [ ( ConfigMapKey (Just "$(") Nothing Nothing
          , simpleWhitespace WsNone WsNone False
          )
        , ( ConfigMapKey (Just "(#") (Just Pattern) Nothing
          , simpleWhitespace WsBoth WsAfter False
          )
        , ( ConfigMapKey (Just "[p|") (Just Pattern) Nothing
          , simpleWhitespace WsNone WsNone False
          )
        , ( ConfigMapKey (Just "default") Nothing Nothing
          , simpleWhitespace WsBoth WsAfter False
          )
        , ( ConfigMapKey (Just "(") (Just Other) Nothing
          , simpleWhitespace WsBoth WsAfter True
          )
        , ( ConfigMapKey (Just "(") (Just Other) (Just RecordDeclaration)
          , simpleWhitespace WsBoth WsAfter True
          )
        , ( ConfigMapKey (Just "(") (Just Type) (Just TypeDeclaration)
          , simpleWhitespace WsNone WsAfter True
          )
        , ( ConfigMapKey (Just "(#") (Just Expression) Nothing
          , simpleWhitespace WsBoth WsAfter False
          )
        , ( ConfigMapKey (Just "[|") Nothing Nothing
          , simpleWhitespace WsNone WsNone False
          )
        , ( ConfigMapKey (Just "*") (Just Type) Nothing
          , simpleWhitespace WsNone WsAfter False
          )
        , ( ConfigMapKey (Just "*") (Just Pattern) Nothing
          , simpleWhitespace WsNone WsAfter False
          )
        , ( ConfigMapKey (Just "(") Nothing Nothing
          , simpleWhitespace WsNone WsAfter False
          )
        , ( ConfigMapKey (Just "[d|") Nothing Nothing
          , simpleWhitespace WsNone WsNone False
          )
        , ( ConfigMapKey (Just "[t|") Nothing Nothing
          , simpleWhitespace WsNone WsNone False
          )
        , ( ConfigMapKey (Just "{") (Just Declaration) Nothing
          , simpleWhitespace WsBefore WsNone False
          )
        -- Record definition
        , ( ConfigMapKey (Just "{") (Just Declaration) (Just RecordDeclaration)
          , simpleWhitespace WsBefore WsAfter True
          )
        -- Record expression
        , ( ConfigMapKey (Just "{") (Just Expression) (Just RecordDeclaration)
          , simpleWhitespace WsBefore WsAfter True
          )
        -- Pattern splice in expresion
        , ( ConfigMapKey (Just "[p|") (Just Expression) Nothing
          , simpleWhitespace WsNone WsNone False
          )
        -- Helps lists not have too much padding in pattern matches
        , ( ConfigMapKey (Just "[") (Just Pattern) Nothing
          , simpleWhitespace WsNone WsNone False
          )
        -- Lists in expressions
        , ( ConfigMapKey (Just "[") (Just Expression) Nothing
          , simpleWhitespace WsBefore WsAfter False
          )
        --- Special compact list for 0 and 1
        , ( ConfigMapKey (Just "compact-list") (Just Expression) Nothing
          , simpleWhitespace WsNone WsNone False
          )
        -- Module lists
        , ( ConfigMapKey (Just "(") (Just Other) (Just ModuleDeclaration)
          , simpleWhitespace WsBefore WsAfter True
          )
        -- Lists in function signatures?
        , ( ConfigMapKey (Just "[") (Just Type) (Just OtherDeclaration)
          , simpleWhitespace WsNone WsNone False
          )
        -- Lists in type declarations (really type families)
        , ( ConfigMapKey (Just "[") (Just Type) (Just TypeDeclaration)
          , simpleWhitespace WsNone WsNone False
          )
        -- Enum and Pararray
        , ( ConfigMapKey (Just "enum_bracket") (Just Expression) Nothing
          , simpleWhitespace WsNone WsNone False
          )
        , ( ConfigMapKey (Just "parray_bracket") (Just Expression) Nothing
          , simpleWhitespace WsNone WsNone False
          )
        -- Imports
        , ( ConfigMapKey (Just "import-all-cons") (Just Other) Nothing
          , simpleWhitespace WsAfter WsNone False
          )
        , ( ConfigMapKey (Just "import-all-parens") (Just Other) Nothing
          , simpleWhitespace WsNone WsNone False
          )
        --- compact import
        , ( ConfigMapKey (Just "imp-compact-(") (Just Other) Nothing
          , simpleWhitespace WsNone WsNone False
          )
        -- Imports
        , ( ConfigMapKey (Just "export-all-cons") (Just Other) Nothing
          , simpleWhitespace WsAfter WsNone False
          )
        , ( ConfigMapKey (Just "export-all-parens") (Just Other) Nothing
          , simpleWhitespace WsNone WsNone False
          )
        -- Export lists
        , ( ConfigMapKey (Just "(") (Just Other) (Just ExportDeclaration)
          , simpleWhitespace WsNone WsAfter True
          )
        -- Lists in record declarations
        , ( ConfigMapKey (Just "[") (Just Type) (Just RecordDeclaration)
          , simpleWhitespace WsNone WsNone False
          )
        -- Class ()
        , ( ConfigMapKey (Just "(") (Just Declaration) (Just ClassDeclaration)
          , simpleWhitespace WsAfter WsAfter True
          )
        , ( ConfigMapKey (Just "(") (Just Type) (Just ClassDeclaration)
          , simpleWhitespace WsBefore WsAfter True
          )
        ]

    cfgPenalty = PenaltyConfig { penaltyMaxLineLength = 80
                               , penaltyLinebreak     = 5
                               , penaltyIndent        = 1
                               , penaltyOverfull      = 10
                               , penaltyOverfullOnce  = 200
                               }
    cfgOptions =
        OptionConfig { cfgOptionSortPragmas                 = True
                     , cfgOptionSplitLanguagePragmas        = True
                     , cfgOptionSortImports                 = SortImportsByGroups
                        [ ImportsGroup [""] ImportsGroupSorted
                        , ImportsGroup ["CiriBot", "Network.Danibot", "KafkaConsumer", "MammonEx", "PrometheusSidecar", "PureLib"] ImportsGroupSorted
                        ]
                     , cfgOptionSortImportLists             = True
                     , cfgOptionSortImportSpecInnerLists    = True
                     , cfgOptionImportCompactSpecialization = (True, True)
                     , cfgOptionAlignSumTypeDecl            = True
                     , cfgOptionFlexibleOneline             = False
                     , cfgOptionPreserveVerticalSpace       = True
                     , cfgOptionMultiIfPadding              = False
                     , cfgOptionLetSpecialization           = True
                     , cfgOptionLetDoSpecialization         = True
                     , cfgOptionLetInPadding                = (False, False)
                     , cfgOptionListCompSpecialization      = True
                     , cfgOptionCompactVerticalList         = Just (True, True)
                     , cfgOptionSimpleTypeApp               = True
                     , cfgOptionAltPadding                  = True
                     , cfgOptionDeclNoBlankLines            = Set.empty
                     }

-- | Base style definition.
base :: Style
base = Style { styleName        = "base"
             , styleAuthor      = "Enno Cramer"
             , styleDescription = "Configurable formatting style"
             , styleConfig      = defaultConfig
             }

chrisDone :: Style
chrisDone = Style { styleName        = "chris-done"
                  , styleAuthor      = "Chris Done"
                  , styleDescription = "Chris Done's style"
                  , styleConfig      = chrisDoneCfg
                  }

cramer :: Style
cramer = Style { styleName        = "cramer"
               , styleAuthor      = "Enno Cramer"
               , styleDescription = "Enno Cramer's style"
               , styleConfig      = cramerCfg
               }

gibiansky :: Style
gibiansky = Style { styleName        = "gibiansky"
                  , styleAuthor      = "Andrew Gibiansky"
                  , styleDescription = "Andrew Gibiansky's style"
                  , styleConfig      = gibianskyCfg
                  }

johanTibell :: Style
johanTibell = Style { styleName        = "johan-tibell"
                    , styleAuthor      = "Johan Tibell"
                    , styleDescription = "Johan Tibell's style"
                    , styleConfig      = johanTibellCfg
                    }

pureCITI :: Style
pureCITI = Style { styleName        = "pure-citi"
                 , styleAuthor      = "Pure CITI"
                 , styleDescription = "Pure CITI's style"
                 , styleConfig      = pureCITICfg
                 }
-- | Styles list, useful for programmatically choosing.
styles :: [Style]
styles = [ base, chrisDone, johanTibell, gibiansky, cramer, pureCITI ]
