{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Floskell.Pretty ( Pretty(..), pretty ) where

import           Control.Applicative          ( (<|>) )
import           Control.Monad
                 ( forM_, guard, replicateM_, unless, void, when )
import           Control.Monad.State.Strict   ( get, gets, modify )

import           Data.Bool                    ( bool )
import           Data.ByteString              ( ByteString )
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BS8
import qualified Data.ByteString.Lazy         as BL
import           Data.Char                    ( ord )
import           Data.List                    ( groupBy, sortBy, sortOn )
import           Data.Maybe                   ( catMaybes, fromMaybe )
import qualified Data.Set                     as Set

import qualified Floskell.Buffer              as Buffer
import           Floskell.Config
import           Floskell.Imports
                 ( groupImports, sortImports, splitImports )
import           Floskell.Printers
import           Floskell.Types

import qualified Language.Haskell.Exts.Pretty as HSE
import           Language.Haskell.Exts.Syntax

-- | Like `span`, but comparing adjacent items.
run :: (a -> a -> Bool) -> [a] -> ([a], [a])
run _ [] = ([], [])
run _ [ x ] = ([ x ], [])
run eq (x : y : xs)
    | eq x y = let (ys, zs) = run eq (y : xs) in (x : ys, zs)
    | otherwise = ([ x ], y : xs)

-- | Like `groupBy`, but comparing adjacent items.
runs :: (a -> a -> Bool) -> [a] -> [[a]]
runs _ [] = []
runs eq xs = let (ys, zs) = run eq xs in ys : runs eq zs

stopImportModule :: TabStop
stopImportModule = TabStop "import-module"

stopImportSpec :: TabStop
stopImportSpec = TabStop "import-spec"

stopRecordField :: TabStop
stopRecordField = TabStop "record"

stopRhs :: TabStop
stopRhs = TabStop "rhs"

stopGuardedRhs :: TabStop
stopGuardedRhs = TabStop "guarded-rhs"

stopModulePragma :: TabStop
stopModulePragma = TabStop "module-pragma"

stopDoLeftArrow :: TabStop
stopDoLeftArrow = TabStop "do-left-arrow"

flattenApp :: Annotated ast
           => (ast NodeInfo -> Maybe (ast NodeInfo, ast NodeInfo))
           -> ast NodeInfo
           -> [ast NodeInfo]
flattenApp fn = go . amap (\info -> info { nodeInfoLeadingComments  = []
                                         , nodeInfoTrailingComments = []
                                         })
  where
    go x = case fn x of
        Just (lhs, rhs) -> let lhs' = go $ copyComments Before x lhs
                               rhs' = go $ copyComments After x rhs
                           in
                               lhs' ++ rhs'
        Nothing -> [ x ]

flattenInfix
    :: (Annotated ast1, Annotated ast2)
    => (ast1 NodeInfo -> Maybe (ast1 NodeInfo, ast2 NodeInfo, ast1 NodeInfo))
    -> ast1 NodeInfo
    -> (ast1 NodeInfo, [(ast2 NodeInfo, ast1 NodeInfo)])
flattenInfix fn = go . amap (\info -> info { nodeInfoLeadingComments  = []
                                           , nodeInfoTrailingComments = []
                                           })
  where
    go x = case fn x of
        Just (lhs, op, rhs) ->
            let (lhs', ops) = go $ copyComments Before x lhs
                (lhs'', ops') = go $ copyComments After x rhs
            in
                (lhs', ops ++ (op, lhs'') : ops')
        Nothing -> (x, [])

-- | Pretty printing prettyHSE using haskell-src-exts pretty printer
prettyHSE :: HSE.Pretty (ast NodeInfo) => ast NodeInfo -> Printer ()
prettyHSE ast = string $ HSE.prettyPrint ast

-- | Type class for pretty-printable types.
class Pretty ast where
    prettyPrint :: ast NodeInfo -> Printer ()
    default prettyPrint
        :: HSE.Pretty (ast NodeInfo) => ast NodeInfo -> Printer ()
    prettyPrint = prettyHSE

-- | Pretty print a syntax tree with annotated comments
pretty :: (Annotated ast, Pretty ast) => ast NodeInfo -> Printer ()
pretty ast = do
    printComments Before ast
    prettyPrint ast
    printComments After ast

prettyOnside :: (Annotated ast, Pretty ast) => ast NodeInfo -> Printer ()
prettyOnside ast = do
    eol <- gets psEolComment
    when eol newline
    nl <- gets psNewline
    if nl
        then do
            printComments Before ast
            onside $ prettyPrint ast
            printComments After ast
        else onside $ pretty ast

-- | Compare two AST nodes ignoring the annotation
compareAST
    :: (Functor ast, Ord (ast ())) => ast NodeInfo -> ast NodeInfo -> Ordering
compareAST a b = void a `compare` void b

-- | Return leading comments.
filterComments :: Annotated a => Location -> a NodeInfo -> [Comment]
filterComments l = filterComments' l . ann

filterComments' :: Location -> NodeInfo -> [Comment]
filterComments' Before = nodeInfoLeadingComments
filterComments' After = nodeInfoTrailingComments

-- | Copy comments from one AST node to another.
copyComments :: (Annotated ast1, Annotated ast2)
             => Location
             -> ast1 NodeInfo
             -> ast2 NodeInfo
             -> ast2 NodeInfo
copyComments Before from to =
    amap (\n ->
          n { nodeInfoLeadingComments = nodeInfoLeadingComments $ ann from })
         to
copyComments After from to =
    amap (\n ->
          n { nodeInfoTrailingComments = nodeInfoTrailingComments $ ann from })
         to

-- | Pretty print a comment.
printComment :: Int -> (Comment, SrcSpan) -> Printer ()
printComment correction (Comment{..}, nextSpan) = do
    col <- getNextColumn
    let padding = max 0 $ srcSpanStartColumn commentSpan + correction - col - 1
    case commentType of
        PreprocessorDirective -> do
            nl <- gets psNewline
            unless nl newline
            column 0 $ string commentText
            modify (\s -> s { psEolComment = True })
        InlineComment -> do
            write $ BS.replicate padding 32
            if srcSpanEndColumn commentSpan - 2 == srcSpanStartColumn commentSpan
              then do
                alCol <- getNextColumn
                write "{-"
                string $ case break ((==) 10 . ord) $ reverse commentText of
                           (pre, _:rest) | all ((==) 32 . ord) pre -> reverse rest
                           _                                       -> commentText
                newline
                column alCol $ write "-}"
              else do
                write "{-"
                string commentText
                write "-}"
            when (srcSpanEndLine commentSpan /= srcSpanStartLine nextSpan) $
                modify (\s -> s { psEolComment = True })
        LineComment -> do
            write $ BS.replicate padding 32
            write "--"
            string commentText
            modify (\s -> s { psEolComment = True })

-- | Print comments of a node.
printComments :: Annotated ast => Location -> ast NodeInfo -> Printer ()
printComments = printCommentsInternal True

-- | Print comments of a node, but do not force newline before leading comments.
printComments' :: Annotated ast => Location -> ast NodeInfo -> Printer ()
printComments' = printCommentsInternal False

printCommentsInternal
    :: Annotated ast => Bool -> Location -> ast NodeInfo -> Printer ()
printCommentsInternal nlBefore loc ast = unless (null comments) $ do
    let firstComment = head comments
    -- Preceeding comments must have a newline before them, but not break onside indent.
    nl <- gets psNewline
    onside' <- gets psOnside
    when nl $ modify $ \s -> s { psOnside = 0 }
    when (loc == Before && not nl && nlBefore) newline
    when (loc == After && not nl && notSameLine firstComment) newline

    col <- getNextColumn
    let correction = case loc of
            Before -> col - srcSpanStartColumn ssi + 1
            After -> col - srcSpanEndColumn ssi + 1
    forM_ (zip comments (tail (map commentSpan comments ++ [ssi]))) $ printComment correction

    -- Write newline before restoring onside indent.
    eol <- gets psEolComment
    when (loc == Before && eol && onside' > 0) newline
    when nl $ modify $ \s -> s { psOnside = onside' }
  where
    ssi = nodeSpan ast

    comments = filterComments loc ast

    notSameLine comment = srcSpanEndLine ssi
        < srcSpanStartLine (commentSpan comment)

-- | Return the configuration name of an operator
opName :: QOp a -> ByteString
opName op = case op of
    (QVarOp _ qname) -> opName' qname
    (QConOp _ qname) -> opName' qname

-- | Return the configuration name of an operator
opName' :: QName a -> ByteString
opName' (Qual _ _ name) = opName'' name
opName' (UnQual _ name) = opName'' name
opName' (Special _ (FunCon _)) = "->"
opName' (Special _ (Cons _)) = ":"
opName' (Special _ _) = ""

-- | Return the configuration name of an operator
opName'' :: Name a -> ByteString
opName'' (Ident _ _) = "``"
opName'' (Symbol _ str) = BS8.pack str

lineDelta :: Annotated ast => ast NodeInfo -> ast NodeInfo -> Int
lineDelta prev next = lineDelta' (ann prev) (ann next)

lineDelta' :: NodeInfo -> NodeInfo -> Int
lineDelta' prev next = nextLine - prevLine
  where
    prevLine = maximum (prevNodeLine : prevCommentLines)

    nextLine = minimum (nextNodeLine : nextCommentLines)

    prevNodeLine = srcSpanEndLine $ nodeInfoSpan prev

    nextNodeLine = srcSpanStartLine $ nodeInfoSpan next

    prevCommentLines = map (srcSpanEndLine . commentSpan) $
        filterComments' After prev

    nextCommentLines = map (srcSpanStartLine . commentSpan) $
        filterComments' Before next

linedFn :: Annotated ast
        => (ast NodeInfo -> Printer ())
        -> Bool
        -> [ast NodeInfo]
        -> Printer ()
linedFn fn ignore xs = do
    preserveP <- getOption cfgOptionPreserveVerticalSpace
    if preserveP && not ignore
        then case xs of
            x : xs' -> do
                cut $ fn x
                forM_ (zip xs xs') $ \(prev, cur) -> do
                    replicateM_ (min 2 (max 1 $ lineDelta prev cur)) newline
                    cut $ fn cur
            [] -> return ()
        else inter newline $ map (cut . fn) xs

lined :: (Annotated ast, Pretty ast) => [ast NodeInfo] -> Printer ()
lined = lined' False

linedOnside :: (Annotated ast, Pretty ast) => [ast NodeInfo] -> Printer ()
linedOnside = linedOnside' False

lined' :: (Annotated ast, Pretty ast) => Bool -> [ast NodeInfo] -> Printer ()
lined' = linedFn pretty

linedOnside' :: (Annotated ast, Pretty ast) => Bool -> [ast NodeInfo] -> Printer ()
linedOnside' = linedFn prettyOnside

listVOpLen :: LayoutContext -> ByteString -> Printer Int
listVOpLen ctx sep = do
    withinDeclaration <- gets psWithinDeclaration
    ws <- getConfig (cfgOpWs' ctx (Just withinDeclaration) sep . cfgOp)
    return $ if wsLinebreak After ws
             then 0
             else BS.length sep + if wsSpace After ws then 1 else 0

listVinternal :: (Annotated ast, Pretty ast)
              => LayoutContext
              -> ByteString
              -> [ast NodeInfo]
              -> Printer ()
listVinternal ctx sep xs = case xs of
    [] -> newline
    (x : xs') -> do
        nl <- gets psNewline
        col <- getNextColumn
        delta <- listVOpLen ctx sep
        let itemCol = if nl && length xs > 1 then col + delta else col
            sepCol = itemCol - delta
        column itemCol $ do
            printComments' Before x
            cut . onside $ prettyPrint x
            printComments After x
        -- `column sepCol` must not be within `column itemCol`, or the
        -- former can suppress onside for the latter.
        forM_ xs' $ \x' -> do
            column itemCol $ printComments Before x'
            column' sepCol $ operatorV ctx sep
            column itemCol $ cut . onside $ prettyPrint x'
            column itemCol $ printComments After x'

listH :: (Annotated ast, Pretty ast)
      => LayoutContext
      -> ByteString
      -> ByteString
      -> ByteString
      -> [ast NodeInfo]
      -> Printer ()
listH _ open close _ [] = do
    write open
    write close

listH ctx open close sep xs =
    groupH ctx open close . inter (operatorH ctx sep) $ map pretty xs

listV :: (Annotated ast, Pretty ast)
      => LayoutContext
      -> ByteString
      -> ByteString
      -> ByteString
      -> [ast NodeInfo]
      -> Printer ()
listV ctx open close sep xs = groupV ctx open close $ do
    withinDeclaration <- gets psWithinDeclaration
    ws <- getConfig (cfgOpWs' ctx (Just withinDeclaration) sep . cfgOp)
    ws' <- getConfig (cfgGroupWs' ctx (Just withinDeclaration) open . cfgGroup)
    unless (wsLinebreak Before ws' || wsSpace After ws' || wsLinebreak After ws
            || not (wsSpace After ws))
           space
    listVinternal ctx sep xs

listG :: (Annotated ast, Pretty ast)
      => LayoutContext
      -> ByteString
      -> ByteString
      -> ByteString
      -> [ast NodeInfo]
      -> Printer ()
listG ctx open close sep xs =
    group ctx open close . inter (operator ctx sep) $ map pretty xs

list :: (Annotated ast, Pretty ast)
     => LayoutContext
     -> ByteString
     -> ByteString
     -> ByteString
     -> [ast NodeInfo]
     -> Printer ()
list ctx open close sep xs = oneline hor <|> ver
  where
    hor = listH ctx open close sep xs

    ver = listV ctx open close sep xs

listH' :: (Annotated ast, Pretty ast)
       => LayoutContext
       -> ByteString
       -> [ast NodeInfo]
       -> Printer ()
listH' ctx sep = inter (operatorH ctx sep) . map pretty

listV' :: (Annotated ast, Pretty ast)
       => LayoutContext
       -> ByteString
       -> [ast NodeInfo]
       -> Printer ()
listV' ctx sep xs =
    if length xs > 1 then listVinternal ctx sep xs else mapM_ pretty xs

list' :: (Annotated ast, Pretty ast)
      => LayoutContext
      -> ByteString
      -> [ast NodeInfo]
      -> Printer ()
list' ctx sep xs = oneline hor <|> ver
  where
    hor = listH' ctx sep xs

    ver = listV' ctx sep xs

listAutoWrap :: (Annotated ast, Pretty ast)
             => LayoutContext
             -> ByteString
             -> ByteString
             -> ByteString
             -> [ast NodeInfo]
             -> Printer ()
listAutoWrap _ open close _ [] = do
    write open
    write close

listAutoWrap ctx open close sep xs =
    aligned . group ctx open close $ listAutoWrap' ctx sep xs

listAutoWrap' :: (Annotated ast, Pretty ast)
              => LayoutContext
              -> ByteString
              -> [ast NodeInfo]
              -> Printer ()
listAutoWrap' _ _ [] = return ()
listAutoWrap' ctx sep (x : xs) = aligned $ do
    withinDeclaration <- gets psWithinDeclaration
    ws <- getConfig (cfgOpWs' ctx (Just withinDeclaration) sep . cfgOp)
    let correction = if wsLinebreak After ws
                     then 0
                     else BS.length sep + if wsSpace After ws then 1 else 0
    col <- getNextColumn
    pretty x
    forM_ xs $ \x' -> do
        printComments Before x'
        cut $ do
            column (col - correction) $ operator ctx sep
            prettyPrint x'
            printComments After x'

measure :: Printer a -> Printer (Maybe Int)
measure p = do
    s <- get
    let s' = s { psBuffer = Buffer.empty, psEolComment = False }
    return $ case execPrinter (oneline p) s' of
        Nothing -> Nothing
        Just (_, s'') -> Just . (\x -> x - psIndentLevel s) . fromIntegral
            . BL.length . Buffer.toLazyByteString $ psBuffer s''

measure' :: Printer a -> Printer (Maybe [Int])
measure' p = fmap (: []) <$> measure p

measureMatch :: Match NodeInfo -> Printer (Maybe [Int])
measureMatch (Match _ name pats _ Nothing) = measure' (prettyApp name pats)
measureMatch (InfixMatch _ pat name pats _ Nothing) = measure' go
  where
    go = do
        pretty pat
        withOperatorFormatting Pattern
                               (opName'' name)
                               (prettyHSE $ VarOp noNodeInfo name)
                               id
        inter spaceOrNewline $ map pretty pats
measureMatch _ = return Nothing

measureDecl :: Decl NodeInfo -> Printer (Maybe [Int])
measureDecl (PatBind _ pat _ Nothing) = measure' (pretty pat)
measureDecl (FunBind _ matches) =
    fmap concat . sequence <$> traverse measureMatch matches
measureDecl _ = return Nothing

measureClassDecl :: ClassDecl NodeInfo -> Printer (Maybe [Int])
measureClassDecl (ClsDecl _ decl) = measureDecl decl
measureClassDecl _ = return Nothing

measureInstDecl :: InstDecl NodeInfo -> Printer (Maybe [Int])
measureInstDecl (InsDecl _ decl) = measureDecl decl
measureInstDecl _ = return Nothing

measureAlt :: Alt NodeInfo -> Printer (Maybe [Int])
measureAlt (Alt _ pat _ Nothing) = measure' (pretty pat)
measureAlt _ = return Nothing

measureGuardedRhs :: GuardedRhs NodeInfo -> Printer (Maybe [Int])
measureGuardedRhs (GuardedRhs _ [stmt] _) = measure' go
  where
    go =
      within GuardDeclaration $ do
        operatorSectionR Pattern "|" $ write "|"
        pretty stmt
measureGuardedRhs (GuardedRhs _ _ _) = do
    return Nothing

measurePragma :: ModulePragma NodeInfo -> Printer (Maybe [Int])
measurePragma p = (fmap . fmap . fmap $ (\x -> x - 4)) $ measure' go
  where
    go = pretty p

measureDoGenerators :: Stmt NodeInfo -> Printer (Maybe [Int])
measureDoGenerators (Generator _ pat _) = measure' go
  where
    go = pretty pat
measureDoGenerators _ = return Nothing

withComputedTabStop :: TabStop
                    -> (AlignConfig -> Bool)
                    -> (a -> Printer (Maybe [Int]))
                    -> [a]
                    -> Printer b
                    -> Printer b
withComputedTabStop =
    withComputedTabStop' sequence

withComputedTabStopEager :: TabStop
                         -> (AlignConfig -> Bool)
                         -> (a -> Printer (Maybe [Int]))
                         -> [a]
                         -> Printer b
                         -> Printer b
withComputedTabStopEager =
    withComputedTabStop' (go . catMaybes)
  where
    go [] = Nothing
    go xs = Just xs

withComputedTabStop' :: ([Maybe [Int]] -> Maybe [[Int]])
                     -> TabStop
                     -> (AlignConfig -> Bool)
                     -> (a -> Printer (Maybe [Int]))
                     -> [a]
                     -> Printer b
                     -> Printer b
withComputedTabStop' coallesce name predicate fn xs p = do
    enabled <- getConfig (predicate . cfgAlign)
    (limAbs, limRel) <- getConfig (cfgAlignLimits . cfgAlign)
    mtabss <- coallesce <$> traverse fn xs
    let tab = do
            tabss <- mtabss
            let tabs = concat tabss
                maxtab = maximum tabs
                mintab = minimum tabs
                delta = maxtab - mintab
                diff = delta * 100 `div` maxtab
            guard enabled
            guard $ delta <= limAbs || diff <= limRel
            return maxtab
    withTabStops [ (name, tab) ] p

------------------------------------------------------------------------
-- Module
-- | Extract the name as a String from a ModuleName
moduleName :: ModuleName a -> String
moduleName (ModuleName _ s) = s

prettyPragmas :: [ModulePragma NodeInfo] -> Printer ()
prettyPragmas ps = do
    -- TODO theres some oddity in preserveVerticalSpace + lang pragmas
    -- seems to use their original locations after sorting, and therefore "sees" whitespace
    -- where None should be.
    -- Need a way to "attach" the empty lines to the Pragma, maybe with a newtype...
    splitP <- getOption cfgOptionSplitLanguagePragmas
    sortP <- getOption cfgOptionSortPragmas
    let ps' = if splitP then concatMap splitPragma ps else ps
    let ps'' = if sortP then sortBy compareAST ps' else ps'
    withComputedTabStop stopModulePragma cfgAlignModulePragmaEnds measurePragma ps $
        inter blankline . map (lined' True) $ groupBy sameType ps''
  where
    splitPragma (LanguagePragma anno langs) =
        map (LanguagePragma anno . (: [])) langs
    splitPragma p = [ p ]

    sameType LanguagePragma{} LanguagePragma{} = True
    sameType OptionsPragma{} OptionsPragma{} = True
    sameType AnnModulePragma{} AnnModulePragma{} = True
    sameType _ _ = False

prettyImports :: [ImportDecl NodeInfo] -> Printer ()
prettyImports is = do
    sortP <- getOption cfgOptionSortImports
    alignModuleP <- getConfig (cfgAlignImportModule . cfgAlign)
    alignSpecP <- getConfig (cfgAlignImportSpec . cfgAlign)
    alignAsMinP <- getConfig (cfgAlignImportAsMin . cfgAlign)
    let lenPreamble :: ImportDecl NodeInfo -> Int
        lenPreamble ImportDecl{..} = 6
                                   + (if importQualified then 9 + 1 else 0)
                                   + (if importSrc then 14 + 1 else 0)
                                   + (if importSafe then 4 + 1 else 0)
        maxPreamble = maximum $ map lenPreamble is
        maxNameLength = maximum $ map (length . moduleName . importModule) is
        alignModule = if alignModuleP then Just maxPreamble else Nothing
        alignSpec = if alignSpecP
                    then Just $ maximum [ maybe 0 (flip (-) 2) alignAsMinP
                                        , fromMaybe 0 alignModule + 1 + maxNameLength
                                        ]
                    else Nothing
    withTabStops [ (stopImportModule, alignModule)
                 , (stopImportSpec, alignSpec)
                 ] $ case sortP of
        NoImportSort -> lined is
        SortImportsByPrefix ->
          prettyGroups . groupImports 0 $ sortImports is
        SortImportsByGroups groups ->
          prettyGroups $ splitImports groups is
  where
    prettyGroups = inter blankline . map (inter newline . map (cut . pretty))

skipBlank :: Annotated ast
          => (ast NodeInfo -> ast NodeInfo -> Bool)
          -> ast NodeInfo
          -> ast NodeInfo
          -> Bool
skipBlank skip a b = skip a b && null (filterComments After a)
    && null (filterComments Before b)

skipBlankAfterDecl :: Decl a -> Bool
skipBlankAfterDecl a = case a of
    TypeSig{} -> True
    DeprPragmaDecl{} -> True
    WarnPragmaDecl{} -> True
    AnnPragma{} -> True
    MinimalPragma{} -> True
    InlineSig{} -> True
    InlineConlikeSig{} -> True
    SpecSig{} -> True
    SpecInlineSig{} -> True
    InstSig{} -> True
    PatSynSig{} -> True
    _ -> False

skipBlankDecl :: Decl NodeInfo -> Decl NodeInfo -> Bool
skipBlankDecl = skipBlank $ \a _ -> skipBlankAfterDecl a

skipBlankClassDecl :: ClassDecl NodeInfo -> ClassDecl NodeInfo -> Bool
skipBlankClassDecl = skipBlank $ \a _ -> case a of
    (ClsDecl _ decl) -> skipBlankAfterDecl decl
    ClsTyDef{} -> True
    ClsDefSig{} -> True
    _ -> False

skipBlankInstDecl :: InstDecl NodeInfo -> InstDecl NodeInfo -> Bool
skipBlankInstDecl = skipBlank $ \a _ -> case a of
    (InsDecl _ decl) -> skipBlankAfterDecl decl
    _ -> False

prettyDecls :: (Annotated ast, Pretty ast)
            => (ast NodeInfo -> ast NodeInfo -> Bool)
            -> DeclarationConstruct
            -> [ast NodeInfo]
            -> Printer ()
prettyDecls fn dc = inter sep . map lined . runs fn
  where
    sep = bool blankline newline . Set.member dc
        =<< getOption cfgOptionDeclNoBlankLines

prettySimpleDecl :: (Annotated ast1, Pretty ast1, Annotated ast2, Pretty ast2)
                 => ast1 NodeInfo
                 -> ByteString
                 -> ast2 NodeInfo
                 -> Printer ()
prettySimpleDecl = prettySimpleDecl' Nothing Nothing

prettySimpleDecl' :: (Annotated ast1, Pretty ast1, Annotated ast2, Pretty ast2)
                 => Maybe (IndentConfig -> Indent)
                 -> Maybe (LayoutConfig -> Layout)
                 -> ast1 NodeInfo
                 -> ByteString
                 -> ast2 NodeInfo
                 -> Printer ()
prettySimpleDecl' indentCfg layout lhs op rhs =
  withIndentConfig (fromMaybe cfgIndentSimpleDeclaration indentCfg) align indentby
  where
    align = do
        pretty lhs
        withLayout (fromMaybe cfgLayoutDeclaration layout) flexAlign verticalAlign

    indentby i = do
        pretty lhs
        withLayout (fromMaybe cfgLayoutDeclaration layout) (flexIndentby i) (verticalIndentby i)

    flexIndentby i =
        indentedBy i $ do
          operator Declaration op
          pretty rhs

    verticalIndentby i =
        indentedBy i $ do
          operatorV Declaration op
          pretty rhs

    flexAlign =
        alignOnOperator Declaration op $ pretty rhs

    verticalAlign =
        alignOnOperatorV Declaration op $ pretty rhs

prettyConDecls :: (Annotated ast, Pretty ast) => [ast NodeInfo] -> Printer ()
prettyConDecls condecls = do
    alignedConDecls <- getOption cfgOptionAlignSumTypeDecl
    if alignedConDecls && length condecls > 1
        then withLayout cfgLayoutDeclaration flex' vertical'
        else withLayout cfgLayoutDeclaration flex vertical
  where
    flex = do
        operator Declaration "="
        withLayout cfgLayoutConDecls flexDecls verticalDecls

    flex' = withLayout cfgLayoutConDecls flexDecls' verticalDecls'

    vertical = do
        operatorV Declaration "="
        withLayout cfgLayoutConDecls flexDecls verticalDecls

    vertical' = withLayout cfgLayoutConDecls flexDecls' verticalDecls'

    flexDecls = listAutoWrap' Declaration "|" condecls

    flexDecls' = horizontalDecls' <|> verticalDecls'

    horizontalDecls' = do
        operatorH Declaration "="
        listH' Declaration "|" condecls

    verticalDecls = listV' Declaration "|" condecls

    verticalDecls' = do
        withOperatorFormattingV Declaration "|" (write "=") id
        listV' Declaration "|" condecls


prettyForall :: (Annotated ast, Pretty ast) => [ast NodeInfo] -> Printer ()
prettyForall vars = do
    write "forall "
    inter space $ map pretty
                $ sortBy
                  (\l r -> nodeInfoSpan (ann l) `compare` nodeInfoSpan (ann r)) vars
    operator Type "."

prettyTypesig :: (Annotated ast, Pretty ast)
              => LayoutContext
              -> [ast NodeInfo]
              -> Type NodeInfo
              -> Printer ()
prettyTypesig ctx names ty = prettyTypesig' ctx names (pretty ty)

prettyTypesig' :: (Annotated ast, Pretty ast)
               => LayoutContext
               -> [ast NodeInfo]
               -> Printer ()
               -> Printer ()
prettyTypesig' ctx names p = do
    inter comma $ map pretty names
    atTabStop stopRecordField
    withIndentConfig cfgIndentTypesig align indentby
  where
    align = alignOnOperator ctx "::" p

    indentby i = indentedBy i $ do
        operator ctx "::"
        nl <- gets psNewline
        when nl $ do
            delta <- listVOpLen ctx "->"
            write $ BS.replicate delta 32
        p

prettyForallAdv :: ( Annotated ast
                   , Pretty ast
                   )
                => Maybe [ast NodeInfo]
                -> Maybe (Context NodeInfo)
                -> Maybe [ast NodeInfo]
                -> Maybe (Context NodeInfo)
                -> Printer a
                -> Printer a
prettyForallAdv mtyvarbinds mcontext mtyvarbinds' mcontext' p = do
    layout <- getConfig (cfgLayoutConstraints . cfgLayout)
    let listy = case layout of
          Flex -> list
          Vertical -> listV
          TryOneline -> listH

    prettyForallContext listy mtyvarbinds mcontext
    prettyForallContext listy mtyvarbinds' mcontext'
    within OtherDeclaration p
  where
    prettyForallContext layoutCt mtvb mct = do
      forM_ mtvb $ \tyvarbinds -> do
          write "forall "
          inter space $ map pretty
                      $ sortBy
                        (\l r -> nodeInfoSpan (ann l) `compare` nodeInfoSpan (ann r)) tyvarbinds
          withOperatorFormattingV Type "." (write "." >> space) id
      forM_ mct $ \context -> do
          case context of
              (CxSingle _ asst) -> pretty asst
              (CxTuple _ assts) -> layoutCt Type "(" ")" "," assts
              (CxEmpty _) -> write "()"
          operatorV Type "=>"

prettyGADT :: ( Foldable f
              , Foldable f1
              )
           => DataOrNew NodeInfo
           -> f (Printer ())
           -> Either (Maybe (Context NodeInfo), DeclHead NodeInfo) (Type NodeInfo)
           -> Maybe (Kind NodeInfo)
           -> [GadtDecl NodeInfo]
           -> f1 (Deriving NodeInfo)
           -> Printer ()
prettyGADT dataornew
           mheadAfter
           mcontextAndDeclheadOrType
           mkind
           gadtdecls
           derivings =
        within GADTDeclaration $ do
          depend' (pretty dataornew >> sequence_ mheadAfter) $ do
              case mcontextAndDeclheadOrType of
                Left (mcontext, declhead) -> do
                  mapM_ pretty mcontext
                  pretty declhead
                Right ty -> pretty ty
              mayM_ mkind $ \kind -> do
                  operator Declaration "::"
                  pretty kind
              write " where"
              newline
              within GADTFieldDeclaration $
                  linedOnside gadtdecls
          mapM_ pretty derivings

prettyApp :: (Annotated ast1, Annotated ast2, Pretty ast1, Pretty ast2)
          => ast1 NodeInfo
          -> [ast2 NodeInfo]
          -> Printer ()
prettyApp = prettyApp' cfgLayoutApp cfgIndentApp True

prettyApp' :: (Annotated ast1, Annotated ast2, Pretty ast1, Pretty ast2)
           => (LayoutConfig -> Layout)
           -> (IndentConfig -> Indent)
           -> Bool
           -> ast1 NodeInfo
           -> [ast2 NodeInfo]
           -> Printer ()
prettyApp' cfgLayout cfgIndent pad fn args = withLayout cfgLayout flex vertical
  where
    flex = do
        pretty fn
        forM_ args $ \arg -> cut $ do
            spaceOrNewline
            pretty arg

    vertical = do
        pretty fn
        withIndent cfgIndent pad $ lined args

prettyInfixApp
    :: (Annotated ast, Pretty ast, Annotated op, HSE.Pretty (op NodeInfo))
    => (op NodeInfo -> ByteString)
    -> LayoutContext
    -> (ast NodeInfo, [(op NodeInfo, ast NodeInfo)])
    -> Printer ()
prettyInfixApp nameFn ctx (lhs, args) =
    withLayout cfgLayoutInfixApp flex vertical
  where
    flex = do
        pretty lhs
        forM_ args $ \(op, arg) -> cut $ do
            withOperatorFormatting ctx (nameFn op) (prettyOp op) id
            pretty arg

    vertical = do
        pretty lhs
        forM_ args $ \(op, arg) -> do
            withOperatorFormattingV ctx (nameFn op) (prettyOp op) id
            pretty arg

    prettyOp op = do
        printComments Before op
        prettyHSE op
        printComments After op

prettyRecord :: (Annotated ast1, Pretty ast1, Annotated ast2, Pretty ast2)
             => (ast2 NodeInfo -> Printer (Maybe Int))
             -> LayoutContext
             -> ast1 NodeInfo
             -> [ast2 NodeInfo]
             -> Printer ()
prettyRecord len ctx name fields = withLayout cfgLayoutRecord flex vertical
  where
    flex = do
        withOperatorFormattingH ctx "record" (pretty name) id
        groupH ctx "{" "}" $ inter (operatorH ctx ",") $
            map prettyOnside fields

    vertical = do
        withOperatorFormatting ctx "record" (pretty name) id
        groupV ctx "{" "}" $ withComputedTabStop stopRecordField
                                                 cfgAlignRecordFields
                                                 (fmap (fmap pure) . len)
                                                 fields $
            listVinternal ctx "," fields

prettyRecordFields :: (Annotated ast, Pretty ast)
                   => (ast NodeInfo -> Printer (Maybe Int))
                   -> LayoutContext
                   -> [ast NodeInfo]
                   -> Printer ()
prettyRecordFields len ctx fields = withLayout cfgLayoutRecord flex vertical
  where
    flex = groupH ctx "{" "}" $ inter (operatorH ctx ",") $
        map prettyOnside fields

    vertical = groupV ctx "{" "}" $
        withComputedTabStop stopRecordField
                            cfgAlignRecordFields
                            (fmap (fmap pure) . len)
                            fields $ listVinternal ctx "," fields

prettyPragma :: ByteString -> Printer () -> Printer ()
prettyPragma name = prettyPragma' name . Just

prettyPragma' :: ByteString -> Maybe (Printer ()) -> Printer ()
prettyPragma' name mp = do
    write "{-# "
    write name
    mayM_ mp $ withPrefix space aligned
    atTabStop stopModulePragma
    write " #-}"

prettyBinds :: Binds NodeInfo -> Printer ()
prettyBinds binds = withIndentBy cfgIndentWhere True $ do
    write "where"
    withIndent cfgIndentWhereBinds True $ pretty binds

instance Pretty Module where
    prettyPrint (Module _ mhead pragmas imports decls) =
        within ModuleDeclaration $
          inter blankline $
            catMaybes [ ifNotEmpty prettyPragmas pragmas
                      , pretty <$> mhead
                      , ifNotEmpty prettyImports imports
                      , ifNotEmpty (prettyDecls skipBlankDecl DeclModule) decls
                      ]
      where
        ifNotEmpty f xs = if null xs then Nothing else Just (f xs)

    prettyPrint ast@XmlPage{} = prettyHSE ast
    prettyPrint ast@XmlHybrid{} = prettyHSE ast

instance Pretty ModuleHead where
    prettyPrint (ModuleHead _ name mwarning mexports) = do
        depend "module" $ do
            pretty name
            mayM_ mwarning $ withPrefix spaceOrNewline pretty
        mayM_ mexports pretty
        mIndentModuleWhere <- getConfig (cfgIndentModuleWhere . cfgIndent)
        case mIndentModuleWhere of
          Just {} ->
            withIndentBy (fromMaybe 0 . cfgIndentModuleWhere) False $
              withOperatorFormatting Declaration "module_where" (write "where") id
          Nothing ->
            withOperatorFormatting Declaration "module_where" (write "where") id

instance Pretty WarningText where
    prettyPrint (DeprText _ s) = write "{-# DEPRECATED " >> string (show s)
        >> write " #-}"
    prettyPrint (WarnText _ s) = write "{-# WARNING " >> string (show s)
        >> write " #-}"

instance Pretty ExportSpecList where
    prettyPrint (ExportSpecList _ exports) =
        withLayout cfgLayoutExportSpecList flex vertical
      where
        flex = do
            space
            listAutoWrap Other "(" ")" "," exports

        vertical = withIndent cfgIndentExportSpecList True $
            groupV Other "(" ")" $
              listVinternal Other "," exports

insertAt :: Int -> a -> [a] -> [a]
insertAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 xs     = a : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []
{-# INLINE insertAt #-}

instance Pretty ExportSpec where
    prettyPrint evar@EModuleContents {} = prettyHSE evar
    prettyPrint evar@EVar {} = prettyHSE evar
    prettyPrint evar@EAbs {} = prettyHSE evar

    prettyPrint (EThingWith _ ewc evar vars) = do
        case (ewc, vars) of
          (EWildcard _ 0, []) ->
              withLayout cfgLayoutExportSpecInnerList
                (do
                  withGroup Other "export-all-cons" "" "" $ pretty evar
                  withGroup Other "export-all-parens" "(" ")" $ write ".."
                )
                (do
                  withGroupV Other "export-all-cons" "" "" $ pretty evar
                  withGroupV Other "export-all-parens" "(" ")" $ write ".."
                )
          (ewc', vars') -> do
              let vars'' = case ewc' of
                            NoWildcard {} -> vars'
                            EWildcard ni i -> insertAt i (VarName ni (Ident ni "..")) vars'
              pretty evar
              withLayout cfgLayoutExportSpecInnerList (flex vars'') (vertical vars'')
      where
        flex vars' = withIndentFlex cfgIndentExportSpecInnerList $ do
            listAutoWrap Other "(" ")" "," vars'

        vertical vars' = withIndent cfgIndentExportSpecInnerList True $ do
            groupV Other "(" ")" $
              listVinternal Other "," vars'


instance Pretty ImportDecl where
    prettyPrint ImportDecl{..} = do
        inter space . map write $
            filter (not . BS.null)
                   [ "import"
                   , if importSrc then "{-# SOURCE #-}" else ""
                   , if importSafe then "safe" else ""
                   , if importQualified then "qualified" else ""
                   ]
        atTabStop stopImportModule
        space
        string $ moduleName importModule
        mayM_ importAs $ \name -> do
            atTabStop stopImportSpec
            write " as "
            pretty name

        mayM_ importSpecs pretty

toComp :: ImportSpec l -> (Int, String)
toComp i@IVar {} = (3, HSE.prettyPrint i)
toComp i@(IAbs _ (NoNamespace {}) _) = (1, HSE.prettyPrint i)
toComp i@(IAbs {}) = (2, HSE.prettyPrint i)
toComp i@IThingAll {} = (1, HSE.prettyPrint i)
toComp i@IThingWith {} = (1, HSE.prettyPrint i)

instance Pretty ImportSpecList where
    prettyPrint (ImportSpecList _ hiding specs) = do
        sortP <- getOption cfgOptionSortImportLists
        let specs' = if sortP then sortOn toComp specs else specs
        atTabStop stopImportSpec
        withLayout cfgLayoutImportSpecList (flex specs') (vertical specs')
      where
        flex imports = withIndentFlex cfgIndentImportSpecList $ do
            when hiding $ write "hiding "
            listAutoWrap Other "(" ")" "," imports

        vertical imports = withIndent cfgIndentImportSpecList True $ do
            when hiding $ write "hiding "

            (zeroSpec, oneSpec) <- getConfig (cfgOptionImportCompactSpecialization . cfgOptions)
            let compactList ctx op open close sep xs =
                    withGroupH ctx op open close . inter (operatorH ctx sep) $ map pretty xs
            case imports of
              xs@[] | zeroSpec -> compactList Other "imp-compact-(" "(" ")" "," xs
              xs@[x] | oneSpec && isCompactImport1 x -> compactList Other "imp-compact-(" "(" ")" "," xs
              _ ->
                groupV Other "(" ")" $
                  listVinternal Other "," imports
        isCompactImport1 x = case x of
                               IVar {} -> True
                               IAbs {} -> True
                               _       -> False

instance Pretty ImportSpec where
    prettyPrint (IThingAll _ ivar) =
        withLayout cfgLayoutImportSpecInnerList
          (do
            withGroup Other "import-all-cons" "" "" $ pretty ivar
            withGroup Other "import-all-parens" "(" ")" $ write ".."
          )
          (do
            withGroupV Other "import-all-cons" "" "" $ pretty ivar
            withGroupV Other "import-all-parens" "(" ")" $ write ".."
          )

    prettyPrint ivar@IVar {} = prettyHSE ivar
    prettyPrint ivar@IAbs {} = prettyHSE ivar

    prettyPrint (IThingWith _ ivar vars) = do
        sortP <- getOption cfgOptionSortImportSpecInnerLists
        let vars' = if sortP then sortOn HSE.prettyPrint vars else vars
        pretty ivar
        withLayout cfgLayoutImportSpecInnerList (flex vars') (vertical vars')
      where
        flex vars' = withIndentFlex cfgIndentImportSpecInnerList $
            listAutoWrap Other "(" ")" "," vars'

        vertical vars' = withIndent cfgIndentImportSpecInnerList True $ do
            (zeroSpec, oneSpec) <- getConfig (cfgOptionImportCompactSpecialization . cfgOptions)
            let compactList ctx op open close sep xs =
                    withGroupH ctx op open close . inter (operatorH ctx sep) $ map pretty xs
            case vars' of
              xs@[] | zeroSpec -> compactList Other "imp-compact-(" "(" ")" "," xs
              xs@[_] | oneSpec -> compactList Other "imp-compact-(" "(" ")" "," xs
              _ ->
                groupV Other "(" ")" $
                  listVinternal Other "," vars'

instance Pretty CName

instance Pretty Assoc

instance Pretty Decl where
    prettyPrint (TypeDecl _ declhead ty) =
        within TypeDeclaration $
          depend "type" $ prettySimpleDecl declhead "=" ty

    prettyPrint (TypeFamDecl _ declhead mresultsig minjectivityinfo) =
        within TypeDeclaration $
            depend "type family" $ do
                pretty declhead
                mayM_ mresultsig pretty
                mayM_ minjectivityinfo pretty

    prettyPrint (ClosedTypeFamDecl _
                                   declhead
                                   mresultsig
                                   minjectivityinfo
                                   typeeqns) =
        within TypeDeclaration $
            depend "type family" $ do
                pretty declhead
                mayM_ mresultsig pretty
                mayM_ minjectivityinfo pretty
                write " where"
                newline
                linedOnside typeeqns

    prettyPrint (DataDecl _ dataornew mcontext declhead qualcondecls derivings) =
        within RecordDeclaration $ do
          depend' (pretty dataornew) $ do
              mapM_ pretty mcontext
              pretty declhead
              unless (null qualcondecls) $ prettyConDecls qualcondecls
          mapM_ pretty derivings

    prettyPrint (GDataDecl _
                           dataornew
                           mcontext
                           declhead
                           mkind
                           gadtdecls
                           derivings) =
        prettyGADT dataornew Nothing (Left (mcontext, declhead)) mkind gadtdecls derivings

    prettyPrint (DataFamDecl _ mcontext declhead mresultsig) =
        depend "data family" $ do
            mapM_ pretty mcontext
            pretty declhead
            mapM_ pretty mresultsig

    prettyPrint (TypeInsDecl _ ty ty') =
        depend "type instance" $ prettySimpleDecl ty "=" ty'

    prettyPrint (DataInsDecl _ dataornew ty qualcondecls derivings) =
        within RecordDeclaration $ do
            depend' (pretty dataornew >> write " instance") $ do
                pretty ty
                prettyConDecls qualcondecls
            mapM_ pretty derivings

    prettyPrint (GDataInsDecl _ dataornew ty mkind gadtdecls derivings) =
        prettyGADT dataornew (Just $ write " instance") (Right ty) mkind gadtdecls derivings

    prettyPrint (ClassDecl _ mcontext declhead fundeps mclassdecls) =
        within ClassDeclaration $ do
            case mcontext of
                Nothing -> withOperatorFormatting Declaration "no-ctx-class" (write "class") id
                Just (CxTuple {}) -> withOperatorFormatting Declaration "class" (write "class") id
                Just (CxSingle {}) -> withOperatorFormatting Declaration "1-ctx-class" (write "class") id
                Just (CxEmpty {}) -> withOperatorFormatting Declaration "0-ctx-class" (write "class") id
            withIndent cfgIndentClass False $ do
                mapM_ pretty mcontext
                pretty declhead
                unless (null fundeps) $ do
                    operator Declaration "|"
                    list' Declaration "," fundeps
                mayM_ mclassdecls $ \decls -> do
                    operator Declaration "where"

                    when (not $ null decls) $
                        withComputedTabStop stopRhs
                                            cfgAlignClass
                                            measureClassDecl
                                            decls $
                            prettyDecls skipBlankClassDecl DeclClass decls

    prettyPrint (InstDecl _ moverlap instrule minstdecls) =
        within ClassDeclaration $ do
            let writeI = do write "instance"
                            mapM_ (\o -> space >> pretty o) moverlap

            case instRuleAnalysis instrule of
                NoContext -> withOperatorFormatting Declaration "no-ctx-instance" writeI id
                TupleContext -> withOperatorFormatting Declaration "instance" writeI id
                OneContext -> withOperatorFormatting Declaration "1-ctx-instance" writeI id
                ZeroContext -> withOperatorFormatting Declaration "0-ctx-instance" writeI id

            withIndent cfgIndentClass False $ do
                pretty instrule
                mayM_ minstdecls $ \decls -> do
                    operator Declaration "where"
                    when (not $ null decls) $
                        withComputedTabStop stopRhs cfgAlignClass measureInstDecl decls $
                        prettyDecls skipBlankInstDecl DeclInstance decls

#if MIN_VERSION_haskell_src_exts(1,20,0)
    prettyPrint (DerivDecl _ mderivstrategy moverlap instrule) =
        within TypeDeclaration $
            depend "deriving" $ do
                mayM_ mderivstrategy $ withPostfix space pretty
                write "instance "
                mayM_ moverlap $ withPostfix space pretty
                pretty instrule
#else
    prettyPrint (DerivDecl _ moverlap instrule) =
        within TypeDeclaration $
            depend "deriving" $ do
                write "instance "
                mayM_ moverlap $ withPostfix space pretty
                pretty instrule
#endif

    prettyPrint (InfixDecl _ assoc mint ops) = onside $ do
        pretty assoc
        mayM_ mint $ withPrefix space int
        space
        inter comma $ map prettyHSE ops

    prettyPrint (DefaultDecl _ types) = do
        write "default "
        listAutoWrap Other "(" ")" "," types

    prettyPrint (SpliceDecl _ expr) = pretty expr

    prettyPrint (TypeSig _ names ty@TyForall {}) =
        within TypeDeclaration $
          onside $ prettyTypesig Declaration names ty

    prettyPrint (TypeSig _ names ty) =
        within TypeDeclaration $
          onside $ prettyTypesig' Declaration names $ within OtherDeclaration $ pretty ty

#if MIN_VERSION_haskell_src_exts(1,21,0)
    prettyPrint (PatSynSig _
                           names
                           mtyvarbinds
                           mcontext
                           mtyvarbinds'
                           mcontext'
                           ty) =
        let p = prettyForallAdv mtyvarbinds mcontext mtyvarbinds' mcontext' (pretty ty)

        in within TypeDeclaration $
            depend "pattern" $ do
                inter comma $ map pretty names
                withIndentConfig cfgIndentPatternsig (align p) (indentby p)
#elif MIN_VERSION_haskell_src_exts(1,20,0)
    prettyPrint (PatSynSig _ names mtyvarbinds mcontext mcontext' ty) =
        let p = prettyForallAdv mtyvarbinds mcontext Nothing mcontext' (pretty ty)
        in within TypeDeclaration $
            depend "pattern" $ do
                inter comma $ map pretty names
                withIndentConfig cfgIndentPatternsig (align p) (indentby p)
#else
    prettyPrint (PatSynSig _ name mtyvarbinds mcontext mcontext' ty) =
        let p = prettyForallAdv mtyvarbinds mcontext Nothing mcontext' (pretty ty)
        in within TypeDeclaration $
            depend "pattern" $ do
                pretty name
                withIndentConfig cfgIndentPatternsig (align p) (indentby p)
#endif
        where
          align = alignOnOperator Declaration "::"

          indentby p i = indentedBy i $ do
              operator Declaration "::"
              nl <- gets psNewline
              when nl $ do
                  delta <- listVOpLen Declaration "->"
                  write $ BS.replicate delta 32
              p

    prettyPrint (FunBind _ matches) =
        withComputedTabStop stopRhs cfgAlignMatches measureMatch matches $
        linedOnside matches

    prettyPrint (PatBind _ pat rhs mbinds) = do
        onside $ do
            pretty pat
            atTabStop stopRhs
            pretty rhs
        mapM_ prettyBinds mbinds

    prettyPrint (PatSyn _ pat pat' patternsyndirection) =
        within PatternDeclaration $ do
            depend "pattern" $  do
              pretty pat
              withLayout cfgLayoutPatternSynonym (operator Declaration sep) (operatorV Declaration sep)
              pretty pat'
            case patternsyndirection of
                ExplicitBidirectional _ decls -> prettyBinds (BDecls noNodeInfo decls)
                _ -> return ()
      where
        sep = case patternsyndirection of
            ImplicitBidirectional -> "="
            ExplicitBidirectional _ _ -> "<-"
            Unidirectional -> "<-"

    prettyPrint (ForImp _ callconv msafety mstring name ty) =
        depend "foreign import" $ do
            pretty callconv
            mayM_ msafety $ withPrefix space pretty
            mayM_ mstring $ withPrefix space (string . show)
            space
            within TypeDeclaration $
                prettyTypesig Declaration [ name ] ty

    prettyPrint (ForExp _ callconv mstring name ty) =
        depend "foreign export" $ do
            pretty callconv
            mayM_ mstring $ withPrefix space (string . show)
            space
            within TypeDeclaration $
                prettyTypesig Declaration [ name ] ty

    prettyPrint (RulePragmaDecl _ rules) =
        if null rules
        then prettyPragma' "RULES" Nothing
        else prettyPragma "RULES" $ mapM_ pretty rules

    prettyPrint (DeprPragmaDecl _ deprecations) =
        if null deprecations
        then prettyPragma' "DEPRECATED" Nothing
        else prettyPragma "DEPRECATED" $ forM_ deprecations $
            \(names, str) -> do
                unless (null names) $ do
                    inter comma $ map pretty names
                    space
                string (show str)

    prettyPrint (WarnPragmaDecl _ warnings) =
        if null warnings
        then prettyPragma' "WARNING" Nothing
        else prettyPragma "WARNING" $ forM_ warnings $ \(names, str) -> do
            unless (null names) $ do
                inter comma $ map pretty names
                space
            string (show str)

    prettyPrint (InlineSig _ inline mactivation qname) = prettyPragma name $ do
        mayM_ mactivation $ withPostfix space pretty
        pretty qname
      where
        name = if inline then "INLINE" else "NOINLINE"

    prettyPrint (InlineConlikeSig _ mactivation qname) =
        prettyPragma "INLINE CONLIKE" $ do
            mayM_ mactivation $ withPostfix space pretty
            pretty qname

    prettyPrint (SpecSig _ mactivation qname types) =
        prettyPragma "SPECIALISE" $ do
            mayM_ mactivation $ withPostfix space pretty
            within TypeDeclaration $
                onside $
                    prettyTypesig' Declaration [qname] $
                        inter (withOperatorFormatting Declaration "specialize_comma" (write ", ") id) $ map pretty types

    prettyPrint (SpecInlineSig _ inline mactivation qname types) =
        prettyPragma name $ do
            mayM_ mactivation $ withPostfix space pretty
            within TypeDeclaration $
                onside $
                    prettyTypesig' Declaration [qname] $
                        inter (withOperatorFormatting Declaration "specialize_comma" (write ", ") id) $ map pretty types
      where
        name = if inline then "SPECIALISE INLINE" else "SPECIALISE NOINLINE"

    prettyPrint (InstSig _ instrule) =
        prettyPragma "SPECIALISE instance" $ pretty instrule

    prettyPrint (AnnPragma _ annotation) =
        prettyPragma "ANN" $ pretty annotation

    prettyPrint (MinimalPragma _ mbooleanformula) =
        prettyPragma "MINIMAL" $ mapM_ pretty mbooleanformula

    -- prettyPrint (RoleAnnotDecl _ qname roles) = undefined
    prettyPrint decl = prettyHSE decl

instance Pretty DeclHead where
    prettyPrint (DHead _ name) = pretty name

    prettyPrint (DHInfix _ tyvarbind name) = do
        pretty tyvarbind
        pretty $ VarOp noNodeInfo name

    prettyPrint (DHParen _ declhead) = parens $ pretty declhead

    prettyPrint (DHApp _ declhead tyvarbind) = depend' (pretty declhead) $
        pretty tyvarbind

data ContextType = NoContext
                 | TupleContext
                 | OneContext
                 | ZeroContext

instRuleAnalysis :: InstRule l -> ContextType
instRuleAnalysis (IParen _ instrule) = instRuleAnalysis instrule
instRuleAnalysis (IRule _ _ Nothing _) = NoContext
instRuleAnalysis (IRule _ _ (Just (CxTuple {})) _) = TupleContext
instRuleAnalysis (IRule _ _ (Just (CxSingle {})) _) = OneContext
instRuleAnalysis (IRule _ _ (Just (CxEmpty {})) _) = ZeroContext

instance Pretty InstRule where
    prettyPrint (IRule _ mtyvarbinds mcontext insthead) = do
        mapM_ prettyForall mtyvarbinds
        mapM_ pretty mcontext
        pretty insthead

    prettyPrint (IParen _ instrule) = group Declaration "(" ")" $ pretty instrule

instance Pretty InstHead where
    prettyPrint (IHCon _ qname) = pretty qname

    prettyPrint (IHInfix _ ty qname) = do
        pretty ty
        space
        pretty qname

    prettyPrint (IHParen _ insthead) = parens $ pretty insthead

    prettyPrint (IHApp _ insthead ty) = depend' (pretty insthead) $ pretty ty

instance Pretty Binds where
    prettyPrint (BDecls _ decls) =
        withComputedTabStop stopRhs cfgAlignWhere measureDecl decls $
        prettyDecls skipBlankDecl DeclWhere decls

    prettyPrint (IPBinds _ ipbinds) = linedOnside ipbinds

instance Pretty IPBind where
    prettyPrint (IPBind _ ipname expr) = prettySimpleDecl ipname "=" expr

instance Pretty InjectivityInfo where
    prettyPrint (InjectivityInfo _ name names) = do
        operator Declaration "|"
        pretty name
        operator Declaration "->"
        inter space $ map pretty names

instance Pretty ResultSig where
    prettyPrint (KindSig _ kind) =
        withLayout cfgLayoutDeclaration flex vertical
      where
        flex = do
            operator Declaration "::"
            pretty kind

        vertical = do
            operatorV Declaration "::"
            pretty kind

    prettyPrint (TyVarSig _ tyvarbind) =
        withLayout cfgLayoutDeclaration flex vertical
      where
        flex = do
            operator Declaration "="
            pretty tyvarbind

        vertical = do
            operatorV Declaration "="
            pretty tyvarbind

instance Pretty ClassDecl where
    prettyPrint (ClsDecl _ decl) = pretty decl

    prettyPrint (ClsDataFam _ mcontext declhead mresultsig) = depend "data" $ do
        mapM_ pretty mcontext
        pretty declhead
        mayM_ mresultsig pretty

    prettyPrint (ClsTyFam _ declhead mresultsig minjectivityinfo) =
        depend "type" $ do
            pretty declhead
            mayM_ mresultsig pretty
            mapM_ pretty minjectivityinfo

    prettyPrint (ClsTyDef _ typeeqn) = depend "type" $ pretty typeeqn

    prettyPrint (ClsDefSig _ name ty) =
        within TypeDeclaration $
          depend "default" $ prettyTypesig Declaration [ name ] ty

instance Pretty InstDecl where
    prettyPrint (InsDecl _ decl) = pretty decl

    prettyPrint (InsType _ ty ty') =
        depend "type" $ prettySimpleDecl ty "=" ty'

    prettyPrint (InsData _ dataornew ty qualcondecls derivings) =
        within RecordDeclaration $
            depend' (pretty dataornew) $ do
                pretty ty
                unless (null qualcondecls) $ prettyConDecls qualcondecls
                mapM_ pretty derivings

    prettyPrint (InsGData _ dataornew ty mkind gadtdecls derivings) =
        within GADTDeclaration $ do
            depend' (pretty dataornew) $ do
                pretty ty
                mayM_ mkind $ \kind -> do
                    operator Declaration "::"
                    pretty kind
                write " where"
                newline
                within GADTFieldDeclaration $
                    linedOnside gadtdecls
            mapM_ pretty derivings

instance Pretty Deriving where
#if MIN_VERSION_haskell_src_exts(1,20,0)
    prettyPrint (Deriving _ mderivstrategy instrules) =
        withIndentBy cfgIndentDeriving True $ do
            withOperatorFormatting Other "deriving" (write "deriving" >> prettyStratBefore) id
            case instrules of
                [ i@IRule{} ] -> pretty i
                [ IParen _ i ] -> listAutoWrap Other "(" ")" "," [ i ]
                _ -> listAutoWrap Other "(" ")" "," instrules
            prettyStratAfter
      where
        (prettyStratBefore, prettyStratAfter) = case mderivstrategy of
#if MIN_VERSION_haskell_src_exts(1,21,0)
            Just x@DerivVia{} -> (return (), space *> pretty x)
#endif
            Just x -> (space *> pretty x <* space, return ())
            _ -> (return (), return ())
#else
    prettyPrint (Deriving _ instrules) = withIndentBy cfgIndentDeriving True $ do
        write "deriving "
        case instrules of
            [ i@IRule{} ] -> pretty i
            [ IParen _ i ] -> listAutoWrap Other "(" ")" "," [ i ]
            _ -> listAutoWrap Other "(" ")" "," instrules
#endif

instance Pretty ConDecl where
    prettyPrint (ConDecl _ name types) = do
        pretty name
        unless (null types) $ do
            space
            oneline hor <|> ver
      where
        hor = inter space $ map pretty types

        ver = aligned $ linedOnside types

    prettyPrint (InfixConDecl _ ty name ty') = do
        pretty ty
        pretty $ ConOp noNodeInfo name
        pretty ty'

    prettyPrint (RecDecl _ name fielddecls) =
        prettyRecord len Declaration name fielddecls
      where
        len (FieldDecl _ names _) = measure $ inter comma $ map pretty names

instance Pretty FieldDecl where
    prettyPrint (FieldDecl _ names ty) = prettyTypesig Declaration names ty

instance Pretty QualConDecl where
    prettyPrint (QualConDecl _ mtyvarbinds mcontext condecl) = do
        mapM_ prettyForall mtyvarbinds
        mapM_ pretty mcontext
        pretty condecl

instance Pretty GadtDecl where
#if MIN_VERSION_haskell_src_exts(1,21,0)
    prettyPrint (GadtDecl _ name _ _ mfielddecls ty) = do
        pretty name
        operator Declaration "::"
        mayM_ mfielddecls $ \decls -> do
            within RecordDeclaration $ do
                prettyRecordFields len Declaration decls
                newline
            operator Type "->"
        within GADTFieldTypeDeclaration $
            pretty ty
#else
    prettyPrint (GadtDecl _ name mfielddecls ty) = do
        pretty name
        operator Declaration "::"
        mayM_ mfielddecls $ \decls -> do
            within RecordDeclaration $ do
                prettyRecordFields len Declaration decls
                newline
            operator Type "->"
        within GADTFieldTypeDeclaration $
            pretty ty
#endif
      where
        len (FieldDecl _ names _) = measure $ inter comma $ map pretty names

instance Pretty Match where
    prettyPrint (Match _ name pats rhs mbinds) = do
        onside $ do
            prettyApp name pats
            atTabStop stopRhs
            pretty rhs
        mapM_ prettyBinds mbinds

    prettyPrint (InfixMatch _ pat name pats rhs mbinds) = do
        onside $ do
            withLayout cfgLayoutInfixApp flex vertical
            atTabStop stopRhs
            pretty rhs
        mapM_ prettyBinds mbinds
      where
        flex = do
            pretty pat
            withOperatorFormatting Pattern
                                   (opName'' name)
                                   (prettyHSE $ VarOp noNodeInfo name)
                                   id
            inter spaceOrNewline $ map pretty pats

        vertical = do
            pretty pat
            withOperatorFormattingV Pattern
                                    (opName'' name)
                                    (prettyHSE $ VarOp noNodeInfo name)
                                    id
            linedOnside pats

prettyUnGuardedRHS :: (Annotated ast, Pretty ast) => LayoutContext -> Bool -> ast NodeInfo -> Printer ()
prettyUnGuardedRHS _ctx sep expr =
      cut $ withLayout cfgLayoutDeclaration flex vertical
    where
      flex = do
          operator Declaration "="
          pretty expr

      vertical = do
          operatorV Declaration "="
          when sep newline
          pretty expr

instance Pretty Rhs where
    prettyPrint (UnGuardedRhs _ expr@Let {}) = do
        letSpecialization <- getConfig (cfgOptionLetSpecialization . cfgOptions)
        prettyUnGuardedRHS Other letSpecialization expr
    prettyPrint (UnGuardedRhs _ expr@ListComp {}) = do
        listCompSpecialization <- getConfig (cfgOptionListCompSpecialization . cfgOptions)
        prettyUnGuardedRHS Other listCompSpecialization expr
    prettyPrint (UnGuardedRhs _ expr@ParComp {}) = do
        listCompSpecialization <- getConfig (cfgOptionListCompSpecialization . cfgOptions)
        prettyUnGuardedRHS Other listCompSpecialization expr
    prettyPrint (UnGuardedRhs _ expr@ParArrayComp {}) = do
        listCompSpecialization <- getConfig (cfgOptionListCompSpecialization . cfgOptions)
        prettyUnGuardedRHS Other listCompSpecialization expr
    prettyPrint (UnGuardedRhs _ expr) =
        prettyUnGuardedRHS Other False expr

    prettyPrint (GuardedRhss _ guardedrhss) =
        within GuardDeclaration $
            withIndent cfgIndentMultiIf True $ do
              withComputedTabStopEager stopGuardedRhs cfgAlignMultiIfRhs measureGuardedRhs guardedrhss $
                  linedOnside guardedrhss

instance Pretty GuardedRhs where
    prettyPrint (GuardedRhs _ stmts expr) =
        withLayout cfgLayoutDeclaration flex vertical
      where
        flex = do
            operatorSectionR Pattern "|" $ write "|"
            inter (operatorH Pattern ",") $ map pretty stmts
            atTabStop stopGuardedRhs
            operator Declaration "="
            pretty expr

        vertical = do
            col <- getNextColumn
            operatorSectionR Pattern "|" $ write "|"
            inter (column' col $ operator Pattern ",") $ map pretty stmts
            atTabStop stopGuardedRhs
            operatorV Declaration "="
            pretty expr

instance Pretty Context where
    prettyPrint (CxSingle _ asst) = do
        pretty asst
        operator Type "=>"

    prettyPrint (CxTuple _ assts) = do
        listG Type "(" ")" "," assts
        operator Type "=>"

    prettyPrint (CxEmpty _) = do
        write "()"
        operator Type "=>"

instance Pretty FunDep where
    prettyPrint (FunDep _ names names') = do
        inter space $ map pretty names
        operator Declaration "->"
        inter space $ map pretty names'

#if MIN_VERSION_haskell_src_exts(1,22,0)
instance Pretty Asst where
    prettyPrint (TypeA _ ty) = pretty ty
    prettyPrint (IParam _ ipname ty) = prettyTypesig Declaration [ ipname ] ty
    prettyPrint (ParenA _ asst) = parens $ pretty asst
#else
instance Pretty Asst where
    prettyPrint (ClassA _ qname types) = do
        pretty qname
        space
        inter space $ map pretty types

    prettyPrint (AppA _ name types) = do
        pretty name
        space
        inter space $ map pretty types

    prettyPrint (InfixA _ ty qname ty') = do
        pretty ty
        withOperatorFormatting Type
                               (opName' qname)
                               (prettyHSE $ QVarOp noNodeInfo qname)
                               id
        pretty ty'

    prettyPrint (IParam _ ipname ty) = prettyTypesig Declaration [ ipname ] ty

    prettyPrint (EqualP _ ty ty') = do
        pretty ty
        operator Type "~"
        pretty ty'

    prettyPrint (ParenA _ asst) = parens $ pretty asst

    prettyPrint (WildCardA _ mname) = do
        write "_"
        mapM_ pretty mname
#endif

instance Pretty Type where
    prettyPrint t = do
        layout <- gets psTypeLayout
        withinDeclaration <- gets psWithinDeclaration
        nestTypeLevel $ do
          lvl <- gets psTypeNestLevel
          case lvl of
              1 -> case layout of
                  TypeFree -> withLayout (withinDeclToLayout withinDeclaration . cfgLayoutType) flex vertical
                  TypeFlex -> prettyF t
                  TypeVertical -> prettyV t
              _ -> prettyF t
      where
        flex = withTypeLayout TypeFlex $ prettyF t

        vertical = withTypeLayout TypeVertical $ prettyV t

        withTypeLayout :: TypeLayout -> Printer () -> Printer ()
        withTypeLayout l p = do
            layout <- gets psTypeLayout
            modify $ \s -> s { psTypeLayout = l }
            p
            modify $ \s -> s { psTypeLayout = layout }

        prettyF (TyForall _ mtyvarbinds mcontext ty) = do
            mapM_ prettyForall mtyvarbinds
            mapM_ pretty mcontext
            pretty ty

        prettyF (TyFun _ ty ty') = do
            lvl <- gets psTypeNestLevel
            pretty ty
            case lvl of
                1 -> withOperatorFormatting Type "type-fun-outer" (write "->") id
                _ -> withOperatorFormattingH Type "type-fun-inner" (write "->") id
            pretty ty'

        prettyF (TyTuple _ boxed tys) =
            case boxed of
                Unboxed -> list Type "(#" "#)" "," tys
                Boxed -> list Type "(" ")" "," tys

#if MIN_VERSION_haskell_src_exts(1,20,0)
        prettyF (TyUnboxedSum _ tys) =
            list Type "(#" "#)" "|" tys
#endif

        prettyF (TyList _ ty) =
            group Type "[" "]" $ pretty ty

        prettyF (TyParArray _ ty) =
            group Type "[:" ":]" $ pretty ty

        prettyF ty@TyApp{} = case flattenApp flatten ty of
            ctor : args -> prettyApp ctor args
            [] -> error "impossible"
          where
            flatten (TyApp _ a b) = Just (a, b)
            flatten _ = Nothing

        prettyF (TyVar _ name) =
            pretty name

        prettyF (TyCon _ qname) =
            pretty qname

        prettyF (TyParen _ ty) =
            parens . withTypeLayout TypeFree $ pretty ty

#if MIN_VERSION_haskell_src_exts(1,20,0)
        prettyF (TyInfix _ ty op ty') = do
            pretty ty
            withOperatorFormatting Type opname (prettyHSE op) id
            pretty ty'
          where
            opname = opName' $ case op of
                PromotedName _ qname -> qname
                UnpromotedName _ qname -> qname
#else
        prettyF (TyInfix _ ty qname ty') = do
            pretty ty
            withOperatorFormatting Type (opName' qname) (prettyHSE qname) id
            pretty ty'
#endif

        prettyF (TyKind _ ty kind) = do
            pretty ty
            operator Type "::"
            pretty kind

        prettyF ty@(TyPromoted _ _promoted) =
            prettyHSE ty

        prettyF (TyEquals _ ty ty') = do
            pretty ty
            operator Type "~"
            pretty ty'

        prettyF (TySplice _ splice) =
            pretty splice

        prettyF (TyBang _ bangtype unpackedness ty) = do
            pretty unpackedness
            pretty bangtype
            pretty ty

        prettyF ty@(TyWildCard _ _mname) =
            prettyHSE ty

        prettyF (TyQuasiQuote _ str str') = do
            write "["
            string str
            write "|"
            string str'
            write "|]"

#if MIN_VERSION_haskell_src_exts(1,21,0)
        prettyF (TyStar _) =
            write "*"
#endif

        prettyV (TyForall _ mtyvarbinds mcontext ty) = do
            prettyForallAdv mtyvarbinds mcontext Nothing Nothing (prettyV ty)

        prettyV (TyFun _ ty ty') = do
            lvl <- gets psTypeNestLevel
            pretty ty
            case lvl of
                1 -> withOperatorFormatting Type "type-fun-outer" (write "->") id
                _ -> withOperatorFormattingH Type "type-fun-inner" (write "->") id
            prettyV ty'

        prettyV ty =
            prettyF ty

#if !MIN_VERSION_haskell_src_exts(1,21,0)
instance Pretty Kind where
    prettyPrint (KindStar _) = write "*"

    prettyPrint (KindFn _ kind kind') = do
        pretty kind
        operator Type "->"
        pretty kind'

    prettyPrint (KindParen _ kind) = parens $ pretty kind

    prettyPrint (KindVar _ qname) = pretty qname

    prettyPrint (KindApp _ kind kind') = do
        pretty kind
        space
        pretty kind'

    prettyPrint (KindTuple _ kinds) = list Type "'(" ")" "," kinds

    prettyPrint (KindList _ kind) = group Type "'[" "]" $ pretty kind
#endif

instance Pretty TyVarBind where
    prettyPrint (KindedVar _ name kind) = parens $ do
        pretty name
        operator Type "::"
        pretty kind

    prettyPrint (UnkindedVar _ name) = pretty name

instance Pretty TypeEqn where
    prettyPrint (TypeEqn _ ty ty') = do
        pretty ty
        operator Type "="
        pretty ty'

flexibleOneline :: Printer a -> Printer a
flexibleOneline p = do
    allowOneline <- getOption cfgOptionFlexibleOneline
    if allowOneline then ignoreOneline p else p

prettyLetIn :: (Annotated ast, Pretty ast)
            => p
            -> Bool
            -> Binds NodeInfo
            -> ast NodeInfo
            -> Printer ()
prettyLetIn _ctx special binds expr = withLayout cfgLayoutLet flex vertical
  where
    flex = do
        write "let "
        prettyOnside (CompactBinds binds)
        spaceOrNewline
        write "in "
        prettyOnside expr

    vertical = do
        (letInPrePadding, letInPadding) <- getConfig (cfgOptionLetInPadding . cfgOptions)
        withIndentAfter cfgIndentLet
                               (do
                                    operator Expression "let"
                                    withIndent cfgIndentLetBinds letInPadding $
                                        pretty (CompactBinds binds))
                               (do
                                    when letInPrePadding newline
                                    let
                                      specialty = if special
                                        then within SpecialDeclaration
                                        else id
                                    specialty $ operator Expression "in"
                                    withIndent cfgIndentLetIn letInPadding $ pretty expr)

instance Pretty Exp where
    prettyPrint (Var _ qname) = pretty qname

    prettyPrint (OverloadedLabel _ str) = do
        write "#"
        string str

    prettyPrint (IPVar _ ipname) = pretty ipname

    prettyPrint (Con _ qname) = pretty qname

    prettyPrint (Lit _ literal) = pretty literal

    prettyPrint e@(InfixApp _ _ qop _) =
        prettyInfixApp opName Expression $ flattenInfix flattenInfixApp e
      where
        flattenInfixApp (InfixApp _ lhs qop' rhs) =
            if compareAST qop qop' == EQ
            then Just (lhs, qop', rhs)
            else Nothing
        flattenInfixApp _ = Nothing

    prettyPrint e@App{} = case flattenApp flatten e of
        fn : args -> prettyApp fn args
        [] -> error "impossible"
      where
        flatten (App _ fn arg) = Just (fn, arg)
        flatten _ = Nothing

    prettyPrint (NegApp _ expr) = do
        write "-"
        pretty expr

    prettyPrint (Lambda _ pats expr) = do
        write "\\"
        maybeSpace
        inter space $ map pretty pats
        flexibleOneline $ do
            operator Expression "->"
            pretty expr
      where
        maybeSpace = case pats of
            PIrrPat{} : _ -> space
            PBangPat{} : _ -> space
            _ -> return ()

    prettyPrint (Let _ binds expr@Do {}) = do
      letDoSpecialization <- getConfig (cfgOptionLetDoSpecialization . cfgOptions)
      prettyLetIn Other letDoSpecialization binds expr
    prettyPrint (Let _ binds expr@MDo {}) = do
      letDoSpecialization <- getConfig (cfgOptionLetDoSpecialization . cfgOptions)
      prettyLetIn Other letDoSpecialization binds expr
    prettyPrint (Let _ binds expr) = prettyLetIn Other False binds expr

    prettyPrint (If _ expr expr' expr'') = withLayout cfgLayoutIf flex vertical
      where
        flex = do
            operatorH Expression "if"
            prettyOnside expr
            spaceOrNewline
            operatorH Expression "then"
            prettyOnside expr'
            spaceOrNewline
            operatorH Expression "else"
            prettyOnside expr''

        vertical = withIndentAfter cfgIndentIf
                                   (do
                                        operator Expression "if"
                                        prettyOnside expr)
                                   (do
                                        newline
                                        operator Expression "then"
                                        prettyOnside expr'
                                        newline
                                        operator Expression "else"
                                        prettyOnside expr'')

    prettyPrint (MultiIf _ guardedrhss) = do
        write "if"
        withIndent cfgIndentMultiIf True . linedOnside $ map GuardedAltR guardedrhss

    prettyPrint (Case _ expr alts) =
        within CaseDeclaration $ do
            write "case "
            pretty expr
            write " of"
            if null alts
                then write " { }"
                else flexibleOneline . withIndent cfgIndentCase True
                    . withComputedTabStop stopRhs cfgAlignCase measureAlt alts $
                    lined alts

    prettyPrint (Do _ stmts) = flexibleOneline $ do
        write "do"
        withIndent cfgIndentDo True $
            withComputedTabStopEager stopDoLeftArrow cfgAlignDoLeftArrow measureDoGenerators stmts $
                linedOnside stmts

    prettyPrint (MDo _ stmts) = flexibleOneline $ do
        write "mdo"
        withIndent cfgIndentDo True $ linedOnside stmts

    prettyPrint (Tuple _ boxed exprs) = case boxed of
        Boxed -> list Expression "(" ")" "," exprs
        Unboxed -> list Expression "(#" "#)" "," exprs

#if MIN_VERSION_haskell_src_exts(1,20,0)
    prettyPrint (UnboxedSum _ before after expr) =
        withLayout cfgLayoutUnboxedSum flex vertical
      where
        flex = group Expression "(#" "#)" $ do
           replicateM_ (before - 1) (withOperatorFormatting Expression "unboxed-alt" (write "|") id)
           when (before > 0) $
              withOperatorFormatting Expression "unboxed-alt-present" (write "|") id
           pretty expr
           replicateM_ after (withOperatorFormatting Expression "unboxed-alt" (write "|") id)

        vertical = groupV Expression "(#" "#)" $ do
           replicateM_ (before - 1) (withOperatorFormattingV Expression "unboxed-alt" (write "|") id)
           when (before > 0) $
              withOperatorFormattingV Expression "unboxed-alt-present" (write "|") id
           pretty expr
           replicateM_ after (withOperatorFormattingV Expression "unboxed-alt" (write "|") id)
#endif

#if MIN_VERSION_haskell_src_exts(1,23,0)
    prettyPrint (ArrOp _ expr) = group Expression "(|" "|)" $ pretty expr
#endif

    prettyPrint (TupleSection _ boxed mexprs) = case boxed of
        Boxed -> list Expression "(" ")" "," $ map (MayAst noNodeInfo) mexprs
        Unboxed -> list Expression "(#" "#)" "," $
            map (MayAst noNodeInfo) mexprs

    prettyPrint (List _ exprs) =
        withLayout cfgLayoutList flex vertical
      where
        flex = group Expression "[" "]" $
            list' Expression "," exprs

        vertical = do
          mcompactVerticalList <- getConfig (cfgOptionCompactVerticalList . cfgOptions)
          case mcompactVerticalList of
              Nothing ->
                groupV Expression "[" "]" $
                  listV' Expression "," exprs
              Just (compactZero, compactOne) -> do
                let
                  groupFn = case exprs of
                      [] | compactZero -> withGroupH Expression "compact-list"
                      [_] | compactOne -> withGroupH Expression "compact-list"
                      _ -> groupV Expression
                groupFn "[" "]" $
                  listV' Expression "," exprs


    prettyPrint (ParArray _ exprs) = list Expression "[:" ":]" "," exprs

    prettyPrint (Paren _ expr) = parens $ pretty expr

    prettyPrint (LeftSection _ expr qop) = parens $ do
        pretty expr
        operatorSectionL Expression (opName qop) $ prettyHSE qop

    prettyPrint (RightSection _ qop expr) = parens $ do
        operatorSectionR Expression (opName qop) $ prettyHSE qop
        pretty expr

    prettyPrint (RecConstr _ qname fieldupdates) =
        within RecordDeclaration $
            prettyRecord len Expression qname fieldupdates
      where
        len (FieldUpdate _ n _) = measure $ pretty n
        len (FieldPun _ n) = measure $ pretty n
        len (FieldWildcard _) = measure $ write ".."

    prettyPrint (RecUpdate _ expr fieldupdates) =
        within RecordDeclaration $
            prettyRecord len Expression expr fieldupdates
      where
        len (FieldUpdate _ n _) = measure $ pretty n
        len (FieldPun _ n) = measure $ pretty n
        len (FieldWildcard _) = measure $ write ".."

    prettyPrint (EnumFrom _ expr) = withGroup Expression "enum_bracket" "[" "]" $ do
        pretty expr
        operatorSectionL Expression "enum_spread" $ write ".."

    prettyPrint (EnumFromTo _ expr expr') = withGroup Expression "enum_bracket" "[" "]" $ do
        pretty expr
        withOperatorFormatting Expression "enum_spread" (write "..") id
        pretty expr'

    prettyPrint (EnumFromThen _ expr expr') = withGroup Expression "enum_bracket" "[" "]" $ do
        pretty expr
        comma
        pretty expr'
        operatorSectionL Expression "enum_spread" $ write ".."

    prettyPrint (EnumFromThenTo _ expr expr' expr'') =
        withGroup Expression "enum_bracket" "[" "]" $ do
            pretty expr
            comma
            pretty expr'
            withOperatorFormatting Expression "enum_spread" (write "..") id
            pretty expr''

    prettyPrint (ParArrayFromTo _ expr expr') = withGroup Expression "parray_bracket" "[:" ":]" $ do
        pretty expr
        withOperatorFormatting Expression "enum_spread" (write "..") id
        pretty expr'

    prettyPrint (ParArrayFromThenTo _ expr expr' expr'') =
        withGroup Expression "parray_bracket" "[:" ":]" $ do
            pretty expr
            comma
            pretty expr'
            withOperatorFormatting Expression "enum_spread" (write "..") id
            pretty expr''

    prettyPrint (ListComp _ expr qualstmts) =
        within ComprehensionDeclaration $ withLayout cfgLayoutListComp flex vertical
      where
        flex = group Expression "[" "]" $ do
            prettyOnside expr
            operator Expression "|"
            list' Expression "," qualstmts

        vertical = groupV Expression "[" "]" $ do
            prettyOnside expr
            operatorV Expression "|"
            listV' Expression "," qualstmts

    prettyPrint (ParComp _ expr qualstmtss) =
        within ComprehensionDeclaration $ withLayout cfgLayoutListComp flex vertical
      where
        flex = group Expression "[" "]" $ do
            prettyOnside expr
            forM_ qualstmtss $ \qualstmts -> cut $ do
                operator Expression "|"
                list' Expression "," qualstmts

        vertical = groupV Expression "[" "]" $ do
            prettyOnside expr
            forM_ qualstmtss $ \qualstmts -> cut $ do
                operatorV Expression "|"
                listV' Expression "," qualstmts

    prettyPrint (ParArrayComp _ expr qualstmtss) =
        within ComprehensionDeclaration $ withLayout cfgLayoutListComp flex vertical
      where
        flex = group Expression "[:" ":]" $ do
            prettyOnside expr
            forM_ qualstmtss $ \qualstmts -> cut $ do
                operator Expression "|"
                list' Expression "," qualstmts

        vertical = groupV Expression "[:" ":]" $ do
            prettyOnside expr
            forM_ qualstmtss $ \qualstmts -> cut $ do
                operatorV Expression "|"
                listV' Expression "," qualstmts

    prettyPrint (ExpTypeSig _ expr typ) = prettyTypesig Expression [ expr ] typ

    prettyPrint (VarQuote _ qname) = do
        write "'"
        pretty qname

    prettyPrint (TypQuote _ qname) = do
        write "''"
        pretty qname

    prettyPrint (BracketExp _ bracket) = pretty bracket

    prettyPrint (SpliceExp _ splice) = pretty splice

    prettyPrint (QuasiQuote _ str str') = do
        write "["
        string str
        write "|"
        string str'
        write "|]"

    prettyPrint (TypeApp _ typ) = do
        write "@"
        pretty typ

    prettyPrint (XTag _ xname xattrs mexpr exprs) = do
        write "<"
        pretty xname
        forM_ xattrs $ withPrefix space pretty
        mayM_ mexpr $ withPrefix space pretty
        write ">"
        mapM_ pretty exprs
        write "</"
        pretty xname
        write ">"

    prettyPrint (XETag _ xname xattrs mexpr) = do
        write "<"
        pretty xname
        forM_ xattrs $ withPrefix space pretty
        mayM_ mexpr $ withPrefix space pretty
        write "/>"

    prettyPrint (XPcdata _ str) = string str

    prettyPrint (XExpTag _ expr) = do
        write "<% "
        pretty expr
        write " %>"

    prettyPrint (XChildTag _ exprs) = do
        write "<%>"
        inter space $ map pretty exprs
        write "</%>"

    prettyPrint (CorePragma _ str expr) = do
        prettyPragma "CORE" . string $ show str
        space
        pretty expr

    prettyPrint (SCCPragma _ str expr) = do
        prettyPragma "SCC" . string $ show str
        space
        pretty expr

    prettyPrint (GenPragma _ str (a, b) (c, d) expr) = do
        prettyPragma "GENERATED" $
            inter space
                  [ string $ show str
                  , int a
                  , write ":"
                  , int b
                  , write "-"
                  , int c
                  , write ":"
                  , int d
                  ]
        space
        pretty expr

    prettyPrint (Proc _ pat expr) = do
        write "proc "
        pretty pat
        operator Expression "->"
        pretty expr

    prettyPrint (LeftArrApp _ expr expr') = do
        pretty expr
        operator Expression "-<"
        pretty expr'

    prettyPrint (RightArrApp _ expr expr') = do
        pretty expr
        operator Expression ">-"
        pretty expr'

    prettyPrint (LeftArrHighApp _ expr expr') = do
        pretty expr
        operator Expression "-<<"
        pretty expr'

    prettyPrint (RightArrHighApp _ expr expr') = do
        pretty expr
        operator Expression ">>-"
        pretty expr'

    prettyPrint (LCase _ alts) = flexibleOneline $ do
        write "\\case"
        if null alts
            then write " { }"
            else withIndent cfgIndentCase True $
                withComputedTabStop stopRhs cfgAlignCase measureAlt alts $
                lined alts

#if !MIN_VERSION_haskell_src_exts(1,20,0)
    prettyPrint (ExprHole _) = write "_"
#endif

instance Pretty Alt where
    prettyPrint (Alt _ pat rhs mbinds) = do
        onside $ do
            resetPatternLevel $ pretty pat
            atTabStop stopRhs
            pretty $ GuardedAlts rhs
        mapM_ prettyBinds mbinds

instance Pretty XAttr where
    prettyPrint (XAttr _ xname expr) = do
        pretty xname
        operator Expression "="
        pretty expr

instance Pretty Pat where
    prettyPrint pa = nestPatternLevel $ prettyInner pa
      where
        prettyInner (PVar _ name) = pretty name

        prettyInner (PLit _ sign literal) = do
            case sign of
                Signless _ -> return ()
                Negative _ -> write "-"
            pretty literal

        prettyInner (PNPlusK _ name integer) = do
            pretty name
            operator Pattern "+"
            int $ fromIntegral integer

        prettyInner p@(PInfixApp _ _ qname _) =
            prettyInfixApp opName Pattern $ flattenInfix flattenPInfixApp p
          where
            flattenPInfixApp (PInfixApp _ lhs qname' rhs) =
                if compareAST qname qname' == EQ
                then Just (lhs, QConOp noNodeInfo qname', rhs)
                else Nothing
            flattenPInfixApp _ = Nothing

        prettyInner (PApp _ qname pats) = prettyApp' cfgLayoutPatternApp cfgIndentPatternApp True qname pats

        prettyInner (PTuple _ boxed pats) = do
          withinDeclaration <- gets psWithinDeclaration
          lvl <- gets psPatternNestLevel
          let listy = case (withinDeclaration, lvl) of
                        (CaseDeclaration, 1) -> listH
                        _                    -> list
          case boxed of
            Boxed -> listy Pattern "(" ")" "," pats
            Unboxed -> listy Pattern "(#" "#)" "," pats

#if MIN_VERSION_haskell_src_exts(1,20,0)
        prettyInner (PUnboxedSum _ before after pat) = group Pattern "(#" "#)"
            . inter space $ replicate before (write "|") ++ [ pretty pat ]
            ++ replicate after (write "|")
#endif

        prettyInner (PList _ pats) = do
            withinDeclaration <- gets psWithinDeclaration
            lvl <- gets psPatternNestLevel
            let listy = case (withinDeclaration, lvl) of
                          (CaseDeclaration, 1) -> listH
                          _                    -> list
            listy Pattern "[" "]" "," pats

        prettyInner (PParen _ pat) = parens $ pretty pat

        prettyInner (PRec _ qname patfields) = do
            withinDeclaration <- gets psWithinDeclaration
            lvl <- gets psPatternNestLevel
            let listy = case (withinDeclaration, lvl) of
                          (CaseDeclaration, 1) -> listH
                          _                    -> list
            withOperatorFormatting Pattern "record" (pretty qname) id
            listy Pattern "{" "}" "," patfields

        prettyInner (PAsPat _ name pat) = do
            pretty name
            operator Pattern "@"
            pretty pat

        prettyInner (PWildCard _) = write "_"

        prettyInner (PIrrPat _ pat) = do
            write "~"
            pretty pat

        prettyInner (PatTypeSig _ pat ty) = prettyTypesig Pattern [ pat ] ty

        prettyInner (PViewPat _ expr pat) = do
            pretty expr
            operator Pattern "->"
            pretty pat

        prettyInner (PRPat _ rpats) = do
            withinDeclaration <- gets psWithinDeclaration
            lvl <- gets psPatternNestLevel
            let listy = case (withinDeclaration, lvl) of
                          (CaseDeclaration, 1) -> listH
                          _                    -> list
            listy Pattern "[" "]" "," rpats

        prettyInner (PXTag _ xname pxattrs mpat pats) = do
            write "<"
            pretty xname
            forM_ pxattrs $ withPrefix space pretty
            mayM_ mpat $ withPrefix space pretty
            write ">"
            mapM_ pretty pats
            write "<"
            pretty xname
            write ">"

        prettyInner (PXETag _ xname pxattrs mpat) = do
            write "<"
            pretty xname
            forM_ pxattrs $ withPrefix space pretty
            mayM_ mpat $ withPrefix space pretty
            write "/>"

        prettyInner (PXPcdata _ str) = string str

        prettyInner (PXPatTag _ pat) = do
            write "<%"
            pretty pat
            write "%>"

        prettyInner (PXRPats _ rpats) = do
            write "<["
            inter space $ map pretty rpats
            write "%>"

#if MIN_VERSION_haskell_src_exts(1,20,0)
        prettyInner (PSplice _ splice) = pretty splice
#endif

        prettyInner (PQuasiQuote _ str str') = do
            write "[$"
            string str
            write "|"
            string str'
            write "|]"

        prettyInner (PBangPat _ pat) = do
            write "!"
            pretty pat

instance Pretty PatField where
    prettyPrint (PFieldPat _ qname pat) = do
        pretty qname
        operator Pattern "="
        pretty pat

    prettyPrint (PFieldPun _ qname) = pretty qname

    prettyPrint (PFieldWildcard _) = write ".."

instance Pretty PXAttr where
    prettyPrint (PXAttr _ xname pat) = do
        pretty xname
        operator Pattern "="
        pretty pat

instance Pretty Literal where
    prettyPrint (Char _ _ str) = do
        write "'"
        string str
        write "'"

    prettyPrint (String _ _ str) = do
        write "\""
        string str
        write "\""

    prettyPrint (Int _ _ str) = string str

    prettyPrint (Frac _ _ str) = string str

    prettyPrint (PrimInt _ _ str) = do
        string str
        write "#"

    prettyPrint (PrimWord _ _ str) = do
        string str
        write "##"

    prettyPrint (PrimFloat _ _ str) = do
        string str
        write "#"

    prettyPrint (PrimDouble _ _ str) = do
        string str
        write "##"

    prettyPrint (PrimChar _ _ str) = do
        write "'"
        string str
        write "'#"

    prettyPrint (PrimString _ _ str) = do
        write "\""
        string str
        write "\"#"

instance Pretty QualStmt where
    prettyPrint (QualStmt _ stmt) = pretty stmt

    prettyPrint (ThenTrans _ expr) = do
        write "then "
        pretty expr

    prettyPrint (ThenBy _ expr expr') = do
        write "then "
        pretty expr
        write " by "
        pretty expr'

    prettyPrint (GroupBy _ expr) = do
        write "then group by "
        pretty expr

    prettyPrint (GroupUsing _ expr) = do
        write "then group using "
        pretty expr

    prettyPrint (GroupByUsing _ expr expr') = do
        write "then group by "
        pretty expr
        write " using "
        pretty expr'

instance Pretty Stmt where
    prettyPrint (Generator _ pat expr) = do
        pretty pat
        atTabStop stopDoLeftArrow
        operator Expression "<-"
        pretty expr

    -- Special case for If in Do,
    prettyPrint (Qualifier _ expr@If{}) = do
        cfg <- getConfig (cfgIndentIf . cfgIndent)
        case cfg of
            Align -> do
                write ""
                indented $ pretty expr
            _ -> pretty expr

    prettyPrint (Qualifier _ expr) = pretty expr

    prettyPrint (LetStmt _ binds) = do
        operator Expression "let"
        pretty $ CompactBinds binds

    prettyPrint (RecStmt _ stmts) = do
        write "rec "
        aligned $ linedOnside stmts

instance Pretty FieldUpdate where
    prettyPrint (FieldUpdate _ qname expr) = do
        pretty qname
        atTabStop stopRecordField
        operator Expression "="
        pretty expr

    prettyPrint (FieldPun _ qname) = pretty qname

    prettyPrint (FieldWildcard _) = write ".."

instance Pretty QOp where
    prettyPrint qop =
        withOperatorFormatting Expression (opName qop) (prettyHSE qop) id

instance Pretty Op where
    prettyPrint (VarOp l name) = prettyPrint (QVarOp l (UnQual noNodeInfo name))
    prettyPrint (ConOp l name) = prettyPrint (QConOp l (UnQual noNodeInfo name))

instance Pretty Bracket where
    prettyPrint (ExpBracket _ expr) = group Expression "[|" "|]" $ pretty expr

#if MIN_VERSION_haskell_src_exts(1,22,0)
    prettyPrint (TExpBracket _ expr) = group Expression "[||" "||]" $ pretty expr
#endif

    prettyPrint (PatBracket _ pat) = group Expression "[p|" "|]" $ pretty pat

    prettyPrint (TypeBracket _ ty) = group Expression "[t|" "|]" $ pretty ty

    prettyPrint (DeclBracket _ decls) =
        group Expression "[d|" "|]" . aligned $ lined decls

instance Pretty Splice where
    prettyPrint (IdSplice _ str) = do
        write "$"
        string str

    prettyPrint (ParenSplice _ expr) = group Expression "$(" ")" $ pretty expr

#if MIN_VERSION_haskell_src_exts(1,22,0)
    prettyPrint (TIdSplice _ str) = do
        write "$$"
        string str

    prettyPrint (TParenSplice _ expr) = group Expression "$$(" ")" $ pretty expr

#endif

instance Pretty ModulePragma where
    prettyPrint (LanguagePragma _ names) =
        prettyPragma "LANGUAGE" . inter comma $ map pretty names

    prettyPrint (OptionsPragma _ mtool str) = prettyPragma name $
        string (trim str)
      where
        name = case mtool of
            Just tool -> "OPTIONS_" `mappend` BS8.pack (HSE.prettyPrint tool)
            Nothing -> "OPTIONS"

        trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

    prettyPrint (AnnModulePragma _ annotation) =
        prettyPragma "ANN" $ pretty annotation

instance Pretty Rule where
    prettyPrint (Rule _ str mactivation mrulevars expr expr') = do
        string (show str)
        space
        mayM_ mactivation $ withPostfix space pretty
        mapM_ prettyForall mrulevars
        pretty expr
        operator Expression "="
        pretty expr'

instance Pretty RuleVar where
    prettyPrint (RuleVar _ name) = pretty name

    prettyPrint (TypedRuleVar _ name ty) =
        parens $ prettyTypesig Declaration [ name ] ty

instance Pretty Activation where
    prettyPrint (ActiveFrom _ pass) = brackets $ int pass

    prettyPrint (ActiveUntil _ pass) = brackets $ do
        write "~"
        int pass

instance Pretty Annotation where
    prettyPrint (Ann _ name expr) = do
        pretty name
        space
        pretty expr

    prettyPrint (TypeAnn _ name expr) = do
        write "type "
        pretty name
        space
        pretty expr

    prettyPrint (ModuleAnn _ expr) = do
        write "module "
        pretty expr

instance Pretty BooleanFormula where
    prettyPrint (VarFormula _ name) = pretty name

    prettyPrint (AndFormula _ booleanformulas) =
        inter comma $ map pretty booleanformulas

    prettyPrint (OrFormula _ booleanformulas) =
        inter (operator Expression "|") $ map pretty booleanformulas

    prettyPrint (ParenFormula _ booleanformula) = parens $ pretty booleanformula

-- Stick with HSE
#if MIN_VERSION_haskell_src_exts(1,20,0)
instance Pretty DerivStrategy
#endif

instance Pretty DataOrNew

instance Pretty BangType

instance Pretty Unpackedness

instance Pretty RPat

instance Pretty ModuleName

instance Pretty QName

instance Pretty Name

instance Pretty IPName

instance Pretty XName

instance Pretty Safety

instance Pretty CallConv

instance Pretty Overlap

-- Helpers
newtype GuardedAltR l = GuardedAltR (GuardedRhs l)
    deriving ( Functor, Annotated )

newtype GuardedAlt l = GuardedAlt (GuardedRhs l)
    deriving ( Functor, Annotated )

instance Pretty GuardedAltR where
    prettyPrint (GuardedAltR (GuardedRhs _ stmts expr)) = cut $ do
        operatorSectionR Pattern "|" $ write "|"
        inter comma $ map pretty stmts
        operator Expression "->"
        pretty expr

instance Pretty GuardedAlt where
    prettyPrint (GuardedAlt (GuardedRhs _ stmts expr)) = cut $ do
        operatorSection Pattern "|" $ write "|"
        inter comma $ map pretty stmts
        operator Expression "->"
        pretty expr

newtype GuardedAlts l = GuardedAlts (Rhs l)
    deriving ( Functor, Annotated )

instance Pretty GuardedAlts where
    prettyPrint (GuardedAlts (UnGuardedRhs _ expr)) = cut $ do
        operator Expression "->"
        pretty expr

    prettyPrint (GuardedAlts (GuardedRhss _ guardedrhss)) = do
        multiIfPadding <- getConfig (cfgOptionMultiIfPadding . cfgOptions)
        withIndent cfgIndentMultiIf multiIfPadding $ linedOnside $ map GuardedAlt guardedrhss

newtype CompactBinds l = CompactBinds (Binds l)
    deriving ( Functor, Annotated )

instance Pretty CompactBinds where
    prettyPrint (CompactBinds (BDecls _ decls)) = aligned $
        withComputedTabStop stopRhs cfgAlignLetBinds measureDecl decls $
        lined decls
    prettyPrint (CompactBinds (IPBinds _ ipbinds)) =
        aligned $ linedOnside ipbinds

data MayAst a l = MayAst l (Maybe (a l))

instance Functor a => Functor (MayAst a) where
    fmap f (MayAst l x) = MayAst (f l) (fmap (fmap f) x)

instance Annotated a => Annotated (MayAst a) where
    ann (MayAst l x) = maybe l ann x

    amap f (MayAst l x) = MayAst (f l) (fmap (amap f) x)

instance (Annotated a, Pretty a) => Pretty (MayAst a) where
    prettyPrint (MayAst _ x) = mapM_ pretty x

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
