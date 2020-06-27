{-# LANGUAGE OverloadedStrings #-}

module Floskell.Printers
    ( getConfig
    , getOption
    , cut
    , oneline
    , ignoreOneline
      -- * Basic printing
    , write
    , string
    , int
    , space
    , newline
    , blankline
    , spaceOrNewline
      -- * Tab stops
    , withTabStops
    , atTabStop
      -- * Combinators
    , mayM_
    , withPrefix
    , withPostfix
    , withIndentConfig
    , withIndent
    , withIndentFlex
    , withIndentAfter
    , withIndentBy
    , withLayout
    , withinDeclToLayout
    , inter
      -- * Indentation
    , getNextColumn
    , column
    , column'
    , aligned
    , indented
    , onside
    , depend
    , depend'
    , parens
    , brackets
      -- * Wrapping
    , group
    , groupH
    , groupV
    , withGroup
    , withGroupH
    , withGroupV
      -- * Operators
    , operator
    , operatorH
    , operatorV
    , alignOnOperator
    , withOperatorFormatting
    , withOperatorFormattingH
    , withOperatorFormattingV
    , operatorSection
    , operatorSectionL
    , operatorSectionR
    , comma
    ) where

import           Control.Applicative        ( (<|>) )
import           Control.Monad              ( guard, unless, when )
import           Control.Monad.Search       ( cost, winner )
import           Control.Monad.State.Strict ( get, gets, modify )

import           Data.ByteString            ( ByteString )
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL
import           Data.List                  ( intersperse )
import qualified Data.Map.Strict            as Map

import qualified Floskell.Buffer            as Buffer
import           Floskell.Config
import           Floskell.Types

-- | Query part of the pretty printer config
getConfig :: (Config -> b) -> Printer b
getConfig f = f <$> gets psConfig

-- | Query pretty printer options
getOption :: (OptionConfig -> a) -> Printer a
getOption f = getConfig (f . cfgOptions)

-- | Line penalty calculation
linePenalty :: Bool -> Int -> Printer Penalty
linePenalty eol col = do
    indentLevel <- gets psIndentLevel
    config <- getConfig cfgPenalty
    let maxcol = penaltyMaxLineLength config
    let pLinebreak = onlyIf eol $ penaltyLinebreak config
    let pIndent = indentLevel * penaltyIndent config
    let pOverfull = onlyIf (col > maxcol) $ penaltyOverfull config
            * (col - maxcol) + penaltyOverfullOnce config
    return . Penalty $ pLinebreak + pIndent + pOverfull
  where
    onlyIf cond penalty = if cond then penalty else 0

-- | Try only the first (i.e. locally best) solution to the given
-- pretty printer.  Use this function to improve performance whenever
-- the formatting of an AST node has no effect on the penalty of any
-- following AST node, such as top-level declarations or case
-- branches.
cut :: Printer a -> Printer a
cut = winner

closeEolComment :: Printer ()
closeEolComment = do
    eol <- gets psEolComment
    when eol newline

withOutputRestriction :: OutputRestriction -> Printer a -> Printer a
withOutputRestriction r p = do
    orig <- gets psOutputRestriction
    modify $ \s -> s { psOutputRestriction = r }
    result <- p
    modify $ \s -> s { psOutputRestriction = orig }
    return result

oneline :: Printer a -> Printer a
oneline p = do
    closeEolComment
    withOutputRestriction NoOverflowOrLinebreak p

ignoreOneline :: Printer a -> Printer a
ignoreOneline = withOutputRestriction Anything

-- | Write out a string, updating the current position information.
write :: ByteString -> Printer ()
write x = do
    closeEolComment
    write' x
  where
    write' x' = do
        state <- get
        let indentLevel = psIndentLevel state
            out = if psNewline state
                  then BS.replicate indentLevel 32 <> x'
                  else x'
            buffer = psBuffer state
            newCol = Buffer.column buffer + BS.length out
        guard $ psOutputRestriction state == Anything || newCol
            < penaltyMaxLineLength (cfgPenalty (psConfig state))
        penalty <- linePenalty False newCol
        when (penalty /= mempty) $ cost mempty penalty
        modify (\s ->
                s { psBuffer = Buffer.write out buffer, psEolComment = False })

-- | Write a string.
string :: String -> Printer ()
string = write . BL.toStrict . BB.toLazyByteString . BB.stringUtf8

-- | Write an integral.
int :: Int -> Printer ()
int = string . show

-- | Write a space.
space :: Printer ()
space = do
    comment <- gets psEolComment
    unless comment $ write " "

-- | Output a newline.
newline :: Printer ()
newline = do
    modify (\s ->
            s { psIndentLevel = psIndentLevel s + psOnside s, psOnside = 0 })
    state <- get
    guard $ psOutputRestriction state /= NoOverflowOrLinebreak
    penalty <- linePenalty True (psColumn state)
    when (penalty /= mempty) $ cost penalty mempty
    modify (\s -> s { psBuffer     = Buffer.newline (psBuffer state)
                    , psEolComment = False
                    })

blankline :: Printer ()
blankline = newline >> newline

spaceOrNewline :: Printer ()
spaceOrNewline = space <|> newline

withTabStops :: [(TabStop, Maybe Int)] -> Printer a -> Printer a
withTabStops stops p = do
    col <- getNextColumn
    oldstops <- gets psTabStops
    modify $ \s ->
        s { psTabStops =
                foldr (\(k, v) -> Map.alter (const $ fmap (col +) v) k)
                      (psTabStops s)
                      stops
          }
    res <- p
    modify $ \s -> s { psTabStops = oldstops }
    return res

atTabStop :: TabStop -> Printer ()
atTabStop tabstop = do
    mstop <- gets (Map.lookup tabstop . psTabStops)
    mayM_ mstop $ \stop -> do
        col <- getNextColumn
        let padding = max 0 (stop - col)
        write (BS.replicate padding 32)

mayM_ :: Maybe a -> (a -> Printer ()) -> Printer ()
mayM_ Nothing _ = return ()
mayM_ (Just x) p = p x

withPrefix :: Applicative f => f a -> (x -> f b) -> x -> f b
withPrefix pre f x = pre *> f x

withPostfix :: Applicative f => f a -> (x -> f b) -> x -> f b
withPostfix post f x = f x <* post

withIndentConfig
    :: (IndentConfig -> Indent) -> Printer a -> (Int -> Printer a) -> Printer a
withIndentConfig fn align indentby = do
    cfg <- getConfig (fn . cfgIndent)
    case cfg of
        Align -> align
        IndentBy i -> indentby i
        AlignOrIndentBy i -> align <|> indentby i

withIndent :: (IndentConfig -> Indent) -> Bool -> Printer a -> Printer a
withIndent fn pad p = withIndentConfig fn align indentby
  where
    align = do
        space
        aligned p

    indentby i = indented i $ do
        when pad newline
        p

withIndentFlex :: (IndentConfig -> Indent) -> Printer a -> Printer a
withIndentFlex fn p = withIndentConfig fn align indentby
  where
    align = do
        space
        aligned p

    indentby i = indented i $ do
        spaceOrNewline
        p

withIndentAfter
    :: (IndentConfig -> Indent) -> Printer () -> Printer a -> Printer a
withIndentAfter fn before p = withIndentConfig fn align indentby
  where
    align = aligned $ do
        withIndentation id before
        p

    indentby i = do
        withIndentation id before
        indented i p

withIndentBy :: (IndentConfig -> Int) -> Bool -> Printer a -> Printer a
withIndentBy fn = withIndent (IndentBy . fn)

withLayout :: (LayoutConfig -> Layout) -> Printer a -> Printer a -> Printer a
withLayout fn flex vertical = do
    cfg <- getConfig (fn . cfgLayout)
    case cfg of
        Flex -> flex
        Vertical -> vertical
        TryOneline -> oneline flex <|> vertical

withinDeclToLayout :: WithinDeclaration -> WithinLayout -> Layout
withinDeclToLayout ModuleDeclaration = wlModuleLayout
withinDeclToLayout RecordDeclaration = wlRecordLayout
withinDeclToLayout GADTDeclaration = wlGADTLayout
withinDeclToLayout GADTFieldDeclaration = wlGADTFieldLayout
withinDeclToLayout GADTFieldTypeDeclaration = wlGADTFieldTypeLayout
withinDeclToLayout TypeDeclaration = wlTypeLayout
withinDeclToLayout SpecialDeclaration = wlSpecialLayout
withinDeclToLayout ComprehensionDeclaration = wlComprehensionLayout
withinDeclToLayout PatternDeclaration = wlPatternLayout
withinDeclToLayout GuardDeclaration = wlGuardLayout
withinDeclToLayout OtherDeclaration = wlOtherLayout

inter :: Printer () -> [Printer ()] -> Printer ()
inter x = sequence_ . intersperse x

-- | Get the column for the next printed character.
getNextColumn :: Printer Int
getNextColumn = do
    st <- get
    return $ if psEolComment st
             then psIndentLevel st + psOnside st
             else max (psColumn st) (psIndentLevel st)

withIndentation :: ((Int, Int) -> (Int, Int)) -> Printer a -> Printer a
withIndentation f p = do
    prevIndent <- gets psIndentLevel
    prevOnside <- gets psOnside
    let (newIndent, newOnside) = f (prevIndent, prevOnside)
    modify (\s -> s { psIndentLevel = newIndent, psOnside = newOnside })
    r <- p
    modify (\s -> s { psIndentLevel = prevIndent, psOnside = prevOnside })
    return r

-- | Set the (newline-) indent level to the given column for the given
-- printer.
column :: Int -> Printer a -> Printer a
column i = withIndentation $ \(pri, o) -> (i, if i > pri then 0 else o)

column' :: Int -> Printer a -> Printer a
column' i = withIndentation $ \(_pri, _o) -> (i, 0)

aligned :: Printer a -> Printer a
aligned p = do
    col <- getNextColumn
    column col p

-- | Increase indentation level by n spaces for the given printer.
indented :: Int -> Printer a -> Printer a
indented i p = do
    level <- gets psIndentLevel
    column (level + i) p

-- | Increase indentation level b n spaces for the given printer, but
-- ignore increase when computing further indentations.
onside :: Printer a -> Printer a
onside p = do
    closeEolComment
    onsideIndent <- getConfig (cfgIndentOnside . cfgIndent)
    withIndentation (\(l, _) -> (l, onsideIndent)) p

depend :: ByteString -> Printer a -> Printer a
depend kw = depend' (write kw)

depend' :: Printer () -> Printer a -> Printer a
depend' kw p = do
    i <- getConfig (cfgIndentOnside . cfgIndent)
    kw
    space
    indented i p

-- | Wrap in parens.
parens :: Printer () -> Printer ()
parens p = do
    write "("
    aligned $ do
        p
        write ")"

-- | Wrap in brackets.
brackets :: Printer () -> Printer ()
brackets p = do
    write "["
    aligned $ do
        p
        write "]"

withGroup :: LayoutContext -> ByteString -> ByteString -> ByteString -> Printer () -> Printer ()
withGroup ctx op open close p = do
    withinDeclaration <- gets psWithinDeclaration
    force <- getConfig (wsForceLinebreak . cfgGroupWs' ctx (Just withinDeclaration) op . cfgGroup)
    if force then vert else oneline hor <|> vert
  where
    hor = withGroupH ctx op open close p

    vert = withGroupV ctx op open close p

withGroupH :: LayoutContext -> ByteString -> ByteString -> ByteString -> Printer () -> Printer ()
withGroupH ctx op open close p = do
    withinDeclaration <- gets psWithinDeclaration
    ws <- getConfig (cfgGroupWs' ctx (Just withinDeclaration) op . cfgGroup)
    write open
    when (wsSpace Before ws) space
    p
    when (wsSpace After ws) space
    write close

withGroupV :: LayoutContext -> ByteString -> ByteString -> ByteString -> Printer () -> Printer ()
withGroupV ctx op open close p = aligned $ do
    withinDeclaration <- gets psWithinDeclaration
    ws <- getConfig (cfgGroupWs' ctx (Just withinDeclaration) op . cfgGroup)
    col <- getNextColumn
    write open
    if wsLinebreak Before ws then newline >> when (wsSpace Before ws) space else when (wsSpace Before ws) space
    p
    if wsLinebreak After ws then when (wsSpace After ws) space >> newline else when (wsSpace After ws) space
    column col $ write close

group :: LayoutContext -> ByteString -> ByteString -> Printer () -> Printer ()
group ctx open = withGroup ctx open open

groupH :: LayoutContext -> ByteString -> ByteString -> Printer () -> Printer ()
groupH ctx open =  withGroupH ctx open open

groupV :: LayoutContext -> ByteString -> ByteString -> Printer () -> Printer ()
groupV ctx open = withGroupV ctx open open

operator :: LayoutContext -> ByteString -> Printer ()
operator ctx op = withOperatorFormatting ctx op (write op) id

operatorH :: LayoutContext -> ByteString -> Printer ()
operatorH ctx op = withOperatorFormattingH ctx op (write op) id

operatorV :: LayoutContext -> ByteString -> Printer ()
operatorV ctx op = withOperatorFormattingV ctx op (write op) id

alignOnOperator :: LayoutContext -> ByteString -> Printer a -> Printer a
alignOnOperator ctx op p =
    withOperatorFormatting ctx op (write op) (aligned . (*> p))

withOperatorFormatting :: LayoutContext
                       -> ByteString
                       -> Printer ()
                       -> (Printer () -> Printer a)
                       -> Printer a
withOperatorFormatting ctx op opp fn = do
    withinDeclaration <- gets psWithinDeclaration
    force <- getConfig (wsForceLinebreak . cfgOpWs' ctx (Just withinDeclaration) op . cfgOp)
    if force then vert else hor <|> vert
  where
    hor = withOperatorFormattingH ctx op opp fn

    vert = withOperatorFormattingV ctx op opp fn

withOperatorFormattingH :: LayoutContext
                        -> ByteString
                        -> Printer ()
                        -> (Printer () -> Printer a)
                        -> Printer a
withOperatorFormattingH ctx op opp fn = do
    withinDeclaration <- gets psWithinDeclaration
    ws <- getConfig (cfgOpWs' ctx (Just withinDeclaration) op . cfgOp)
    when (wsSpace Before ws) space
    fn $ do
        opp
        when (wsSpace After ws) space

withOperatorFormattingV :: LayoutContext
                        -> ByteString
                        -> Printer ()
                        -> (Printer () -> Printer a)
                        -> Printer a
withOperatorFormattingV ctx op opp fn = do
    withinDeclaration <- gets psWithinDeclaration
    ws <- getConfig (cfgOpWs' ctx (Just withinDeclaration) op . cfgOp)
    if wsLinebreak Before ws then newline >> when (wsSpace Before ws) space else when (wsSpace Before ws) space
    fn $ do
        opp
        if wsLinebreak After ws then when (wsSpace After ws) space >> newline else when (wsSpace After ws) space

operatorSection :: LayoutContext -> ByteString -> Printer () -> Printer ()
operatorSection ctx op opp = do
    withinDeclaration <- gets psWithinDeclaration
    ws <- getConfig (cfgOpWs' ctx (Just withinDeclaration) op . cfgOp)
    when (wsSpace Before ws) space
    opp
    when (wsSpace After ws) space

operatorSectionL :: LayoutContext -> ByteString -> Printer () -> Printer ()
operatorSectionL ctx op opp = do
    withinDeclaration <- gets psWithinDeclaration
    ws <- getConfig (cfgOpWs' ctx (Just withinDeclaration) op . cfgOp)
    when (wsSpace Before ws) space
    opp

operatorSectionR :: LayoutContext -> ByteString -> Printer () -> Printer ()
operatorSectionR ctx op opp = do
    withinDeclaration <- gets psWithinDeclaration
    ws <- getConfig (cfgOpWs' ctx (Just withinDeclaration) op . cfgOp)
    opp
    when (wsSpace After ws) space

comma :: Printer ()
comma = operator Expression ","

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
