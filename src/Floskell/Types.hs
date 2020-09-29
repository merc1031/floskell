{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | All types.
module Floskell.Types
    ( OutputRestriction(..)
    , TypeLayout(..)
    , Penalty(..)
    , TabStop(..)
    , Printer(..)
    , execPrinter
    , runPrinter
    , PrintState(..)
    , psLine
    , psColumn
    , psNewline
    , within
    , nestTypeLevel
    , nestPatternLevel
    , resetPatternLevel
    , initialPrintState
    , Config(..)
    , SrcSpan(..)
    , CommentType(..)
    , Comment(..)
    , NodeInfo(..)
    , noNodeInfo
    , nodeSpan
    , Location(..)
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Search
                 ( MonadSearch, Search, runSearchBest )
import           Control.Monad.State.Strict
                 ( MonadState(..), StateT, execStateT, gets, modify', runStateT )

import qualified Data.Map.Strict              as Map
import           Data.Semigroup               as Sem

import           Floskell.Buffer              ( Buffer )
import qualified Floskell.Buffer              as Buffer
import           Floskell.Config              ( Config(..), Location(..), WithinDeclaration(OtherDeclaration) )

import           Language.Haskell.Exts.SrcLoc ( SrcSpan(..), mkSrcSpan, noLoc )
import           Language.Haskell.Exts.Syntax ( Annotated(..) )

data OutputRestriction = Anything | NoOverflow | NoOverflowOrLinebreak
    deriving ( Eq, Ord, Show )

data TypeLayout = TypeFree | TypeFlex | TypeVertical
    deriving ( Eq, Ord, Show )

newtype Penalty = Penalty Int
    deriving ( Eq, Ord, Num, Show )

newtype TabStop = TabStop String
    deriving ( Eq, Ord, Show )

instance Sem.Semigroup Penalty where
    (<>) = (+)

instance Monoid Penalty where
    mempty = 0

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

-- | A pretty printing monad.
newtype Printer a =
    Printer { unPrinter :: StateT PrintState (Search Penalty) a }
    deriving ( Applicative, Monad, Functor, MonadState PrintState
             , MonadSearch Penalty, MonadPlus, Alternative )

execPrinter :: Printer a -> PrintState -> Maybe (Penalty, PrintState)
execPrinter m s = runSearchBest $ execStateT (unPrinter m) s

runPrinter :: Printer a -> PrintState -> Maybe (Penalty, (a, PrintState))
runPrinter m s = runSearchBest $ runStateT (unPrinter m) s

-- | The state of the pretty printer.
data PrintState =
    PrintState { psBuffer :: !Buffer -- ^ Output buffer
               , psIndentLevel :: !Int -- ^ Current indentation level.
               , psOnside :: !Int -- ^ Extra indentation is necessary with next line break.
               , psTabStops :: !(Map.Map TabStop Int) -- ^ Tab stops for alignment.
               , psConfig :: !Config -- ^ Style definition.
               , psEolComment :: !Bool -- ^ An end of line comment has just been outputted.
               , psOutputRestriction :: !OutputRestriction
               , psTypeLayout :: !TypeLayout
               , psWithinDeclaration :: !WithinDeclaration
               , psTypeNestLevel :: !Int
               , psPatternNestLevel :: !Int
               }

psLine :: PrintState -> Int
psLine = Buffer.line . psBuffer

psColumn :: PrintState -> Int
psColumn = Buffer.column . psBuffer

psNewline :: PrintState -> Bool
psNewline = (== 0) . Buffer.column . psBuffer

initialPrintState :: Config -> PrintState
initialPrintState config =
    PrintState Buffer.empty 0 0 Map.empty config False Anything TypeFree OtherDeclaration 0 0

within :: WithinDeclaration -> Printer a -> Printer a
within w f = do
  oldWithin <- gets psWithinDeclaration
  modify' $ \s -> s { psWithinDeclaration = w }
  r <- f
  modify' $ \s -> s { psWithinDeclaration = oldWithin }
  pure r

nestTypeLevel :: Printer a -> Printer a
nestTypeLevel f = do
  lvl <- gets psTypeNestLevel
  modify' $ \s -> s { psTypeNestLevel = lvl + 1 }
  r <- f
  modify' $ \s -> s { psTypeNestLevel = lvl }
  pure r

nestPatternLevel :: Printer a -> Printer a
nestPatternLevel f = do
  lvl <- gets psPatternNestLevel
  modify' $ \s -> s { psPatternNestLevel = lvl + 1 }
  r <- f
  modify' $ \s -> s { psPatternNestLevel = lvl }
  pure r

resetPatternLevel :: Printer a -> Printer a
resetPatternLevel f = do
  lvl <- gets psPatternNestLevel
  modify' $ \s -> s { psPatternNestLevel = 0 }
  r <- f
  modify' $ \s -> s { psPatternNestLevel = lvl }
  pure r

data CommentType = InlineComment | LineComment | PreprocessorDirective
    deriving ( Show )

data Comment = Comment { commentType :: !CommentType
                       , commentSpan :: !SrcSpan
                       , commentText :: !String
                       }
    deriving ( Show )

-- | Information for each node in the AST.
data NodeInfo =
    NodeInfo { nodeInfoSpan :: !SrcSpan               -- ^ Location info from the parser.
             , nodeInfoLeadingComments :: ![Comment]  -- ^ Leading comments attached to this node.
             , nodeInfoTrailingComments :: ![Comment] -- ^ Trailing comments attached to this node.
             }
    deriving ( Show )

-- | Empty NodeInfo
noNodeInfo :: NodeInfo
noNodeInfo = NodeInfo (mkSrcSpan noLoc noLoc) [] []

nodeSpan :: Annotated ast => ast NodeInfo -> SrcSpan
nodeSpan = nodeInfoSpan . ann
