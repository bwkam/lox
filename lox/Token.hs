{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Token where

import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as Set
import Text.Megaparsec (PosState (PosState, pstateInput, pstateLinePrefix, pstateOffset, pstateSourcePos, pstateTabWidth), SourcePos (sourceLine), Stream (Tokens, chunkEmpty, chunkLength, chunkToTokens, take1_, takeN_, takeWhile_, tokenToChunk, tokensToChunk), Token, TraversableStream (reachOffset), VisualStream (showTokens, tokensLength))
import TokenType (TokenType (Tab))

data LoxTok = LoxTok
  { tokenType :: TokenType,
    lexeme :: Maybe String
  }
  deriving (Ord, Eq)

data WithPos a = WithPos
  { startPos :: SourcePos,
    endPos :: SourcePos,
    tokenLength :: Int,
    tokenVal :: a
  }
  deriving (Eq, Ord)

data LoxTokStream = LoxTokStream
  { streamInput :: String,
    unStream :: [WithPos LoxTok]
  }

instance Show LoxTok where
  show :: LoxTok -> String
  show (LoxTok tt _) = show tt

instance Show (WithPos LoxTok) where
  show (WithPos _ _ _ x) = show x

instance Stream LoxTokStream where
  type Token LoxTokStream = WithPos LoxTok
  type Tokens LoxTokStream = [WithPos LoxTok]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (LoxTokStream _ []) = Nothing
  take1_ (LoxTokStream str (t : ts)) = Just (t, LoxTokStream (drop (tokensLength pxy (t :| [])) str) ts)

  takeN_ :: Int -> LoxTokStream -> Maybe (Tokens LoxTokStream, LoxTokStream)
  takeN_ n (LoxTokStream str ts)
    | n <= 0 = Just ([], LoxTokStream str ts)
    | null ts = Nothing
    | otherwise =
        let (x, ts') = splitAt n ts
         in case NE.nonEmpty x of
              Nothing -> Just (x, LoxTokStream str ts')
              Just chunk -> Just (x, LoxTokStream (drop (tokensLength pxy chunk) str) ts')

  takeWhile_ f (LoxTokStream str ts) =
    let (x, ts') = DL.span f ts
     in case NE.nonEmpty x of
          Nothing -> (x, LoxTokStream str ts')
          Just chunk -> (x, LoxTokStream (drop (tokensLength pxy chunk) str) ts')

pxy :: Proxy LoxTokStream
pxy = Proxy

instance VisualStream LoxTokStream where
  showTokens Proxy = DL.intercalate "" . NE.toList . fmap (show . tokenVal)
  tokensLength Proxy xs = sum (fmap tokenLength xs)

instance TraversableStream LoxTokStream where
  reachOffset o PosState {..} =
    ( Just (prefix ++ restOfLine),
      PosState
        { pstateInput =
            LoxTokStream
              { streamInput = postStr,
                unStream = post
              },
          pstateOffset = max pstateOffset o,
          pstateSourcePos = newSourcePos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = prefix
        }
    )
    where
      prefix =
        if sameLine
          then pstateLinePrefix ++ preLine
          else preLine
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> case unStream pstateInput of
            [] -> pstateSourcePos
            xs -> endPos (last xs)
          (x : _) -> startPos x
      (pre, post) = splitAt (o - pstateOffset) (unStream pstateInput)
      (preStr, postStr) = splitAt tokensConsumed (streamInput pstateInput)
      preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = takeWhile (/= '\n') postStr
