{

{-# LANGUAGE RecordWildCards #-}

module Lexer (
  scanTokens
) where

import Control.Monad (liftM, ap)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Data.Word (Word8)

import Base

}

tokens :-
<0> {
  $white+    ;
  \\         { scanMetaChar }
  \.         { \_ -> return TokenDot }
  \+         { \_ -> return TokenPlus }
  \-         { \_ -> return TokenHyphen }
  \*         { \_ -> return TokenStar }
  \^         { \_ -> return TokenCircumflex }
  \|         { \_ -> return TokenVBar }
  \(         { \_ -> return TokenOParen }
  \)         { \_ -> return TokenCParen }
  \[         { \_ -> return TokenOBrack }
  \]         { \_ -> return TokenCBrack }

  .          { \s -> return $ TokenChar (head s) }
}

<metac> {
  \\         { \_ -> return $ TokenChar '\\' }
  \.         { \_ -> return $ TokenChar '.' }
  \+         { \_ -> return $ TokenChar '+' }
  \-         { \_ -> return $ TokenChar '-' }
  \*         { \_ -> return $ TokenChar '*' }
  \^         { \_ -> return $ TokenChar '^' }
  \|         { \_ -> return $ TokenChar '|' }
  \(         { \_ -> return $ TokenChar '(' }
  \)         { \_ -> return $ TokenChar ')' }
  \[         { \_ -> return $ TokenChar '[' }
  \]         { \_ -> return $ TokenChar ']' }

}

{

utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
   go oc
     | oc <= 0x7f   = [ oc ]
     | oc <= 0x7ff  = [ 0xc0 + (oc `shiftR` 6)
                      , 0x80 + oc .&. 0x3f
                      ]
     | oc <= 0xffff = [ 0xe0 + (oc `shiftR` 12)
                      , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                      , 0x80 + oc .&. 0x3f
                      ]
     | otherwise    = [ 0xf0 + (oc `shiftR` 18)
                      , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                      , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                      , 0x80 + oc .&. 0x3f
                      ]

type AlexInput = (Char, [Word8], String)

data AlexState = AlexState {
  alexInput :: AlexInput
, alexScd   :: [Int]
}

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Functor Alex where
  fmap = liftM

instance Applicative Alex where
  pure = return
  (<*>) = ap

instance Monad Alex where
  m >>= k = Alex $
    \s -> case unAlex m s of
            Left err -> Left err
            Right (s', a) -> unAlex (k a) s'
  return a = Alex $ \s -> Right (s, a)

type AlexAction = String -> Alex Token

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c, _, _) = c

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (c, (b:bs), s) = Just (b, (c, bs, s))
alexGetByte (_, [], []) = Nothing
alexGetByte (_, [], (c:s)) = let (b:bs) = utf8Encode c
                              in Just (b, (c, bs, s))

alexGetInput :: Alex AlexInput
alexGetInput = Alex $ \s@AlexState {..} -> Right (s, alexInput)

alexSetInput :: AlexInput -> Alex ()
alexSetInput input = Alex $ \s -> Right (s { alexInput = input }, ())

alexError :: Alex a
alexError = Alex $ \_ -> Left "lexical error"

alexGetStartCode :: Alex [Int]
alexGetStartCode = Alex $ \s@AlexState {..} -> Right (s, alexScd)

alexPushStartCode :: Int -> Alex ()
alexPushStartCode sc = Alex $ \s@AlexState {..} -> Right (s { alexScd = sc:alexScd }, ())

alexPopStartCode :: Alex ()
alexPopStartCode = Alex $ \s@AlexState {..} -> Right (s { alexScd = tail alexScd }, ())

runAlex :: AlexState -> Alex a -> Either String (AlexState, a)
runAlex s (Alex f) = f s

scanMetaChar :: String -> Alex Token
scanMetaChar _ = do
  alexPushStartCode metac
  t <- monadicScanToken
  alexPopStartCode
  case t of
    Nothing -> alexError
    Just t -> return t

monadicScanToken :: Alex (Maybe Token)
monadicScanToken = do
  inp@(_, _, str) <- alexGetInput
  sc <- head <$> alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> return Nothing
    AlexError _ -> alexError
    AlexSkip inp' _ -> do
      alexSetInput inp'
      monadicScanToken
    AlexToken inp' len action -> do
      alexSetInput inp'
      Just <$> action (take len str)

scanTokens :: String -> Either String [Token]
scanTokens inp = go $ AlexState { alexInput = ('\n', [], inp), alexScd = [0] }
  where
    go s = case runAlex s monadicScanToken of
             Left err -> Left err
             Right (s', x) -> do
               case x of
                 Nothing -> return []
                 Just x -> case go s' of
                   Left err -> Left err
                   Right xs -> Right (x:xs)

}
