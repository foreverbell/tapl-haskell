{

{-# LANGUAGE RecordWildCards #-}

module Lexer (
  scanTokens
) where

import Control.Monad (liftM, ap)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Data.Word (Word8)

import Types

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

type AlexInput = (Char,     -- previous char
                  [Word8],  -- pending bytes on current char
                  String)   -- current input string

data AlexState = AlexState {
  alexInput :: String,      -- the current input
  alexChar  :: !Char,       -- the character before the input
  alexBytes :: [Word8],
  alexScd   :: [Int]        -- the current startcode
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
            Right (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> Right (s, a)

type AlexAction = String -> Alex Token

data AlexResult = IntermediateToken Token | IntermediateEOF

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c, _, _) = c

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (c, (b:bs), s) = Just (b, (c, bs, s))
alexGetByte (_, [], []) = Nothing
alexGetByte (_, [], (c:s)) = let (b:bs) = utf8Encode c
                              in Just (b, (c, bs, s))

alexGetInput :: Alex AlexInput
alexGetInput = Alex $ \s@AlexState{..} -> Right (s, (alexChar, alexBytes, alexInput))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (c, bs, inp) = Alex $ \s -> Right (s { alexChar = c, alexBytes = bs, alexInput = inp }, ())

alexError :: Alex a
alexError = Alex $ \_ -> Left "lexical error"

alexGetStartCode :: Alex [Int]
alexGetStartCode = Alex $ \s@AlexState{..} -> Right (s, alexScd)

alexPushStartCode :: Int -> Alex ()
alexPushStartCode sc = Alex $ \s@AlexState{..} -> Right (s { alexScd = sc:alexScd }, ())

alexPopStartCode :: Alex ()
alexPopStartCode = Alex $ \s@AlexState{..} -> Right (s { alexScd = tail alexScd }, ())

runAlex :: AlexState -> Alex a -> Either String (AlexState, a)
runAlex s (Alex f) = f s

scanMetaChar :: String -> Alex Token
scanMetaChar _ = do
  alexPushStartCode metac
  t <- monadicScanToken
  alexPopStartCode
  case t of
    IntermediateEOF -> alexError
    IntermediateToken t -> return t

monadicScanToken :: Alex AlexResult
monadicScanToken = do
  inp@(_, _, str) <- alexGetInput
  sc <- head <$> alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> return IntermediateEOF
    AlexError _ -> alexError
    AlexSkip inp' _ -> do
      alexSetInput inp'
      monadicScanToken
    AlexToken inp' len action -> do
      alexSetInput inp'
      IntermediateToken <$> action (take len str)

scanTokens :: String -> Either String [Token]
scanTokens inp = go $ AlexState { alexInput = inp
                                , alexChar = '\n'
                                , alexBytes = []
                                , alexScd = [0]
                                }
  where
    go s = case runAlex s monadicScanToken of
             Left err -> Left err
             Right (s', x) -> do
               case x of
                 IntermediateEOF -> return []
                 IntermediateToken x -> do
                   case go s' of
                     Left err -> Left err
                     Right xs -> Right (x:xs)


}
