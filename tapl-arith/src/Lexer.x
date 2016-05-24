{

module Lexer (
  scanTokens
) where

import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Data.Word (Word8)

import Types

}

$digit = 0-9
$lower = a-z
$upper = A-Z
$kwchar = [a-zA-Z]

tokens :-
  $white+    ;
  $digit+    { \s -> return $ TokenInt (read s) }
  $kwchar+   { \s -> case lookupKeyword s of
                       Just token -> return token
                       Nothing -> alexFail }
  \(         { \_ -> return TokenLBracket }
  \)         { \_ -> return TokenRBracket }

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

type AlexAction = String -> Token

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c, _, _) = c

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (c, (b:bs), s) = Just (b, (c, bs, s))
alexGetByte (_, [], []) = Nothing
alexGetByte (_, [], (c:s)) = let (b:bs) = utf8Encode c
                              in Just (b, (c, bs, s))

alexFail :: Either String a
alexFail = Left "lexical error"

scanTokens :: String -> Either String [Token]
scanTokens str = go ('\n', [], str)
  where
    go inp@(_, _, input) = do
      case alexScan inp 0 of
        AlexEOF -> return []
        AlexError _ -> alexFail
        AlexSkip inp' _ -> go inp'
        AlexToken inp' length action -> do
          cur <- action (take length input)
          rest <- go inp'
          return (cur:rest)

lookupKeyword :: String -> Maybe Token
lookupKeyword kw = lookup kw keywords
  where
    keywords = [ ("if", TokenIf)
               , ("then", TokenThen)
               , ("else", TokenElse)
               , ("succ", TokenSucc)
               , ("pred", TokenPred)
               , ("iszero", TokenIsZero)
               , ("true", TokenTrue)
               , ("false", TokenFalse)
               ]

}
