{

module Lexer (
  alexScanTokens
, Token (..)
, Located (..)
) where

import Data.Word (Word8)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

}

$digit = 0-9

tokens :-
  $white+    ;
  $digit+    { \_ s -> TokenInt (read s) }
  \(         { \_ _ -> TokenLB }
  \)         { \_ _ -> TokenRB }
  [\+ \- \*] { \_ s -> TokenBinOp (head s) }
   
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

data AlexPosn = AlexPn !Int !Int !Int

type AlexInput = (AlexPosn, -- current position
                  Char,     -- previous char
                  [Word8],  -- pending bytes on current char
                  String)   -- current input string

type AlexAction = AlexPosn -> String -> Token

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1) 1
alexMove (AlexPn a l c) _    = AlexPn (a+1) l (c+1)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_, c, _, _) = c

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (_, _, [], []) = Nothing
alexGetByte (p, _, [], (c:s)) = let p' = alexMove p c 
                                    (b:bs) = utf8Encode c
                                 in p' `seq` Just (b, (p', c, bs, s))

alexScanTokens :: String -> Either String [Located Token]
alexScanTokens str = go (alexStartPos, '\n', [], str)
  where 
    go inp@(pos, _, _, str) = do
      case alexScan inp 0 of
        AlexEOF -> return []
        AlexError ((AlexPn _ line column), _, _, _) -> Left $ "lexical error at line " ++ show line ++ ", column " ++ show column
        AlexSkip  inp' len -> go inp'
        AlexToken inp'@((AlexPn _ line column), _, _, _) len action -> do
          let cur = action pos (take len str)
          rest <- go inp'
          return (Located (line, column) cur:rest)

data Token = TokenInt Integer
           | TokenBinOp Char
           | TokenLB
           | TokenRB
  deriving (Show)

data Located a = Located (Int, Int) a

}
