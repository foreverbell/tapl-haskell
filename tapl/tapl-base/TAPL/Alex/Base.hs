module TAPL.Alex.Base (
  AlexInput
, alexInputPrevChar
, alexGetByte
) where

import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Data.Word (Word8)

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

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c, _, _) = c

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (c, (b:bs), s) = Just (b, (c, bs, s))
alexGetByte (_, [], []) = Nothing
alexGetByte (_, [], (c:s)) = let (b:bs) = utf8Encode c
                              in Just (b, (c, bs, s))
