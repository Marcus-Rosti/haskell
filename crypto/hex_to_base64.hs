--Marcus Rosti
--20141009
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 ()
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16

byte = fst $ B16.decode "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" :: B.ByteString

main = print $ B64.encode byte