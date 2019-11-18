module String.Deburr exposing (deburr)

{-| Deburr
    A small package exposing the deburr function, which converts unicode
    characters with burrs (umlauts, accents, etc) to their ASCII counterparts.
    The function intelligently handles capitals and some other edge cases.

    @docs deburr
-}


import Char exposing (isUpper, fromCode)


{-| Deburr a string, removing umlauts, accents, etc.

    >>> deburr "Jeg spiser brød."
    "Jeg spiser brod."
-}
deburr : String -> String
deburr =
  let f x a = case a of
        (Nothing, _) -> (Just x, deburrLetter x Nothing)
        (Just c,  xs) -> (Just x, deburrLetter x (Just c) ++ xs)
  in Tuple.second << String.foldr f (Nothing, "")


deburrLetter : Char -> Maybe Char -> String
deburrLetter n nxt =
  if  --------------------------------------------- A
    n == fromCode 0xc0   || n == fromCode 0xc1   || n == fromCode 0xc2   ||
    n == fromCode 0xc3   || n == fromCode 0xc4   || n == fromCode 0xc5   ||
    n == fromCode 0x0100 || n == fromCode 0x0102 || n == fromCode 0x0104    then "A"
  else if  ----------------------------------------------- a
    n == fromCode 0xe0   || n == fromCode 0xe1   || n == fromCode 0xe2   ||
    n == fromCode 0xe3   || n == fromCode 0xe4   || n == fromCode 0xe5   ||
    n == fromCode 0x0101 || n == fromCode 0x0103 || n == fromCode 0x0105    then "a"
  else if  ----------------------------------------------- C
    n == fromCode 0xc7   || n == fromCode 0x0106 || n == fromCode 0x0108 ||
    n == fromCode 0x010a || n == fromCode 0x010c                     then "C"
  else if  ----------------------------------------------- c
    n == fromCode 0xe7   || n == fromCode 0x0107 || n == fromCode 0x0109 ||
    n == fromCode 0x010b || n == fromCode 0x010d                     then "c"
  else if  ----------------------------------------------- D
    n == fromCode 0xd0   || n == fromCode 0x010e || n == fromCode 0x0110    then "D"
  else if  ----------------------------------------------- d
    n == fromCode 0xf0   || n == fromCode 0x010f || n == fromCode 0x0111    then "d"
  else if  ----------------------------------------------- E
    n == fromCode 0xc8   || n == fromCode 0xc9   || n == fromCode 0xca   ||
    n == fromCode 0xcb   || n == fromCode 0x0112 || n == fromCode 0x0114 ||
    n == fromCode 0x0116 || n == fromCode 0x0118 || n == fromCode 0x011a    then "E"
  else if  ----------------------------------------------- e
    n == fromCode 0xe8   || n == fromCode 0xe9   || n == fromCode 0xea   ||
    n == fromCode 0xeb   || n == fromCode 0x0113 || n == fromCode 0x0115 ||
    n == fromCode 0x0117 || n == fromCode 0x0119 || n == fromCode 0x011b    then "e"
  else if  ----------------------------------------------- G
    n == fromCode 0x011c || n == fromCode 0x011e || n == fromCode 0x0120 ||
    n == fromCode 0x0122                                      then "G"
  else if  ----------------------------------------------- g
    n == fromCode 0x011d || n == fromCode 0x011f || n == fromCode 0x0121 ||
    n == fromCode 0x0123                                      then "g"
               else if     n == fromCode 0x0124 || n == fromCode 0x0126    then "H"  --- H
               else if     n == fromCode 0x0125 || n == fromCode 0x0127    then "h"  --- h
               else if  ----------------------------------------------- I
    n == fromCode 0xcc   || n == fromCode 0xcd   || n == fromCode 0xce   ||
    n == fromCode 0xcf   || n == fromCode 0x0128 || n == fromCode 0x012a ||
    n == fromCode 0x012c || n == fromCode 0x012e || n == fromCode 0x0130    then "I"
               else if  ----------------------------------------------- i
    n == fromCode 0xec   || n == fromCode 0xed   || n == fromCode 0xee   ||
    n == fromCode 0xef   || n == fromCode 0x0129 || n == fromCode 0x012b ||
    n == fromCode 0x012d || n == fromCode 0x012f || n == fromCode 0x0131    then "i"
               else if     n == fromCode 0x0134                     then "J"  --- J
               else if     n == fromCode 0x0135                     then "j"  --- j
               else if     n == fromCode 0x0136                     then "K"  --- K
               else if     n == fromCode 0x0137 || n == fromCode 0x0138    then "k"  --- k
               else if  ----------------------------------------------- L
    n == fromCode 0x0139 || n == fromCode 0x013b || n == fromCode 0x013d ||
    n == fromCode 0x013f || n == fromCode 0x0141                     then "L"
               else if  ----------------------------------------------- l
    n == fromCode 0x013a || n == fromCode 0x013c || n == fromCode 0x013e ||
    n == fromCode 0x0140 || n == fromCode 0x0142                     then "l"
               else if  ----------------------------------------------- N
    n == fromCode 0xd1   || n == fromCode 0x0143 || n == fromCode 0x0145 ||
    n == fromCode 0x0147 || n == fromCode 0x014a                     then "N"
               else if  ----------------------------------------------- n
    n == fromCode 0xf1   || n == fromCode 0x0144 || n == fromCode 0x0146 ||
    n == fromCode 0x0148 || n == fromCode 0x014b                     then "n"
               else if  ----------------------------------------------- O
    n == fromCode 0xd2   || n == fromCode 0xd3   || n == fromCode 0xd4   ||
    n == fromCode 0xd5   || n == fromCode 0xd6   || n == fromCode 0xd8   ||
    n == fromCode 0x014c || n == fromCode 0x014e || n == fromCode 0x0150    then "O"
               else if  ----------------------------------------------- o
    n == fromCode 0xf2   || n == fromCode 0xf3   || n == fromCode 0xf4   ||
    n == fromCode 0xf5   || n == fromCode 0xf6   || n == fromCode 0xf8   ||
    n == fromCode 0x014d || n == fromCode 0x014f || n == fromCode 0x0151    then "o"
               else if  ----------------------------------------------- R
    n == fromCode 0x0154 || n == fromCode 0x0156 || n == fromCode 0x0158    then "R"
               else if  ----------------------------------------------- r
    n == fromCode 0x0155 || n == fromCode 0x0157 || n == fromCode 0x0159    then "r"
               else if  ----------------------------------------------- S
    n == fromCode 0x015a || n == fromCode 0x015c || n == fromCode 0x015e ||
    n == fromCode 0x0160                                      then "S"
               else if  ----------------------------------------------- s
    n == fromCode 0x015b || n == fromCode 0x015d || n == fromCode 0x015f ||
    n == fromCode 0x0161 || n == fromCode 0x017f                     then "s"
               else if  ----------------------------------------------- T
    n == fromCode 0x0162 || n == fromCode 0x0164 || n == fromCode 0x0166    then "T"
               else if  ----------------------------------------------- t
    n == fromCode 0x0163 || n == fromCode 0x0165 || n == fromCode 0x0167    then "t"
               else if  ----------------------------------------------- U
    n == fromCode 0xd9   || n == fromCode 0xda   || n == fromCode 0xdb   ||
    n == fromCode 0xdc   || n == fromCode 0x0168 || n == fromCode 0x016a ||
    n == fromCode 0x016c || n == fromCode 0x016e || n == fromCode 0x0170 ||
    n == fromCode 0x0172                                      then "U"
               else if  ----------------------------------------------- u
    n == fromCode 0xf9   || n == fromCode 0xfa   || n == fromCode 0xfb   ||
    n == fromCode 0xfc   || n == fromCode 0x0169 || n == fromCode 0x016b ||
    n == fromCode 0x016d || n == fromCode 0x016f || n == fromCode 0x0171 ||
    n == fromCode 0x0173                                      then "u"
               else if     n == fromCode 0x0174                     then "W"  --- W
               else if     n == fromCode 0x0175                     then "w"  --- w
               else if  ----------------------------------------------- Y
    n == fromCode 0xdd   || n == fromCode 0x0176 || n == fromCode 0x0178    then "Y"
               else if  ----------------------------------------------- y
    n == fromCode 0xfd   || n == fromCode 0xff   || n == fromCode 0x0177    then "y"
               else if  ----------------------------------------------- Z
    n == fromCode 0x0179 || n == fromCode 0x017b || n == fromCode 0x017d    then "Z"
               else if  ----------------------------------------------- z
    n == fromCode 0x017a || n == fromCode 0x017c || n == fromCode 0x017e    then "z"
               else if n == fromCode 0xc6 && Maybe.withDefault False (Maybe.map isUpper nxt)   then "AE"  -- AE
               else if n == fromCode 0xc6                              then "Ae"  -- Ae
               else if n == fromCode 0xe6                              then "ae"  -- ae
               else if n == fromCode 0xde && Maybe.withDefault False (Maybe.map isUpper nxt)   then "TH"  -- TH
               else if n == fromCode 0xde                              then "Th"  -- Th
               else if n == fromCode 0xfe                              then "th"  -- th
               else if n == fromCode 0xdf                              then "ss"  -- ss
               else if n == fromCode 0x0132                            then "IJ"  -- IJ
               else if n == fromCode 0x0133                            then "ij"  -- ij
               else if n == fromCode 0x0152 && Maybe.withDefault False (Maybe.map isUpper nxt) then "OE"  -- OE
               else if n == fromCode 0x0152                            then "Oe"  -- Oe
               else if n == fromCode 0x0153                            then "oe"  -- oe
               else if n == fromCode 0x0149                            then "n"  -- n
               else String.fromChar n
