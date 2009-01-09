{-----------------------------------------------------------------
 
  (c) 2008-2009 Markus Dittrich 
 
  This program is free software; you can redistribute it 
  and/or modify it under the terms of the GNU General Public 
  License Version 3 as published by the Free Software Foundation. 
 
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License Version 3 for more details.
 
  You should have received a copy of the GNU General Public 
  License along with this program; if not, write to the Free 
  Software Foundation, Inc., 59 Temple Place - Suite 330, 
  Boston, MA 02111-1307, USA.

--------------------------------------------------------------------}

-- | implementation of a simple JSON parser with lots of
-- help from RWH. Thanks Don and friends ;)
module Json ( module ApplicativeParsec
            , parse_json 
            , JValue(..)
            , unpack_json
            , fromJAry
            , fromJObj
            ) where


-- imports
import Numeric
import ApplicativeParsec



-- | types for JSON parsing
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)
            | JArray (JAry JValue)
              deriving(Eq,Ord,Show)

newtype JAry a = JAry { fromJAry :: [a] }
  deriving(Eq,Ord,Show)


newtype JObj a = JObj { fromJObj :: [(String,a)] }
  deriving(Eq,Ord,Show)


-- | entry point for JSON parser
parse_json :: CharParser () JValue
parse_json = spaces *> text
             <?> "JSON parse error"
  where
    text :: CharParser () JValue
    text = JObject <$> parse_object 
       <|> JArray <$> parse_array



-- | parser for series of elements enclosed by two 
-- elements; make sure to remove all intervening spaces
parse_series :: Char -> CharParser () a -> Char -> CharParser () [a]
parse_series left parser right = 
  between (char left <* spaces) (char right) $
  (parser <* spaces) `sepBy` (char ',' <* spaces)



-- | parser for J arrays
parse_array :: CharParser () (JAry JValue)
parse_array = JAry <$> parse_series '[' parse_value ']'



-- | parser for J objects
parse_object :: CharParser () (JObj JValue)
parse_object = JObj <$> parse_series '{' parse_field '}'

  where
    parse_field = (,) <$> ( parse_string <* char ':' <* spaces)
                      <*> parse_value


-- | parser for J values
parse_value :: CharParser () JValue
parse_value = value <* spaces 

  where
    value = choice [ JString <$> parse_string
                   , JNumber <$> parse_number
                   , JObject <$> parse_object
                   , JArray  <$> parse_array
                   , JBool   <$> parse_bool
                   , JNull   <$  string "null"
                   ]
        <?> "JSON value"



-- | parser for JBool
parse_bool :: CharParser () Bool
parse_bool = True <$ string "true"
         <|> False <$ string "false" 



-- | parser for numbers
parse_number :: CharParser () Double
parse_number = 

  do s <- getInput 
     case readSigned readFloat s of
       [(n,s')] -> n <$ setInput s'
       _        -> empty



-- | parser for strings
parse_string :: CharParser () String
parse_string = between (char '\"') (char '\"') (many jchar)

  where
    jchar = char '\\' *> (parse_escape <|> parse_unicode)
        <|> satisfy (`notElem` "\"\\")



-- | parser for escape sequences
parse_escape :: CharParser () Char
parse_escape = choice ( zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")

  where
    decode c r = r <$ char c



-- | parser for unicode
parse_unicode :: CharParser () Char
parse_unicode = char 'u' *> ( decode <$> count 4 hexDigit)

  where
    decode x = toEnum code

      where 
        ((code,_):_) = readHex x



-- | unpack parsed JSON data so we can parse it into a PackageInfo
-- NOTE: We need more error detection here
unpack_json :: JObj JValue -> [(String,JValue)]
unpack_json obj = 

  let 
    (_:content) = fromJObj obj
    (JObject results) = snd $ head content
  in
    fromJObj results



