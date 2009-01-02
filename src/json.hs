---------------------------------------------------------------------
-- 
-- implementation of a simple JSON parser with lots of
-- help from RWH. Thanks Don and friends ;)
--
-- (C) 2008 Markus Dittrich, licensed under the GPL v3
----------------------------------------------------------------------

-- | implementation of a simple JSON parser with lots of
-- help from RWH. Thanks Don and friends ;)

module Main where

-- imports
import Network.Curl 
import Numeric
import ApplicativeParsec
import Debug.Trace


-- url for testing 
testUrl = "http://aur.archlinux.org/rpc.php?type=info&arg=slim-cursor"::String


-- | simple function to request json info from aur
request_json :: String -> IO (Maybe String)
request_json url = withCurlDo $ do 
                     h <- initialize
                     (status,response) <- curlGetString url [] 
                     case status of
                       CurlOK -> return $ Just response
                       _      -> return Nothing


-- | simple data structure for keeping track of JSON data
-- for use with pacman and friends
data PackageInfo 
  = PackageInfo 
    { id          :: String
    , name        :: String
    , version     :: String
    , description :: String
    , url         :: String
    , license     :: String
    }


-- | stuff for JSON parsing
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


-- | parse retrieved info into a PackageInfo structure
parse_package_info :: JValue -> IO () 
parse_package_info (JObject val) = 

  let
    (_:foo) = fromJObj val
    (JObject bar) = snd $ head foo
    final = lookup "Version" $ fromJObj bar
  in
    case final of
      Nothing -> print "Nothing found"
      Just (JString a)  -> print a





-- | main driver for testing only
main :: IO ()
main = request_json testUrl >>= \resp ->
       case resp of
         Nothing -> putStrLn "Connection Failed"
         Just content  -> 
           case parse parse_json "" content of
            Left e  -> putStrLn "Parse error" >> print e
            Right r -> parse_package_info r
