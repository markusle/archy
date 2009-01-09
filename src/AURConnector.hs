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

-- | module providing functionality to connect to the Arch linux
-- servers to retrieve the current status of packages
module AURConnector (retrieve_aur_info) where

-- imports
import Network.Curl 

-- local imports
import ArchyCommon
import Json


-- base url for retrieving AUR package info 
baseURL = "http://aur.archlinux.org/rpc.php?type=info&arg=" ::String


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
    { aurID      :: String
    , aurName    :: String
    , aurVersion :: String
    , aurDesc    :: String
    , aurUrl     :: String
    , aurLicense :: String
    }
  deriving(Show)


-- | parse retrieved info into a PackageInfo structure
parse_package_info :: JValue -> Maybe PackageInfo
parse_package_info (JObject val) = 
  populate_packageInfo $ unpack_json val
parse_package_info _             = Nothing




-- | populate PackageInfo unpacked JSON data
populate_packageInfo :: [(String,JValue)] -> Maybe PackageInfo
populate_packageInfo info =
  lookup "ID" info          >>= \(JString infoID) -> 
  lookup "Name" info        >>= \(JString infoName) ->
  lookup "Version" info     >>= \(JString infoVersion) ->
  lookup "Description" info >>= \(JString infoDesc) ->
  lookup "URLPath" info     >>= \(JString infoUrl) ->
  lookup "License" info     >>= \(JString infoLicense) ->
  return $ PackageInfo { 
                         aurID      = infoID
                       , aurName    = infoName 
                       , aurVersion = infoVersion
                       , aurDesc    = infoDesc
                       , aurUrl     = infoUrl
                       , aurLicense = infoLicense
                       } 



-- | given a package name retrieve the packageInfo from the arch 
-- linux mirrors
retrieve_packageInfo :: String -> IO (Maybe PackageInfo)
retrieve_packageInfo name =

  let 
    url = baseURL ++ name
  in 
    request_json url >>= \jsonData ->
    case jsonData of
      Nothing      -> return Nothing
      Just content -> case parse parse_json "" content of
        Left e  -> return Nothing
        Right r -> return $ parse_package_info r 



-- | function grabbing the info available on the Arch Linux
-- mirrors for each AUR package present on the system, and
-- adding it to the AurPackage data structure
retrieve_aur_info :: [AurPackage] -> IO [AurPackage]
retrieve_aur_info = retrieve_aur_info' []

  where
    retrieve_aur_info' :: [AurPackage] -> [AurPackage] 
                       -> IO [AurPackage]
    retrieve_aur_info' acc [] = return acc
    retrieve_aur_info' acc (x:xs) = 
      retrieve_packageInfo (name x) >>= \result ->
        case result of
          Nothing    -> retrieve_aur_info' (x:acc) xs
          Just info  -> retrieve_aur_info' ( 
                     x { availableVersion = aurVersion info
                       , description      = aurDesc info
                       , url              = aurUrl info
                       , license          = aurLicense info
                       }:acc) xs 

