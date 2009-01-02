
import System.IO
import System.Process
--import System.Posix.Process

import Debug.Trace




{-|
  data structure holding the package info for an aur
  package
-}
data AurPackage = AurPackage { name :: String
                             , installedVersion :: String 
                             , availableVersion :: String }
  deriving(Show)


{-|
  function retrieving installed AUR packages via pacman
-}
get_aur_packages :: IO (Maybe [AurPackage])
get_aur_packages =

  runInteractiveProcess "/usr/bin/pacman" ["-Qm"] Nothing Nothing >>=
  \(_,hdl,_,_) -> hGetContents hdl >>=
  \content -> let packageList = parseContent content in 
                return packageList


{-|
  function parsing the raw pacman output into a list 
  of AurPackages
-}
parseContent :: String -> Maybe [AurPackage]
parseContent = parseContent' [] . lines

  where
    parseContent' :: [AurPackage] -> [String] -> Maybe [AurPackage]
    parseContent' []  []     =  Nothing
    parseContent' acc []     =  Just acc
    parseContent' acc (x:xs) = 
      let
        items   = words x
        package = AurPackage (items!!0) (items!!1) ""
      in
        parseContent' (package:acc) xs




main :: IO ()
main = 
  do
   havePackages <- get_aur_packages 
   case havePackages of
    Nothing -> return ()
    Just packages -> 
      return ()
