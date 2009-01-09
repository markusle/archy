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

-- | main archy driver
module Main where


-- local imports
import ArchyCommon
import AURConnector
import PacmanWrapper
import PrettyPrint


-- | check for packages that are out of date
show_updates :: [AurPackage] -> IO ()
show_updates [] = return ()
show_updates (x:xs) = 
  do 
    print_package_name (name x)
    if installedVersion x /= availableVersion x then
      do
        putColorStrLn Red outOfDateMsg
        show_updates xs
      else do
        putColorStrLn Green uptoDateMsg
        show_updates xs

  where
    print_package_name :: String -> IO ()
    print_package_name pkgName = putColorStr Yellow nameString
      where
        dots = replicate (25 - length(pkgName)) '.'
        nameString = pkgName ++ dots ++ "  ::  "


    outOfDateMsg = "Out of date " ++ "(" ++ installedVersion x 
                   ++ " -> " ++ availableVersion x ++ ")"
    uptoDateMsg  = "Up to date " ++ "(" ++ availableVersion x ++ ")"



-- | main
main :: IO ()
main = 
  do
   havePackages <- get_installed_aur_packages 
   case havePackages of
    Nothing -> return ()
    Just packages -> do
      info <- retrieve_aur_info packages
      show_updates info
      
