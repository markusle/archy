{-----------------------------------------------------------------
 
  (c) 2008 Markus Dittrich 
 
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


-- | wrapper functions around Arch linux's pacman package manager 
module PacmanWrapper ( get_installed_aur_packages ) where


-- imports
import System.IO
import System.Process


-- local imports
import ArchyCommon



-- | function retrieving installed AUR packages via pacman
get_installed_aur_packages :: IO (Maybe [AurPackage])
get_installed_aur_packages =

  runInteractiveProcess "/usr/bin/pacman" ["-Qm"] Nothing Nothing >>=
  \(_,hdl,_,_) -> hGetContents hdl >>=
  \content -> let packageList = parseContent content in 
                return packageList



-- | function parsing the raw pacman output into a list 
-- of AurPackages
parseContent :: String -> Maybe [AurPackage]
parseContent = parseContent' [] . lines

  where
    parseContent' :: [AurPackage] -> [String] -> Maybe [AurPackage]
    parseContent' []  []     =  Nothing
    parseContent' acc []     =  Just acc
    parseContent' acc (x:xs) = 
      let
        items   = words x
        package = AurPackage (items!!0) (items!!1) "" "" "" ""
      in
        parseContent' (package:acc) xs



