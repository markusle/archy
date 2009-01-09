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

-- | this module parses the command line options for the 
-- word count utility
module CommandLineParser ( getArgs
                         , module CommandLineParser
                         , module System.Console.GetOpt 
                         ) where

-- imports
import System
import System.Console.GetOpt

-- local imports
import DisplayFunctions


-- | data structure for keeping track of command
-- line requests. 
data CommandLineOpt = None | Status 
  deriving(Eq)


-- | data structure for keeping track of selected command 
-- line options
data Options = Options {
  userRequest    :: CommandLineOpt,
  requestString  :: String
} 


{-|
   default selections
-}
defaultOptions :: Options
defaultOptions = Options {
  userRequest   = None,
  requestString = ""
}


{-|
   available command line flags
-}
options :: [OptDescr (Options -> IO Options)]
options = [ 
  Option ['s'] ["status"] (NoArg show_status)
    "show status of currently installed AUR packages",
  Option ['v'] ["version-info"] (NoArg version_info) 
    "show version information"]


-- | extractor function for searched package 
show_status :: Options -> IO Options
show_status opt = 
  return opt { userRequest = Status }


-- | extractor function for version info
version_info :: Options -> IO Options
version_info _ = 
  do
    show_version
    exitWith ExitSuccess
