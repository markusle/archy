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
import AURConnector
import CommandLineParser
import DisplayFunctions
import PacmanWrapper


-- | main
main :: IO ()
main = do

  -- parse command line
  args <- getArgs
  let (actions, _, _) = getOpt RequireOrder options args
  opts <- foldl (>>=) ( return defaultOptions ) actions

  -- extract requested action
  let Options {
      userRequest   = request
    --, requestString = pattern
    } = opts


  -- dispatch
  case request of 
    Status -> do
      havePackages <- get_installed_aur_packages 
      case havePackages of
        Nothing -> return ()
        Just packages -> do
          info <- retrieve_aur_info packages
          show_updates info
   
    _      -> putStrLn $ usageInfo "Usage: hark [options]\n" options  
