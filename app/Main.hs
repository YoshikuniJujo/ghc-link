{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.List
import System.Environment

import DynFlags
import SysTools
import Packages
import LinkOptions

import GHC

main :: IO ()
main = do
	args0 <- getArgs
	let	(minusB_args, args1) = partition ("-B" `isPrefixOf`) args0
		mbMinusB
			| null minusB_args = Nothing
			| otherwise = Just . drop 2 $ last minusB_args
	mySettings <- initSysTools mbMinusB
	myLlvmConfig <- initLlvmTargets mbMinusB
	dflags0 <- initDynFlags
		$ defaultDynFlags mySettings myLlvmConfig
	(dflags1, _, _) <- parseDynamicFlags dflags0
		$ map (mkGeneralLocated "") args1
	(dflags2, _) <- initPackages dflags1
	lopts <- getLinkOptions False dflags2 args1 []
	putStrLn . unwords $ map showOpt lopts
