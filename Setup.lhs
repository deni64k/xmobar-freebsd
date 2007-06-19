#!/usr/bin/env runhaskell

> import Distribution.Simple
> import Distribution.PackageDescription
> import Distribution.Setup
> import Distribution.Simple.Utils
> import Distribution.Simple.LocalBuildInfo
> import Distribution.Program
> import Distribution.PreProcess

> import System.FilePath.Posix
> import System.Directory
> import Data.List


> main = defaultMainWithHooks defaultUserHooks {haddockHook = xmonadHaddock}

> -- a different implementation of haddock hook from
> -- from Distribution.Simple: will use synopsis and description for 
> -- building executables' documentation.

> xmonadHaddock pkg_descr lbi hooks (HaddockFlags hoogle verbose) = do
>   confHaddock <- do let programConf = withPrograms lbi
>                         haddockName = programName haddockProgram
>                     mHaddock <- lookupProgram haddockName programConf
>                     maybe (die "haddock command not found") return mHaddock

>   let tmpDir = (buildDir lbi) </> "tmp"
>   createDirectoryIfMissing True tmpDir
>   createDirectoryIfMissing True haddockPref
>   preprocessSources pkg_descr lbi verbose (allSuffixHandlers hooks)
>   setupMessage "Running Haddock for" pkg_descr

>   let outputFlag  = if hoogle then "--hoogle" else "--html"
>       showPkg     = showPackageId (package pkg_descr)
>       showDepPkgs = map showPackageId (packageDeps lbi)
                          
>   withExe pkg_descr $ \exe -> do 
>     let bi = buildInfo exe
>     inFiles <- getModulePaths bi (otherModules bi)
>     srcMainPath <- findFile (hsSourceDirs bi) (modulePath exe)

>     let prologName = showPkg ++ "-haddock-prolog.txt"
>     writeFile prologName (description pkg_descr ++ "\n")

>     let exeTargetDir = haddockPref </> exeName exe
>         outFiles = srcMainPath : inFiles
>         haddockFile = exeTargetDir </> (haddockName pkg_descr)

>     createDirectoryIfMissing True exeTargetDir
>     rawSystemProgram verbose confHaddock
>                          ([outputFlag,
>                            "--odir=" ++ exeTargetDir,
>                            "--title=" ++ showPkg ++ ": " ++ synopsis pkg_descr,
>                            "--package=" ++ showPkg,
>                            "--dump-interface=" ++ haddockFile,
>                            "--prologue=" ++ prologName
>                           ]
>                           ++ map ("--use-package=" ++) showDepPkgs
>                           ++ programArgs confHaddock
>                           ++ (if verbose > 4 then ["--verbose"] else [])
>                           ++ outFiles
>                          )
>     removeFile prologName
  
> getModulePaths :: BuildInfo -> [String] -> IO [FilePath]
> getModulePaths bi =
>    fmap concat .
>       mapM (flip (moduleToFilePath (hsSourceDirs bi)) ["hs", "lhs"])
    

> allSuffixHandlers :: Maybe UserHooks
>                   -> [PPSuffixHandler]
> allSuffixHandlers hooks
>     = maybe knownSuffixHandlers
>       (\h -> overridesPP (hookedPreProcessors h) knownSuffixHandlers)
>       hooks
>     where
>       overridesPP :: [PPSuffixHandler] -> [PPSuffixHandler] -> [PPSuffixHandler]
>       overridesPP = unionBy (\x y -> fst x == fst y)
