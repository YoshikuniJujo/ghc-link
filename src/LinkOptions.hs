{-# LANGUAGE CPP, NamedFieldPuns, NondecreasingIndentation #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LinkOptions (getLinkOptions) where

#include "HsVersions.h"

import Packages
import SysTools
import SysTools.ExtraObj
import Module
import DynFlags
import Util
import Platform
import FileCleanup

import System.Directory
import System.FilePath

getLinkOptions ::
	Bool -> DynFlags -> [FilePath] -> [InstalledUnitId] -> IO [Option]
getLinkOptions staticLink dflags o_files dep_packages = do
    let platform = targetPlatform dflags
        mySettings = settings dflags
        verbFlags = getVerbFlags dflags
        output_fn = exeFileName staticLink dflags

    -- get the full list of packages to link with, by combining the
    -- explicit packages with the auto packages and all of their
    -- dependencies, and eliminating duplicates.

    full_output_fn <- if isAbsolute output_fn
                      then return output_fn
                      else do d <- getCurrentDirectory
                              return $ normalise (d </> output_fn)
    pkg_lib_paths <- getPackageLibraryPath dflags dep_packages
    let pkg_lib_path_opts = concatMap get_pkg_lib_path_opts pkg_lib_paths
        get_pkg_lib_path_opts l
         | osElfTarget (platformOS platform) &&
           dynLibLoader dflags == SystemDependent &&
           WayDyn `elem` ways dflags
            = let libpath = if gopt Opt_RelativeDynlibPaths dflags
                            then "$ORIGIN" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
                  -- See Note [-Xlinker -rpath vs -Wl,-rpath]
                  rpath = if gopt Opt_RPath dflags
                          then ["-Xlinker", "-rpath", "-Xlinker", libpath]
                          else []
                  -- Solaris 11's linker does not support -rpath-link option. It silently
                  -- ignores it and then complains about next option which is -l<some
                  -- dir> as being a directory and not expected object file, E.g
                  -- ld: elf error: file
                  -- /tmp/ghc-src/libraries/base/dist-install/build:
                  -- elf_begin: I/O error: region read: Is a directory
                  rpathlink = if (platformOS platform) == OSSolaris2
                              then []
                              else ["-Xlinker", "-rpath-link", "-Xlinker", l]
              in ["-L" ++ l] ++ rpathlink ++ rpath
         | osMachOTarget (platformOS platform) &&
           dynLibLoader dflags == SystemDependent &&
           WayDyn `elem` ways dflags &&
           gopt Opt_RPath dflags
            = let libpath = if gopt Opt_RelativeDynlibPaths dflags
                            then "@loader_path" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
              in ["-L" ++ l] ++ ["-Xlinker", "-rpath", "-Xlinker", libpath]
         | otherwise = ["-L" ++ l]

    let
      dead_strip
        | gopt Opt_WholeArchiveHsLibs dflags = []
        | otherwise = if osSubsectionsViaSymbols (platformOS platform)
                        then ["-Wl,-dead_strip"]
                        else []
    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    extraLinkObj <- mkExtraObjToLinkIntoBinary dflags
    noteLinkObjs <- mkNoteObjsToLinkIntoBinary dflags dep_packages

    let
      (pre_hs_libs, post_hs_libs)
        | gopt Opt_WholeArchiveHsLibs dflags
        = if platformOS platform == OSDarwin
            then (["-Wl,-all_load"], [])
              -- OS X does not have a flag to turn off -all_load
            else (["-Wl,--whole-archive"], ["-Wl,--no-whole-archive"])
        | otherwise
        = ([],[])

    pkg_link_opts <- do
        (package_hs_libs, extra_libs, other_flags) <- getPackageLinkOpts dflags dep_packages
        return $ if staticLink
            then package_hs_libs -- If building an executable really means making a static
                                 -- library (e.g. iOS), then we only keep the -l options for
                                 -- HS packages, because libtool doesn't accept other options.
                                 -- In the case of iOS these need to be added by hand to the
                                 -- final link in Xcode.
            else other_flags ++ dead_strip
                  ++ pre_hs_libs ++ package_hs_libs ++ post_hs_libs
                  ++ extra_libs
                 -- -Wl,-u,<sym> contained in other_flags
                 -- needs to be put before -l<package>,
                 -- otherwise Solaris linker fails linking
                 -- a binary with unresolved symbols in RTS
                 -- which are defined in base package
                 -- the reason for this is a note in ld(1) about
                 -- '-u' option: "The placement of this option
                 -- on the command line is significant.
                 -- This option must be placed before the library
                 -- that defines the symbol."

    -- frameworks
    pkg_framework_opts <- getPkgFrameworkOpts dflags platform dep_packages
    let framework_opts = getFrameworkOpts dflags platform

        -- probably _stub.o files
    let extra_ld_inputs = ldInputs dflags

    -- Here are some libs that need to be linked at the *end* of
    -- the command line, because they contain symbols that are referred to
    -- by the RTS.  We can't therefore use the ordinary way opts for these.
    let debug_opts | WayDebug `elem` ways dflags = [
#if defined(HAVE_LIBBFD)
                        "-lbfd", "-liberty"
#endif
                         ]
                   | otherwise                   = []

        thread_opts | WayThreaded `elem` ways dflags = [
#if NEED_PTHREAD_LIB
                        "-lpthread"
#endif
                        ]
                    | otherwise                      = []

    rc_objs <- maybeCreateManifest dflags output_fn

    return (
                       map SysTools.Option verbFlags
                      ++ [ SysTools.Option "-o"
                         , SysTools.FileOption "" output_fn
                         ]
                      ++ libmLinkOpts
                      ++ map SysTools.Option (
                         []

                      -- See Note [No PIE when linking]
                      ++ picCCOpts dflags

                      -- Permit the linker to auto link _symbol to _imp_symbol.
                      -- This lets us link against DLLs without needing an "import library".
                      ++ (if platformOS platform == OSMinGW32
                          then ["-Wl,--enable-auto-import"]
                          else [])

                      -- '-no_compact_unwind'
                      -- C++/Objective-C exceptions cannot use optimised
                      -- stack unwinding code. The optimised form is the
                      -- default in Xcode 4 on at least x86_64, and
                      -- without this flag we're also seeing warnings
                      -- like
                      --     ld: warning: could not create compact unwind for .LFB3: non-standard register 5 being saved in prolog
                      -- on x86.
                      ++ (if sLdSupportsCompactUnwind mySettings &&
                             not staticLink &&
                             (platformOS platform == OSDarwin) &&
                             case platformArch platform of
                               ArchX86 -> True
                               ArchX86_64 -> True
                               ArchARM {} -> True
                               ArchARM64  -> True
                               _ -> False
                          then ["-Wl,-no_compact_unwind"]
                          else [])

                      -- '-Wl,-read_only_relocs,suppress'
                      -- ld gives loads of warnings like:
                      --     ld: warning: text reloc in _base_GHCziArr_unsafeArray_info to _base_GHCziArr_unsafeArray_closure
                      -- when linking any program. We're not sure
                      -- whether this is something we ought to fix, but
                      -- for now this flags silences them.
                      ++ (if platformOS   platform == OSDarwin &&
                             platformArch platform == ArchX86 &&
                             not staticLink
                          then ["-Wl,-read_only_relocs,suppress"]
                          else [])

                      ++ (if sLdIsGnuLd mySettings &&
                             not (gopt Opt_WholeArchiveHsLibs dflags)
                          then ["-Wl,--gc-sections"]
                          else [])

                      ++ o_files
                      ++ lib_path_opts)
                      ++ extra_ld_inputs
                      ++ map SysTools.Option (
                         rc_objs
                      ++ framework_opts
                      ++ pkg_lib_path_opts
                      ++ extraLinkObj:noteLinkObjs
                      ++ pkg_link_opts
                      ++ pkg_framework_opts
                      ++ debug_opts
                      ++ thread_opts
                    ))

exeFileName :: Bool -> DynFlags -> FilePath
exeFileName staticLink dflags
  | Just s <- outputFile dflags =
      case platformOS (targetPlatform dflags) of
          OSMinGW32 -> s <?.> "exe"
          _         -> if staticLink
                         then s <?.> "a"
                         else s
  | otherwise =
      if platformOS (targetPlatform dflags) == OSMinGW32
      then "main.exe"
      else if staticLink
           then "liba.a"
           else "a.out"
 where s <?.> ext | null (takeExtension s) = s <.> ext
                  | otherwise              = s

maybeCreateManifest
   :: DynFlags
   -> FilePath                          -- filename of executable
   -> IO [FilePath]                     -- extra objects to embed, maybe
maybeCreateManifest dflags exe_filename
 | platformOS (targetPlatform dflags) == OSMinGW32 &&
   gopt Opt_GenManifest dflags
    = do let manifest_filename = exe_filename <.> "manifest"

         writeFile manifest_filename $
             "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"++
             "  <assembly xmlns=\"urn:schemas-microsoft-com:asm.v1\" manifestVersion=\"1.0\">\n"++
             "  <assemblyIdentity version=\"1.0.0.0\"\n"++
             "     processorArchitecture=\"X86\"\n"++
             "     name=\"" ++ dropExtension exe_filename ++ "\"\n"++
             "     type=\"win32\"/>\n\n"++
             "  <trustInfo xmlns=\"urn:schemas-microsoft-com:asm.v3\">\n"++
             "    <security>\n"++
             "      <requestedPrivileges>\n"++
             "        <requestedExecutionLevel level=\"asInvoker\" uiAccess=\"false\"/>\n"++
             "        </requestedPrivileges>\n"++
             "       </security>\n"++
             "  </trustInfo>\n"++
             "</assembly>\n"

         -- Windows will find the manifest file if it is named
         -- foo.exe.manifest. However, for extra robustness, and so that
         -- we can move the binary around, we can embed the manifest in
         -- the binary itself using windres:
         if not (gopt Opt_EmbedManifest dflags) then return [] else do

         rc_filename <- newTempName dflags TFL_CurrentModule "rc"
         rc_obj_filename <-
           newTempName dflags TFL_GhcSession (objectSuf dflags)

         writeFile rc_filename $
             "1 24 MOVEABLE PURE " ++ show manifest_filename ++ "\n"
               -- magic numbers :-)
               -- show is a bit hackish above, but we need to escape the
               -- backslashes in the path.

         runWindres dflags $ map SysTools.Option $
               ["--input="++rc_filename,
                "--output="++rc_obj_filename,
                "--output-format=coff"]
               -- no FileOptions here: windres doesn't like seeing
               -- backslashes, apparently

         removeFile manifest_filename

         return [rc_obj_filename]
 | otherwise = return []
