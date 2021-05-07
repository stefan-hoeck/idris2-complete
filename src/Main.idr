||| Experimenting with shell auto-completion for Idris2
module Main

import IdrisPaths

import Core.Context
import Core.Directory
import Core.Metadata
import Core.Options
import Core.Unify

import Idris.Env
import Idris.Version
import Idris.Package.Types

import Core.Name.Namespace
import Core.Options

import Libraries.Utils.Path
import Libraries.Data.List1 as Lib
import Libraries.System.Directory.Tree
import Data.List1
import Data.Strings
import Data.Maybe
import Data.String
import System
import System.Directory

--------------------------------------------------------------------------------
--          Options
--------------------------------------------------------------------------------

-- These are copied right now, since lots of this stuff is private in
-- Idris.CommandLine
public export
data PkgCommand
      = Build
      | Install
      | MkDoc
      | Typecheck
      | Clean
      | REPL
      | Init

export
Show PkgCommand where
  show Build = "--build"
  show Install = "--install"
  show MkDoc = "--mkdoc"
  show Typecheck = "--typecheck"
  show Clean = "--clean"
  show REPL = "--repl"
  show Init = "--init"

public export
data DirCommand
      = LibDir -- show top level package directory

export
Show DirCommand where
  show LibDir = "--libdir"

||| CLOpt - possible command line options
public export
data CLOpt
  =
   ||| Only typecheck the given file
  CheckOnly |
   ||| The output file from the code generator
  OutputFile String |
   ||| Execute a given function after checking the source file
  ExecFn String |
   ||| Use a specific code generator
  SetCG String |
   ||| Pass a directive to the code generator
  Directive String |
   ||| Don't implicitly import Prelude
  NoPrelude |
   ||| Set source directory
  SourceDir String |
   ||| Set build directory
  BuildDir String |
   ||| Set output directory
  OutputDir String |
   ||| Generate profile data when compiling (backend dependent)
  Profile |
   ||| Show the installation prefix
  ShowPrefix |
   ||| Display Idris version
  Version |
   ||| Display help text
  Help |
   ||| Suppress the banner
  NoBanner |
   ||| Run Idris 2 in quiet mode
  Quiet |
   ||| Run Idris 2 in verbose mode (cancels quiet if it's the default)
  Verbose |
   ||| Set the console width for REPL output
  ConsoleWidth (Maybe Nat) |
   ||| Whether to use color in the console output
  Color Bool |
   ||| Set the log level globally
  Logging LogLevel |
   ||| Add a package as a dependency
  PkgPath String |
   ||| Build or install a given package, depending on PkgCommand
  Package PkgCommand String |
   ||| Show locations of data/library directories
  Directory DirCommand |
   ||| The input Idris file
  InputFile String |
   ||| Whether or not to run in IdeMode (easily parsable for other tools)
  IdeMode |
   ||| Whether or not to run IdeMode (using a socket instead of stdin/stdout)
  IdeModeSocket String |
   ||| Run as a checker for the core language TTImp
  Yaffle String |
   ||| Dump metadata from a .ttm file
  Metadata String |
   ||| Dump cases before compiling
  DumpCases String |
   ||| Dump lambda lifted defs before compiling
  DumpLifted String |
   ||| Dump ANF defs before compiling
  DumpANF String |
   ||| Dump VM code defs before compiling
  DumpVMCode String |
   ||| Run a REPL command then exit immediately
  RunREPL String |
  IgnoreMissingIPKG |
  FindIPKG |
  Timing |
  DebugElabCheck |
  BlodwenPaths

data OptType
  = Required String
   | Optional String
   | RequiredNat String
   | AutoNat String
   | RequiredLogLevel String

Show OptType where
  show (Required a) = "<" ++ a ++ ">"
  show (RequiredNat a) = "<" ++ a ++ ">"
  show (RequiredLogLevel a) = "<" ++ a ++ ">"
  show (Optional a) = "[" ++ a ++ "]"
  show (AutoNat a) = "<" ++ a ++ ">"

ActType : List OptType -> Type
ActType [] = List CLOpt
ActType (Required a :: as) = String -> ActType as
ActType (RequiredNat a :: as) = Nat -> ActType as
ActType (RequiredLogLevel a :: as) = LogLevel -> ActType as
ActType (Optional a :: as) = Maybe String -> ActType as
ActType (AutoNat a :: as) = Maybe Nat -> ActType as

||| Extract the host and port to bind the IDE socket to
export
ideSocketModeAddress : List CLOpt -> (String, Int)
ideSocketModeAddress []  = ("localhost", 38398)
ideSocketModeAddress (IdeModeSocket hp :: _) =
  let (h, p) = Strings.break (== ':') hp
      port = fromMaybe 38398 (portPart p >>= parsePositive)
      host = if h == "" then "localhost" else h
  in (host, port)
  where
    portPart : String -> Maybe String
    portPart p = if p == ""
                    then Nothing
                    else Just $ assert_total $ prim__strTail p
ideSocketModeAddress (_ :: rest) = ideSocketModeAddress rest

formatSocketAddress : (String, Int) -> String
formatSocketAddress (host, port) = host ++ ":" ++ show port

record OptDesc where
  constructor MkOpt
  flags : List String
  argdescs : List OptType
  action : ActType argdescs
  help : Maybe String

optSeparator : OptDesc
optSeparator = MkOpt [] [] [] Nothing

showDefault : Show a => a -> String
showDefault x = "(default " ++ show x ++ ")"

options : List OptDesc
options = [MkOpt ["--check", "-c"] [] [CheckOnly]
              (Just "Exit after checking source file"),
           MkOpt ["--output", "-o"] [Required "file"] (\f => [OutputFile f, Quiet])
              (Just "Specify output file"),
           MkOpt ["--exec", "-x"] [Required "name"] (\f => [ExecFn f, Quiet])
              (Just "Execute function after checking source file"),
           MkOpt ["--no-prelude"] [] [NoPrelude]
              (Just "Don't implicitly import Prelude"),
           MkOpt ["--codegen", "--cg"] [Required "backend"] (\f => [SetCG f])
              (Just $ "Set code generator " ++ showDefault (codegen defaultSession)),
           MkOpt ["--directive"] [Required "directive"] (\d => [Directive d])
              (Just $ "Pass a directive to the current code generator"),
           MkOpt ["--package", "-p"] [Required "package"] (\f => [PkgPath f])
              (Just "Add a package as a dependency"),
           MkOpt ["--source-dir"] [Required "dir"] (\d => [SourceDir d])
              (Just $ "Set source directory"),
           MkOpt ["--build-dir"] [Required "dir"] (\d => [BuildDir d])
              (Just $ "Set build directory"),
           MkOpt ["--output-dir"] [Required "dir"] (\d => [OutputDir d])
              (Just $ "Set output directory"),
           MkOpt ["--profile"] [] [Profile]
              (Just "Generate profile data when compiling, if supported"),

           optSeparator,
           MkOpt ["--prefix"] [] [ShowPrefix]
              (Just "Show installation prefix"),
           MkOpt ["--paths"] [] [BlodwenPaths]
              (Just "Show paths"),
           MkOpt ["--libdir"] [] [Directory LibDir]
              (Just "Show library directory"),

           optSeparator,
           MkOpt ["--init"] [Optional "package file"]
              (\ f => [Package Init (fromMaybe "" f)])
              (Just "Interactively initialise a new project"),

           MkOpt ["--build"] [Required "package file"] (\f => [Package Build f])
              (Just "Build modules/executable for the given package"),
           MkOpt ["--install"] [Required "package file"] (\f => [Package Install f])
              (Just "Install the given package"),
           MkOpt ["--mkdoc"] [Required "package file"] (\f => [Package MkDoc f])
              (Just "Build documentation for the given package"),
           MkOpt ["--typecheck"] [Required "package file"] (\f => [Package Typecheck f])
              (Just "Typechecks the given package without code generation"),
           MkOpt ["--clean"] [Required "package file"] (\f => [Package Clean f])
              (Just "Clean intermediate files/executables for the given package"),
           MkOpt ["--repl"] [Required "package file"] (\f => [Package REPL f])
              (Just "Build the given package and launch a REPL instance."),
           MkOpt ["--find-ipkg"] [] [FindIPKG]
              (Just "Find and use an .ipkg file in a parent directory."),
           MkOpt ["--ignore-missing-ipkg"] [] [IgnoreMissingIPKG]
              (Just "Fail silently if a dependency is missing."),

           optSeparator,
           MkOpt ["--ide-mode"] [] [IdeMode]
              (Just "Run the REPL with machine-readable syntax"),
           MkOpt ["--ide-mode-socket"] [Optional "host:port"]
                 (\hp => [IdeModeSocket $ fromMaybe (formatSocketAddress (ideSocketModeAddress [])) hp])
              (Just $ "Run the ide socket mode on given host and port " ++
                      showDefault (formatSocketAddress (ideSocketModeAddress []))),

           optSeparator,
           MkOpt ["--client"] [Required "REPL command"] (\f => [RunREPL f])
              (Just "Run a REPL command then quit immediately"),
           MkOpt ["--timing"] [] [Timing]
              (Just "Display timing logs"),

           optSeparator,
           MkOpt ["--no-banner"] [] [NoBanner]
              (Just "Suppress the banner"),
           MkOpt ["--quiet", "-q"] [] [Quiet]
              (Just "Quiet mode; display fewer messages"),
           MkOpt ["--console-width"] [AutoNat "console width"] (\l => [ConsoleWidth l])
              (Just "Width for console output (0 for unbounded) (auto by default)"),
           MkOpt ["--color", "--colour"] [] ([Color True])
              (Just "Forces colored console output (enabled by default)"),
           MkOpt ["--no-color", "--no-colour"] [] ([Color False])
              (Just "Disables colored console output"),
           MkOpt ["--verbose"] [] [Verbose]
              (Just "Verbose mode (default)"),
           MkOpt ["--log"] [RequiredLogLevel "log level"] (\l => [Logging l])
              (Just "Global log level (0 by default)"),

           optSeparator,
           MkOpt ["--version", "-v"] [] [Version]
              (Just "Display version string"),
           MkOpt ["--help", "-h", "-?"] [] [Help]
              (Just "Display help text"),

           -- Internal debugging options
           MkOpt ["--yaffle", "--ttimp"] [Required "ttimp file"] (\f => [Yaffle f])
              Nothing, -- run ttimp REPL rather than full Idris
           MkOpt ["--ttm" ] [Required "ttimp file"] (\f => [Metadata f])
              Nothing, -- dump metadata information from the given ttm file
           MkOpt ["--dumpcases"] [Required "output file"] (\f => [DumpCases f])
              Nothing, -- dump case trees to the given file
           MkOpt ["--dumplifted"] [Required "output file"] (\f => [DumpLifted f])
              Nothing, -- dump lambda lifted trees to the given file
           MkOpt ["--dumpanf"] [Required "output file"] (\f => [DumpANF f])
              Nothing, -- dump ANF to the given file
           MkOpt ["--dumpvmcode"] [Required "output file"] (\f => [DumpVMCode f])
              Nothing, -- dump VM Code to the given file
           MkOpt ["--debug-elab-check"] [] [DebugElabCheck]
              Nothing -- do more elaborator checks (currently conversion in LinearCheck)
           ]

--------------------------------------------------------------------------------
--          Context
--------------------------------------------------------------------------------

-- More copy paste
-- Add extra data from the "IDRIS2_x" environment variables
updateEnv : {auto c : Ref Ctxt Defs} ->
            Core ()
updateEnv
    = do defs <- get Ctxt
         bprefix <- coreLift $ idrisGetEnv "IDRIS2_PREFIX"
         the (Core ()) $ case bprefix of
              Just p => setPrefix p
              Nothing => setPrefix yprefix
         bpath <- coreLift $ idrisGetEnv "IDRIS2_PATH"
         the (Core ()) $ case bpath of
              Just path => do traverseList1_ addExtraDir (map trim (split (==pathSeparator) path))
              Nothing => pure ()
         bdata <- coreLift $ idrisGetEnv "IDRIS2_DATA"
         the (Core ()) $ case bdata of
              Just path => do traverseList1_ addDataDir (map trim (split (==pathSeparator) path))
              Nothing => pure ()
         blibs <- coreLift $ idrisGetEnv "IDRIS2_LIBS"
         the (Core ()) $ case blibs of
              Just path => do traverseList1_ addLibDir (map trim (split (==pathSeparator) path))
              Nothing => pure ()
         pdirs <- coreLift $ idrisGetEnv "IDRIS2_PACKAGE_PATH"
         the (Core ()) $ case pdirs of
              Just path => do traverseList1_ addPackageDir (map trim (split (==pathSeparator) path))
              Nothing => pure ()
         cg <- coreLift $ idrisGetEnv "IDRIS2_CG"
         the (Core ()) $ case cg of
              Just e => case getCG (options defs) e of
                             Just cg => setCG cg
                             Nothing => throw (InternalError ("Unknown code generator " ++ show e))
              Nothing => pure ()

         -- IDRIS2_PATH goes first so that it overrides this if there's
         -- any conflicts. In particular, that means that setting IDRIS2_PATH
         -- for the tests means they test the local version not the installed
         -- version
         defs <- get Ctxt
         -- These might fail while bootstrapping
         addDataDir (prefix_dir (dirs (options defs)) </>
                        ("idris2-" ++ showVersion False version) </> "support")
         addLibDir (prefix_dir (dirs (options defs)) </>
                        ("idris2-" ++ showVersion False version) </> "lib")
         Just cwd <- coreLift $ currentDir
              | Nothing => throw (InternalError "Can't get current directory")
         addLibDir cwd

--------------------------------------------------------------------------------
--          Package Names
--------------------------------------------------------------------------------

exploreNonHidden : (root : Path) -> IO (Tree root)
exploreNonHidden r = map (filter notHidden notHidden) $ explore r
  where notHidden : {root : _} -> FileName root -> Bool
        notHidden = not . isPrefixOf "." . fileName

-- List of directory entries at the given path
dirsAt : String -> IO (List String)
dirsAt pth = do MkTree _ ds <- exploreNonHidden (parse pth)
                pure (map (fileName . fst) ds)

-- List of directory entries at the given path
dirsPathsAt : String -> IO (List String)
dirsPathsAt pth = do MkTree _ ds <- exploreNonHidden (parse pth)
                     pure (map (\p => (toFilePath $ fst p) ++ "/") ds)

-- List of file entries at the given path
filesAt : String -> IO (List String)
filesAt pth = do MkTree fs _ <- exploreNonHidden (parse pth)
                 pure (map fileName fs)

-- lots of duplications with functionality in `Idris.SetOptions`. Will have
-- to extract the common parts once this goes to Idris2 itself
export
packageNames : String -> IO (List String)
packageNames dname = map (map (fst . getVersion)) $ dirsAt dname
  where
    toVersion : String -> Maybe PkgVersion
    toVersion = map MkPkgVersion
              . traverse parsePositive
              . split (== '.')

    getVersion : String -> (String, Maybe PkgVersion)
    getVersion str =
      -- Split the dir name into parts concatenated by "-"
      -- treating the last part as the version number
      -- and the initial parts as the package name.
      -- For reasons of backwards compatibility, we also
      -- accept hyphenated directory names without a part
      -- corresponding to a version number.
      case Lib.unsnoc $ split (== '-') str of
        (Nil, last) => (last, Nothing)
        (init,last) =>
          case toVersion last of
            Just v  => (concat $ intersperse "-" init, Just v)
            Nothing => (str, Nothing)

findIpkg : {auto c : Ref Ctxt Defs} -> Core (List String)
findIpkg =
  do Just srcdir <- coreLift currentDir
       | Nothing => throw (InternalError "Can't get current directory")
     fs <- coreLift $ filesAt srcdir
     pure $ filter (".ipkg" `isSuffixOf`) fs

findPackages : {auto c : Ref Ctxt Defs} -> Core (List String)
findPackages =
  do defs <- get Ctxt
     let globaldir = prefix_dir (dirs (options defs)) </>
                           "idris2-" ++ showVersion False version
     let depends = depends_dir (dirs (options defs))
     Just srcdir <- coreLift currentDir
         | Nothing => throw (InternalError "Can't get current directory")
     let localdir = srcdir </> depends

     -- Get candidate directories from the global install location,
     -- and the local package directory
     locFiles <- coreLift $ packageNames localdir
     globFiles <- coreLift $ packageNames globaldir
     pure $ locFiles ++ globFiles

dirExists : String -> IO Bool
dirExists fp = do
  Right dir <- openDir fp
    | Left _ => pure False
  closeDir dir
  pure True

completeDir : String -> Core (List String)
completeDir s = do b <- coreLift $ dirExists s
                   if b
                      then coreLift $ dirsPathsAt s
                      else case parent s of
                             Just p  => coreLift $ dirsPathsAt p
                             Nothing => pure []

listDirs : Core (List String)
listDirs = do d <- coreLift currentDir
              completeDir $ fromMaybe "." d

--------------------------------------------------------------------------------
--          Options
--------------------------------------------------------------------------------

sortedNub : Ord a => List a -> List a
sortedNub = nub . sort
  where nub : List a -> List a
        nub (a :: t@(b :: _)) = if a == b then nub t else a :: nub t
        nub xs                = xs

prefixOnly : String -> List String -> List String
prefixOnly x = sortedNub . filter (x `isPrefixOf`)

optStrings : List String
optStrings = options >>= flags

codegens : List String
codegens = map fst $ availableCGs defaults

opts : {auto c : Ref Ctxt Defs} -> List String -> Core (List String)
opts []         = pure []
opts ["idris2"] = pure optStrings

-- codegens
opts ("--cg" :: xs)           = pure codegens
opts ("--codegen" :: xs)      = pure codegens
opts (x :: "--cg" :: xs)      = pure $ prefixOnly x codegens
opts (x :: "--codegen" :: xs) = pure $ prefixOnly x codegens

-- packages
opts ("-p" :: xs)             = findPackages
opts ("--package" :: xs)      = findPackages
opts (x :: "-p" :: xs)        = prefixOnly x  <$> findPackages
opts (x :: "--package" :: xs) = prefixOnly x  <$> findPackages

-- with package files
opts ("--build" :: xs)          = findIpkg
opts ("--install" :: xs)        = findIpkg
opts ("--mkdoc" :: xs)          = findIpkg
opts ("--typecheck" :: xs)      = findIpkg
opts ("--clean" :: xs)          = findIpkg
opts ("--repl" :: xs)           = findIpkg
opts (x :: "--build" :: xs)     = prefixOnly x  <$> findIpkg
opts (x :: "--install" :: xs)   = prefixOnly x  <$> findIpkg
opts (x :: "--mkdoc" :: xs)     = prefixOnly x  <$> findIpkg
opts (x :: "--typecheck" :: xs) = prefixOnly x  <$> findIpkg
opts (x :: "--clean" :: xs)     = prefixOnly x  <$> findIpkg
opts (x :: "--repl" :: xs)      = prefixOnly x  <$> findIpkg

-- with directories
opts ("--source-dir" :: xs)       = listDirs
opts ("--build-dir" :: xs)        = listDirs
opts ("--output-dir" :: xs)       = listDirs
opts (x :: "--source-dir" :: xs)  = prefixOnly x <$> completeDir x
opts (x :: "--build-dir" :: xs)   = prefixOnly x <$> completeDir x
opts (x :: "--output-dir" :: xs)  = prefixOnly x <$> completeDir x

-- options
opts (x :: xs) = pure $ prefixOnly x  optStrings

--------------------------------------------------------------------------------
--          Main
--------------------------------------------------------------------------------

-- Parts of stMain from Idris.Driver
stMain : String -> Core (List String)
stMain str = do defs <- initDefs
                c <- newRef Ctxt defs
                setWorkingDir "."
                updateEnv
                opts . reverse. filter ((> 0) . length) $ words str

main : IO ()
main = do s    <- getEnv "IDRIS_WORDS"
          opts <- coreRun (stMain $ fromMaybe "" s) (\_ => pure []) pure
          putStr $ unwords opts
