import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.Setup
import Distribution.PackageDescription

main = defaultMainWithHooks simpleUserHooks { preConf = minisatHook }

minisatHook _ flags = do
  rawSystemExit (fromFlag $ configVerbosity flags) "env" ["make"]
  return emptyHookedBuildInfo
