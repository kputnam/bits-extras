{-# LANGUAGE CPP #-}
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo, buildDir)
import System.Cmd(system)

ps :: String
#if mingw32_HOST_OS || mingw32_TARGET_OS
ps = ['\\']
#else
ps = ['/']
#endif

main = defaultMainWithHooks hooks
  where hooks = simpleUserHooks { runTests = runTests' }

runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTests' _ _ _ lbi = system testprog >> return ()
  where testprog = (buildDir lbi) ++ ps ++ "test" ++ ps ++ "test"
