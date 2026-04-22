module Main where

import Control.Monad.IO.Class (liftIO)
import GHC
  ( InteractiveImport (IIDecl)
  , LoadHowMuch (LoadAllTargets)
  , SuccessFlag (Failed, Succeeded)
  , TcRnExprMode (TM_Inst)
  , exprType
  , getSessionDynFlags
  , guessTarget
  , load
  , mkModuleName
  , runGhc
  , setContext
  , setSessionDynFlags
  , setTargets
  , simpleImportDecl
  )
import GHC.Paths (libdir)
import GHC.Utils.Outputable (showPprUnsafe)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)

renderSuccess :: SuccessFlag -> String
renderSuccess = \case
  Succeeded -> "Succeeded"
  Failed -> "Failed"

targetPath :: FilePath
targetPath = "spike-target/src/Demo.hs"

main :: IO ()
main = do
  putStrLn ("[spike] libdir = " ++ libdir)
  putStrLn ("[spike] target = " ++ targetPath)
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags dflags
    tgt <- guessTarget targetPath Nothing Nothing
    setTargets [tgt]
    ok <- load LoadAllTargets
    liftIO (putStrLn ("[spike] load result = " ++ renderSuccess ok))
    case ok of
      Succeeded -> do
        setContext
          [ IIDecl (simpleImportDecl (mkModuleName "Prelude"))
          , IIDecl (simpleImportDecl (mkModuleName "Demo"))
          ]
        ty1 <- exprType TM_Inst "map (+1)"
        liftIO (putStrLn ("[spike] exprType \"map (+1)\" = " ++ showPprUnsafe ty1))
        ty2 <- exprType TM_Inst "greet"
        liftIO (putStrLn ("[spike] exprType \"greet\"     = " ++ showPprUnsafe ty2))
        liftIO exitSuccess
      _ -> liftIO (exitWith (ExitFailure 1))
