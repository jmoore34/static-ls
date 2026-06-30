module App.Ghcid (
  ghcid,
) where

import System.Directory (getCurrentDirectory)
import System.Process.Typed (proc, runProcess_)

ghcid :: [String] -> IO ()
ghcid args = do
  ghcidPwd <- getCurrentDirectory
  let ghcidProcessConfig = proc "ghcid" $ ["-o", "--repl-options=\"-fmax-uncovered-patterns=20\"", "ghcid.txt", "--setup", ":!pwd > " <> ghcidPwd <> "/.ghcid_session"] <> args
  runProcess_ ghcidProcessConfig
