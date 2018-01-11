{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedLists            #-}

module Main where

import           Universum hiding (bracket)
import           Data.Maybe (fromJust)
import           Formatting (sformat, shown, (%))
import           Mockable (Production, runProduction)
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..), SimpleNodeArgs (..))
import qualified Pos.Client.CLI as CLI
import           Pos.Core (GenesisData (..), genesisData)
import           Pos.Launcher (HasConfigurations, NodeParams (..), LoggingParams(..),
                               withConfigurations, loggerBracket)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)
import           System.Wlog (LoggerName, logInfo)
import           Wallet.Node (runWalletNode)

----------------------------------------------------------------------------------------------------

loggerName :: LoggerName
loggerName = "node"

action
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => SimpleNodeArgs
    -> Production ()
action (SimpleNodeArgs (commonNodeArgs@CommonNodeArgs {..}) (nodeArgs@NodeArgs {..})) = do
    whenJust cnaDumpGenesisDataPath $ CLI.dumpGenesisData True
    logInfo $ sformat ("System start time is " % shown) $ gdStartTime genesisData
    currentParams <- CLI.getNodeParams loggerName commonNodeArgs nodeArgs
    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
    let sscParams = CLI.gtSscParams commonNodeArgs vssSK (npBehaviorConfig currentParams)
    runWalletNode sscParams currentParams

main :: IO ()
main = withCompileInfo $(retrieveCompileTimeInfo) $ do
    args@(CLI.SimpleNodeArgs commonNodeArgs _) <- CLI.getSimpleNodeOptions
    let loggingParams = CLI.loggingParams "wallet-backend" commonNodeArgs
    let conf = CLI.configurationOptions (CLI.commonArgs commonNodeArgs)
    loggerBracket (loggingParams { lpConsoleLog = Just False }) . runProduction $ do
        CLI.printFlags
        withConfigurations conf $ action args
