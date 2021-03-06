{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launcher.
         runNodeReal
       ) where

import           Universum

import           Pos.Chain.Ssc (SscParams)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Core.Configuration (epochSlots)
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Txp (txpGlobalSettings)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Launcher.Param (NodeParams (..))
import           Pos.Launcher.Resource (NodeResources (..),
                     bracketNodeResources)
import           Pos.Launcher.Runner (runRealMode)
import           Pos.Launcher.Scenario (runNode)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.Trace.Named (TraceNamed, natTrace)
import           Pos.WorkMode (EmptyMempoolExt, RealMode)


-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
runNodeReal
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => TraceNamed IO
    -> ProtocolMagic
    -> TxpConfiguration
    -> NodeParams
    -> SscParams
    -> [Diffusion (RealMode EmptyMempoolExt) -> RealMode EmptyMempoolExt ()]
    -> IO ()
runNodeReal logTrace pm txpConfig np sscnp plugins =
    bracketNodeResources (natTrace liftIO logTrace) np sscnp (txpGlobalSettings pm txpConfig) (initNodeDBs pm epochSlots)
        action
  where
    action :: NodeResources EmptyMempoolExt -> IO ()
    action nr@NodeResources {..} =
      runRealMode logTrace pm txpConfig nr (runNode (natTrace liftIO logTrace) pm txpConfig nr plugins)
