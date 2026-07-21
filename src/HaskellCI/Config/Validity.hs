{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module HaskellCI.Config.Validity where

import HaskellCI.Prelude

import HaskellCI.Config
import HaskellCI.Config.Ubuntu
import HaskellCI.Error
import HaskellCI.Jobs
import HaskellCI.MonadErr

-- Validity checks shared in common among all backends.
checkConfigValidity :: MonadErr HsCiError m => Config -> JobVersions -> m ()
checkConfigValidity Config {..} _  = do
    unless (cfgUbuntu >= Focal) $
        throwErr $ ValidationError $ prettyShow cfgUbuntu ++ "distribution is not supported"

    unless cfgGhcupCabal $
        throwErr $ ValidationError $ "The GHCUP is the only supported installation method for cabal-install"

    -- We rely on the build semaphore, introduced in cabal-install 3.12.
    case cfgCabalInstallVersion of
        Just v | v < mkVersion [3,12] ->
            throwErr $ ValidationError $ "cabal-install " ++ prettyShow v ++ " is not supported, the minimum supported version is 3.12"
        _ -> pure ()
