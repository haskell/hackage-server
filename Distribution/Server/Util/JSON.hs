{-# LANGUAGE CPP #-}
module Distribution.Server.Util.JSON where

#if MIN_VERSION_aeson(0,6,2)
import Data.Aeson.Types

-- | Using these aeson 'Options' ensures that dervived instances are compatible
-- with the way these instances were generated before aeson-0.6.2. See
-- haskell/hackage-server#73 and bos/aeson#141
compatibilityOptions :: Options
compatibilityOptions =
      defaultOptions {
        sumEncoding = ObjectWithSingleField
      , allNullaryToStringTag = False
      }
#endif
