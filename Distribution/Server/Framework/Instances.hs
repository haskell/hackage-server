{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, CPP,
             TypeFamilies, TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Typeable', 'Binary', 'Serialize', 'Text', and 'NFData' instances for various
-- types from Cabal, and other standard libraries.
--
-- Major version changes may break this module.
--

module Distribution.Server.Framework.Instances (
    PackageIdentifier_v0,
    compatAesonOptions,
    compatAesonOptionsDropPrefix,
  ) where

import Distribution.Text
import Distribution.Server.Framework.MemSize

import Distribution.Package  (PackageIdentifier(..), PackageName(..))
import Distribution.Compiler (CompilerFlavor(..), CompilerId(..))
import Distribution.System   (OS(..), Arch(..))
import Distribution.PackageDescription (FlagName(..))
import Distribution.Version

import Data.Time (Day(..), DiffTime, UTCTime(..))
import Control.Applicative
import Control.DeepSeq

import Data.Serialize as Serialize
import Data.SafeCopy hiding (Version)
import Test.QuickCheck

import qualified Data.Aeson as A
import Data.Aeson.Types as Aeson

import Happstack.Server

import Data.Maybe (fromJust)
import Data.List (stripPrefix)

import qualified Text.PrettyPrint as PP (text)
import Distribution.Compat.ReadP (readS_to_P)
#if !(MIN_VERSION_bytestring(0,10,0))
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
#endif


deriveSafeCopy 2 'extension ''PackageName
deriveSafeCopy 2 'extension ''PackageIdentifier

-- These types are not defined in this package, so we cannot easily control
-- changing these instances when the types change. So it's not safe to derive
-- them (except for the really stable ones).

instance SafeCopy Version where
    version = 2
    kind    = extension
    putCopy (Version ns _) = contain $ safePut ns
    getCopy = contain $ (\ns -> Version ns []) <$> safeGet

instance SafeCopy VersionRange where
    version = 2
    kind    = extension
    putCopy = contain . foldVersionRange'
                          (putWord8 0)
                          (\v     -> putWord8 1 >> safePut v)
                          (\v     -> putWord8 2 >> safePut v)
                          (\v     -> putWord8 3 >> safePut v)
                          (\v     -> putWord8 4 >> safePut v)
                          (\v     -> putWord8 5 >> safePut v)
                          (\v _   -> putWord8 6 >> safePut v)
                          (\r1 r2 -> putWord8 7 >> r1 >> r2)
                          (\r1 r2 -> putWord8 8 >> r1 >> r2)
                          (\r     -> putWord8 9 >> r)
    getCopy = contain getVR
      where
        getVR = do
          tag <- getWord8
          case tag of
            0 -> return anyVersion
            1 -> thisVersion      <$> safeGet
            2 -> laterVersion     <$> safeGet
            3 -> earlierVersion   <$> safeGet
            4 -> orLaterVersion   <$> safeGet
            5 -> orEarlierVersion <$> safeGet
            6 -> withinVersion    <$> safeGet
            7 -> unionVersionRanges     <$> getVR <*> getVR
            8 -> intersectVersionRanges <$> getVR <*> getVR
            9 -> VersionRangeParens     <$> getVR
            _ -> fail "VersionRange.getCopy: bad tag"

instance SafeCopy OS where
    putCopy (OtherOS s) = contain $ putWord8 0 >> safePut s
    putCopy Linux       = contain $ putWord8 1
    putCopy Windows     = contain $ putWord8 2
    putCopy OSX         = contain $ putWord8 3
    putCopy FreeBSD     = contain $ putWord8 4
    putCopy OpenBSD     = contain $ putWord8 5
    putCopy NetBSD      = contain $ putWord8 6
    putCopy Solaris     = contain $ putWord8 7
    putCopy AIX         = contain $ putWord8 8
    putCopy HPUX        = contain $ putWord8 9
    putCopy IRIX        = contain $ putWord8 10
    putCopy HaLVM       = contain $ putWord8 11
    putCopy IOS         = contain $ putWord8 12
    putCopy DragonFly   = contain $ putWord8 13
    putCopy Ghcjs       = contain $ putWord8 14
    putCopy Hurd        = contain $ putWord8 15
    putCopy Android     = contain $ putWord8 16

    getCopy = contain $ do
      tag <- getWord8
      case tag of
        0  -> return OtherOS <*> safeGet
        1  -> return Linux
        2  -> return Windows
        3  -> return OSX
        4  -> return FreeBSD
        5  -> return OpenBSD
        6  -> return NetBSD
        7  -> return Solaris
        8  -> return AIX
        9  -> return HPUX
        10 -> return IRIX
        11 -> return HaLVM
        12 -> return IOS
        13 -> return DragonFly
        14 -> return Ghcjs
        15 -> return Hurd
        16 -> return Android
        _  -> fail "SafeCopy OS getCopy: unexpected tag"

instance SafeCopy  Arch where
    putCopy (OtherArch s) = contain $ putWord8 0 >> safePut s
    putCopy I386          = contain $ putWord8 1
    putCopy X86_64        = contain $ putWord8 2
    putCopy PPC           = contain $ putWord8 3
    putCopy PPC64         = contain $ putWord8 4
    putCopy Sparc         = contain $ putWord8 5
    putCopy Arm           = contain $ putWord8 6
    putCopy Mips          = contain $ putWord8 7
    putCopy SH            = contain $ putWord8 8
    putCopy IA64          = contain $ putWord8 9
    putCopy S390          = contain $ putWord8 10
    putCopy Alpha         = contain $ putWord8 11
    putCopy Hppa          = contain $ putWord8 12
    putCopy Rs6000        = contain $ putWord8 13
    putCopy M68k          = contain $ putWord8 14
    putCopy Vax           = contain $ putWord8 15
    putCopy JavaScript    = contain $ putWord8 16

    getCopy = contain $ do
      tag <- getWord8
      case tag of
        0  -> return OtherArch <*> safeGet
        1  -> return I386
        2  -> return X86_64
        3  -> return PPC
        4  -> return PPC64
        5  -> return Sparc
        6  -> return Arm
        7  -> return Mips
        8  -> return SH
        9  -> return IA64
        10 -> return S390
        11 -> return Alpha
        12 -> return Hppa
        13 -> return Rs6000
        14 -> return M68k
        15 -> return Vax
        16 -> return JavaScript
        _  -> fail "SafeCopy Arch getCopy: unexpected tag"

instance SafeCopy CompilerFlavor where
    putCopy (OtherCompiler s) = contain $ putWord8 0 >> safePut s
    putCopy GHC               = contain $ putWord8 1
    putCopy NHC               = contain $ putWord8 2
    putCopy YHC               = contain $ putWord8 3
    putCopy Hugs              = contain $ putWord8 4
    putCopy HBC               = contain $ putWord8 5
    putCopy Helium            = contain $ putWord8 6
    putCopy JHC               = contain $ putWord8 7
    putCopy LHC               = contain $ putWord8 8
    putCopy UHC               = contain $ putWord8 9
    putCopy (HaskellSuite s)  = contain $ putWord8 10 >> safePut s
    putCopy GHCJS             = contain $ putWord8 11

    getCopy = contain $ do
      tag <- getWord8
      case tag of
        0  -> return OtherCompiler <*> safeGet
        1  -> return GHC
        2  -> return NHC
        3  -> return YHC
        4  -> return Hugs
        5  -> return HBC
        6  -> return Helium
        7  -> return JHC
        8  -> return LHC
        9  -> return UHC
        10 -> return HaskellSuite <*> safeGet
        11 -> return GHCJS
        _  -> fail "SafeCopy CompilerFlavor getCopy: unexpected tag"


deriveSafeCopy 0 'base ''CompilerId
deriveSafeCopy 0 'base ''FlagName


instance FromReqURI PackageIdentifier where
  fromReqURI = simpleParse

instance FromReqURI PackageName where
  fromReqURI = simpleParse

instance FromReqURI Version where
  fromReqURI = simpleParse


-- rough versions of RNF for these
#if !(MIN_VERSION_bytestring(0,10,0))
instance NFData LBS.ByteString where
    rnf bs = LBS.length bs `seq` ()

instance NFData SBS.ByteString where
    rnf bs = bs `seq` ()
#endif

instance NFData Response where
    rnf res@(Response{}) = rnf (rsBody res) `seq` rnf (rsHeaders res)
    rnf _ = ()

instance NFData HeaderPair where
    rnf (HeaderPair a b) = rnf a `seq` rnf b

#if !(MIN_VERSION_deepseq(1,3,0))
instance NFData Version where
    rnf (Version branch tags) = rnf branch `seq` rnf tags
#endif

#if !(MIN_VERSION_time(1,4,0))
instance NFData Day where
    rnf (ModifiedJulianDay a) = rnf a
#endif

instance MemSize Response where
    memSize (Response a b c d e) = memSize5 a b c d e
    memSize (SendFile{})         = 42

instance MemSize HeaderPair where
    memSize (HeaderPair a b) = memSize2 a b

instance MemSize RsFlags where
    memSize (RsFlags a) = memSize1 a

instance MemSize Length where
    memSize _ = memSize0

instance Text Day where
  disp  = PP.text . show
  parse = readS_to_P (reads :: ReadS Day)

instance Text UTCTime where
  disp  = PP.text . show
  parse = readS_to_P (reads :: ReadS UTCTime)

-------------------
-- Arbitrary instances
--

instance Arbitrary PackageName where
  arbitrary = PackageName <$> vectorOf 4 (choose ('a', 'z'))

#if !(MIN_VERSION_QuickCheck(2,9,0))
instance Arbitrary Version where
  arbitrary = Version <$> listOf1 (choose (1, 15)) <*> pure []
#endif

instance Arbitrary PackageIdentifier where
  arbitrary = PackageIdentifier <$> arbitrary <*> arbitrary

instance Arbitrary CompilerFlavor where
  arbitrary = oneof [ pure OtherCompiler <*> vectorOf 3 (choose ('A', 'Z'))
                    , pure GHC, pure NHC, pure YHC, pure Hugs, pure HBC
                    , pure Helium, pure JHC, pure LHC, pure UHC ]

instance Arbitrary CompilerId where
  arbitrary = CompilerId <$> arbitrary <*> arbitrary

instance Arbitrary Arch where
  arbitrary = oneof [ pure OtherArch <*> vectorOf 3 (choose ('A', 'Z'))
                    , pure I386, pure X86_64, pure PPC, pure PPC64, pure Sparc
                    , pure Arm, pure Mips, pure SH, pure IA64, pure S390
                    , pure Alpha, pure Hppa, pure Rs6000, pure M68k, pure Vax ]

instance Arbitrary OS where
  arbitrary = oneof [ pure OtherOS <*> vectorOf 3 (choose ('A', 'Z'))
                    , pure Linux, pure Windows, pure OSX, pure FreeBSD
                    , pure OpenBSD, pure NetBSD, pure Solaris, pure AIX
                    , pure HPUX, pure IRIX, pure HaLVM, pure IOS ]

instance Arbitrary FlagName where
  arbitrary = FlagName <$> vectorOf 4 (choose ('a', 'z'))

-- Below instances copied from
-- <http://hackage.haskell.org/package/quickcheck-instances-0.2.0/docs/src/Test-QuickCheck-Instances.html>

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> (2000 +) <$> arbitrary
    shrink    = (ModifiedJulianDay <$>) . shrink . toModifiedJulianDay

instance Arbitrary DiffTime where
    arbitrary = arbitrarySizedFractional
#if MIN_VERSION_time(1,3,0)
    shrink    = shrinkRealFrac
#else
    shrink    = (fromRational <$>) . shrink . toRational
#endif

instance Arbitrary UTCTime where
    arbitrary =
        UTCTime
        <$> arbitrary
        <*> (fromRational . toRational <$> choose (0::Double, 86400))
    shrink ut@(UTCTime day dayTime) =
        [ ut { utctDay     = d' } | d' <- shrink day     ] ++
        [ ut { utctDayTime = t' } | t' <- shrink dayTime ]

--------------------------
-- Old SafeCopy versions
--

newtype PackageIdentifier_v0 = PackageIdentifier_v0 PackageIdentifier
    deriving (Eq, Ord)

instance SafeCopy PackageIdentifier_v0
instance Serialize PackageIdentifier_v0 where
    put (PackageIdentifier_v0 pkgid) = Serialize.put (show pkgid)
    get = PackageIdentifier_v0 . read <$> Serialize.get

instance Migrate PackageIdentifier where
    type MigrateFrom PackageIdentifier = PackageIdentifier_v0
    migrate (PackageIdentifier_v0 pkgid) = pkgid


newtype PackageName_v0 = PackageName_v0 PackageName

instance SafeCopy PackageName_v0 where
    putCopy (PackageName_v0 nm) = contain $ textPut_v0 nm
    getCopy = contain $ PackageName_v0 <$> textGet_v0

instance Migrate PackageName where
    type MigrateFrom PackageName = PackageName_v0
    migrate (PackageName_v0 pn) = pn


newtype Version_v0 = Version_v0 Version

instance SafeCopy Version_v0 where
    putCopy (Version_v0 v) = contain $ textPut_v0 v
    getCopy = contain $ Version_v0 <$> textGet_v0

instance Migrate Version where
    type MigrateFrom Version = Version_v0
    migrate (Version_v0 v) = v


newtype VersionRange_v0 = VersionRange_v0 VersionRange

instance SafeCopy VersionRange_v0 where
    putCopy (VersionRange_v0 v) = contain $ textPut_v0 v
    getCopy = contain $ VersionRange_v0 <$> textGet_v0

instance Migrate VersionRange where
    type MigrateFrom VersionRange = VersionRange_v0
    migrate (VersionRange_v0 v) = v


textGet_v0 :: Text a => Serialize.Get a
textGet_v0 = (fromJust . simpleParse) <$> Serialize.get

textPut_v0 :: Text a => a -> Serialize.Put
textPut_v0 = Serialize.put . display

---------------------------------------------------------------------

--------------------------
-- Aeson instance helper
--

-- | Using these aeson 'Options' ensures that dervived instances are compatible
-- with the way these instances were generated before aeson-0.6.2. See
-- haskell/hackage-server#73 and bos/aeson#141
compatAesonOptions :: Aeson.Options
compatAesonOptions =
    Aeson.defaultOptions {
      sumEncoding           = ObjectWithSingleField,
      allNullaryToStringTag = False
    }

compatAesonOptionsDropPrefix :: String -> Aeson.Options
compatAesonOptionsDropPrefix prefix =
    compatAesonOptions {
      fieldLabelModifier = dropPrefix
    }
  where
    dropPrefix str = case stripPrefix prefix str of
                       Nothing   -> error err
                       Just str' -> str'
      where
        err = "compatAesonOptionsDropPrefix: expected field prefix of "
           ++ show prefix ++ ", but got " ++ show str
