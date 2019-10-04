{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import           Prelude

import           Control.Monad.Extra (unlessM)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except
                   (ExceptT(..), runExceptT, withExceptT)
import           Data.Aeson (FromJSON(..), Value(..), (.:?), (.:))
import           Data.Aeson.Internal ((<?>))
import           Data.Bifunctor (bimap)
import           Data.Set (Set)
import           Data.String (IsString)
import           Data.Text (Text, pack, unpack)
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson as AE
import qualified Data.Aeson.Internal as AE
import qualified Data.Aeson.Types as AE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Char as Char
import qualified Data.HashMap.Lazy as HMap
import qualified Data.List as List
import qualified Data.Scientific as Sci
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Network.URI as URI
import qualified Options.Applicative as Opt
import           Options.Applicative
                   ( (<**>), Parser, ParserInfo, help, long
                   , metavar, showHelpOnEmpty, strOption)

import qualified System.Directory as Dir
import qualified System.FilePath as Path
import           System.FilePath ((</>), (<.>))

import           System.Exit (exitFailure)

main :: IO ()
main = do
  ValidateSubmission root submission
    <- Opt.customExecParser (Opt.prefs showHelpOnEmpty) opts
  r <- runExceptT $ validateRegistrySubmission root submission
  case r of
    Left e -> do
      putStrLn $ "Errors during validation:\n" <> unpack e
      exitFailure
    Right _ -> pure ()
  where
    opts :: ParserInfo CLI
    opts =
      Opt.info (parseCLI <**> Opt.helper)
        ( Opt.fullDesc
          <> Opt.header
          "validator - validate testnet stake pool registry submission entries."
        )

parseCLI :: Parser CLI
parseCLI =
  ValidateSubmission
    <$> (RegistryRoot <$> parseFilePath
           "registry-root"
           "Root directory of the testnet stake pool registry.")
    <*> (SubmissionFile <$> parseFilePath
           "registry-submission"
           "File to scrutinise for registry submission.")
 where
   parseFilePath :: String -> String -> Parser FilePath
   parseFilePath optname desc =
     strOption $ long optname <> metavar "FILEPATH" <> help desc

data CLI
  = ValidateSubmission
    RegistryRoot
    -- ^ Root directory of the registry.
    SubmissionFile
    -- ^ File scrutinised as a submission.

newtype RegistryRoot =
  RegistryRoot FilePath
  deriving (Eq, Ord, Show, IsString)

newtype SubmissionFile =
  SubmissionFile FilePath
  deriving (Eq, Ord, Show, IsString)

data Submission = Submission
  { sId :: !Id
  , sTicker :: !Ticker
    -- | Absolute URI.
  , sHomepage :: !URI.URI
  , sPledgeAddress :: Maybe PledgeAddress
  }

-- | Stake pool ID, Bech32-encoded public key.
newtype Id = Id { unId :: Bech32 }
  deriving (Eq)

-- | Stake pool ticker, a 3-4 all-ASCII-uppercase character ID.
newtype Ticker = Ticker { unTicker :: Text }
  deriving (Eq, Ord, Show)

-- | Bech32-encoded address.
newtype PledgeAddress = PledgeAddress { unPledgeAddress :: Bech32 }

-- | Represent all information from the Bech32 decoder.
data Bech32 = Bech32
  { bechHumanReadable :: Bech32.HumanReadablePart
  , bechDataPart      :: Bech32.DataPart
  } deriving (Eq)

instance Show Bech32 where
  show = show . bechHumanReadable

-- | Given a stake pool registry root and a submission file,
--   perform full validation of relative file structure
--   & JSON content of the submission.
validateRegistrySubmission
  :: RegistryRoot
  -> SubmissionFile
  -> ExceptT Text IO Submission
validateRegistrySubmission (RegistryRoot root) (SubmissionFile fp) = do
  -- Directory structure/access checks.
  --
  afp <- lift $ Dir.makeAbsolute fp
  unlessM (lift $ Dir.doesFileExist afp) $
    checks ["Submission doesn't exist: " <> pack afp]
  unlessM (lift $ Dir.readable <$> Dir.getPermissions afp) $
    checks ["Submission not readable: " <> pack afp]
  unlessM (lift $ Dir.doesDirectoryExist root) $
    checks ["Registry root doesn't exist: " <> pack afp]

  let (dir', file) = Path.splitFileName afp
      dir = Path.dropTrailingPathSeparator dir'
      (parentParentDir, parentDir) = Path.splitFileName dir

  checks $
    ["Submission not in 'registry' directory"
    | parentDir /= "registry"] <>
    ["Submission not properly relative the registry root"
    | Path.dropTrailingPathSeparator root
      /= Path.dropTrailingPathSeparator parentParentDir]

  let (pubkeyFile, ext) = Path.splitExtension file
      sigPath = dir </> pubkeyFile <.> "sig"

  unlessM (lift $ Dir.doesFileExist sigPath) $
    checks ["Missing signature file: " <> pack sigPath]
  unlessM (lift $ Dir.readable <$> Dir.getPermissions sigPath) $
    checks ["Signature file not readable: " <> pack sigPath]
  checks $
    ["Submission filename doesn't have .json extension: " <> pack file
    | ext /= ".json" ]

  pubkey <- case validatePublicKey Ed25519 (pack pubkeyFile) of
    Left e -> err $
      ["Submission filename (actually public key) invalid: " <> e]
    Right x -> pure x

  contents <- lift $ BSL.fromStrict <$> BS.readFile afp
  case AE.eitherDecode contents of
    Left e -> err $ ["Submission content:\n" <> pack e]
    Right entry -> do

      checks $
        [ pack $ "Internal 'id' doesn't match filename: "
          <> show (unId $ sId entry)  <> " vs. " <> show (unId pubkey)
        | sId entry /= pubkey ]

      -- All cheap checks done, let's do the expensive one now.
      dupTickers <- withExceptT (("While reading all registry entries: "<>)) $
        duplicateTickers dir

      checks $
        [ "Duplicate tickers: " <>
          pack (unpack $ Text.intercalate ", " $ unTicker <$> dupTickers)
        | not (List.null dupTickers) ]

      -- All valid.
      pure entry

 where
   checks :: [Text] -> ExceptT Text IO ()
   checks [] = ExceptT . pure . Right $ ()
   checks es = err es

   err :: [Text] -> ExceptT Text IO a
   err es = ExceptT . pure . Left . Text.unlines $ es

   duplicateTickers :: FilePath -> ExceptT Text IO [Ticker]
   duplicateTickers dir = do
     paths <- ((dir </>) <$>) <$> (lift $ Dir.listDirectory dir)
     -- We maintain the invariant that 'registry' contains only files.
     tickers <- withExceptT pack $ ExceptT $
       sequence <$> traverse entryTicker (selectEntries paths)

     pure . Set.toList $ duplicates tickers
     where
       selectEntries :: [FilePath] -> [FilePath]
       selectEntries paths =
         flip filter paths $ (==".json") . Path.takeExtension

       entryTicker :: FilePath -> IO (Either String Ticker)
       entryTicker f =
         (bimap (("While parsing '"<>f<>"': ")<>) sTicker) . AE.eitherDecode . BSL.fromStrict
           <$> BS.readFile f

-- | Type of public key.
data PubKeyType
  = Ed25519

instance Show PubKeyType where
  show Ed25519 = "ed25519"

registryPubKeyType :: PubKeyType
registryPubKeyType = Ed25519

--------------------------------------------------------------------------------
-- * Ancillary
--
-- | Submission identifier is actually a 32-byte base16-encoded public key.
--   We additionally require that the hexadecimal must be all-lowercase.
validatePublicKey :: PubKeyType -> Text -> Either Text Id
validatePublicKey pkType text =
  if      not ((Char.toLower <$> s) == s)
  then fail $ "Public key not all-lowercase: " <> s
  else Id <$> decodeBech32 "Public key" Bech32.decode
       (pack (show pkType) <> "_" <> text)
  where s = unpack text

duplicates :: (Ord a) => [a] -> Set a
duplicates xs = snd $ foldl step (Set.empty, Set.empty) xs
  where
    step :: Ord a => (Set a, Set a) -> a -> (Set a, Set a)
    step (seen, dups) x =
      if Set.member x seen
      then (seen, Set.insert x dups)
      else (Set.insert x seen, dups)

--------------------------------------------------------------------------------
-- * Instances
--
instance FromJSON Submission where
  parseJSON = AE.withObject "Submission" $ \v->
    case validateFields v of
      [] -> Submission
              <$> v .: "id" <?> AE.Key "id"
              <*> v .: "ticker" <?> AE.Key "ticker"
              <*> v .: "homepage" <?> AE.Key "homepate"
              <*> v .:? "pledge_address" <?> AE.Key "pledge_address"
      xs -> fail $ List.unlines xs
    where validateFields :: AE.Object -> [String]
          validateFields v =
            [ "Missing fields: " <> commaList missing
            | not (Set.null missing) ] <>
            [ "Unexpected fields: " <> commaList unexpected
            | not (Set.null unexpected) ]
            where
              keys, mandatory, optional', missing, unexpected :: Set Text
              keys = Set.fromList $ HMap.keys v
              mandatory = Set.fromList ["id", "ticker", "homepage"]
              optional' = Set.fromList ["pledge_address"]
              missing = mandatory `Set.difference` keys
              unexpected = keys `Set.difference` (mandatory <> optional')

              commaList :: Set Text -> String
              commaList = List.intercalate ", " . (show . unpack <$>) . Set.toList

instance FromJSON Id where
  parseJSON = AE.withText "Id" $
    (either (fail . unpack) pure) . validatePublicKey registryPubKeyType

instance FromJSON Ticker where
  parseJSON = AE.withText "Ticker" $ \v ->
    let s = unpack v
    in if (length s >= 3 && length s <= 4
           && all Char.isAsciiUpper s)
    then pure $ Ticker v
    else fail $ "Not an uppercase ASCII of length 3 or 4: " <> take 32 s

instance FromJSON URI.URI where
  parseJSON = AE.withText "Text" $ \v ->
    case URI.parseAbsoluteURI (unpack v) of
      Nothing -> fail $ "Not an absolute URI: " <> unpack v
      Just x -> pure x

-- | Given a description and a Bech32 decoder, run it on the input text.
decodeBech32 :: String -> (Text -> Either Bech32.DecodingError (Bech32.HumanReadablePart, Bech32.DataPart)) -> Text -> Either Text Bech32
decodeBech32 desc decoder text =
  case decoder text of
    Left e -> fail $ desc <> " decoding error for '"<>unpack text<>"':\n" <> show e
    Right (humanPart, dataPart) -> pure $ Bech32 humanPart dataPart

instance FromJSON PledgeAddress where
  parseJSON = AE.withText "PledgeAddress" $ \v->
    either (fail . unpack) (pure . PledgeAddress) $
      decodeBech32 "Pledge address" Bech32.decodeLenient v

-- | @'withBoundedScientific' expected f value@ applies @f@ to the 'Scientific' number
-- when @value@ is a 'Number' and fails using @'typeMismatch' expected@
-- otherwise.
--
-- The conversion will also fail with a @'typeMismatch' if the
-- 'Scientific' exponent is larger than 1024.
_withBoundedScientific :: String -> (Sci.Scientific -> AE.Parser a) -> Value -> AE.Parser a
_withBoundedScientific _ f v@(Number scientific) =
    if Sci.base10Exponent scientific > 1024
    then AE.typeMismatch "a number with exponent <= 1024" v
    else f scientific
_withBoundedScientific expected _ v = AE.typeMismatch expected v
