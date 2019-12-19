{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import           Prelude hiding (fail)

import           Data.Either
import           Control.Applicative(optional)
import           Control.Arrow (first)
import           Control.Monad (unless)
import           Control.Monad.Extra (unlessM, void)
import           Control.Monad.Fail (MonadFail(..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except
                   (ExceptT(..), runExceptT, withExceptT)
import           Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.:?), (.:), (.=))
import           Data.Aeson.Internal ((<?>))
import           Data.Aeson.Encode.Pretty         (encodePretty)
import           Data.Bifunctor (bimap)
import           Data.Set (Set)
import           Data.String (IsString)
import           Data.Text (Text, pack, unpack)
import           GHC.Generics (Generic)
import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson as AE
import qualified Data.Aeson.Internal as AE
import qualified Data.Aeson.Types as AE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as LBU
import qualified Data.Char as Char
import qualified Data.HashMap.Lazy as HMap
import qualified Data.List as List
import qualified Data.Scientific as Sci
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
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
  cmd <- Opt.customExecParser (Opt.prefs showHelpOnEmpty) opts

  res <- runExceptT $
    case cmd of
      ValidateSubmission root submission ->
        void $ validateRegistrySubmission root submission
      PrepareSubmission name desc ticker uri pledge fp ->
        prepareRegistrySubmission name desc ticker uri pledge fp
          >>= writeRegistrySubmission
  case res of
    Left e -> do
      putStrLn $ "Errors:\n" <> unpack e
      exitFailure
    Right _ -> pure ()

  where
    opts :: ParserInfo CLI
    opts =
      Opt.info (parseCLI <**> Opt.helper)
        ( Opt.fullDesc
          <> Opt.header
          "registry - process testnet stake pool registry submissions"
        )

parseCLI :: Parser CLI
parseCLI = Opt.subparser $
  command' "validate-submission" "Validate stake pool registry entry submission."
    (ValidateSubmission
       <$> (RegistryRoot <$> parseFilePath "DIR"
              "registry-root"
              "Root directory of the testnet stake pool registry.")
       <*> (SubmissionFile <$> parseFilePath "FILEPATH"
              "registry-submission"
              "File to scrutinise for registry submission.")) <>
  command' "prepare-submission" "Make a stake pool registry entry submission."
    (PrepareSubmission
       <$> parseName
              "name"
              "1 to 50 UTF-16 characters."
       <*> optional (parseDescription
              "description"
              "optional 1 to 255 UTF-16 characters.")
       <*> parseTicker
              "ticker"
              "3 to 5 upper-ASCII-character stake pool ticker."
       <*> parseURI
              "homepage"
              "Absolute URI of the stake pool's public web presence."
       <*> parsePledgeAddress
              "pledge-address"
              "Pledge address, Bech32-encoded string."
       <*> (PublicKeyFile <$> parseFilePath "FILEPATH"
              "public-key-file"
              "Public key file, Bech32-encoded -- output of 'jcli key to-public'"))
 where
   parseName :: String -> String -> Parser Name
   parseName opt desc =
     (Opt.option (Opt.eitherReader (validateName . Text.pack)) $
       long opt <> help desc <> metavar "NAME")

   parseDescription :: String -> String -> Parser Description
   parseDescription opt desc =
     (Opt.option (Opt.eitherReader (validateDescription . Text.pack)) $
       long opt <> help desc <> metavar "DESCRIPTION")

   parseTicker :: String -> String -> Parser Ticker
   parseTicker opt desc =
     (Opt.option (Opt.eitherReader validateTicker) $
       long opt <> help desc <> metavar "TICKER")

   parseURI :: String -> String -> Parser URI.URI
   parseURI opt desc =
     (Opt.option (Opt.eitherReader validateURI) $
       long opt <> help desc <> metavar "URI")

   parsePledgeAddress :: String -> String -> Parser PledgeAddress
   parsePledgeAddress opt desc =
     (Opt.option (Opt.eitherReader validatePledgeAddress) $
       long opt <> help desc <> metavar "PLEDGE-ADDRESS")

   parseFilePath :: String -> String -> String -> Parser FilePath
   parseFilePath meta optname desc =
     strOption $ long optname <> metavar meta <> help desc

   command'
     :: Prelude.String
     -> Prelude.String
     -> Parser a
     -> Opt.Mod Opt.CommandFields a
   command' c descr p =
     Opt.command c $ Opt.info (p <**> Opt.helper) $ mconcat [
       Opt.progDesc descr
     ]

data CLI

  = ValidateSubmission
      !RegistryRoot
      -- ^ Root directory of the registry.
      !SubmissionFile
      -- ^ File scrutinised as a submission.

  | PrepareSubmission
      !Name
      -- ^ A short name, max 50 characters
      !(Maybe Description)
      -- ^ An optional description, max 255 characters
      !Ticker
      -- ^ Ticker, 3-5 upper-case ASCII letters.
      !URI.URI
      -- ^ Absolute URI of the stake pool  homepage.
      !PledgeAddress
      -- ^ Pledge address, Bech32-encoded.
      !PublicKeyFile
      -- ^ Public key file, Bech32-encoded.

newtype RegistryRoot =
  RegistryRoot FilePath
  deriving (Eq, Ord, Show, IsString)

newtype SubmissionFile =
  SubmissionFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype PublicKeyFile =
  PublicKeyFile FilePath
  deriving (Eq, Ord, Show, IsString)

data Submission = Submission
  { sOwner :: !Owner
  , sName :: !Name
  , sDescription :: !(Maybe Description)
  , sTicker :: !Ticker
    -- | Absolute URI.
  , sHomepage :: !URI.URI
  , sPledgeAddress :: PledgeAddress
  } deriving (Generic)

-- | An owner public key, Bech32-encoded
newtype Owner = Owner { unOwner :: Bech32 }
  deriving (Eq)

-- | Stake pool ticker, a 3-5 all-ASCII-uppercase character ID.
newtype Ticker = Ticker { unTicker :: Text }
  deriving (Eq, Ord, Show)

-- | Stake pool name, free form text 1-50 characters
newtype Name = Name { unName :: Text }
  deriving (Eq, Show)

-- | Stake pool description, free form text 1-255 characters
newtype Description = Description { unDescription :: Text }
  deriving (Eq, Show)

-- | Bech32-encoded address.
newtype PledgeAddress = PledgeAddress { unPledgeAddress :: Bech32 }

-- | Represent all information from the Bech32 decoder.
data Bech32 = Bech32
  { bechHumanReadable  :: Bech32.HumanReadablePart
  , bechDataPart       :: Bech32.DataPart
  } deriving (Eq)

-- | Type of public key.
data PubKeyType
  = Ed25519

instance Show PubKeyType where
  show Ed25519 = "ed25519"

registryPubKeyType :: PubKeyType
registryPubKeyType = Ed25519

--------------------------------------------------------------------------------
-- * Submission maker
prepareRegistrySubmission
  :: Name
  -> Maybe Description
  -> Ticker
  -> URI.URI
  -> PledgeAddress
  -> PublicKeyFile
  -> ExceptT Text IO Submission
prepareRegistrySubmission name desc ticker uri pledge (PublicKeyFile fp) = do
  unlessM (lift $ Dir.doesFileExist fp) $
    checks ["Public key doesn't exist: " <> pack fp]
  unlessM (lift $ Dir.readable <$> Dir.getPermissions fp) $
    checks ["Submission not readable: " <> pack fp]

  contents <- Text.strip <$> (lift $ Text.readFile fp)
  pubKey <- validatePublicKey contents

  pure $ Submission pubKey name desc ticker uri pledge
 where
   err :: [Text] -> ExceptT Text IO a
   err es = ExceptT . pure . Left . Text.unlines $ es

   checks :: [Text] -> ExceptT Text IO ()
   checks [] = ExceptT . pure . Right $ ()
   checks es = err es

writeRegistrySubmission :: Submission -> ExceptT Text IO ()
writeRegistrySubmission s = do
  lift . putStrLn $ "Writing submission to " <> json
  lift . BSL.writeFile json . encodePretty $ s
  lift . putStrLn $ "You'll need to provide a signature in " <> sig
 where
   json, sig :: FilePath
   json = unpack outputName <.> "json"
   sig  = unpack outputName <.> "sig"

   outputName :: Text
   outputName = encodeBech32 $ unOwner $ sOwner s

--------------------------------------------------------------------------------
-- * Main validator
--
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

  let derivedPubKey = pack pubkeyFile
  pubkey <- case validatePublicKey derivedPubKey of
    Left e -> err $
      ["Submission filename (actually public key) invalid: " <> pack e]
    Right x -> pure x

  contents <- lift $ BSL.fromStrict <$> BS.readFile afp
  case AE.eitherDecodeWith AE.jsonNoDup AE.ifromJSON contents of
    Left (_, e) -> err $ ["Submission content:\n" <> pack e]
    Right entry -> do

      checks $
        [ pack $ "Internal 'owner' doesn't match filename: "
          <> show (encodeBech32 $ unOwner $ sOwner entry) <> " vs. "
          <> show (encodeBech32 $ unOwner $ pubkey)
        | sOwner entry /= pubkey ]

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

--------------------------------------------------------------------------------
-- * Validators / decoders
--
-- | Submission identifier is actually a public key.
--   We additionally require that the string must be all-lowercase.
validatePublicKey :: MonadFail m => Text -> m Owner
validatePublicKey text =
  if      not ((Char.toLower <$> s) == s)
  then fail $ "Public key not all-lowercase: " <> s
  else Owner <$> decodeBech32 "Owner Public key" publicKeyHRP isPublicKey text
  where
    s = unpack text
    isPublicKey bech32@(Bech32 _ datapart) = bech32 <$ do
      unless ((BS.length <$> Bech32.dataPartToBytes datapart) == Just pubkeyLength) $
        fail "Invalid public key length."

validateTicker :: MonadFail m => String -> m Ticker
validateTicker s =
  if length s >= 3
  && length s <= 5
  && all (\c -> Char.isAlphaNum c && (Char.isDigit c || Char.isUpper c)) s
  then pure $ Ticker (pack s)
  else fail $ "Not an uppercase ASCII of length 3 to 5: " <> take 32 s

validateURI :: MonadFail m => String -> m URI.URI
validateURI s =
  case URI.parseAbsoluteURI s of
    Nothing -> fail $ "Not an absolute URI: " <> s
    Just x@URI.URI{URI.uriScheme, URI.uriAuthority} ->
      case uriAuthority of
        Nothing -> fail $ "URI has no authority: " <> s
        Just URI.URIAuth{URI.uriRegName}
          | length uriRegName < 3 -> fail $ "URI authority too short: " <> s
          | uriScheme == "https:" -> pure x
          | otherwise -> fail $ "Not an https URI: " <> s

-- | Based on the following ABNF grammar, and assuming a testing discrimination:
--
--     ADDRESS           = ADDRESS-SINGLE / ADDRESS-GROUP / ADDRESS-ACCOUNT / ADDRESS-MULTISIG
--     ADDRESS-SINGLE    = (%x03 / %x83) SPENDINGKEY
--     ADDRESS-GROUP     = (%x04 / %x84) SPENDINGKEY SINGLE-ACNT-ID
--     ADDRESS-ACCOUNT   = (%x05 / %x85) SINGLE-ACNT-ID
--     ADDRESS-MULTISIG  = (%x06 / %x86) MULTI-ACNT-ID
--
--     SPENDINGKEY       = 32OCTET
--     SINGLE-ACNT-ID    = 32OCTET
--     MULTI-ACNT-ID     = 32OCTET
--
-- source: https://github.com/input-output-hk/chain-libs/blob/master/chain-impl-mockchain/doc/format.abnf#L157-L161
validatePledgeAddress :: MonadFail m => String -> m PledgeAddress
validatePledgeAddress s =
  PledgeAddress <$>
    decodeBech32 "Pledge address" addrHRP isAddress (pack s)
  where
    isAddress bech32@(Bech32 _ datapart) = case Bech32.dataPartToBytes datapart of
      Nothing -> fail "Couldn't decode address internal payload!?"
      Just bytes -> bech32 <$ case first BS.unpack (BS.splitAt 1 bytes) of
        (discriminant, rest) | discriminant == [0x83] ->
          unless (BS.length rest == pubkeyLength) $ fail
            "Invalid payload for a 'single' address."
        (discriminant, rest) | discriminant == [0x84] ->
          unless (BS.length rest == 2*pubkeyLength) $ fail
            "Invalid payload for a 'grouped' address."
        (discriminant, rest) | discriminant == [0x85] ->
          unless (BS.length rest == pubkeyLength) $ fail
            "Invalid payload for an 'account' address."
        (discriminant, rest) | discriminant == [0x86] ->
          unless (BS.length rest == pubkeyLength) $ fail
            "Invalid payload for a 'multisig' address."
        _ ->
          fail "Unrecognized network discriminant for the given address."

validateName :: MonadFail m => Text -> m Name
validateName =
  fmap Name . validateLength "Name" (minLength, maxLength)
    where
      minLength = 1
      maxLength = 50

validateDescription :: MonadFail m => Text -> m Description
validateDescription =
  fmap Description . validateLength "Description" (minLength, maxLength)
    where
      minLength = 1
      maxLength = 255

validateLength :: MonadFail m => String -> (Int, Int) -> Text -> m Text
validateLength desc (minLength, maxLength) txt =
  let n = Text.length txt in
  if n >= minLength && n <= maxLength
  then pure txt
  else fail $ unwords
    [ desc
    , "has an invalid length ("
    , show n
    , ")"
    , ". Length should be between"
    , show minLength
    , ","
    , show maxLength
    ]

addrHRP
  :: Bech32.HumanReadablePart
addrHRP =
  either (error . show) id $ Bech32.humanReadablePartFromText "addr"

publicKeyHRP
  :: Bech32.HumanReadablePart
publicKeyHRP =
  either (error . show) id $ Bech32.humanReadablePartFromText "ed25519_pk"

-- | Typical length of a public key (non-extended)
pubkeyLength :: Int
pubkeyLength = 32

-- | Given a description and a Bech32 decoder, run it on the input text.
decodeBech32
  :: MonadFail m
  => String
  -> Bech32.HumanReadablePart
  -> (Bech32 -> m Bech32)
  -> Text
  -> m Bech32
decodeBech32 desc expectedHumanPart validate text =
  case Bech32.decodeLenient text of
    Left e ->
      fail $ desc <> " decoding error for '"<>unpack text<>"':\n" <> show e
    Right (humanPart, _) | humanPart /= expectedHumanPart -> do
      let actual = Bech32.humanReadablePartToText humanPart
      let expected = Bech32.humanReadablePartToText expectedHumanPart
      fail $ unwords
        [ desc, "has an unexpected human-readable part (", show actual , ")."
        , "Expected prefix is:", show expected
        ]
    Right (humanPart, dataPart) ->
      validate $ Bech32 humanPart dataPart

--------------------------------------------------------------------------------
-- * Instances
--
instance FromJSON Submission where
  parseJSON = AE.withObject "Submission" $ \v->
    case validateFields v of
      [] -> Submission
              <$> v .: "owner"
              <*> v .: "name"
              <*> v .:? "description"
              <*> v .: "ticker"
              <*> v .: "homepage"
              <*> v .: "pledge_address"
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
              mandatory = Set.fromList
                [ "owner"
                , "name"
                , "ticker"
                , "homepage"
                , "pledge_address"
                ]
              optional' = Set.fromList
                [ "description"
                ]
              missing = mandatory `Set.difference` keys
              unexpected = keys `Set.difference` (mandatory <> optional')

              commaList :: Set Text -> String
              commaList = List.intercalate ", " . (show . unpack <$>) . Set.toList

instance FromJSON Owner where
  parseJSON = AE.withText "Owner" $ validatePublicKey

instance FromJSON Name where
  parseJSON = AE.withText "Name" $ validateName

instance FromJSON Description where
  parseJSON = AE.withText "Description" $ validateDescription

instance FromJSON Ticker where
  parseJSON = AE.withText "Ticker" $
    validateTicker . unpack

instance FromJSON URI.URI where
  parseJSON = AE.withText "Text" $ validateURI . unpack

instance FromJSON PledgeAddress where
  parseJSON = AE.withText "PledgeAddress" $ validatePledgeAddress . unpack

instance ToJSON Submission where
  toJSON s = AE.object $
    [ "owner"                .= sOwner s
    , "name"                 .= sName s
    , "description"          .= sDescription s
    , "ticker"               .= sTicker s
    , "homepage"             .= sHomepage s
    , "pledge_address"       .= sPledgeAddress s
    ]

instance ToJSON Owner where
  toJSON = toJSON . unOwner

instance ToJSON Name where
  toJSON = toJSON . unName

instance ToJSON Description where
  toJSON = toJSON . unDescription

instance ToJSON Ticker where
  toJSON = AE.String . unTicker

instance ToJSON PledgeAddress where
  toJSON = toJSON . unPledgeAddress

instance Show Bech32 where
  show = show . bechHumanReadable

instance ToJSON Bech32 where
  toJSON = toJSON . encodeBech32

encodeBech32 :: Bech32 -> Text
encodeBech32 (Bech32 hrp datapart) = Bech32.encodeLenient hrp datapart

--------------------------------------------------------------------------------
-- * Ancillary
--
duplicates :: (Ord a) => [a] -> Set a
duplicates xs = snd $ foldl step (Set.empty, Set.empty) xs
  where
    step :: Ord a => (Set a, Set a) -> a -> (Set a, Set a)
    step (seen, dups) x =
      if Set.member x seen
      then (seen, Set.insert x dups)
      else (Set.insert x seen, dups)

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

--------------------------------------------------------------------------------
-- * Orphans
--
instance MonadFail (Either String) where
  fail = Left

instance ToJSON URI.URI where
  toJSON = AE.String . pack . show
