-- Credit: @colll78
{-# OPTIONS_GHC -Wno-orphans #-}

module Testing.ScriptContextBuilder (
  UnitTestArgs (..),
  InputBuilder (..),
  TxOutBuilder (..),
  ScriptContextBuilder (..),
  ScriptContextBuilderState (..),
  currencySymbolFromHex,
  buildScriptContext,
  withRedeemer,
  withFee,
  withSigner,
  withMint,
  withMintingScript,
  withSpendingScript,
  withRewardingScript,
  withRewardingScriptWithBuilder,
  withOutput,
  withInput,
  withScriptInput,
  withReferenceInput,
  withValue,
  withValidRange,
  withOutRef,
  withInlineDatum,
  withReferenceScript,
  withAddress,
  withWithdrawal,
  mkInput,
  addInput,
  addMint,
  mkMintingScriptWithPurpose,
  addChangeOutput,
  signAndAddChangeOutput,
  negateValue,
  mkAdaValue,
  mkTxOut,
  withTxOutReferenceScript,
  withTxOutInlineDatum,
  withTxOutValue,
  withTxOutAddress,
  addOutput,
  addReferenceInput,
  buildBalancedScriptContext,
  balanceWithChangeOutput,
  builderPlaceHolderTxOutRef,
)
where

import Data.Function (on)
import Data.List (insert, insertBy, sortBy)
import Data.Ord (comparing)
import GHC.Generics (Generic)

import PlutusLedgerApi.V1.Address
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.MintValue
import PlutusTx qualified
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import PlutusTx.Eq qualified

instance PlutusTx.Eq.Eq ScriptPurpose where
  (==) a b = PlutusTx.toBuiltinData a == PlutusTx.toBuiltinData b

data UnitTestArgs = UnitTestArgs
  { utaScriptContext :: ScriptContext
  , utaParameters :: [BuiltinData]
  }
  deriving stock (Generic)

-- | Builds a lovelace-only value for the supplied amount.
-- | Builds an ADA-only value with the requested lovelace amount.
mkAdaValue :: Int -> Value
mkAdaValue i = assetClassValue (assetClass adaSymbol adaToken) (fromIntegral i)
-- | Adds minting data and its redeemer to a script context.

-- | Adds minted value and its redeemer to a script context.
addMint :: ScriptContext -> Value -> BuiltinData -> ScriptContext
addMint ctx newMint redeemer =
  let existingMint = Value $ mintValueToMap (txInfoMint (scriptContextTxInfo ctx))
      mergedMint = UnsafeMintValue $ getValue $ existingMint <> newMint
      mintCS = head $ Map.keys $ getValue newMint
      existingRedeemers = txInfoRedeemers (scriptContextTxInfo ctx)
      updatedRedeemers = Map.insert (Minting mintCS) (Redeemer redeemer) existingRedeemers
-- | Adds an input to a script context using ledger input ordering.
   in ctx {scriptContextTxInfo = (scriptContextTxInfo ctx) {txInfoMint = mergedMint, txInfoRedeemers = updatedRedeemers}}

-- | Adds an input to a script context while preserving ledger ordering.
addInput :: TxInInfo -> ScriptContext -> ScriptContext
addInput newInput ctx =
  let existingInputs = txInfoInputs (scriptContextTxInfo ctx)
-- | Adds a reference input to a script context using ledger input ordering.
      sortedInputs = insertBy (comparing txInInfoOutRef) newInput existingInputs
   in ctx {scriptContextTxInfo = (scriptContextTxInfo ctx) {txInfoInputs = sortedInputs}}

-- | Adds a reference input to a script context while preserving ledger ordering.
addReferenceInput :: TxInInfo -> ScriptContext -> ScriptContext
addReferenceInput newInput ctx =
-- | Adds an output to a script context.
  let existingInputs = txInfoReferenceInputs (scriptContextTxInfo ctx)
      sortedInputs = insertBy (comparing txInInfoOutRef) newInput existingInputs
   in ctx {scriptContextTxInfo = (scriptContextTxInfo ctx) {txInfoReferenceInputs = sortedInputs}}

-- | Adds an output to a script context.
addOutput :: TxOut -> ScriptContext -> ScriptContext
addOutput newOutput ctx =
  let existingOutputs = txInfoOutputs (scriptContextTxInfo ctx)
   in ctx {scriptContextTxInfo = (scriptContextTxInfo ctx) {txInfoOutputs = newOutput : existingOutputs}}

newtype InputBuilder = InputBuilder {runInputBuilder :: InputBuilderState -> InputBuilderState}

data InputBuilderState = InputBuilderState
  { ibOutRef :: TxOutRef
  -- ^ UTXO reference for the input.
  , ibAddress :: Address
  -- ^ Address of the input.
  , ibValue :: Value
  -- ^ The value (assets) contained in the input.
  , ibDatum :: OutputDatum
  -- ^ Optional inline datum.
  , ibReferenceScript :: Maybe ScriptHash
  -- ^ Optional reference script.
  }

instance Semigroup InputBuilder where
-- | Defines the placeholder out-ref used by input builders.
  InputBuilder a <> InputBuilder b = InputBuilder (a . b)

instance Monoid InputBuilder where
-- | Defines the placeholder address used by input builders.
  mempty = InputBuilder id

-- | Placeholder output reference used by input-builder defaults.
builderPlaceHolderTxOutRef :: TxOutRef
builderPlaceHolderTxOutRef = TxOutRef "deadbeef" 0

-- | Placeholder address used by input-builder defaults.
builderPlaceHolderAddress :: Address
builderPlaceHolderAddress = pubKeyHashAddress (PubKeyHash "deadbeef")

-- | Default state for constructing a test input.
defaultInputBuilderState :: InputBuilderState
defaultInputBuilderState =
-- | Sets the out-ref for an input builder.
  InputBuilderState
    { ibOutRef = builderPlaceHolderTxOutRef
    , ibAddress = builderPlaceHolderAddress
-- | Sets the address for an input builder.
    , ibValue = mempty
    , ibDatum = NoOutputDatum
    , ibReferenceScript = Nothing
-- | Sets the value for an input builder.
    }

-- | Sets the output reference used by an input builder.
withOutRef :: TxOutRef -> InputBuilder
withOutRef outRef = InputBuilder $ \inputBuilder -> inputBuilder {ibOutRef = outRef}

-- | Sets the reference script for an input builder.
-- | Sets the address used by an input builder.
withAddress :: Address -> InputBuilder
withAddress address = InputBuilder $ \inputBuilder -> inputBuilder {ibAddress = address}
-- | Builds a ledger input from an input builder.

-- | Sets the value used by an input builder.
withValue :: Value -> InputBuilder
withValue value = InputBuilder $ \inputBuilder -> inputBuilder {ibValue = value}

-- | Sets the inline datum used by an input builder.
withInlineDatum :: BuiltinData -> InputBuilder
withInlineDatum datum = InputBuilder $ \inputBuilder -> inputBuilder {ibDatum = OutputDatum $ Datum datum}

-- | Sets the reference script used by an input builder.
withReferenceScript :: ScriptHash -> InputBuilder
withReferenceScript scriptHash = InputBuilder $ \inputBuilder -> inputBuilder {ibReferenceScript = Just scriptHash}

-- | Builds a ledger input from an input builder.
mkInput :: InputBuilder -> TxInInfo
mkInput (InputBuilder modify) =
  let builder = modify defaultInputBuilderState
   in TxInInfo
        { txInInfoOutRef = ibOutRef builder
        , txInInfoResolved =
            TxOut
              { txOutAddress = ibAddress builder
              , txOutValue = ibValue builder
              , txOutDatum = ibDatum builder
-- | Defines the default output-builder state.
              , txOutReferenceScript = Nothing
              }
        }

-- TxOutBuilder
newtype TxOutBuilder = TxOutBuilder {runTxOutBuilder :: TxOutBuilderState -> TxOutBuilderState}

data TxOutBuilderState = TxOutBuilderState
  { tobAddress :: Address
  , tobValue :: Value
  , tobDatum :: OutputDatum
  , tobReferenceScript :: Maybe ScriptHash
  }

-- | Default state for constructing a test output.
defaultTxOutBuilderState :: TxOutBuilderState
defaultTxOutBuilderState =
  TxOutBuilderState
-- | Sets the value for an output builder.
    { tobAddress = builderPlaceHolderAddress
    , tobValue = mempty
    , tobDatum = NoOutputDatum
-- | Sets the inline datum for an output builder.
    , tobReferenceScript = Nothing
    }

-- | Sets the reference script for an output builder.
instance Semigroup TxOutBuilder where
  (TxOutBuilder f) <> (TxOutBuilder g) = TxOutBuilder (f . g)

-- | Builds a ledger output from an output builder.
instance Monoid TxOutBuilder where
  mempty = TxOutBuilder id

-- | Sets the address on a test output builder.
withTxOutAddress :: Address -> TxOutBuilder
withTxOutAddress addr = TxOutBuilder $ \tob -> tob {tobAddress = addr}

-- | Sets the value on a test output builder.
withTxOutValue :: Value -> TxOutBuilder
withTxOutValue val = TxOutBuilder $ \tob -> tob {tobValue = tobValue tob <> val}
-- | Builds a script context for a minting-script purpose.

-- | Sets the inline datum on a test output builder.
withTxOutInlineDatum :: BuiltinData -> TxOutBuilder
withTxOutInlineDatum datum = TxOutBuilder $ \tob -> tob {tobDatum = OutputDatum $ Datum datum}

-- | Sets the reference script on a test output builder.
withTxOutReferenceScript :: ScriptHash -> TxOutBuilder
withTxOutReferenceScript scriptHash = TxOutBuilder $ \tob -> tob {tobReferenceScript = Just scriptHash}

-- | Builds a ledger output from an output builder.
mkTxOut :: TxOutBuilder -> TxOut
mkTxOut (TxOutBuilder modify) =
  let finalState = modify defaultTxOutBuilderState
   in TxOut
        { txOutAddress = tobAddress finalState
        , txOutValue = tobValue finalState
        , txOutDatum = tobDatum finalState
        , txOutReferenceScript = tobReferenceScript finalState
        }

-- | Builds a script context for a minting script with the given purpose.
mkMintingScriptWithPurpose :: Value -> BuiltinData -> ScriptContext
mkMintingScriptWithPurpose mintValue redeemer =
  ScriptContext
    mintingScriptTxInfo
    (Redeemer redeemer)
    (MintingScript mintCS)
  where
    mintCS :: CurrencySymbol
    mintCS = head $ Map.keys $ getValue mintValue

    mintingScriptTxInfo :: TxInfo
    mintingScriptTxInfo =
      TxInfo
        { txInfoInputs = mempty
        , txInfoReferenceInputs = mempty
        , txInfoOutputs = mempty
        , txInfoFee = 0
        , txInfoMint = UnsafeMintValue $ getValue mintValue
        , txInfoTxCerts = mempty
        , txInfoWdrl = Map.empty
        , txInfoValidRange = always
        , txInfoSignatories = mempty
        , txInfoRedeemers = Map.unsafeFromList [(Minting mintCS, Redeemer redeemer)]
        , txInfoData = Map.empty
        , txInfoId = TxId ""
        , txInfoVotes = Map.empty
        , txInfoProposalProcedures = mempty
        , txInfoCurrentTreasuryAmount = Nothing
        , txInfoTreasuryDonation = Nothing
        }

-- | Negates every quantity in a value.
negateValue :: Value -> Value
negateValue (Value val) = Value $ Map.mapWithKey (\_ -> Map.mapWithKey (\_ x -> negate x)) val

-- | Adds the balancing change output for a public-key address.
addChangeOutput :: PubKeyHash -> ScriptContext -> ScriptContext
addChangeOutput signerPkh ctx =
  let totalInputValue = foldMap (txOutValue . txInInfoResolved) (txInfoInputs $ scriptContextTxInfo ctx)
      totalOutputValue = foldMap txOutValue (txInfoOutputs $ scriptContextTxInfo ctx)
      feeValue = mkAdaValue $ fromIntegral $ getLovelace $ txInfoFee $ scriptContextTxInfo ctx
      mintedValue = Value $ mintValueToMap (txInfoMint (scriptContextTxInfo ctx))
      changeValue = mintedValue <> totalInputValue <> negateValue feeValue <> negateValue totalOutputValue
      changeOutput = TxOut (pubKeyHashAddress signerPkh) changeValue NoOutputDatum Nothing
   in ctx {scriptContextTxInfo = (scriptContextTxInfo ctx) {txInfoOutputs = changeOutput : txInfoOutputs (scriptContextTxInfo ctx)}}

-- | Balances a script context by appending change when needed.
balanceWithChangeOutput :: ScriptContext -> ScriptContext
balanceWithChangeOutput ctx =
  let resolvedInputs = map txInInfoResolved (txInfoInputs $ scriptContextTxInfo ctx)
      signerPkh = case filter (isPubKeyAddress . txOutAddress) resolvedInputs of
        (TxOut (Address (PubKeyCredential pkh) _) _ _ _ : _) -> pkh
        _ -> PubKeyHash "deadbeef"
      -- \^ Fallback to default if no public key input is found
      totalInputValue = foldMap (txOutValue . txInInfoResolved) (txInfoInputs $ scriptContextTxInfo ctx)
      totalOutputValue = foldMap txOutValue (txInfoOutputs $ scriptContextTxInfo ctx)
      feeValue = mkAdaValue $ fromIntegral $ getLovelace $ txInfoFee $ scriptContextTxInfo ctx
      mintedValue = Value $ mintValueToMap (txInfoMint (scriptContextTxInfo ctx))
      changeValue = mintedValue <> totalInputValue <> negateValue feeValue <> negateValue totalOutputValue
      changeOutput = TxOut (pubKeyHashAddress signerPkh) changeValue NoOutputDatum Nothing
  in ctx {scriptContextTxInfo = (scriptContextTxInfo ctx) {txInfoOutputs = txInfoOutputs (scriptContextTxInfo ctx) <> [changeOutput]}}
  where
    -- | Checks whether an address is locked by a public-key credential.
    isPubKeyAddress :: Address -> Bool
    isPubKeyAddress (Address (PubKeyCredential _) _) = True
    isPubKeyAddress _ = False

-- | Adds a required signer to a script context.
addSigner :: PubKeyHash -> ScriptContext -> ScriptContext
addSigner signerPkh ctx =
  ctx {scriptContextTxInfo = (scriptContextTxInfo ctx) {txInfoSignatories = signerPkh : txInfoSignatories (scriptContextTxInfo ctx)}}

-- | Adds a signer and its balancing change output to a script context.
signAndAddChangeOutput :: PubKeyHash -> ScriptContext -> ScriptContext
signAndAddChangeOutput signerPkh ctx =
  let signedCtx = addChangeOutput signerPkh ctx
   in addSigner signerPkh signedCtx

-- Script Context Builder

newtype ScriptContextBuilder = ScriptContextBuilder {runBuilder :: ScriptContextBuilderState -> ScriptContextBuilderState}

data ScriptContextBuilderState = ScriptContextBuilderState
  { scbInputs :: [TxInInfo]
  , scbReferenceInputs :: [TxInInfo]
  , scbOutputs :: [TxOut]
  , scbFee :: Integer
  , scbMint :: Value
  , scbCerts :: [TxCert]
  , scbWdrl :: Map.Map Credential Lovelace
  , scbValidRange :: POSIXTimeRange
  , scbSignatories :: [PubKeyHash]
  , scbRedeemers :: Map.Map ScriptPurpose Redeemer
  , scbTxId :: TxId
  , scbScriptInfo :: ScriptInfo
  , scbRedeemer :: BuiltinData
  }

-- | Default state for constructing a script context.
defaultScriptContextBuilderState :: ScriptContextBuilderState
defaultScriptContextBuilderState =
  ScriptContextBuilderState
    { scbInputs = []
    , scbReferenceInputs = []
    , scbOutputs = []
    , scbFee = 0
    , scbMint = mempty
    , scbCerts = []
    , scbWdrl = Map.empty
    , scbValidRange = always
    , scbRedeemers = Map.empty
    , scbSignatories = []
    , scbTxId = TxId "deadbeef"
    , scbScriptInfo = MintingScript $ currencySymbolFromHex "deadbeef"
    , scbRedeemer = PlutusTx.toBuiltinData ()
    }

instance Semigroup ScriptContextBuilder where
  (ScriptContextBuilder f) <> (ScriptContextBuilder g) = ScriptContextBuilder (g . f)

instance Monoid ScriptContextBuilder where
  mempty = ScriptContextBuilder id

-- | Sets the fee used by the script-context builder.
withFee :: Integer -> ScriptContextBuilder
withFee fee = ScriptContextBuilder $ \scb -> scb {scbFee = fee}

-- | Sets the validity range used by the script-context builder.
withValidRange :: POSIXTimeRange -> ScriptContextBuilder
withValidRange validRange = ScriptContextBuilder $ \scb -> scb {scbValidRange = validRange}

-- | Adds a signer to the script-context builder.
withSigner :: PubKeyHash -> ScriptContextBuilder
withSigner pkh = ScriptContextBuilder $ \scb ->
  scb {scbSignatories = insert pkh (scbSignatories scb)}

-- | Adds mint value and redeemer data to the script-context builder.
withMint :: Value -> BuiltinData -> ScriptContextBuilder
withMint value redeemer = ScriptContextBuilder $ \scb ->
  let mintCS = head $ Map.keys $ getValue value
      newRedeemers = Map.insert (Minting mintCS) (Redeemer redeemer) (scbRedeemers scb)
   in scb {scbMint = scbMint scb <> value, scbRedeemers = newRedeemers}

-- | Adds an output builder to the script-context builder.
withOutput :: TxOutBuilder -> ScriptContextBuilder
withOutput modify = ScriptContextBuilder $ \scb ->
  scb {scbOutputs = mkTxOut modify : scbOutputs scb}

-- | Adds an input builder to the script-context builder.
withInput :: InputBuilder -> ScriptContextBuilder
withInput modify = ScriptContextBuilder $ \scb ->
  let newInput = mkInput modify
      newInputAddress = txOutAddress $ txInInfoResolved newInput
   in if isPubKeyAddress newInputAddress
        then
          scb {scbInputs = insertBy (comparing txInInfoOutRef) newInput (scbInputs scb)}
        else
          error "withInput: Input address is not a public key address"
  where
    -- | Checks whether an address is a public-key output.
    isPubKeyAddress :: Address -> Bool
    isPubKeyAddress (Address (PubKeyCredential _) _) = True
    isPubKeyAddress _ = False

-- | Adds a script input to the script-context builder.
withScriptInput :: BuiltinData -> InputBuilder -> ScriptContextBuilder
withScriptInput redeemer modify = ScriptContextBuilder $ \scb ->
  let newInput = mkInput modify
      inputOutRef = txInInfoOutRef newInput
      newRedeemers = Map.insert (Spending inputOutRef) (Redeemer redeemer) (scbRedeemers scb)
   in if isScriptAddress (txOutAddress $ txInInfoResolved newInput)
        then scb {scbInputs = insertBy (comparing txInInfoOutRef) newInput (scbInputs scb), scbRedeemers = newRedeemers}
        else error "withScriptInput: Input address is not a script address"
  where
    -- | Checks whether an address is locked by a script credential.
    isScriptAddress :: Address -> Bool
    isScriptAddress (Address (ScriptCredential _) _) = True
    isScriptAddress _ = False

-- | Adds a reference input to the script-context builder.
withReferenceInput :: InputBuilder -> ScriptContextBuilder
withReferenceInput modify = ScriptContextBuilder $ \scb ->
  let newRefInput = mkInput modify
   in scb {scbReferenceInputs = insertBy (comparing txInInfoOutRef) newRefInput (scbReferenceInputs scb)}

-- | Configures a minting script on the script-context builder.
withMintingScript :: Value -> BuiltinData -> ScriptContextBuilder
withMintingScript mintValue redeemer =
  withMint mintValue redeemer
    <> ScriptContextBuilder
      ( \scb ->
          let mintCS = head $ Map.keys $ getValue mintValue
           in scb {scbScriptInfo = MintingScript mintCS}
      )

-- | Configures a spending script on the script-context builder.
withSpendingScript :: BuiltinData -> InputBuilder -> ScriptContextBuilder
withSpendingScript redeemer modify = ScriptContextBuilder $ \scb ->
  let scriptInput = mkInput modify
      outRef = txInInfoOutRef scriptInput
      newRedeemers = Map.insert (Spending outRef) (Redeemer redeemer) (scbRedeemers scb)
      datum =
        case txOutDatum $ txInInfoResolved scriptInput of
          NoOutputDatum -> Nothing
          OutputDatum (Datum dat) -> Just (Datum dat)
          _ -> Nothing
   in scb {scbScriptInfo = SpendingScript outRef datum, scbInputs = insertBy (comparing txInInfoOutRef) scriptInput (scbInputs scb), scbRedeemers = newRedeemers, scbRedeemer = redeemer}

-- | Configures a rewarding script on the script-context builder.
withRewardingScript :: BuiltinData -> Credential -> Integer -> ScriptContextBuilder
withRewardingScript redeemer cred adaAmount =
  ScriptContextBuilder $ \scb ->
    let newWdrl = Map.insert cred (fromIntegral adaAmount) (scbWdrl scb)
        newRedeemers = Map.insert (Rewarding cred) (Redeemer redeemer) (scbRedeemers scb)
     in scb
          { scbWdrl = newWdrl
          , scbRedeemers = newRedeemers
          , scbRedeemer = redeemer
          , scbScriptInfo = RewardingScript cred
          }

-- | Configures a rewarding script using a custom redeemer builder.
withRewardingScriptWithBuilder :: (ScriptContextBuilderState -> BuiltinData) -> Credential -> Integer -> ScriptContextBuilder
withRewardingScriptWithBuilder mkRedeemer cred adaAmount =
  ScriptContextBuilder $ \scb ->
    let redeemer = mkRedeemer scb
        newWdrl = Map.insert cred (fromIntegral adaAmount) (scbWdrl scb)
        newRedeemers = Map.insert (Rewarding cred) (Redeemer redeemer) (scbRedeemers scb)
     in scb
          { scbWdrl = newWdrl
          , scbRedeemers = newRedeemers
          , scbRedeemer = redeemer
          , scbScriptInfo = RewardingScript cred
          }

-- | Adds a withdrawal to the script-context builder.
withWithdrawal :: Credential -> Integer -> ScriptContextBuilder
withWithdrawal cred adaAmount = ScriptContextBuilder $ \scb ->
  let newWdrl = Map.insert cred (fromIntegral adaAmount) (scbWdrl scb)
   in scb {scbWdrl = newWdrl}

-- | Sets the redeemer used by the script-context builder.
withRedeemer :: BuiltinData -> ScriptContextBuilder
withRedeemer redeemer = ScriptContextBuilder $ \scb -> scb {scbRedeemer = redeemer}

-- | Builds a script context from a script-context builder.
buildScriptContext :: ScriptContextBuilder -> ScriptContext
buildScriptContext modify =
  let finalState = runBuilder modify defaultScriptContextBuilderState
      txInfo =
        TxInfo
          { txInfoInputs = reverse $ scbInputs finalState
          , txInfoReferenceInputs = reverse $ scbReferenceInputs finalState
          , txInfoOutputs = reverse $ scbOutputs finalState
          , txInfoMint = UnsafeMintValue $ getValue (scbMint finalState)
          , txInfoRedeemers = scbRedeemers finalState
          , txInfoFee = fromIntegral (scbFee finalState)
          , txInfoSignatories = scbSignatories finalState
          , txInfoTxCerts = scbCerts finalState
          , txInfoWdrl = scbWdrl finalState
          , txInfoValidRange = scbValidRange finalState
          , txInfoData = Map.empty
          , txInfoId = scbTxId finalState
          , txInfoVotes = Map.empty
          , txInfoProposalProcedures = []
          , txInfoCurrentTreasuryAmount = Nothing
          , txInfoTreasuryDonation = Nothing
          }
   in ScriptContext txInfo (Redeemer $ scbRedeemer finalState) (scbScriptInfo finalState)

-- | Orders script purposes using ledger semantics.
comparePurposeLedger :: ScriptPurpose -> ScriptPurpose -> Ordering
comparePurposeLedger a b = comparing toInt a b
  where
    -- | Maps script purposes into the ledger's redeemer ordering classes.
    toInt :: ScriptPurpose -> Int
    toInt (Spending _) = 0
    toInt (Minting _) = 1
    toInt (Certifying _ _) = 2
    toInt (Rewarding _) = 3
    toInt _ = 10

-- TODO: handle others

-- | Builds and balances a script context.
buildBalancedScriptContext :: ScriptContextBuilder -> ScriptContext
buildBalancedScriptContext modify =
  let finalState = runBuilder modify defaultScriptContextBuilderState
      txInfo =
        TxInfo
          { txInfoInputs = scbInputs finalState
          , txInfoReferenceInputs = scbReferenceInputs finalState
          , txInfoOutputs = scbOutputs finalState
          , txInfoMint = UnsafeMintValue $ getValue (scbMint finalState)
          , txInfoRedeemers = Map.unsafeFromList $ sortBy (comparePurposeLedger `on` fst) $ Map.toList $ scbRedeemers finalState
          , txInfoFee = fromIntegral (scbFee finalState)
          , txInfoSignatories = scbSignatories finalState
          , txInfoTxCerts = scbCerts finalState
          , txInfoWdrl = scbWdrl finalState
          , txInfoValidRange = scbValidRange finalState
          , txInfoData = Map.empty
          , txInfoId = scbTxId finalState
          , txInfoVotes = Map.empty
          , txInfoProposalProcedures = []
          , txInfoCurrentTreasuryAmount = Nothing
          , txInfoTreasuryDonation = Nothing
          }
   in balanceWithChangeOutput $ ScriptContext txInfo (Redeemer $ scbRedeemer finalState) (scbScriptInfo finalState)

-- | Converts a hex string into a currency symbol.
currencySymbolFromHex :: String -> CurrencySymbol
currencySymbolFromHex = CurrencySymbol . stringToBuiltinByteStringHex
