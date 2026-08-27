{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofsFixture
Description : A committed-block fixture for driving fraud-proof step validators.

The Haskell counterpart of @lib/midgard/fraud-proofs/native-binding-fixture-v1.ak@,
which the Aiken tree keeps for the @test@ blocks embedded in its step validators
and which has no place in the Plutarch library: it is test scaffolding, not
on-chain surface.

What it builds is one block and the L1 transactions that drive a step against it
— canonical compact CBOR, the §3 transaction id, a header whose
@transactions_root@ is the counted commitment over a raw MPF root, and the hub
and state-queue reference inputs a step reads its evidence from. Every encoder
here is written from the spec rather than taken from the port, so a change on
either side fails a test instead of two copies agreeing.

Several transactions, because that is what the families need between them: 'tx1'
and 'tx2' conflict (same spend inputs, different fee, hence different canonical
ids), 'tx3' is an honest transaction of the same block spending something else,
'txEmpty' spends nothing at all, 'txUnsigned' and 'txBadSig' are the two
witness-set violations, and 'txScriptSpend' spends 'tx3''s script-locked output
without witnessing the script.
-}
module Testing.FraudProofsFixture (
  -- * Contexts
  spendContext,
  asMinting,
  asRewarding,

  -- * The thread
  stepDatum,
  threadInput,
  stepOutput,
  convictionOutput,
  fraudProofMintEntry,
  cancelRedeemer,
  cancelMintEntry,

  -- * Redeemer payloads
  inclusionArgs,
  bareInclusionArgs,
  bodyOpening,
  witnessOpening,
  phasEntry,
  pexcludesEntry,
  pexcludesEntryWith,
  emptyProof,
  redeemerCarriedNonMembership,

  -- * The block's evidence
  referenceInputs,
  headerTransactionsRoot,
  commitCountedRoot,
  transactionsDomain,
  withdrawalsDomain,
  l2Count,

  -- * The block's one withdrawal event
  withdrawalId,
  withdrawalKeyBytes,
  withdrawalInfoData,
  withdrawalValueBytes,
  withdrawalsPhasRoot,
  headerWithdrawalsRoot,
  withdrawalCount,
  singleEntryPhasRoot,
  membershipProof,

  -- * The transactions
  Tx (..),
  spendInputsOf,
  tx1,
  tx2,
  tx3,
  txEmpty,
  txUnsigned,
  txBadSig,
  txScriptSpend,
  tx1Id,
  tx2Id,
  tx3Id,
  txEmptyId,
  txUnsignedId,
  txBadSigId,
  txScriptSpendId,
  tx1Cbor,
  tx2Cbor,
  tx3Cbor,
  txEmptyCbor,
  txUnsignedCbor,
  txBadSigCbor,
  txScriptSpendCbor,
  spendInputsPreimage,
  referenceInputsPreimage,
  outputsPreimage,
  outputCollectionPreimage,
  outputItem,
  midgardOutputCbor,
  scriptAddressBytes,
  pubKeyAddressBytes,
  requiredSignersPreimage,
  addressWitnessesPreimage,
  scriptWitnessesPreimage,
  scriptWitnessCollectionPreimage,
  versionedScriptItem,
  versionedScriptHashOf,
  nativeScriptBytes,
  otherNativeScriptBytes,
  lockedScriptHash,
  witnessSetCborOf,
  witnessSetCborFrom,
  witnessSetHashesOf,
  witnessSetHashOf,
  witnessOpeningRaw,
  compactWith,
  compactWithValidity,
  compactOf,
  txIdOf,
  Witness (..),
  verKeyFor,
  keyHashFor,
  signWith,
  sharedInputRef,
  otherInputRef,
  inputData,
  encodedInput,

  -- * Reference encoders
  arrayHeader,
  wrapItem,
  cborInt,
  defBytes32,
  hash32,
  blake2b256,
  blake2b224,
  serialise,

  -- * Identities
  policyFor,
  ctPolicy,
  fpPolicy,
  hubPolicy,
  hubOracleHash,
  stateQueuePolicy,
  certificatePolicy,
  stepScript,
  nextScript,
  otherScript,
  fpSpendScript,
  prover,
  threadName,
  otherThreadName,
  phasRoot,
  otherRoot,
  prevUtxosRoot,
  fraudProofAddress,
  otherAddress,
  phasHash,
  pexcludesHash,
  unCS,
  adaValue,
  ownRef,
  outRefN,
) where

import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Interval (always)
import PlutusLedgerApi.V1.Value (
  CurrencySymbol (..),
  TokenName (..),
  Value,
  getValue,
  singleton,
 )
import PlutusLedgerApi.V3 (
  Address,
  Credential (..),
  Datum (..),
  OutputDatum (..),
  PubKeyHash (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo (..),
  ScriptPurpose (..),
  TxId (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (dataToBuiltinData, fromBuiltin, toBuiltin)

--------------------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------------------

spendContext ::
  PD.Data ->
  PD.Data ->
  [TxInInfo] ->
  [TxOut] ->
  [TxInInfo] ->
  [(ScriptPurpose, Redeemer)] ->
  Value ->
  ScriptContext
spendContext datum redeemer inputs outputs refInputs redeemers mint =
  ScriptContext
    ( emptyTxInfo
        { txInfoInputs = inputs
        , txInfoOutputs = outputs
        , txInfoReferenceInputs = refInputs
        , txInfoRedeemers = AssocMap.unsafeFromList redeemers
        , txInfoMint = UnsafeMintValue (getValue mint)
        , txInfoSignatories = [PubKeyHash (toBuiltin prover)]
        }
    )
    (Redeemer (dataToBuiltinData redeemer))
    (SpendingScript ownRef (Just (Datum (dataToBuiltinData datum))))

asMinting :: ScriptContext -> ScriptContext
asMinting (ScriptContext txInfo r _) = ScriptContext txInfo r (MintingScript ctPolicy)

asRewarding :: ScriptContext -> ScriptContext
asRewarding (ScriptContext txInfo r _) =
  ScriptContext txInfo r (RewardingScript (ScriptCredential (ScriptHash (toBuiltin stepScript))))

emptyTxInfo :: TxInfo
emptyTxInfo =
  TxInfo
    { txInfoInputs = []
    , txInfoReferenceInputs = []
    , txInfoOutputs = []
    , txInfoFee = 0
    , txInfoMint = UnsafeMintValue AssocMap.empty
    , txInfoTxCerts = []
    , txInfoWdrl = AssocMap.empty
    , txInfoValidRange = always
    , txInfoSignatories = []
    , txInfoRedeemers = AssocMap.empty
    , txInfoData = AssocMap.empty
    , txInfoId = TxId (toBuiltin (BS.replicate 32 0x00))
    , txInfoVotes = AssocMap.empty
    , txInfoProposalProcedures = []
    , txInfoCurrentTreasuryAmount = Nothing
    , txInfoTreasuryDonation = Nothing
    }

--------------------------------------------------------------------------------
-- The thread
--------------------------------------------------------------------------------

-- | @ct.StepDatum@ — the prover, and the step's own state.
stepDatum :: Maybe PD.Data -> PD.Data
stepDatum mState =
  PD.Constr 0 [PD.B prover, maybe (PD.Constr 1 []) (\d -> PD.Constr 0 [d]) mState]

threadInput :: TxInInfo
threadInput =
  TxInInfo
    ownRef
    ( TxOut
        (scriptHashAddress (ScriptHash (toBuiltin stepScript)))
        (adaValue 2_000_000 <> singleton ctPolicy (TokenName (toBuiltin threadName)) 1)
        NoOutputDatum
        Nothing
    )

stepOutput :: BS.ByteString -> Maybe PD.Data -> TxOut
stepOutput script mState =
  TxOut
    (scriptHashAddress (ScriptHash (toBuiltin script)))
    (adaValue 2_000_000 <> singleton ctPolicy (TokenName (toBuiltin threadName)) 1)
    (OutputDatum (Datum (dataToBuiltinData (stepDatum mState))))
    Nothing

{- | The conviction: the thread's own asset name minted under the fraud-proof
policy, parked at the always-fails address with the prover's name in its datum.
-}
convictionOutput :: Address -> BS.ByteString -> TxOut
convictionOutput address name =
  TxOut
    address
    (adaValue 2_000_000 <> singleton fpPolicy (TokenName (toBuiltin name)) 1)
    (OutputDatum (Datum (dataToBuiltinData (PD.Constr 0 [PD.B prover]))))
    Nothing

-- | @fraud_proof.MintRedeemer@, which names the thread the conviction belongs to.
fraudProofMintEntry :: BS.ByteString -> (ScriptPurpose, Redeemer)
fraudProofMintEntry name =
  (Minting fpPolicy, Redeemer (dataToBuiltinData (PD.Constr 0 [PD.B name])))

-- | @ct.Cancel@ — an input index and the index of the burning mint redeemer.
cancelRedeemer :: PD.Data
cancelRedeemer = PD.Constr 0 [PD.I 0, PD.I 0]

{- | The computation-thread policy's @BurnForCancellation@ redeemer. Naming
another thread is how a cancel fails: the token burnt has to be this thread's.
-}
cancelMintEntry :: BS.ByteString -> (ScriptPurpose, Redeemer)
cancelMintEntry name =
  (Minting ctPolicy, Redeemer (dataToBuiltinData (PD.Constr 2 [PD.B name])))

--------------------------------------------------------------------------------
-- Redeemer payloads
--------------------------------------------------------------------------------

{- | @common.NativeTxInclusionCarriage@'s redeemer-carried arm, wrapping
@NativeTxInclusionArgs@. Nine fields, in declaration order.
-}
inclusionArgs :: BS.ByteString -> BS.ByteString -> BS.ByteString -> PD.Data
inclusionArgs txId cbor root = PD.Constr 0 [bareInclusionArgs txId cbor root]

{- | @common.NativeTxInclusionArgs@ on its own. Some families' steps take the
carriage and some take these args directly; the two are different redeemer
types, so a step of one shape cannot be driven with the other's payload.
-}
bareInclusionArgs :: BS.ByteString -> BS.ByteString -> BS.ByteString -> PD.Data
bareInclusionArgs txId cbor root =
  PD.Constr
    0
    [ PD.I 0 -- input index
    , PD.I 0 -- output index
    , PD.I 0 -- hub reference input index
    , PD.I 1 -- state queue node reference input index
    , PD.B txId
    , PD.B cbor
    , PD.B root
    , emptyProof
    , PD.I 0 -- phas withdrawal redeemer index
    ]

{- | @field_opening_v1.BodyFieldOpening@ — the compact bytes and a tier-1
carriage holding the whole preimage.
-}
bodyOpening :: BS.ByteString -> BS.ByteString -> PD.Data
bodyOpening cbor preimage = PD.Constr 0 [PD.B cbor, PD.Constr 0 [PD.B preimage]]

{- | @field_opening_v1.WitnessFieldOpening@ — the compact bytes, the witness set
the door re-derives against the anchored hash, and a tier-1 carriage.

The witness set is a nested @Constr 0@ of three hashes, not three fields spliced
into the opening: it ports from an Aiken record.
-}
witnessOpening :: BS.ByteString -> Tx -> BS.ByteString -> PD.Data
witnessOpening cbor witnessSetOf = witnessOpeningRaw cbor (witnessSetHashesOf witnessSetOf)

{- | The same, with the three collection hashes given directly rather than taken
off a transaction — which is how a test builds a witness set no fixture
transaction commits.
-}
witnessOpeningRaw ::
  BS.ByteString ->
  (BS.ByteString, BS.ByteString, BS.ByteString) ->
  BS.ByteString ->
  PD.Data
witnessOpeningRaw cbor (addr, script, redeemer) preimage =
  PD.Constr
    1
    [ PD.B cbor
    , PD.Constr 0 [PD.B addr, PD.B script, PD.B redeemer]
    , PD.Constr 0 [PD.B preimage]
    ]

{- | @common.NonMembershipCarriage@'s redeemer-carried arm: the proof itself, and
the index of the @pexcludes@ withdrawal that walks it.
-}
redeemerCarriedNonMembership :: PD.Data
redeemerCarriedNonMembership = PD.Constr 0 [emptyProof, PD.I 0]

-- | The @pexcludes@ withdrawal an absence proof delegates its walk to.
pexcludesEntry :: BS.ByteString -> BS.ByteString -> (ScriptPurpose, Redeemer)
pexcludesEntry root key = pexcludesEntryWith root key emptyProof

-- | The same, with the proof under the caller's control.
pexcludesEntryWith :: BS.ByteString -> BS.ByteString -> PD.Data -> (ScriptPurpose, Redeemer)
pexcludesEntryWith root key proof =
  ( Rewarding (ScriptCredential (ScriptHash (toBuiltin pexcludesHash)))
  , Redeemer (dataToBuiltinData (PD.List [PD.B root, PD.B key, proof]))
  )

-- | The @phas@ withdrawal the redeemer-carried route delegates its walk to.
phasEntry :: BS.ByteString -> BS.ByteString -> BS.ByteString -> (ScriptPurpose, Redeemer)
phasEntry root key value =
  ( Rewarding (ScriptCredential (ScriptHash (toBuiltin phasHash)))
  , Redeemer (dataToBuiltinData (PD.List [PD.B root, PD.B key, PD.B value, emptyProof]))
  )

emptyProof :: PD.Data
emptyProof = PD.List []

--------------------------------------------------------------------------------
-- The block's evidence
--------------------------------------------------------------------------------

referenceInputs :: [TxInInfo]
referenceInputs = [hubRefIn, nodeRefIn]

hubRefIn :: TxInInfo
hubRefIn =
  TxInInfo
    (outRefN 1)
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol hubPolicy)))
        ( adaValue 2_000_000
            <> singleton hubPolicy (TokenName (toBuiltin ("MIDGARD_HUB_ORACLE" :: BS.ByteString))) 1
        )
        (OutputDatum (Datum (dataToBuiltinData hubDatum)))
        Nothing
    )

{- | The hub oracle's datum: twelve policy ids, thirteen addresses and one more
policy. Only the state-queue policy — slot 4 — is read here, and it is read
positionally, so the surrounding shape has to be right for the read to land.
-}
hubDatum :: PD.Data
hubDatum =
  PD.Constr
    0
    ( map (PD.B . unCS . policyFor) [0x41, 0x42, 0x43, 0x44]
        <> [PD.B (unCS stateQueuePolicy)]
        <> [PD.B (unCS (policyFor (0x45 + i))) | i <- [0 .. 6]]
        <> replicate 13 addressData
        <> [PD.B (unCS (policyFor 0x4f))]
    )
  where
    addressData = PD.Constr 0 [PD.Constr 1 [PD.B (unCS (policyFor 0x42))], PD.Constr 1 []]

nodeRefIn :: TxInInfo
nodeRefIn =
  TxInInfo
    (outRefN 2)
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol stateQueuePolicy)))
        ( adaValue 2_000_000
            <> singleton stateQueuePolicy (TokenName (toBuiltin ("MBLC" <> headerHash))) 1
        )
        (OutputDatum (Datum (dataToBuiltinData element)))
        Nothing
    )
  where
    element = PD.Constr 0 [PD.Constr 1 [node], PD.Constr 1 []]
    node = PD.Constr 0 [headerData, PD.B ""]

{- | @ledger_state.HeaderV1@. Slot 4 is @transactions_root@ and slot 11 is
@l2_transaction_count@; the two are read together, because the root the header
commits is the counted commitment over the raw one.
-}
headerData :: PD.Data
headerData =
  PD.Constr
    0
    [ PD.B (hash32 0x01)
    , PD.B (hash32 0x02)
    , PD.B headerWithdrawalsRoot
    , PD.B (hash32 0x04)
    , PD.B headerTransactionsRoot
    , PD.B (hash32 0x06)
    , PD.B (hash32 0x07)
    , PD.B (hash32 0x08)
    , PD.B (hash32 0x09)
    , PD.I withdrawalCount
    , PD.I 0
    , PD.I l2Count
    , PD.I 0
    , PD.I 0
    , PD.I 0
    , PD.I 0
    , PD.I 100
    , PD.I 200
    , PD.I 0
    , PD.I 0
    , PD.I 0
    , PD.I 0
    , PD.B (BS.replicate 28 0x02)
    , PD.B prover
    , PD.I 1
    ]

headerTransactionsRoot :: BS.ByteString
headerTransactionsRoot = commitCountedRoot transactionsDomain phasRoot l2Count

{- | @transition_trace.commit_counted_root@, rebuilt from the spec so that a
change on either side fails a test.
-}
commitCountedRoot :: Integer -> BS.ByteString -> Integer -> BS.ByteString
commitCountedRoot domainTag root count =
  blake2b256
    ( "MidgardRootCountV1"
        <> serialise (PD.Constr domainTag [])
        <> root
        <> serialise (PD.I count)
    )

-- | @TransactionsV1RootDomain@ is the third constructor of @RootDomain@.
transactionsDomain :: Integer
transactionsDomain = 2

-- | @WithdrawalsRootDomain@ is the first.
withdrawalsDomain :: Integer
withdrawalsDomain = 0

--------------------------------------------------------------------------------
-- The block's one withdrawal event
--------------------------------------------------------------------------------

{- | The block commits exactly one withdrawal, and it takes 'sharedInputRef' off
L2 — which is the reference input 'tx1', 'tx2' and 'tx3' all carry, so any of
them is a transaction the withdrawn-reference-input family has something to say
about.
-}
withdrawalId :: PD.Data
withdrawalId = PD.Constr 0 [PD.B (BS.replicate 32 0x61), PD.I 0]

withdrawalKeyBytes :: BS.ByteString
withdrawalKeyBytes = serialise withdrawalId

{- | @ledger_state.WithdrawalInfo@ over a named L2 output reference.

Only @l2_outref@ and @validity@ are read by anything ported, but the whole
structure is built because the value's /bytes/ are what the tree commits: a
shorter stand-in would hash differently and the membership walk would be checking
a different entry than the one the step decodes.
-}
withdrawalInfoData :: (BS.ByteString, Integer) -> PD.Data -> PD.Data
withdrawalInfoData (txId, index) validity =
  PD.Constr
    0
    [ PD.Constr
        0
        [ PD.Constr 0 [PD.B txId, PD.I index] -- l2_outref
        , PD.B prover -- l2_owner
        , PD.Map [] -- l2_value
        , PD.Constr 0 [PD.Constr 1 [PD.B fpSpendScript], PD.Constr 1 []] -- l1_address
        , PD.Constr 0 [] -- l1_datum: NoDatum
        ]
    , PD.B "" -- signature
    , validity
    ]

-- | The event the block actually commits: valid, and naming 'sharedInputRef'.
withdrawalValueBytes :: BS.ByteString
withdrawalValueBytes = serialise (withdrawalInfoData sharedInputRef (PD.Constr 0 []))

{- | The raw MPF root of a one-entry tree.

With an empty proof the library's @including@ reduces to
@combine(suffix(path, 0), blake2b_256(value))@, and @suffix@ at cursor 0 is
@0xff@ followed by the whole path. Written out here rather than taken from the
port so that a change to either side fails a test.
-}
singleEntryPhasRoot :: BS.ByteString -> BS.ByteString -> BS.ByteString
singleEntryPhasRoot keyBytes valueBytes =
  blake2b256 (BS.cons 0xff (blake2b256 keyBytes <> blake2b256 valueBytes))

withdrawalsPhasRoot :: BS.ByteString
withdrawalsPhasRoot = singleEntryPhasRoot withdrawalKeyBytes withdrawalValueBytes

withdrawalCount :: Integer
withdrawalCount = 1

-- | Slot 2 of the header: the counted commitment over 'withdrawalsPhasRoot'.
headerWithdrawalsRoot :: BS.ByteString
headerWithdrawalsRoot =
  commitCountedRoot withdrawalsDomain withdrawalsPhasRoot withdrawalCount

{- | @transition_trace.RootMembershipProof@ — seven fields, in declaration order.

The key and value travel in the witness /and/ are handed to the verifier
separately; the step serialises them itself rather than trusting the witness's
own idea of its encoding.
-}
membershipProof ::
  Integer -> BS.ByteString -> BS.ByteString -> Integer -> PD.Data -> PD.Data -> PD.Data
membershipProof domain root phasRoot' count key value =
  PD.Constr
    0
    [ PD.Constr domain []
    , PD.B root
    , PD.B phasRoot'
    , PD.I count
    , key
    , value
    , PD.List [] -- the proof: empty, since the tree holds one entry
    ]

l2Count :: Integer
l2Count = 17

--------------------------------------------------------------------------------
-- The three transactions
--------------------------------------------------------------------------------

{- | The disputed output reference, spent by both conflicting transactions. Its
index is 0, which is also the position it occupies in each collection — the two
are unrelated and the fixtures keep them apart by giving @otherInputRef@ a
different transaction id rather than a different index.
-}
sharedInputRef, otherInputRef :: (BS.ByteString, Integer)
sharedInputRef = (BS.replicate 32 0x71, 0)
otherInputRef = (BS.replicate 32 0x72, 0)

-- | @native_tx/types.MidgardTxInput@ as the decoder hands it back.
inputData :: (BS.ByteString, Integer) -> PD.Data
inputData (txId, index) = PD.Constr 0 [PD.B txId, PD.I index]

{- | @components.encode_midgard_tx_input@ — the /ledger/ MPF's key encoding: a
definite two-element array, not a serialised Plutus constructor.
-}
encodedInput :: (BS.ByteString, Integer) -> BS.ByteString
encodedInput = spendInputItem

-- | Two conflicting transactions: same spend inputs, different fee.
tx1, tx2, tx3 :: Tx
tx1 =
  Tx
    { tFee = 1_000_000
    , tSpendInputs = [sharedInputRef]
    , tReferenceInputs = [sharedInputRef]
    , tOutputCount = 2
    , tRequiredSigners = [0]
    , tWitnesses = [Witness 0 True]
    , tScripts = []
    , tValidityStart = 0
    , tValidityEnd = 65536
    }
tx2 =
  Tx
    { tFee = 2_000_000
    , tSpendInputs = [sharedInputRef]
    , tReferenceInputs = [sharedInputRef]
    , tOutputCount = 2
    , tRequiredSigners = [0]
    , tWitnesses = [Witness 0 True]
    , tScripts = []
    , tValidityStart = 0
    , tValidityEnd = 65536
    }

{- | An honest third transaction of the same block, spending something else.

Its spend inputs and reference inputs are deliberately __different__. §4 removed
field-index domain separation, so a field-0 and a field-1 preimage over the same
items commit identically — a transaction whose two collections matched would make
a step that read the wrong slot indistinguishable from one that read the right
one. This is the fixture that tells them apart.
-}
tx3 =
  Tx
    { tFee = 3_000_000
    , tSpendInputs = [otherInputRef]
    , tReferenceInputs = [sharedInputRef]
    , tOutputCount = 3
    , tRequiredSigners = [0]
    , tWitnesses = [Witness 0 True]
    , tScripts = []
    , tValidityStart = 0
    , tValidityEnd = 65536
    }

-- | A transaction that spends nothing — the zero-input violation.
txEmpty :: Tx
txEmpty =
  Tx
    { tFee = 4_000_000
    , tSpendInputs = []
    , tReferenceInputs = []
    , tOutputCount = 0
    , tRequiredSigners = []
    , tWitnesses = []
    , tScripts = []
    , tValidityStart = 0
    , tValidityEnd = 65536
    }

{- | A transaction requiring signer 1 and witnessed only by signer 0 — the
missing-signature violation.
-}
txUnsigned :: Tx
txUnsigned = tx1 {tFee = 5_000_000, tRequiredSigners = [1], tWitnesses = [Witness 0 True]}

{- | A transaction whose single witness carries a signature over the wrong
message — the invalid-signature violation.
-}
txBadSig :: Tx
txBadSig = tx1 {tFee = 6_000_000, tWitnesses = [Witness 0 False]}

{- | A transaction spending slot 0 of 'tx3' — which is script-locked — and
carrying no script witness at all. The missing-native-script violation.

Its spend input names a /real/ fixture transaction id rather than an opaque one,
because the family's step-03 binds the transaction that produced the disputed
output and checks that its verified id is the one the input names. An input
pointing at nothing would make step-03 unreachable and steps 04–06 untestable.
-}
txScriptSpend :: Tx
txScriptSpend =
  Tx
    { tFee = 7_000_000
    , tSpendInputs = [(tx3Id, 0)]
    , tReferenceInputs = []
    , tOutputCount = 1
    , tRequiredSigners = [0]
    , tWitnesses = [Witness 0 True]
    , tScripts = []
    , tValidityStart = 0
    , tValidityEnd = 65536
    }

data Tx = Tx
  { tFee :: Integer
  , tSpendInputs :: [(BS.ByteString, Integer)]
  , tReferenceInputs :: [(BS.ByteString, Integer)]
  , tOutputCount :: Int
  , tRequiredSigners :: [Int]
  -- ^ Signer indices whose key hashes field 4 commits.
  , tWitnesses :: [Witness]
  -- ^ Address witnesses field 7 commits.
  , tScripts :: [(Integer, BS.ByteString)]
  -- ^ Script witnesses field 6 commits, each a language tag and its bytes.
  , tValidityStart :: Integer
  -- ^ Inclusive lower bound, or @env.posix_time_none@ (@-1@) for unbounded.
  , tValidityEnd :: Integer
  -- ^ __Exclusive__ upper bound, or @-1@ for unbounded.
  }

spendInputsOf :: Tx -> [(BS.ByteString, Integer)]
spendInputsOf = tSpendInputs

tx1Id, tx2Id, tx3Id, txEmptyId, txUnsignedId, txBadSigId, txScriptSpendId :: BS.ByteString
tx1Id = txIdOf tx1
tx2Id = txIdOf tx2
tx3Id = txIdOf tx3
txEmptyId = txIdOf txEmpty
txUnsignedId = txIdOf txUnsigned
txBadSigId = txIdOf txBadSig
txScriptSpendId = txIdOf txScriptSpend

tx1Cbor, tx2Cbor, tx3Cbor, txEmptyCbor, txUnsignedCbor, txBadSigCbor, txScriptSpendCbor ::
  BS.ByteString
tx1Cbor = compactOf tx1
tx2Cbor = compactOf tx2
tx3Cbor = compactOf tx3
txEmptyCbor = compactOf txEmpty
txUnsignedCbor = compactOf txUnsigned
txBadSigCbor = compactOf txBadSig
txScriptSpendCbor = compactOf txScriptSpend

--------------------------------------------------------------------------------
-- Reference encoders (§2.5, §3, §5.1, §5.3)
--------------------------------------------------------------------------------

{- | §5.1: a definite array header followed by one definite byte string per
item. §5.3 fields 0 and 1: @82 ‖ 58 20 tx_id ‖ 19 index_be16@, 38 bytes an item.
-}
spendInputsPreimage :: Tx -> BS.ByteString
spendInputsPreimage tx = inputCollectionPreimage (tSpendInputs tx)

-- | §5.3 field 1 has field 0's item shape and stride; only the slot differs.
referenceInputsPreimage :: Tx -> BS.ByteString
referenceInputsPreimage tx = inputCollectionPreimage (tReferenceInputs tx)

inputCollectionPreimage :: [(BS.ByteString, Integer)] -> BS.ByteString
inputCollectionPreimage refs =
  arrayHeader (length refs) <> BS.concat [wrapItem (spendInputItem r) | r <- refs]

{- | §5.3 field 2 is variable-width, so its items differ in size on purpose: the
count a step reads has to come from the §5.1 walk rather than from arithmetic.
-}
outputsPreimage :: Tx -> BS.ByteString
outputsPreimage tx = outputCollectionPreimage (tOutputCount tx)

outputCollectionPreimage :: Int -> BS.ByteString
outputCollectionPreimage n =
  arrayHeader n <> BS.concat [wrapItem (outputItem i) | i <- [0 .. n - 1]]

{- | The output at slot @i@ of every fixture transaction.

__Slot 0 is script-locked__, at the hash of 'nativeScriptBytes' under the native
language tag, which is what makes @missing-native-script-tx@'s step-04 have
something to find. Every other slot is key-locked, so an index naming one of them
is a step reading a credential that is not a script.

The widths differ on purpose: slot 0 carries no datum (an @a2@ map) and the rest
carry datums of growing length (@a3@). §5.3 field 2 is variable-width, so a step
that reached item @n@ by multiplying a stride would land in the middle of an
item rather than at its head.
-}
outputItem :: Int -> BS.ByteString
outputItem 0 = midgardOutputCbor (scriptAddressBytes lockedScriptHash) 2_000_000 Nothing
outputItem i =
  midgardOutputCbor
    (pubKeyAddressBytes (keyHashFor i))
    (fromIntegral (2_000_000 + i))
    (Just (BS.replicate (4 * i) (fromIntegral (0xc0 + i))))

{- | @components.encode_midgard_tx_output@ for the two shapes this fixture uses:
a CBOR map keyed @0@ address, @1@ value and optionally @2@ datum, with absent
entries simply missing rather than @null@ — which is what keeps one output from
having two encodings.

The value is ada-only, so its asset map is the empty @a0@.
-}
midgardOutputCbor :: BS.ByteString -> Integer -> Maybe BS.ByteString -> BS.ByteString
midgardOutputCbor addressBytes lovelace mDatum =
  case mDatum of
    Nothing -> "\xa2" <> required
    Just datumCbor -> "\xa3" <> required <> "\x02" <> wrapItem datumCbor
  where
    required =
      "\x00" <> wrapItem addressBytes <> "\x01" <> ("\x82" <> cborInt lovelace <> "\xa0")

{- | @components.encode_midgard_address@ for a script-locked address with no
stake credential: type 7, network 0, unprotected, so the header byte is
@7 * 16 + 0 + 0@.
-}
scriptAddressBytes :: BS.ByteString -> BS.ByteString
scriptAddressBytes = BS.cons 0x70

-- | The same for a key-locked address: type 6.
pubKeyAddressBytes :: BS.ByteString -> BS.ByteString
pubKeyAddressBytes = BS.cons 0x60

spendInputItem :: (BS.ByteString, Integer) -> BS.ByteString
spendInputItem (txId, index) =
  BS.concat
    [ "\x82"
    , defBytes32 txId
    , "\x19"
    , BS.pack [fromIntegral (index `div` 256), fromIntegral index]
    ]

arrayHeader :: Int -> BS.ByteString
arrayHeader n
  | n <= 23 = BS.pack [fromIntegral (0x80 + n)]
  | n <= 255 = BS.pack [0x98, fromIntegral n]
  | otherwise = BS.pack [0x99, fromIntegral (n `div` 256), fromIntegral n]

wrapItem :: BS.ByteString -> BS.ByteString
wrapItem bytes
  | n <= 23 = BS.cons (fromIntegral (0x40 + n)) bytes
  | otherwise = BS.pack [0x58, fromIntegral n] <> bytes
  where
    n = BS.length bytes

-- | The compact body: twelve entries, the six field commitments among them.
compactBody :: Tx -> BS.ByteString
compactBody tx =
  BS.concat
    [ "\x8c"
    , defBytes32 (blake2b256 (spendInputsPreimage tx))
    , defBytes32 (blake2b256 (referenceInputsPreimage tx))
    , defBytes32 (blake2b256 (outputsPreimage tx))
    , cborInt (tFee tx)
    , cborInt (tValidityStart tx)
    , cborInt (tValidityEnd tx)
    , defBytes32 (hash32 0x04)
    , defBytes32 (blake2b256 (requiredSignersPreimage tx))
    , defBytes32 (hash32 0x06)
    , defBytes32 (hash32 0x07)
    , defBytes32 (hash32 0x08)
    , cborInt 1
    ]

compactOf :: Tx -> BS.ByteString
compactOf tx = compactWith tx (witnessSetHashOf tx)

{- | The compact structure with a chosen @witness_set_hash@ rather than the
transaction's own.

The whole missing-native-script and signature story fits in this one function's
existence: §3's id preimage is 'compactBody', so /every/ value passed here yields
compact bytes that re-derive to the same transaction id.
-}
compactWith :: Tx -> BS.ByteString -> BS.ByteString
compactWith tx wsHash = compactWithValidity tx wsHash 3

{- | The same, with the §2.5 validity code chosen too.

Every fixture transaction here carries code 3 by default, because the families
this fixture was built for are all about transactions the operator already called
invalid. A rule that only fires on a transaction the block declared /valid/ needs
code 0, and the id is unaffected either way — §3's preimage is the body alone, so
the code sits outside it.
-}
compactWithValidity :: Tx -> BS.ByteString -> Integer -> BS.ByteString
compactWithValidity tx wsHash validityCode =
  BS.concat ["\x84", cborInt 1, compactBody tx, defBytes32 wsHash, cborInt validityCode]

{- | §2.5's witness set: three collection hashes, @0x83@.

Every transaction here commits a real one, because the witness-set families need
the compact structure's trailing @witness_set_hash@ to be a value their step-01
can /read/ rather than one their step-02 could invent. §3's id preimage is the
body alone, so that tail is the only place the witness set is pinned.
-}
witnessSetCborOf :: Tx -> BS.ByteString
witnessSetCborOf = witnessSetCborFrom . witnessSetHashesOf

-- | The three collection hashes §2.5 commits, in order: address, script, redeemer.
witnessSetHashesOf :: Tx -> (BS.ByteString, BS.ByteString, BS.ByteString)
witnessSetHashesOf tx =
  ( blake2b256 (addressWitnessesPreimage tx)
  , blake2b256 (scriptWitnessesPreimage tx)
  , hash32 0x13
  )

witnessSetCborFrom :: (BS.ByteString, BS.ByteString, BS.ByteString) -> BS.ByteString
witnessSetCborFrom (addr, script, redeemer) =
  BS.concat ["\x83", defBytes32 addr, defBytes32 script, defBytes32 redeemer]

witnessSetHashOf :: Tx -> BS.ByteString
witnessSetHashOf = blake2b256 . witnessSetCborOf

{- | §5.3 field 7: @82 ‖ 58 20 vkey ‖ 58 40 signature@, 101 bytes an item.

Signatures are real Ed25519, generated here with @cardano-crypto-class@, and the
message signed is the transaction's own §3 id — which is what makes the
invalid-signature family's negative case (a witness that /does/ verify)
constructible at all.
-}
addressWitnessesPreimage :: Tx -> BS.ByteString
addressWitnessesPreimage tx =
  arrayHeader (length (tWitnesses tx))
    <> BS.concat [wrapItem (addressWitnessItem tx w) | w <- tWitnesses tx]

addressWitnessItem :: Tx -> Witness -> BS.ByteString
addressWitnessItem tx w =
  BS.concat
    [ "\x82"
    , defBytes32 (verKeyFor (wKeyIndex w))
    , "\x58\x40" <> signature
    ]
  where
    signature
      | wValid w = signWith (wKeyIndex w) (txIdOf tx)
      | otherwise = signWith (wKeyIndex w) "not this transaction"

{- | §5.3 field 6: the script witnesses, variable-width because scripts are.

An item is @82 ‖ language_tag ‖ definite bytes@ — the same encoding
@components.encode_midgard_versioned_script@ writes, rebuilt here from the format
rather than taken from the port.
-}
scriptWitnessesPreimage :: Tx -> BS.ByteString
scriptWitnessesPreimage = scriptWitnessCollectionPreimage . tScripts

scriptWitnessCollectionPreimage :: [(Integer, BS.ByteString)] -> BS.ByteString
scriptWitnessCollectionPreimage scripts =
  arrayHeader (length scripts)
    <> BS.concat [wrapItem (versionedScriptItem tag bytes) | (tag, bytes) <- scripts]

-- | One @MidgardVersionedScript@'s canonical bytes.
versionedScriptItem :: Integer -> BS.ByteString -> BS.ByteString
versionedScriptItem tag bytes = "\x82" <> cborInt tag <> wrapItem bytes

{- | @script_proof_v1.versioned_script_hash@ — @blake2b_224@ over the language
tag byte followed by the script bytes.

The tag is the /language/ tag, 0 native, 3 Plutus V3, 128 Midgard V1, and not the
constructor index. For a native script the two coincide, which is exactly why the
fixture also exercises tag 3.
-}
versionedScriptHashOf :: Integer -> BS.ByteString -> BS.ByteString
versionedScriptHashOf tag bytes = blake2b224 (BS.cons (fromIntegral tag) bytes)

{- | The native script the missing-native-script-tx family is about, and one that
is not it.

The bytes are opaque here: nothing in the fraud proof parses a native script, it
only hashes one. What matters is that the two differ and that neither is 23 bytes
or shorter, so their §5.1 wrappers take the @58 xx@ form rather than the packed
one.
-}
nativeScriptBytes, otherNativeScriptBytes :: BS.ByteString
nativeScriptBytes = "\x82\x00\x58\x1c" <> BS.replicate 28 0x81
otherNativeScriptBytes = "\x82\x00\x58\x1c" <> BS.replicate 28 0x82

-- | The credential slot 0 of every fixture output is locked by.
lockedScriptHash :: BS.ByteString
lockedScriptHash = versionedScriptHashOf 0 nativeScriptBytes

-- | An address witness: whose key, and whether its signature actually verifies.
data Witness = Witness {wKeyIndex :: Int, wValid :: Bool}
  deriving stock (Eq, Show)

signKeyFor :: Int -> DSIGN.SignKeyDSIGN Ed25519DSIGN
signKeyFor i = DSIGN.genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 (fromIntegral i)))

-- | The 32-byte verification key of signer @i@.
verKeyFor :: Int -> BS.ByteString
verKeyFor = DSIGN.rawSerialiseVerKeyDSIGN . DSIGN.deriveVerKeyDSIGN . signKeyFor

-- | @get_verification_key_hash@ — @blake2b_224@ over the key.
keyHashFor :: Int -> BS.ByteString
keyHashFor = blake2b224 . verKeyFor

signWith :: Int -> BS.ByteString -> BS.ByteString
signWith i msg = DSIGN.rawSerialiseSigDSIGN (DSIGN.signDSIGN () msg (signKeyFor i))

blake2b224 :: BS.ByteString -> BS.ByteString
blake2b224 = fromBuiltin . Builtins.blake2b_224 . toBuiltin

{- | §5.3 field 4: raw 28-byte verification-key hashes, stride 30. The signers a
transaction /requires/, which need not be the ones that witnessed it — the whole
missing-signature family is about that gap.
-}
requiredSignersPreimage :: Tx -> BS.ByteString
requiredSignersPreimage tx =
  arrayHeader (length (tRequiredSigners tx))
    <> BS.concat [wrapItem (keyHashFor i) | i <- tRequiredSigners tx]

-- | §3: the id preimage is the /body/ alone, under the version byte.
txIdOf :: Tx -> BS.ByteString
txIdOf tx = blake2b256 ("MidgardNativeTxBodyV1" <> cborInt 1 <> compactBody tx)

cborInt :: Integer -> BS.ByteString
cborInt n
  | n >= 0 = major 0 n
  | otherwise = major 1 (-1 - n)
  where
    major base v
      | v <= 23 = BS.pack [fromIntegral (base * 32 + v)]
      | v <= 255 = BS.pack [fromIntegral (base * 32 + 24), fromIntegral v]
      | v <= 65535 = BS.pack [fromIntegral (base * 32 + 25)] <> be 2 v
      | otherwise = BS.pack [fromIntegral (base * 32 + 26)] <> be 4 v
    be w v = BS.pack [fromIntegral (v `div` (256 ^ i) `mod` 256) | i <- [w - 1, w - 2 .. 0 :: Integer]]

defBytes32 :: BS.ByteString -> BS.ByteString
defBytes32 h = "\x58\x20" <> h

hash32 :: Int -> BS.ByteString
hash32 n = blake2b256 (BS.pack [fromIntegral n])

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

serialise :: PD.Data -> BS.ByteString
serialise = fromBuiltin . Builtins.serialiseData . dataToBuiltinData

--------------------------------------------------------------------------------
-- Identities
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = CurrencySymbol (toBuiltin (BS.replicate 28 (fromIntegral n)))

ctPolicy, fpPolicy, hubPolicy, stateQueuePolicy, certificatePolicy :: CurrencySymbol
ctPolicy = policyFor 0x11
fpPolicy = policyFor 0x12
hubPolicy = policyFor 0x13
stateQueuePolicy = policyFor 0x14
certificatePolicy = policyFor 0x16

{- | The step under test sits at 'stepScript' and its successor at 'nextScript';
'otherScript' is neither, so an output sent there is a diverted thread.
-}
stepScript, nextScript, otherScript, fpSpendScript :: BS.ByteString
stepScript = BS.replicate 28 0x21
nextScript = BS.replicate 28 0x22
otherScript = BS.replicate 28 0x23
fpSpendScript = BS.replicate 28 0x24

prover :: BS.ByteString
prover = BS.replicate 28 0x31

categoryId, headerHash, threadName, otherThreadName :: BS.ByteString
categoryId = BS.pack [0x00, 0x00, 0x00, 0x07]
headerHash = BS.replicate 28 0xaa
threadName = categoryId <> headerHash
otherThreadName = categoryId <> BS.replicate 28 0xbb

phasRoot, otherRoot, prevUtxosRoot :: BS.ByteString
phasRoot = BS.replicate 32 0x51
otherRoot = BS.replicate 32 0x52

-- | The header's initial-ledger root, at slot 0 of @HeaderV1@.
prevUtxosRoot = hash32 0x01

{- | The hub oracle is named to a step by its /script hash/, which is the same
28 bytes as its minting policy id.
-}
hubOracleHash :: ScriptHash
hubOracleHash = ScriptHash (unCurrencySymbol hubPolicy)

fraudProofAddress, otherAddress :: Address
fraudProofAddress = scriptHashAddress (ScriptHash (toBuiltin fpSpendScript))
otherAddress = scriptHashAddress (ScriptHash (toBuiltin otherScript))

-- | @env.plutarch_phas_validator_hash@, copied independently from @env/default.ak@.
phasHash :: BS.ByteString
phasHash = unhexed "1fc59ff54da02f2535d64b40b647a8826c8b3d914d7ba5257f5b2721"

-- | @env.plutarch_pexcludes_validator_hash@.
pexcludesHash :: BS.ByteString
pexcludesHash = unhexed "a9ec251d6476217b1abccd5f035dec1272a4b04f640f503fca9e734d"

unhexed :: String -> BS.ByteString
unhexed = BS.pack . go
  where
    go (a : b : rest) = fromIntegral (digit a * 16 + digit b) : go rest
    go _ = []
    digit c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | otherwise = fromEnum c - fromEnum 'a' + 10 :: Int

unCS :: CurrencySymbol -> BS.ByteString
unCS = fromBuiltin . unCurrencySymbol

adaValue :: Integer -> Value
adaValue = singleton (CurrencySymbol "") (TokenName "")

ownRef :: TxOutRef
ownRef = outRefN 0

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId (toBuiltin (BS.replicate 32 0x01)))
