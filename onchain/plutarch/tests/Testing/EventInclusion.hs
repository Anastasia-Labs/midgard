{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.EventInclusion
Description : Tests for @settlement.valid_event_inclusion@.

Three arms, and the interesting thing is how each derives the value it proves
membership of. A deposit's info is read straight from its datum; a withdrawal's
has the claimed verdict substituted into it; a transaction order has no stored
value at all, so the whole @ForcedInclusionTxV1@ is reassembled around the
claimed verdict.

The verdict cases are the ones that matter: the claimant names a verdict, and
the operator's committed root has to corroborate it.
-}
module Testing.EventInclusion (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), singleton)
import PlutusLedgerApi.V3 (
  Credential (..),
  Datum (..),
  OutputDatum (..),
  Redeemer (..),
  ScriptHash (..),
  ScriptPurpose (Rewarding),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
  toBuiltinData,
 )
import PlutusTx.Builtins (
  blake2b_256,
  builtinDataToData,
  dataToBuiltinData,
  fromBuiltin,
  serialiseData,
  toBuiltin,
 )
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Settlement (
  PEventMembershipProof,
  PEventType,
  pvalidEventInclusion,
 )
import Testing.Eval (passertEval, pfails)
import Testing.ScriptContextBuilder (currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Event Inclusion Tests"
    [ testGroup
        "deposit"
        [ testCase "proves a deposit the settlement's tree contains" $
            passertEval $ runDeposit depositEventInfo
        , testCase "rejects a deposit whose info differs from the tree's" $
            passertEval $ pnot #$ runDeposit otherDepositInfo
        ]
    , testGroup
        "withdrawal"
        [ testCase "proves a withdrawal under the claimed verdict" $
            passertEval $ runWithdrawal verdictOwner verdictOwner
        , -- The verdict is substituted into the info before the proof, so a
          -- verdict the operator never committed cannot corroborate.
          testCase "rejects a verdict the committed root does not carry" $
            passertEval $ pnot #$ runWithdrawal verdictValue verdictOwner
        , testCase "proves a withdrawal the operator judged valid" $
            passertEval $ runWithdrawal verdictValid verdictValid
        ]
    , testGroup
        "transaction order"
        [ testCase "proves an order under the claimed verdict" $
            passertEval $ runTxOrder txValid txValid
        , -- The stored value is rebuilt around the verdict, so this is the same
          -- substitution property one level deeper.
          testCase "rejects a verdict the committed root does not carry" $
            passertEval $ pnot #$ runTxOrder txFailedScript txValid
        , testCase "proves an order judged to have a failed script" $
            passertEval $ runTxOrder txFailedScript txFailedScript
        ]
    , testGroup
        "proof wrappers"
        [ -- The wrapper is what stops a proof for one tree being handed to
          -- another arm.
          testCase "rejects a deposit proof supplied to the withdrawal arm" $
            pfails $ runMismatched
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

depositPolicy, withdrawalPolicy, txOrderPolicy :: CurrencySymbol
depositPolicy = repeatedByte 0x21
withdrawalPolicy = repeatedByte 0x22
txOrderPolicy = repeatedByte 0x23

repeatedByte :: Int -> CurrencySymbol
repeatedByte b = currencySymbolFromHex (concatMap hex (replicate 28 b))
  where
    hex n = let xs = "0123456789abcdef" in [xs !! (n `div` 16), xs !! (n `mod` 16)]

eventName :: TokenName
eventName = TokenName "evt"

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101")

phasValidatorHash :: BS.ByteString
phasValidatorHash =
  either (error "bad hex") id (Base16.decode (BS8.pack "1fc59ff54da02f2535d64b40b647a8826c8b3d914d7ba5257f5b2721"))

phasRoot :: BS.ByteString
phasRoot = BS.replicate 32 0xaa

itemCount :: Integer
itemCount = 2

-- | @commit_counted_root@ for a given domain tag.
countedRoot :: Integer -> BS.ByteString
countedRoot domainTag =
  fromBuiltin $
    blake2b_256
      ( toBuiltin ("MidgardRootCountV1" :: BS.ByteString)
          <> serialiseData (dataToBuiltinData (PD.Constr domainTag []))
          <> toBuiltin phasRoot
          <> serialiseData (dataToBuiltinData (PD.I itemCount))
      )

serialisedOf :: PD.Data -> BS.ByteString
serialisedOf = fromBuiltin . serialiseData . dataToBuiltinData

eventId :: PD.Data
eventId = builtinDataToData (toBuiltinData (outRefN 0))

--------------------------------------------------------------------------------
-- Event payloads
--------------------------------------------------------------------------------

depositEventInfo, otherDepositInfo :: PD.Data
depositEventInfo = PD.Constr 0 [addrData, PD.I 0, PD.Constr 1 []]
otherDepositInfo = PD.Constr 0 [addrData, PD.I 9, PD.Constr 1 []]

addrData :: PD.Data
addrData =
  PD.Constr 0 [PD.Constr 1 [PD.B (fromBuiltin (unCurrencySymbol depositPolicy))], PD.Constr 1 []]

verdictValid, verdictOwner, verdictValue :: PD.Data
verdictValid = PD.Constr 0 []
verdictOwner = PD.Constr 3 []
verdictValue = PD.Constr 4 []

-- | @WithdrawalInfo { body, signature, validity }@.
withdrawalInfoWith :: PD.Data -> PD.Data
withdrawalInfoWith validity = PD.Constr 0 [PD.B "body", PD.B "sig", validity]

txValid, txFailedScript :: PD.Data
txValid = PD.Constr 0 []
txFailedScript = PD.Constr 3 []

nativeSource :: PD.Data
nativeSource = PD.Constr 0 [PD.B "cbor", PD.B "wits", PD.B "lens"]

-- | @TxOrderPayloadV1 { tx_id, transaction_commitment, source, terminal_ref }@.
txOrderPayload :: PD.Data
txOrderPayload = PD.Constr 0 [PD.B "txid", PD.B "commit", nativeSource, PD.Constr 1 []]

-- | @ForcedInclusionTxV1 { tx_id, source, operator_validity }@.
forcedInclusionTx :: PD.Data -> PD.Data
forcedInclusionTx validity = PD.Constr 0 [PD.B "txid", nativeSource, validity]

--------------------------------------------------------------------------------
-- Applying the term
--------------------------------------------------------------------------------

{- | An authenticated event reference input.

@datum@ is the event's own datum; the NFT is what
@get_authentic_input_with_nft_at@ checks.
-}
eventRefIn :: CurrencySymbol -> PD.Data -> TxInInfo
eventRefIn policy datum =
  TxInInfo
    (outRefN 5)
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol policy)))
        (mkAdaValue 2_000_000 <> singleton policy eventName 1)
        (OutputDatum (Datum (dataToBuiltinData datum)))
        Nothing
    )

{- | The redeemer list carrying the @phas@ withdrawal for @provenValue@.

The proof itself is delegated, so what the test varies is which key/value the
delegated call was made for.
-}
phasRedeemers :: PD.Data -> PD.Data -> [(ScriptPurpose, Redeemer)]
phasRedeemers key value =
  [
    ( Rewarding (ScriptCredential (ScriptHash (toBuiltin phasValidatorHash)))
    , Redeemer
        ( dataToBuiltinData
            ( PD.List
                [ PD.B phasRoot
                , PD.B (serialisedOf key)
                , PD.B (serialisedOf value)
                , PD.List [PD.B "step"]
                ]
            )
        )
    )
  ]

-- | A membership witness for @key@/@value@ under @domainTag@.
witnessFor :: Integer -> PD.Data -> PD.Data -> PD.Data
witnessFor domainTag key value =
  PD.Constr
    0
    [ PD.Constr domainTag []
    , PD.B (countedRoot domainTag)
    , PD.B phasRoot
    , PD.I itemCount
    , PD.B (serialisedOf key)
    , PD.B (serialisedOf value)
    , PD.List [PD.B "step"]
    ]

run ::
  forall s.
  PD.Data ->
  PD.Data ->
  TxInInfo ->
  PD.Data ->
  PD.Data ->
  Term s PBool
run eventTypeData proofData refIn provenKey provenValue =
  pvalidEventInclusion
    (coerceTo @PEventType eventTypeData)
    (pdata (pconstant depositPolicy))
    (pdata (pconstant withdrawalPolicy))
    (pdata (pconstant txOrderPolicy))
    (pconstant (countedRoot 3))
    (pconstant (countedRoot 0))
    (pconstant (countedRoot 1))
    (coerceTo @PEventMembershipProof proofData)
    (pdata (pconstant eventName))
    0
    (pconstant [refIn])
    (pconstant (phasRedeemers provenKey provenValue))

coerceTo :: forall a s. (PIsData a) => PD.Data -> Term s a
coerceTo d = pfromData (punsafeCoerce (pconstant @PData d))

{- | A deposit inclusion proof. @storedInfo@ is what the tree records; the
event's datum always carries 'depositEventInfo'.
-}
runDeposit :: forall s. PD.Data -> Term s PBool
runDeposit storedInfo =
  run
    (PD.Constr 0 [])
    (PD.Constr 0 [witnessFor 3 eventId storedInfo])
    (eventRefIn depositPolicy (PD.Constr 0 [eventId, depositEventInfo]))
    eventId
    storedInfo

{- | A withdrawal inclusion proof.

@claimed@ is the verdict in the redeemer; @committed@ is the one the tree
records. They agree in the positive cases and differ in the negative one.
-}
runWithdrawal :: forall s. PD.Data -> PD.Data -> Term s PBool
runWithdrawal claimed committed =
  run
    (PD.Constr 1 [claimed])
    (PD.Constr 1 [witnessFor 0 eventId (withdrawalInfoWith committed)])
    (eventRefIn withdrawalPolicy (PD.Constr 0 [eventId, withdrawalInfoWith verdictValid]))
    eventId
    (withdrawalInfoWith committed)

-- | A transaction-order inclusion proof, with the same claimed/committed split.
runTxOrder :: forall s. PD.Data -> PD.Data -> Term s PBool
runTxOrder claimed committed =
  run
    (PD.Constr 2 [claimed])
    (PD.Constr 2 [witnessFor 1 eventId (forcedInclusionTx committed)])
    -- An OptimisticDatum: the event is field 0.
    ( eventRefIn
        txOrderPolicy
        (PD.Constr 0 [PD.Constr 0 [eventId, txOrderPayload], PD.I 0, PD.B "w", addrData, PD.Constr 0 []])
    )
    eventId
    (forcedInclusionTx committed)

-- | A withdrawal claim handed a @DepositMembership@ wrapper.
runMismatched :: forall s. Term s PBool
runMismatched =
  run
    (PD.Constr 1 [verdictOwner])
    (PD.Constr 0 [witnessFor 0 eventId (withdrawalInfoWith verdictOwner)])
    (eventRefIn withdrawalPolicy (PD.Constr 0 [eventId, withdrawalInfoWith verdictValid]))
    eventId
    (withdrawalInfoWith verdictOwner)
