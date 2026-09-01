{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofsCommon
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/fraud-proofs/common.ak@.

This module is the scaffolding every fraud-proof family stands on, so its
failure modes are the failure modes of about 160 validators at once. The tests
are grouped by what they defend:

  * __Wire format.__ The carriage types travel in redeemers built by off-chain
    tooling, so their @Constr@ tags and field order are an ABI. Each is compared
    against a 'PD.Data' value written out by hand from @common.ak@.

  * __Thread identity.__ A step transition must carry exactly one thread token
    from input to output and must not change the prover. The negative cases
    walk each of those individually, including the extra-token cases that are
    what actually prevent double satisfaction.

  * __Evidence.__ The counted-root authentication is recomputed here from
    @transition_trace.commit_counted_root@ rather than taken from the port, so
    a change on either side fails a test instead of two copies agreeing. The
    same goes for the compact native-transaction CBOR and its transaction id.

  * __Carriage equivalence.__ The redeemer-carried and published-chunk arms are
    driven through the same fixture and asserted to reach the same @validation@
    call with the same authenticated evidence — which is the property the
    @_with@ split exists to provide.

Rejection mode is asserted, not just failure: Aiken's @expect@ aborts, and a
handful of predicates here deliberately return @False@ instead. 'pfails' and
@passertEval (pnot # ...)@ are used accordingly, so a port that swapped one for
the other would fail these tests even though both "reject".
-}
module Testing.FraudProofsCommon (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, singleton)
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (PubKeyCredential, ScriptCredential),
  Datum (..),
  DatumHash (..),
  OutputDatum (..),
  PubKeyHash (..),
  Redeemer (..),
  ScriptHash (..),
  ScriptPurpose (Minting, Rewarding),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (dataToBuiltinData, fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PPubKeyHash,
  PRedeemer,
  PScriptHash,
  PScriptPurpose,
  PTokenName,
  PTxInInfo,
  PTxOut,
 )
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.ComputationThread (PStepDatum (..))
import Midgard.FraudProof (PFraudProofDatum (..))
import Midgard.FraudProofs.ChunkedInclusion (
  PPublishedProofCarriage (..),
  pdelegatedChunkMembership,
 )
import Midgard.FraudProofs.Common (
  PNativeTxInclusionArgs (..),
  PNativeTxInclusionCarriage (..),
  PNonMembershipCarriage (..),
  PPublishedChunkInclusionArgs (..),
  pcancel,
  pcarriageTransactionsPhasRoot,
  pcontinue,
  pfinalize,
  ppassCommittedTransactionsLeafToNextStep,
  ppassNativeTxToNextStep,
  ppassNativeTxToNextStepCarried,
  pvalidateOutputToFraudProver,
  pverifyCommittedTransactionsLeafInStateQueueNode,
  pverifyNativeTxInStateQueueNode,
  pverifyNativeTxInStateQueueNodeWith,
  pverifyNonMembershipCarried,
 )
import Midgard.FraudProofs.NativeTx.Types (PVerifiedMidgardNativeTxCompact (..))
import Midgard.LedgerState (PHeaderV1 (..))
import Testing.Eval (passertEval, pfails)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Fraud Proof Common Tests"
    [ testGroup "wire format" wireFormatTests
    , testGroup "carriage accessors" carriageTests
    , testGroup "validate_output_to_fraud_prover" payoutTests
    , testGroup "verify_non_membership_carried" nonMembershipTests
    , testGroup "cancel" cancelTests
    , testGroup "continue" continueTests
    , testGroup "finalize" finalizeTests
    , testGroup "verify_committed_transactions_leaf_in_state_queue_node" leafTests
    , testGroup "verify_native_tx_in_state_queue_node" nativeTxTests
    , testGroup "pass_native_tx_to_next_step" passTests
    ]

--------------------------------------------------------------------------------
-- Wire format
--------------------------------------------------------------------------------

{- | The carriage types are built off-chain and read on-chain, so their encoding
is an interface. Each expectation below is written out from @common.ak@'s
declaration order rather than read back from the port.
-}
wireFormatTests :: [TestTree]
wireFormatTests =
  [ testCase "NativeTxInclusionArgs is Constr 0 with nine fields in declaration order" $
      passertEval $
        pencodes (inclusionArgsT defaultArgs) (inclusionArgsData defaultArgs)
  , testCase "PublishedChunkInclusionArgs is Constr 0 with eight fields" $
      passertEval $
        pencodes (publishedArgsT defaultPublished) (publishedArgsData defaultPublished)
  , testCase "RedeemerCarriedInclusion is Constr 0 wrapping the args" $
      passertEval $
        pencodes
          (pcon (PRedeemerCarriedInclusion (pdata (inclusionArgsT defaultArgs))))
          (PD.Constr 0 [inclusionArgsData defaultArgs])
  , testCase "PublishedChunkInclusion is Constr 1 wrapping the args" $
      passertEval $
        pencodes
          (pcon (PPublishedChunkInclusion (pdata (publishedArgsT defaultPublished))))
          (PD.Constr 1 [publishedArgsData defaultPublished])
  , testCase "RedeemerCarriedNonMembership is Constr 0 with proof then index" $
      passertEval $
        pencodes
          ( pcon
              ( PRedeemerCarriedNonMembership
                  { pnonMembership'proof = punsafeCoerce (pconstant @PData emptyProof)
                  , pnonMembership'scriptRedeemerIndex = pdata (pconstant @PInteger 7)
                  }
              )
          )
          (PD.Constr 0 [emptyProof, PD.I 7])
  , testCase "PublishedChunkNonMembership is Constr 1 wrapping the chunk carriage" $
      passertEval $
        pencodes
          ( pcon
              ( PPublishedChunkNonMembership
                  {ppublishedNonMembership'carriage = pdata (chunkCarriageT [3, 4])}
              )
          )
          (PD.Constr 1 [PD.Constr 0 [PD.List [PD.I 3, PD.I 4]]])
  , -- Aiken records are Constr 0. `finalize` rebuilds this datum and compares
    -- it byte-for-byte to the produced output's, so a bare-list encoding would
    -- reject every genuine finalisation.
    testCase "fraud_proof.Datum is Constr 0, not a bare list" $
      passertEval $
        pencodes
          (pcon (PFraudProofDatum (pdata (pconstant prover))))
          (PD.Constr 0 [PD.B prover])
  ]

--------------------------------------------------------------------------------
-- Carriage accessors
--------------------------------------------------------------------------------

carriageTests :: [TestTree]
carriageTests =
  [ testCase "carriage_transactions_phas_root reads the redeemer-carried arm" $
      passertEval $
        pcarriageTransactionsPhasRoot
          # pcon (PRedeemerCarriedInclusion (pdata (inclusionArgsT defaultArgs)))
          #== pconstant phasRoot
  , testCase "carriage_transactions_phas_root reads the published-chunk arm" $
      passertEval $
        pcarriageTransactionsPhasRoot
          # pcon (PPublishedChunkInclusion (pdata (publishedArgsT defaultPublished)))
          #== pconstant phasRoot
  , -- The two arms name the same commitment; that is the whole reason a step
    -- may thread the root forward without knowing which arm it came from.
    testCase "both arms agree on the root for the same evidence" $
      passertEval $
        pcarriageTransactionsPhasRoot
          # pcon (PRedeemerCarriedInclusion (pdata (inclusionArgsT defaultArgs)))
          #== ( pcarriageTransactionsPhasRoot
                  # pcon (PPublishedChunkInclusion (pdata (publishedArgsT defaultPublished)))
              )
  ]

--------------------------------------------------------------------------------
-- validate_output_to_fraud_prover
--------------------------------------------------------------------------------

payoutTests :: [TestTree]
payoutTests =
  [ testCase "accepts a key-hash output belonging to the prover" $
      passertEval $
        pvalidateOutputToFraudProver
          # pconstant (payoutTo (PubKeyCredential (PubKeyHash (toBuiltin prover))))
          # pconstant prover
  , testCase "rejects a key-hash output belonging to someone else" $
      passertEval $
        pnot
          #$ pvalidateOutputToFraudProver
          # pconstant (payoutTo (PubKeyCredential (PubKeyHash (toBuiltin otherProver))))
          # pconstant prover
  , -- The Aiken original returns False rather than aborting on a script
    -- address; a reward paid to a script is not a reward paid to the prover.
    testCase "returns False, not an error, for a script output" $
      passertEval $
        pnot
          #$ pvalidateOutputToFraudProver
          # pconstant (payoutTo (ScriptCredential (ScriptHash (toBuiltin stepScript))))
          # pconstant prover
  ]

payoutTo :: Credential -> TxOut
payoutTo cred =
  TxOut (addressOf cred) (adaValue 2_000_000) NoOutputDatum Nothing

--------------------------------------------------------------------------------
-- verify_non_membership_carried
--------------------------------------------------------------------------------

nonMembershipTests :: [TestTree]
nonMembershipTests =
  [ testCase "accepts a redeemer-carried absence attested by pexcludes" $
      passertEval $ runNonMembership redeemerCarried [pexcludesEntry phasRoot absentKey]
  , testCase "aborts when the pexcludes redeemer names another root" $
      pfails $ runNonMembership redeemerCarried [pexcludesEntry otherRoot absentKey]
  , testCase "aborts when the pexcludes redeemer names another key" $
      pfails $ runNonMembership redeemerCarried [pexcludesEntry phasRoot presentKey]
  , testCase "aborts when no pexcludes withdrawal is present" $
      pfails $ runNonMembership redeemerCarried []
  , -- The phas hash is not the pexcludes hash: an absence claim may not be
    -- backed by a membership attestation.
    testCase "aborts when only a phas withdrawal is present" $
      pfails $
        runNonMembership redeemerCarried [phasEntry phasRoot absentKey leafValue emptyProof]
  , testCase "accepts a published-chunk absence attested by the chunked verifier" $
      passertEval $
        runNonMembership publishedNonMembership [chunkEntry nonMembershipClaimData]
  , -- The delegated check compares claims for equality, so a mismatch is False
    -- rather than an abort. That distinction survives into the caller's `and`.
    testCase "returns False when the published claim names another root" $
      passertEval $
        pnot
          #$ runNonMembership
            publishedNonMembership
            [chunkEntry (claimData 1 otherRoot absentKey absentValueHash [5, 6])]
  , testCase "returns False when the published claim is a membership claim" $
      passertEval $
        pnot
          #$ runNonMembership
            publishedNonMembership
            [chunkEntry (claimData 0 phasRoot absentKey absentValueHash [5, 6])]
  , testCase "returns False when the published claim names other chunks" $
      passertEval $
        pnot
          #$ runNonMembership
            publishedNonMembership
            [chunkEntry (claimData 1 phasRoot absentKey absentValueHash [7])]
  ]

redeemerCarried :: forall s. Term s PNonMembershipCarriage
redeemerCarried =
  pcon
    ( PRedeemerCarriedNonMembership
        { pnonMembership'proof = punsafeCoerce (pconstant @PData emptyProof)
        , pnonMembership'scriptRedeemerIndex = pdata (pconstant @PInteger 0)
        }
    )

publishedNonMembership :: forall s. Term s PNonMembershipCarriage
publishedNonMembership =
  pcon (PPublishedChunkNonMembership {ppublishedNonMembership'carriage = pdata (chunkCarriageT [5, 6])})

nonMembershipClaimData :: PD.Data
nonMembershipClaimData = claimData 1 phasRoot absentKey absentValueHash [5, 6]

runNonMembership ::
  forall s.
  (forall s'. Term s' PNonMembershipCarriage) ->
  [(ScriptPurpose, Redeemer)] ->
  Term s PBool
runNonMembership carriage rs =
  pverifyNonMembershipCarried
    carriage
    (pconstant phasRoot)
    (pconstant absentKey)
    (inputsT [])
    (redeemersT rs)

--------------------------------------------------------------------------------
-- cancel
--------------------------------------------------------------------------------

cancelTests :: [TestTree]
cancelTests =
  [ testCase "accepts an authentic cancellation" $
      passertEval $ runCancel defaultCancel
  , -- The burn is the computation thread policy's business; what this checks is
    -- that the policy ran on the cancellation branch for this exact token.
    testCase "aborts when the mint redeemer is Success rather than BurnForCancellation" $
      pfails $ runCancel defaultCancel {cMintRedeemer = PD.Constr 1 [PD.B threadName]}
  , testCase "aborts when the mint redeemer is Init" $
      pfails $ runCancel defaultCancel {cMintRedeemer = PD.Constr 0 [PD.I 0]}
  , testCase "aborts when the burned asset name is another thread's" $
      pfails $ runCancel defaultCancel {cMintRedeemer = PD.Constr 2 [PD.B otherThreadName]}
  , testCase "aborts when the mint redeemer belongs to another policy" $
      pfails $ runCancel defaultCancel {cMintPurpose = Minting otherPolicy}
  , testCase "aborts when the spent input is not the one being validated" $
      pfails $ runCancel defaultCancel {cInputRef = outRefN 9}
  , testCase "aborts when the input carries a token of another policy" $
      pfails $
        runCancel
          defaultCancel {cInputValue = adaValue 2_000_000 <> singleton otherPolicy (TokenName (toBuiltin threadName)) 1}
  , -- One extra token and `get_single_asset_from_value_apart_from_ada` no
    -- longer has a single answer.
    testCase "aborts when the input carries an extra token" $
      pfails $
        runCancel
          defaultCancel {cInputValue = threadInputValue <> singleton otherPolicy (TokenName "x") 1}
  , testCase "aborts when the input carries two of the thread token" $
      pfails $
        runCancel
          defaultCancel
            {cInputValue = adaValue 2_000_000 <> singleton ctPolicy (TokenName (toBuiltin threadName)) 2}
  , testCase "aborts when the fraud prover did not sign" $
      pfails $ runCancel defaultCancel {cSigners = [otherProver]}
  , testCase "aborts when nobody signed" $
      pfails $ runCancel defaultCancel {cSigners = []}
  , -- Another signature alongside the prover's is not a problem; the prover's
    -- absence is.
    testCase "accepts an additional unrelated signature" $
      passertEval $ runCancel defaultCancel {cSigners = [otherProver, prover]}
  , testCase "aborts when the step datum is absent" $
      pfails $ runCancel defaultCancel {cDatum = Nothing}
  ]

data Cancel = Cancel
  { cMintRedeemer :: PD.Data
  , cMintPurpose :: ScriptPurpose
  , cInputRef :: TxOutRef
  , cInputValue :: Value
  , cDatum :: Maybe (BS.ByteString, Maybe PD.Data)
  , cSigners :: [BS.ByteString]
  }

defaultCancel :: Cancel
defaultCancel =
  Cancel
    { cMintRedeemer = PD.Constr 2 [PD.B threadName]
    , cMintPurpose = Minting ctPolicy
    , cInputRef = ownRef
    , cInputValue = threadInputValue
    , cDatum = Just (prover, Nothing)
    , cSigners = [prover]
    }

runCancel :: forall s. Cancel -> Term s PBool
runCancel c =
  pcancel
    (pdata (pconstant ctPolicy))
    (mStepDatumT (cDatum c))
    0
    (pconstant ownRef)
    0
    (inputsT [stepInput (cInputRef c) (cInputValue c) NoOutputDatum])
    (redeemersT [(cMintPurpose c, Redeemer (dataToBuiltinData (cMintRedeemer c)))])
    (signersT (cSigners c))

--------------------------------------------------------------------------------
-- continue
--------------------------------------------------------------------------------

continueTests :: [TestTree]
continueTests =
  [ testCase "accepts a well-formed step transition" $
      passertEval $ runContinue defaultStep (\_ _ _ _ _ _ -> pconstant True)
  , -- The six values handed to the family are the whole interface; a
    -- transposition here would misroute every family at once.
    testCase "hands the family the input script, token name, prover and both states" $
      passertEval $
        runContinue defaultStep $
          \inScript assetName fraudProver mInputState outScript outState ->
            pand'List
              [ inScript #== pdata (pconstant (ScriptHash (toBuiltin stepScript)))
              , assetName #== pdata (pconstant (TokenName (toBuiltin threadName)))
              , fraudProver #== pdata (pconstant (PubKeyHash (toBuiltin prover)))
              , outScript #== pdata (pconstant (ScriptHash (toBuiltin nextScript)))
              , outState #== pconstant @PData outputState
              , pmatch mInputState $ \case
                  PDNothing -> pconstant True
                  PDJust _ -> pconstant False
              ]
  , testCase "passes a Some input state through unchanged" $
      passertEval $
        runContinue defaultStep {sInputState = Just inputState} $
          \_ _ _ mInputState _ _ ->
            pmatch mInputState $ \case
              PDJust d -> pfromData d #== pconstant @PData inputState
              PDNothing -> pconstant False
  , -- `continue` returns the family's verdict directly rather than turning a
    -- False into an abort, so a family may reject without erroring.
    testCase "returns the family's False verdict rather than aborting" $
      passertEval $ pnot #$ runContinue defaultStep (\_ _ _ _ _ _ -> pconstant False)
  , testCase "aborts when the spent input is not the one being validated" $
      pfails $ runContinue defaultStep {sInputRef = outRefN 9} accept
  , testCase "aborts when the input sits at a key-hash address" $
      pfails $ runContinue defaultStep {sInputAtScript = False} accept
  , testCase "aborts when the input carries a token of another policy" $
      pfails $ runContinue defaultStep {sInputValue = otherPolicyValue} accept
  , testCase "aborts when the input carries an extra token" $
      pfails $
        runContinue defaultStep {sInputValue = threadInputValue <> singleton otherPolicy (TokenName "x") 1} accept
  , testCase "aborts when the output sits at a key-hash address" $
      pfails $ runContinue defaultStep {sOutputAtScript = False} accept
  , -- Carrying the same token forward is what prevents double satisfaction:
    -- two threads cannot both be advanced by one output.
    testCase "aborts when the output carries another thread's token" $
      pfails $ runContinue defaultStep {sOutputValue = otherThreadValue} accept
  , testCase "aborts when the output carries a token of another policy" $
      pfails $ runContinue defaultStep {sOutputValue = otherPolicyValue} accept
  , testCase "aborts when the output carries an extra token alongside the thread's" $
      pfails $
        runContinue defaultStep {sOutputValue = threadInputValue <> singleton otherPolicy (TokenName "x") 1} accept
  , testCase "aborts when the output carries two of the thread token" $
      pfails $
        runContinue
          defaultStep
            {sOutputValue = adaValue 2_000_000 <> singleton ctPolicy (TokenName (toBuiltin threadName)) 2}
          accept
  , testCase "aborts when the output datum is a hash rather than inline" $
      pfails $ runContinue defaultStep {sOutputDatumInline = False} accept
  , testCase "aborts when the output carries a reference script" $
      pfails $ runContinue defaultStep {sOutputReferenceScript = True} accept
  , -- The prover is who gets paid; letting it change mid-thread would let a
    -- thread be stolen at any step.
    testCase "aborts when the output datum names another fraud prover" $
      pfails $ runContinue defaultStep {sOutputProver = otherProver} accept
  , testCase "aborts when the output datum carries no state" $
      pfails $ runContinue defaultStep {sOutputState = Nothing} accept
  ]

accept ::
  forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PTokenName) ->
  Term s (PAsData PPubKeyHash) ->
  Term s (PMaybeData PData) ->
  Term s (PAsData PScriptHash) ->
  Term s PData ->
  Term s PBool
accept _ _ _ _ _ _ = pconstant True

data Step = Step
  { sInputRef :: TxOutRef
  , sInputAtScript :: Bool
  , sInputValue :: Value
  , sInputState :: Maybe PD.Data
  , sOutputAtScript :: Bool
  , sOutputValue :: Value
  , sOutputDatumInline :: Bool
  , sOutputReferenceScript :: Bool
  , sOutputProver :: BS.ByteString
  , sOutputState :: Maybe PD.Data
  }

defaultStep :: Step
defaultStep =
  Step
    { sInputRef = ownRef
    , sInputAtScript = True
    , sInputValue = threadInputValue
    , sInputState = Nothing
    , sOutputAtScript = True
    , sOutputValue = threadInputValue
    , sOutputDatumInline = True
    , sOutputReferenceScript = False
    , sOutputProver = prover
    , sOutputState = Just outputState
    }

runContinue ::
  forall s.
  Step ->
  ( Term s (PAsData PScriptHash) ->
    Term s (PAsData PTokenName) ->
    Term s (PAsData PPubKeyHash) ->
    Term s (PMaybeData PData) ->
    Term s (PAsData PScriptHash) ->
    Term s PData ->
    Term s PBool
  ) ->
  Term s PBool
runContinue s k =
  pcontinue
    (pdata (pconstant ctPolicy))
    (stepDatumT prover (sInputState s))
    0
    0
    (pconstant ownRef)
    (inputsT [stepInputOf s])
    (outputsT [stepOutputOf s])
    k

stepInputOf :: Step -> TxInInfo
stepInputOf s =
  TxInInfo
    (sInputRef s)
    ( TxOut
        ( if sInputAtScript s
            then addressOf (ScriptCredential (ScriptHash (toBuiltin stepScript)))
            else addressOf (PubKeyCredential (PubKeyHash (toBuiltin prover)))
        )
        (sInputValue s)
        NoOutputDatum
        Nothing
    )

stepOutputOf :: Step -> TxOut
stepOutputOf s =
  TxOut
    ( if sOutputAtScript s
        then addressOf (ScriptCredential (ScriptHash (toBuiltin nextScript)))
        else addressOf (PubKeyCredential (PubKeyHash (toBuiltin prover)))
    )
    (sOutputValue s)
    ( if sOutputDatumInline s
        then OutputDatum (Datum (dataToBuiltinData datum))
        else OutputDatumHash datumHashPlaceholder
    )
    (if sOutputReferenceScript s then Just (ScriptHash (toBuiltin otherScript)) else Nothing)
  where
    datum =
      PD.Constr
        0
        [ PD.B (sOutputProver s)
        , maybe (PD.Constr 1 []) (\d -> PD.Constr 0 [d]) (sOutputState s)
        ]

--------------------------------------------------------------------------------
-- finalize
--------------------------------------------------------------------------------

finalizeTests :: [TestTree]
finalizeTests =
  [ testCase "accepts a well-formed finalisation" $
      passertEval $ runFinalize defaultFinal (\_ _ _ _ -> pconstant True)
  , testCase "hands the family the input script, token name and prover" $
      passertEval $
        runFinalize defaultFinal $ \inScript assetName fraudProver _ ->
          pand'List
            [ inScript #== pdata (pconstant (ScriptHash (toBuiltin stepScript)))
            , assetName #== pdata (pconstant (TokenName (toBuiltin threadName)))
            , fraudProver #== pdata (pconstant (PubKeyHash (toBuiltin prover)))
            ]
  , testCase "returns the family's False verdict rather than aborting" $
      passertEval $ pnot #$ runFinalize defaultFinal (\_ _ _ _ -> pconstant False)
  , -- Convictions are permanent records parked at an always-fails script.
    testCase "aborts when the conviction is parked at another address" $
      pfails $ runFinalize defaultFinal {fOutputAddress = otherAddress} acceptFinal
  , testCase "aborts when the fraud proof token is of another policy" $
      pfails $ runFinalize defaultFinal {fOutputValue = fraudProofValueOf otherPolicy threadName} acceptFinal
  , -- The conviction must name the very thread it ends.
    testCase "aborts when the fraud proof token names another thread" $
      pfails $ runFinalize defaultFinal {fOutputValue = fraudProofValueOf fpPolicy otherThreadName} acceptFinal
  , testCase "aborts when the conviction carries an extra token" $
      pfails $
        runFinalize
          defaultFinal
            {fOutputValue = fraudProofValueOf fpPolicy threadName <> singleton otherPolicy (TokenName "x") 1}
          acceptFinal
  , testCase "aborts when the conviction datum carries extra state" $
      pfails $
        runFinalize defaultFinal {fOutputDatum = PD.Constr 0 [PD.B prover, PD.I 1]} acceptFinal
  , testCase "aborts when the conviction datum names another prover" $
      pfails $ runFinalize defaultFinal {fOutputDatum = PD.Constr 0 [PD.B otherProver]} acceptFinal
  , -- The regression pin for the datum's encoding: a bare CBOR list is not a
    -- Constr 0, and the comparison is on the encoded bytes.
    testCase "aborts when the conviction datum is a bare list rather than Constr 0" $
      pfails $ runFinalize defaultFinal {fOutputDatum = PD.List [PD.B prover]} acceptFinal
  , testCase "aborts when the conviction datum is under another constructor" $
      pfails $ runFinalize defaultFinal {fOutputDatum = PD.Constr 1 [PD.B prover]} acceptFinal
  , testCase "aborts when the conviction datum is a hash rather than inline" $
      pfails $ runFinalize defaultFinal {fInlineDatum = False} acceptFinal
  , testCase "aborts when the conviction carries a reference script" $
      pfails $ runFinalize defaultFinal {fReferenceScript = True} acceptFinal
  , -- Minting the conviction is what burns the thread token, so the policy must
    -- have run and must have named this thread.
    testCase "aborts when the fraud proof mint redeemer names another thread" $
      pfails $
        runFinalize defaultFinal {fMintRedeemer = PD.Constr 0 [PD.B otherThreadName, PD.I 0]} acceptFinal
  , testCase "aborts when the mint redeemer belongs to another policy" $
      pfails $ runFinalize defaultFinal {fMintPurpose = Minting otherPolicy} acceptFinal
  , testCase "aborts when the input carries an extra token" $
      pfails $
        runFinalize
          defaultFinal {fInputValue = threadInputValue <> singleton otherPolicy (TokenName "x") 1}
          acceptFinal
  , testCase "aborts when the spent input is not the one being validated" $
      pfails $ runFinalize defaultFinal {fInputRef = outRefN 9} acceptFinal
  ]

acceptFinal ::
  forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PTokenName) ->
  Term s (PAsData PPubKeyHash) ->
  Term s (PMaybeData PData) ->
  Term s PBool
acceptFinal _ _ _ _ = pconstant True

data Final = Final
  { fInputRef :: TxOutRef
  , fInputValue :: Value
  , fOutputAddress :: Address
  , fOutputValue :: Value
  , fOutputDatum :: PD.Data
  , fInlineDatum :: Bool
  , fReferenceScript :: Bool
  , fMintRedeemer :: PD.Data
  , fMintPurpose :: ScriptPurpose
  }

defaultFinal :: Final
defaultFinal =
  Final
    { fInputRef = ownRef
    , fInputValue = threadInputValue
    , fOutputAddress = fraudProofAddress
    , fOutputValue = fraudProofValueOf fpPolicy threadName
    , fOutputDatum = PD.Constr 0 [PD.B prover]
    , fInlineDatum = True
    , fReferenceScript = False
    , fMintRedeemer = PD.Constr 0 [PD.B threadName, PD.I 0]
    , fMintPurpose = Minting fpPolicy
    }

runFinalize ::
  forall s.
  Final ->
  ( Term s (PAsData PScriptHash) ->
    Term s (PAsData PTokenName) ->
    Term s (PAsData PPubKeyHash) ->
    Term s (PMaybeData PData) ->
    Term s PBool
  ) ->
  Term s PBool
runFinalize f k =
  pfinalize
    (pdata (pconstant ctPolicy))
    (pdata (pconstant fpPolicy))
    (pdata (pconstant fraudProofAddress))
    (stepDatumT prover (Just inputState))
    0
    0
    0
    (pconstant ownRef)
    (inputsT [stepInput (fInputRef f) (fInputValue f) NoOutputDatum])
    (outputsT [output])
    (redeemersT [(fMintPurpose f, Redeemer (dataToBuiltinData (fMintRedeemer f)))])
    k
  where
    output =
      TxOut
        (fOutputAddress f)
        (fOutputValue f)
        ( if fInlineDatum f
            then OutputDatum (Datum (dataToBuiltinData (fOutputDatum f)))
            else OutputDatumHash datumHashPlaceholder
        )
        (if fReferenceScript f then Just (ScriptHash (toBuiltin otherScript)) else Nothing)

fraudProofValueOf :: CurrencySymbol -> BS.ByteString -> Value
fraudProofValueOf policy name =
  adaValue 2_000_000 <> singleton policy (TokenName (toBuiltin name)) 1

--------------------------------------------------------------------------------
-- verify_committed_transactions_leaf_in_state_queue_node
--------------------------------------------------------------------------------

{- | The codec-free evidence primitive. It is the cleanest place to test the
authentication chain, because the leaf is opaque: every failure below is a
failure of hub identity, node identity, counted-root authentication or the
opening itself, and never of the transaction codec.
-}
leafTests :: [TestTree]
leafTests =
  [ testCase "accepts an authenticated committed leaf" $
      passertEval $ runLeaf defaultEvidence
  , testCase "hands back the challenged header" $
      passertEval $
        runLeafWith defaultEvidence $ \header ->
          pmatch (pfromData header) $ \PHeaderV1 {pheader'l2TransactionCount} ->
            pfromData pheader'l2TransactionCount #== pconstant l2Count
  , -- The hub oracle is where the state queue policy comes from; a UTxO under
    -- another policy is not the hub.
    testCase "aborts when the hub oracle reference is substituted" $
      pfails $ runLeaf defaultEvidence {eHubRefPolicy = otherPolicy}
  , testCase "aborts when the hub names another state queue policy" $
      pfails $ runLeaf defaultEvidence {eHubStateQueue = otherPolicy}
  , testCase "aborts when the queue node is under another policy" $
      pfails $ runLeaf defaultEvidence {eNodePolicy = otherPolicy}
  , -- A thread opened against block A may not be advanced with evidence from
    -- block B, which is exactly this check.
    testCase "aborts when the queue node is a different block from the thread's" $
      pfails $ runLeaf defaultEvidence {eNodeHeaderHash = otherHeaderHash}
  , testCase "aborts when the thread token names a different block" $
      pfails $ runLeaf defaultEvidence {eThreadName = otherThreadName}
  , -- Only the genuine raw root re-commits to the header's counted value under
    -- this block's transaction count.
    testCase "aborts when the prover supplies a forged raw root" $
      pfails $ runLeaf defaultEvidence {ePhasRoot = otherRoot}
  , testCase "aborts when the header commits the raw root uncounted" $
      pfails $ runLeaf defaultEvidence {eCommittedRoot = Just phasRoot}
  , testCase "aborts when the header's transaction count is not the committed one" $
      pfails $ runLeaf defaultEvidence {eHeaderCount = Just (l2Count + 1)}
  , testCase "aborts when the phas withdrawal is absent" $
      pfails $ runLeaf defaultEvidence {eWithdrawal = Nothing}
  , testCase "aborts when the phas withdrawal opens another root" $
      pfails $ runLeaf defaultEvidence {eWithdrawal = Just (otherRoot, presentKey, leafValue)}
  , testCase "aborts when the phas withdrawal opens another key" $
      pfails $ runLeaf defaultEvidence {eWithdrawal = Just (phasRoot, absentKey, leafValue)}
  , testCase "aborts when the phas withdrawal opens another value" $
      pfails $ runLeaf defaultEvidence {eWithdrawal = Just (phasRoot, presentKey, otherLeafValue)}
  , -- The whole point of this primitive: the leaf value need not be a
    -- well-formed native transaction, and the key need not be its id.
    testCase "accepts a leaf whose value is not a native transaction" $
      passertEval $
        runLeaf
          defaultEvidence
            { eLeafValue = "not a transaction"
            , eWithdrawal = Just (phasRoot, presentKey, "not a transaction")
            }
  ]

runLeaf :: forall s. Evidence -> Term s PBool
runLeaf e = runLeafWith e (const (pconstant True))

runLeafWith ::
  forall s r.
  Evidence ->
  (Term s (PAsData PHeaderV1) -> Term s r) ->
  Term s r
runLeafWith e k =
  pverifyCommittedTransactionsLeafInStateQueueNode
    (pconstant presentKey)
    (pconstant (eLeafValue e))
    (pconstant (ePhasRoot e))
    (pconstant @PData emptyProof)
    (pdata (pconstant (TokenName (toBuiltin (eThreadName e)))))
    (pdata (pconstant (ScriptHash (unCurrencySymbol (eHubPolicy e)))))
    0
    1
    0
    (inputsT (evidenceReferenceInputs e))
    (redeemersT (evidenceRedeemers e))
    k

--------------------------------------------------------------------------------
-- verify_native_tx_in_state_queue_node
--------------------------------------------------------------------------------

nativeTxTests :: [TestTree]
nativeTxTests =
  [ testCase "accepts an authenticated native transaction" $
      passertEval $ runNativeTx nativeEvidence
  , testCase "hands back the decoded compact transaction" $
      passertEval $
        runNativeTxWith nativeEvidence $ \verifiedId _header view ->
          pand'List
            [ verifiedId #== pconstant nativeTxId
            , pmatch view $
                \PVerifiedMidgardNativeTxCompact {pverified'version} -> pverified'version #== 1
            ]
  , -- The codec precondition: the value opened must be a canonical native V1
    -- transaction whose id is the key.
    testCase "aborts when the leaf value is not canonical native CBOR" $
      pfails $
        runNativeTx
          nativeEvidence
            { eLeafValue = "not a transaction"
            , eWithdrawal = Just (phasRoot, nativeTxId, "not a transaction")
            }
  , testCase "aborts when the leaf value hashes to another transaction id" $
      pfails $
        runNativeTx
          nativeEvidence
            { eLeafValue = otherCompactCbor
            , eWithdrawal = Just (phasRoot, nativeTxId, otherCompactCbor)
            }
  , testCase "aborts when trailing bytes follow the compact transaction" $
      pfails $
        runNativeTx
          nativeEvidence
            { eLeafValue = compactCbor <> "\x00"
            , eWithdrawal = Just (phasRoot, nativeTxId, compactCbor <> "\x00")
            }
  , -- The rest of the chain is shared with the codec-free twin, so one case
    -- each is enough to show it is wired the same way.
    testCase "aborts when the hub oracle reference is substituted" $
      pfails $ runNativeTx nativeEvidence {eHubRefPolicy = otherPolicy}
  , testCase "aborts when the queue node is a different block" $
      pfails $ runNativeTx nativeEvidence {eNodeHeaderHash = otherHeaderHash}
  , testCase "aborts when the prover supplies a forged raw root" $
      pfails $ runNativeTx nativeEvidence {ePhasRoot = otherRoot}
  , testCase "aborts when the phas withdrawal is absent" $
      pfails $ runNativeTx nativeEvidence {eWithdrawal = Nothing}
  , -- The published-chunk arm authenticates the identical commitment and only
    -- moves where the proof's bytes travelled.
    testCase "the published-chunk arm accepts the same evidence" $
      passertEval $ runNativeTxChunked nativeEvidence
  , -- Unlike `verify_non_membership_carried`, which hands its verdict back, the
    -- opening here sits under an `expect`: a claim that does not match aborts.
    testCase "the published-chunk arm aborts on a claim for another root" $
      pfails $
        runNativeTxChunkedWithClaim
          nativeEvidence
          (claimData 0 otherRoot nativeTxId (blake2b256 compactCbor) [5, 6])
  , testCase "the published-chunk arm aborts on a non-membership claim" $
      pfails $
        runNativeTxChunkedWithClaim
          nativeEvidence
          (claimData 1 phasRoot nativeTxId (blake2b256 compactCbor) [5, 6])
  , testCase "the published-chunk arm still enforces the codec precondition" $
      pfails $ runNativeTxChunked nativeEvidence {eLeafValue = "not a transaction"}
  , testCase "the published-chunk arm still enforces the counted root" $
      pfails $ runNativeTxChunked nativeEvidence {ePhasRoot = otherRoot}
  ]

runNativeTx :: forall s. Evidence -> Term s PBool
runNativeTx e = runNativeTxWith e (\_ _ _ -> pconstant True)

runNativeTxWith ::
  forall s r.
  Evidence ->
  ( Term s PByteString ->
    Term s (PAsData PHeaderV1) ->
    Term s PVerifiedMidgardNativeTxCompact ->
    Term s r
  ) ->
  Term s r
runNativeTxWith e k =
  pverifyNativeTxInStateQueueNode
    (pconstant nativeTxId)
    (pconstant (eLeafValue e))
    (pconstant (ePhasRoot e))
    (pconstant @PData emptyProof)
    (pdata (pconstant (TokenName (toBuiltin (eThreadName e)))))
    (pdata (pconstant (ScriptHash (unCurrencySymbol (eHubPolicy e)))))
    0
    1
    0
    (inputsT (evidenceReferenceInputs e))
    (redeemersT (evidenceRedeemers e))
    k

runNativeTxChunked :: forall s. Evidence -> Term s PBool
runNativeTxChunked e =
  runNativeTxChunkedWithClaim e (claimData 0 (ePhasRoot e) nativeTxId (blake2b256 (eLeafValue e)) [5, 6])

runNativeTxChunkedWithClaim :: forall s. Evidence -> PD.Data -> Term s PBool
runNativeTxChunkedWithClaim e claim =
  pverifyNativeTxInStateQueueNodeWith
    (pconstant nativeTxId)
    (pconstant (eLeafValue e))
    (pconstant (ePhasRoot e))
    (pdata (pconstant (TokenName (toBuiltin (eThreadName e)))))
    (pdata (pconstant (ScriptHash (unCurrencySymbol (eHubPolicy e)))))
    0
    1
    (inputsT (evidenceReferenceInputs e))
    ( \root key value ->
        pdelegatedChunkMembership
          # pconstant chunkedVerifyHash
          # redeemersT [chunkEntry claim]
          # chunkCarriageT [5, 6]
          # root
          # key
          # value
    )
    (\_ _ _ -> pconstant True)

--------------------------------------------------------------------------------
-- pass_native_tx_to_next_step
--------------------------------------------------------------------------------

{- | The two arms of the seam, driven through one fixture. What is asserted is
that they reach the same @validation@ call with the same evidence: a family
written against one arm therefore behaves identically on the other.
-}
passTests :: [TestTree]
passTests =
  [ testCase "accepts a first step over a redeemer-carried opening" $
      passertEval $ runPass defaultArgs defaultStep alwaysPass
  , testCase "the redeemer-carried arm hands the family the thread and the evidence" $
      passertEval $ runPass defaultArgs defaultStep passAssertions
  , -- `expect validation(...)` aborts in Aiken; a family that merely returns
    -- False must not be silently downgraded to a False here.
    testCase "aborts rather than returning False when the family rejects" $
      pfails $ runPass defaultArgs defaultStep alwaysFail
  , testCase "aborts when the step transition itself is malformed" $
      pfails $ runPass defaultArgs defaultStep {sOutputProver = otherProver} alwaysPass
  , testCase "aborts when the evidence names a forged root" $
      pfails $ runPass defaultArgs {aPhasRoot = otherRoot} defaultStep alwaysPass
  , testCase "aborts when the evidence names another transaction" $
      pfails $ runPass defaultArgs {aTxId = otherNativeTxId} defaultStep alwaysPass
  , -- Both arms of the carriage reach the same validation call with the same
    -- evidence, which is the property the `_with` split exists to provide.
    testCase "the carried dispatcher routes a redeemer arm to the direct helper" $
      passertEval $ runPassCarried redeemerCarriage alwaysPass
  , testCase "accepts a first step over a published-chunk opening" $
      passertEval $ runPassCarried publishedCarriage alwaysPass
  , testCase "the published-chunk arm hands the family the very same values" $
      passertEval $ runPassCarried publishedCarriage passAssertions
  , testCase "aborts when the family rejects on the published-chunk arm" $
      pfails $ runPassCarried publishedCarriage alwaysFail
  , -- The codec-free twin shares the transition and the authentication and
    -- differs only in what reaches the family.
    testCase "the committed-leaf twin hands the family the raw key and value" $
      passertEval $
        runPassLeaf leafValue $ \_ _ _ _ _ _ _ key value ->
          pand'List [key #== pconstant presentKey, value #== pconstant leafValue]
  , testCase "the committed-leaf twin accepts a value that is not a transaction" $
      passertEval $ runPassLeaf "not a transaction" (\_ _ _ _ _ _ _ _ _ -> pconstant True)
  , testCase "the committed-leaf twin still aborts on a forged root" $
      pfails $ runPassLeafWith (leafArgs leafValue) {aPhasRoot = otherRoot} leafValue alwaysLeafPass
  ]

alwaysPass :: forall s. NativeValidation s
alwaysPass _ _ _ _ _ _ _ _ _ = pconstant True

alwaysFail :: forall s. NativeValidation s
alwaysFail _ _ _ _ _ _ _ _ _ = pconstant False

alwaysLeafPass :: forall s. LeafValidation s
alwaysLeafPass _ _ _ _ _ _ _ _ _ = pconstant True

type NativeValidation (s :: S) =
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PTokenName) ->
  Term s (PAsData PPubKeyHash) ->
  Term s (PMaybeData PData) ->
  Term s (PAsData PScriptHash) ->
  Term s PData ->
  Term s (PAsData PHeaderV1) ->
  Term s PByteString ->
  Term s PVerifiedMidgardNativeTxCompact ->
  Term s PBool

type LeafValidation (s :: S) =
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PTokenName) ->
  Term s (PAsData PPubKeyHash) ->
  Term s (PMaybeData PData) ->
  Term s (PAsData PScriptHash) ->
  Term s PData ->
  Term s (PAsData PHeaderV1) ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PBool

{- | Everything the family is handed, checked at once. Driven through both
carriage arms, so a divergence between them fails here.
-}
passAssertions :: forall s. NativeValidation s
passAssertions inScript assetName fraudProver mInputState outScript outState header txId view =
  pand'List
    [ inScript #== pdata (pconstant (ScriptHash (toBuiltin stepScript)))
    , assetName #== pdata (pconstant (TokenName (toBuiltin threadName)))
    , fraudProver #== pdata (pconstant (PubKeyHash (toBuiltin prover)))
    , outScript #== pdata (pconstant (ScriptHash (toBuiltin nextScript)))
    , outState #== pconstant @PData outputState
    , txId #== pconstant nativeTxId
    , pmatch mInputState $ \case
        PDNothing -> pconstant True
        PDJust _ -> pconstant False
    , pmatch (pfromData header) $
        \PHeaderV1 {pheader'l2TransactionCount} ->
          pfromData pheader'l2TransactionCount #== pconstant l2Count
    , pmatch view $
        \PVerifiedMidgardNativeTxCompact {pverified'txId} -> pverified'txId #== pconstant nativeTxId
    ]

{- | Both the direct @phas@ withdrawal and the chunked verifier's claim are
present in every pass fixture. They live under different script hashes, so
including both shows that neither arm is disturbed by the other's redeemer.
-}
passRedeemers :: [(ScriptPurpose, Redeemer)]
passRedeemers =
  [ phasEntry phasRoot nativeTxId compactCbor emptyProof
  , chunkEntry (claimData 0 phasRoot nativeTxId (blake2b256 compactCbor) [5, 6])
  ]

runPass :: forall s. Args -> Step -> NativeValidation s -> Term s PBool
runPass a s v =
  ppassNativeTxToNextStep
    (pdata (pconstant ctPolicy))
    (pdata (pconstant (ScriptHash (unCurrencySymbol hubPolicy))))
    (mStepDatumT (Just (prover, sInputState s)))
    (inclusionArgsT a)
    (pconstant ownRef)
    (inputsT [stepInputOf s])
    (inputsT (evidenceReferenceInputs nativeEvidence))
    (outputsT [stepOutputOf s])
    (redeemersT passRedeemers)
    v

runPassCarried ::
  forall s.
  (forall s'. Term s' PNativeTxInclusionCarriage) ->
  NativeValidation s ->
  Term s PBool
runPassCarried carriage v =
  ppassNativeTxToNextStepCarried
    (pdata (pconstant ctPolicy))
    (pdata (pconstant (ScriptHash (unCurrencySymbol hubPolicy))))
    (mStepDatumT (Just (prover, sInputState defaultStep)))
    carriage
    (pconstant ownRef)
    (inputsT [stepInputOf defaultStep])
    (inputsT (evidenceReferenceInputs nativeEvidence))
    (outputsT [stepOutputOf defaultStep])
    (redeemersT passRedeemers)
    v

redeemerCarriage, publishedCarriage :: forall s. Term s PNativeTxInclusionCarriage
redeemerCarriage = pcon (PRedeemerCarriedInclusion (pdata (inclusionArgsT defaultArgs)))
publishedCarriage = pcon (PPublishedChunkInclusion (pdata (publishedArgsT defaultPublished)))

runPassLeaf :: forall s. BS.ByteString -> LeafValidation s -> Term s PBool
runPassLeaf value = runPassLeafWith (leafArgs value) value

runPassLeafWith :: forall s. Args -> BS.ByteString -> LeafValidation s -> Term s PBool
runPassLeafWith a value v =
  ppassCommittedTransactionsLeafToNextStep
    (pdata (pconstant ctPolicy))
    (pdata (pconstant (ScriptHash (unCurrencySymbol hubPolicy))))
    (mStepDatumT (Just (prover, Nothing)))
    (inclusionArgsT a)
    (pconstant ownRef)
    (inputsT [stepInputOf defaultStep])
    (inputsT (evidenceReferenceInputs defaultEvidence))
    (outputsT [stepOutputOf defaultStep])
    (redeemersT [phasEntry phasRoot presentKey value emptyProof])
    v

leafArgs :: BS.ByteString -> Args
leafArgs value = Args presentKey value phasRoot

--------------------------------------------------------------------------------
-- Identities
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = CurrencySymbol (toBuiltin (BS.replicate 28 (fromIntegral n)))

ctPolicy, fpPolicy, hubPolicy, stateQueuePolicy, otherPolicy :: CurrencySymbol
ctPolicy = policyFor 0x11
fpPolicy = policyFor 0x12
hubPolicy = policyFor 0x13
stateQueuePolicy = policyFor 0x14
otherPolicy = policyFor 0x15

stepScript, nextScript, otherScript, fpSpendScript :: BS.ByteString
stepScript = BS.replicate 28 0x21
nextScript = BS.replicate 28 0x22
otherScript = BS.replicate 28 0x23
fpSpendScript = BS.replicate 28 0x24

prover, otherProver :: BS.ByteString
prover = BS.replicate 28 0x31
otherProver = BS.replicate 28 0x32

categoryId :: BS.ByteString
categoryId = BS.pack [0x00, 0x00, 0x00, 0x07]

headerHash, otherHeaderHash :: BS.ByteString
headerHash = BS.replicate 28 0xaa
otherHeaderHash = BS.replicate 28 0xbb

threadName, otherThreadName :: BS.ByteString
threadName = categoryId <> headerHash
otherThreadName = categoryId <> otherHeaderHash

phasRoot, otherRoot :: BS.ByteString
phasRoot = BS.replicate 32 0x51
otherRoot = BS.replicate 32 0x52

presentKey, absentKey :: BS.ByteString
presentKey = BS.replicate 32 0x61
absentKey = BS.replicate 32 0x62

leafValue, otherLeafValue :: BS.ByteString
leafValue = "committed leaf"
otherLeafValue = "another leaf"

l2Count :: Integer
l2Count = 17

fraudProofAddress, otherAddress :: Address
fraudProofAddress = scriptHashAddress (ScriptHash (toBuiltin fpSpendScript))
otherAddress = scriptHashAddress (ScriptHash (toBuiltin otherScript))

-- | @env.plutarch_phas_validator_hash@, copied independently from @env/default.ak@.
phasHash :: BS.ByteString
phasHash = unhexed "1fc59ff54da02f2535d64b40b647a8826c8b3d914d7ba5257f5b2721"

-- | @env.plutarch_pexcludes_validator_hash@.
pexcludesHash :: BS.ByteString
pexcludesHash = unhexed "a9ec251d6476217b1abccd5f035dec1272a4b04f640f503fca9e734d"

-- | @env.mpf_chunked_verify_validator_hash@.
chunkedVerifyHash :: BS.ByteString
chunkedVerifyHash = unhexed "cb5a7ec4def35ce3ec75c40919992e1b4e8839b4f6b6a2d3b06e7469"

unhexed :: String -> BS.ByteString
unhexed = BS.pack . go
  where
    go (a : b : rest) = fromIntegral (digit a * 16 + digit b) : go rest
    go [] = []
    go _ = error "unhexed: odd length"
    digit c = maybe (error "unhexed: bad digit") id (lookup c (zip "0123456789abcdef" [0 ..]))

-- | @env.empty_merkle_tree_root@.
emptyMerkleTreeRoot :: BS.ByteString
emptyMerkleTreeRoot =
  unhexed "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

absentValueHash :: BS.ByteString
absentValueHash = BS.replicate 32 0x00

emptyProof :: PD.Data
emptyProof = PD.List []

inputState, outputState :: PD.Data
inputState = PD.I 1
outputState = PD.I 2

ownRef :: TxOutRef
ownRef = outRefN 0

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId (toBuiltin (BS.replicate 32 0x01)))

-- | A well-formed datum hash, so the non-inline cases fail on the datum's
-- /form/ rather than on a malformed fixture.
datumHashPlaceholder :: DatumHash
datumHashPlaceholder = DatumHash (toBuiltin (BS.replicate 32 0x77))

--------------------------------------------------------------------------------
-- Values and addresses
--------------------------------------------------------------------------------

adaValue :: Integer -> Value
adaValue = singleton (CurrencySymbol "") (TokenName "")

threadInputValue, otherThreadValue, otherPolicyValue :: Value
threadInputValue = adaValue 2_000_000 <> singleton ctPolicy (TokenName (toBuiltin threadName)) 1
otherThreadValue = adaValue 2_000_000 <> singleton ctPolicy (TokenName (toBuiltin otherThreadName)) 1
otherPolicyValue = adaValue 2_000_000 <> singleton otherPolicy (TokenName (toBuiltin threadName)) 1

addressOf :: Credential -> Address
addressOf cred = Address cred Nothing

stepInput :: TxOutRef -> Value -> OutputDatum -> TxInInfo
stepInput ref value datum =
  TxInInfo
    ref
    (TxOut (addressOf (ScriptCredential (ScriptHash (toBuiltin stepScript)))) value datum Nothing)

--------------------------------------------------------------------------------
-- Reference CBOR, recomputed from the format
--------------------------------------------------------------------------------

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

serialise :: PD.Data -> BS.ByteString
serialise = fromBuiltin . Builtins.serialiseData . dataToBuiltinData

{- | @transition_trace.commit_counted_root@, rebuilt from @transition-trace.ak@.

Written here rather than reused from the port so that a change on either side
fails a test instead of two copies agreeing.
-}
commitCountedRoot :: Integer -> BS.ByteString -> Integer -> BS.ByteString
commitCountedRoot domainTag root count
  | count == 0 && root == emptyMerkleTreeRoot = emptyMerkleTreeRoot
  | otherwise =
      blake2b256
        ( "MidgardRootCountV1"
            <> serialise (PD.Constr domainTag [])
            <> root
            <> serialise (PD.I count)
        )

-- | @TransactionsV1RootDomain@ is the third constructor of @RootDomain@.
transactionsDomain :: Integer
transactionsDomain = 2

-- | Minimal CBOR for an integer, over the widths these fixtures use.
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

{- | A canonical compact native V1 transaction body: an array of twelve, three
hashes, three numbers, five more hashes and the network id.
-}
compactBody :: Integer -> BS.ByteString
compactBody fee =
  BS.concat
    [ "\x8c"
    , defBytes32 (hash32 0x01)
    , defBytes32 (hash32 0x02)
    , defBytes32 (hash32 0x03)
    , cborInt fee
    , cborInt (-5)
    , cborInt 65536
    , defBytes32 (hash32 0x04)
    , defBytes32 (hash32 0x05)
    , defBytes32 (hash32 0x06)
    , defBytes32 (hash32 0x07)
    , defBytes32 (hash32 0x08)
    , cborInt 1
    ]

compactOf :: Integer -> BS.ByteString
compactOf fee =
  BS.concat ["\x84", cborInt 1, compactBody fee, defBytes32 (hash32 0x09), cborInt 3]

txIdOf :: Integer -> BS.ByteString
txIdOf fee = blake2b256 ("MidgardNativeTxBodyV1" <> cborInt 1 <> compactBody fee)

compactCbor, otherCompactCbor :: BS.ByteString
compactCbor = compactOf 1000000
otherCompactCbor = compactOf 999999

nativeTxId, otherNativeTxId :: BS.ByteString
nativeTxId = txIdOf 1000000
otherNativeTxId = txIdOf 999999

--------------------------------------------------------------------------------
-- Redeemer entries
--------------------------------------------------------------------------------

phasEntry ::
  BS.ByteString -> BS.ByteString -> BS.ByteString -> PD.Data -> (ScriptPurpose, Redeemer)
phasEntry root key value proof =
  ( Rewarding (ScriptCredential (ScriptHash (toBuiltin phasHash)))
  , Redeemer (dataToBuiltinData (PD.List [PD.B root, PD.B key, PD.B value, proof]))
  )

pexcludesEntry :: BS.ByteString -> BS.ByteString -> (ScriptPurpose, Redeemer)
pexcludesEntry root key =
  ( Rewarding (ScriptCredential (ScriptHash (toBuiltin pexcludesHash)))
  , Redeemer (dataToBuiltinData (PD.List [PD.B root, PD.B key, emptyProof]))
  )

chunkEntry :: PD.Data -> (ScriptPurpose, Redeemer)
chunkEntry claim =
  ( Rewarding (ScriptCredential (ScriptHash (toBuiltin chunkedVerifyHash)))
  , Redeemer (dataToBuiltinData claim)
  )

-- | @chunked_inclusion_v1.ChunkedProofClaim@, written out from its declaration.
claimData :: Integer -> BS.ByteString -> BS.ByteString -> BS.ByteString -> [Integer] -> PD.Data
claimData mode root key valueHash chunks =
  PD.Constr
    0
    [ PD.Constr mode []
    , PD.B root
    , PD.B key
    , PD.B valueHash
    , PD.List (map PD.I chunks)
    ]

--------------------------------------------------------------------------------
-- Evidence fixtures
--------------------------------------------------------------------------------

data Evidence = Evidence
  { eHubPolicy :: CurrencySymbol
  -- ^ The hub the /step/ was parameterised with — the identity it demands.
  , eHubRefPolicy :: CurrencySymbol
  -- ^ The hub the reference input actually carries. Separate from 'eHubPolicy'
  -- so that a substituted oracle is a real substitution rather than a
  -- self-consistent relabelling of both sides at once.
  , eHubStateQueue :: CurrencySymbol
  , eNodePolicy :: CurrencySymbol
  , eNodeHeaderHash :: BS.ByteString
  , eThreadName :: BS.ByteString
  , ePhasRoot :: BS.ByteString
  , eCommittedRoot :: Maybe BS.ByteString
  , eHeaderCount :: Maybe Integer
  , eLeafValue :: BS.ByteString
  , eWithdrawal :: Maybe (BS.ByteString, BS.ByteString, BS.ByteString)
  }

defaultEvidence :: Evidence
defaultEvidence =
  Evidence
    { eHubPolicy = hubPolicy
    , eHubRefPolicy = hubPolicy
    , eHubStateQueue = stateQueuePolicy
    , eNodePolicy = stateQueuePolicy
    , eNodeHeaderHash = headerHash
    , eThreadName = threadName
    , ePhasRoot = phasRoot
    , eCommittedRoot = Nothing
    , eHeaderCount = Nothing
    , eLeafValue = leafValue
    , eWithdrawal = Just (phasRoot, presentKey, leafValue)
    }

{- | The same block and the same authenticated root, opened at a leaf that /is/
a canonical native transaction. Only the leaf changes between this and
'defaultEvidence', which is what makes the codec precondition the only
difference the two exercise.
-}
nativeEvidence :: Evidence
nativeEvidence =
  defaultEvidence
    { eLeafValue = compactCbor
    , eWithdrawal = Just (phasRoot, nativeTxId, compactCbor)
    }

evidenceRedeemers :: Evidence -> [(ScriptPurpose, Redeemer)]
evidenceRedeemers e =
  case eWithdrawal e of
    Nothing -> []
    Just (root, key, value) -> [phasEntry root key value emptyProof]

evidenceReferenceInputs :: Evidence -> [TxInInfo]
evidenceReferenceInputs e = [hubRefIn e, nodeRefIn e]

hubRefIn :: Evidence -> TxInInfo
hubRefIn e =
  TxInInfo
    (outRefN 1)
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol (eHubRefPolicy e))))
        ( adaValue 2_000_000
            <> singleton (eHubRefPolicy e) (TokenName (toBuiltin ("MIDGARD_HUB_ORACLE" :: BS.ByteString))) 1
        )
        (OutputDatum (Datum (dataToBuiltinData (hubDatum (eHubStateQueue e)))))
        Nothing
    )

nodeRefIn :: Evidence -> TxInInfo
nodeRefIn e =
  TxInInfo
    (outRefN 2)
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol (eNodePolicy e))))
        ( adaValue 2_000_000
            <> singleton (eNodePolicy e) (TokenName (toBuiltin ("MBLC" <> eNodeHeaderHash e))) 1
        )
        (OutputDatum (Datum (dataToBuiltinData element)))
        Nothing
    )
  where
    element = PD.Constr 0 [PD.Constr 1 [node], PD.Constr 1 []]
    node = PD.Constr 0 [headerDataOf e, PD.B ""]

headerDataOf :: Evidence -> PD.Data
headerDataOf e =
  PD.Constr
    0
    ( [ PD.B (hash32 0x01)
      , PD.B (hash32 0x02)
      , PD.B (hash32 0x03)
      , PD.B (hash32 0x04)
      , PD.B committedRoot
      , PD.B (hash32 0x06)
      , PD.B (hash32 0x07)
      , PD.B (hash32 0x08)
      , PD.B (hash32 0x09)
      , PD.I 0
      , PD.I 0
      , PD.I count
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
    )
  where
    count = maybe l2Count id (eHeaderCount e)
    committedRoot =
      maybe (commitCountedRoot transactionsDomain phasRoot l2Count) id (eCommittedRoot e)

hubDatum :: CurrencySymbol -> PD.Data
hubDatum stateQueue =
  PD.Constr
    0
    ( [ PD.B (cs (policyFor 0x41))
      , PD.B (cs (policyFor 0x42))
      , PD.B (cs (policyFor 0x43))
      , PD.B (cs (policyFor 0x44))
      , PD.B (cs stateQueue)
      ]
        <> [PD.B (cs (policyFor (0x45 + i))) | i <- [0 .. 6]]
        <> replicate 13 addressData
        <> [PD.B (cs (policyFor 0x4f))]
    )
  where
    cs = fromBuiltin . unCurrencySymbol
    addressData = PD.Constr 0 [PD.Constr 1 [PD.B (cs (policyFor 0x42))], PD.Constr 1 []]

--------------------------------------------------------------------------------
-- Argument fixtures
--------------------------------------------------------------------------------

data Args = Args
  { aTxId :: BS.ByteString
  , aCbor :: BS.ByteString
  , aPhasRoot :: BS.ByteString
  }

defaultArgs :: Args
defaultArgs = Args nativeTxId compactCbor phasRoot

inclusionArgsData :: Args -> PD.Data
inclusionArgsData a =
  PD.Constr
    0
    [ PD.I 0
    , PD.I 0
    , PD.I 0
    , PD.I 1
    , PD.B (aTxId a)
    , PD.B (aCbor a)
    , PD.B (aPhasRoot a)
    , emptyProof
    , PD.I 0
    ]

inclusionArgsT :: forall s. Args -> Term s PNativeTxInclusionArgs
inclusionArgsT a =
  pcon $
    PNativeTxInclusionArgs
      { pinclusionArgs'inputIndex = pdata (pconstant @PInteger 0)
      , pinclusionArgs'outputIndex = pdata (pconstant @PInteger 0)
      , pinclusionArgs'hubRefInputIndex = pdata (pconstant @PInteger 0)
      , pinclusionArgs'stateQueueNodeRefInputIndex = pdata (pconstant @PInteger 1)
      , pinclusionArgs'nativeTxId = pdata (pconstant (aTxId a))
      , pinclusionArgs'nativeTxCompactCbor = pdata (pconstant (aCbor a))
      , pinclusionArgs'transactionsPhasRoot = pdata (pconstant (aPhasRoot a))
      , pinclusionArgs'txMembershipProof = punsafeCoerce (pconstant @PData emptyProof)
      , pinclusionArgs'inclusionProofScriptWithdrawRedeemerIndex = pdata (pconstant @PInteger 0)
      }

data Published = Published
  { pTxId :: BS.ByteString
  , pCbor :: BS.ByteString
  , pPhasRoot :: BS.ByteString
  , pChunks :: [Integer]
  }

defaultPublished :: Published
defaultPublished = Published nativeTxId compactCbor phasRoot [5, 6]

publishedArgsData :: Published -> PD.Data
publishedArgsData p =
  PD.Constr
    0
    [ PD.I 0
    , PD.I 0
    , PD.I 0
    , PD.I 1
    , PD.B (pTxId p)
    , PD.B (pCbor p)
    , PD.B (pPhasRoot p)
    , PD.List (map PD.I (pChunks p))
    ]

publishedArgsT :: forall s. Published -> Term s PPublishedChunkInclusionArgs
publishedArgsT p =
  pcon $
    PPublishedChunkInclusionArgs
      { ppublishedArgs'inputIndex = pdata (pconstant @PInteger 0)
      , ppublishedArgs'outputIndex = pdata (pconstant @PInteger 0)
      , ppublishedArgs'hubRefInputIndex = pdata (pconstant @PInteger 0)
      , ppublishedArgs'stateQueueNodeRefInputIndex = pdata (pconstant @PInteger 1)
      , ppublishedArgs'nativeTxId = pdata (pconstant (pTxId p))
      , ppublishedArgs'nativeTxCompactCbor = pdata (pconstant (pCbor p))
      , ppublishedArgs'transactionsPhasRoot = pdata (pconstant (pPhasRoot p))
      , ppublishedArgs'orderedChunkReferenceInputIndices =
          pdata (pconstant (pChunks p))
      }

chunkCarriageT :: forall s. [Integer] -> Term s PPublishedProofCarriage
chunkCarriageT chunks =
  pcon
    ( PPublishedProofCarriage
        {pcarriage'orderedChunkReferenceInputIndices = pdata (pconstant chunks)}
    )

--------------------------------------------------------------------------------
-- Plutarch plumbing
--------------------------------------------------------------------------------

-- | Whether a data-encoded value's wire form is exactly the expected 'PD.Data'.
pencodes :: forall a s. (PIsData a) => Term s a -> PD.Data -> Term s PBool
pencodes value expected = pforgetData (pdata value) #== pconstant expected

inputsT :: forall s. [TxInInfo] -> Term s (PBuiltinList (PAsData PTxInInfo))
inputsT = pconstant

outputsT :: forall s. [TxOut] -> Term s (PBuiltinList (PAsData PTxOut))
outputsT = pconstant

signersT :: forall s. [BS.ByteString] -> Term s (PBuiltinList (PAsData PPubKeyHash))
signersT = pconstant . map (PubKeyHash . toBuiltin)

redeemersT ::
  forall s.
  [(ScriptPurpose, Redeemer)] ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)))
redeemersT = pconstant

stepDatumT :: forall s. BS.ByteString -> Maybe PD.Data -> Term s PStepDatum
stepDatumT p md =
  pcon $
    PStepDatum
      { pstep'fraudProver = pdata (pconstant (PubKeyHash (toBuiltin p)))
      , pstep'data = case md of
          Nothing -> pcon PDNothing
          Just d -> pcon (PDJust (punsafeCoerce (pconstant @PData d)))
      }

mStepDatumT ::
  forall s. Maybe (BS.ByteString, Maybe PD.Data) -> Term s (PMaybeData PStepDatum)
mStepDatumT Nothing = pcon PDNothing
mStepDatumT (Just (p, md)) = pcon (PDJust (pdata (stepDatumT p md)))
