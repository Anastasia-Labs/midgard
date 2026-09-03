{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.ComputationThreadValidator
Description : Behavioural tests for the Plutarch port of
              @validators/computation-thread.ak@.

A computation thread is a fraud proof in progress. Opening one is the branch
with substance, and these tests are named after the Aiken @ct_*@ tests and
assert the same conditions, so a divergence between the two implementations
fails the same case on each side. The fixtures are this port's own — Aiken's
come from a 1,086-line native-binding fixture module that is not itself ported.

The through-line of the @Init@ cases: a thread names what is being proved and
which block it is proved against, and neither may be forged, substituted, or
left dangling.
-}
module Testing.ComputationThreadValidator (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, getValue, singleton)
import PlutusLedgerApi.V3 (
  Credential (ScriptCredential),
  Datum (..),
  OutputDatum (..),
  PubKeyHash (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo (MintingScript),
  ScriptPurpose (Rewarding),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
  scriptContextTxInfo,
  txInfoMint,
  txInfoOutputs,
  txInfoRedeemers,
  txInfoReferenceInputs,
  txInfoSignatories,
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins (BuiltinData, dataToBuiltinData, fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.ComputationThread (computationThreadMintValidator)
import Testing.Eval (pfails, psucceeds)
import Testing.ScriptContextBuilder (buildScriptContext, currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Computation Thread Validator Tests"
    [ testGroup
        "mint / Init"
        [ testCase "ct_init_accepts_authentic_catalogue_backed_thread" $
            psucceeds $ runInit defaultInit
        , -- The output must go to the very script that will check the proof.
          testCase "ct_init_rejects_first_step_not_at_proven_category" $
            pfails $ runInit defaultInit {iOutputScript = otherScript}
        , -- A thread starts having computed nothing.
          testCase "ct_init_rejects_preloaded_step_state" $
            pfails $ runInit defaultInit {iStepData = Just (PD.I 1)}
        , -- The reward is payable to the datum's prover, so only that prover
          -- may open the thread.
          testCase "ct_init_rejects_missing_prover_signature" $
            pfails $ runInit defaultInit {iSigners = [otherProver]}
        , -- Two threads in one transaction would let one catalogue proof back
          -- two different fraud claims.
          testCase "ct_init_rejects_parallel_thread_mint_in_one_tx" $
            pfails $
              runInit
                defaultInit
                  { iExtraMint = Just (singleton threadPolicy (TokenName "other") 1)
                  }
        , testCase "ct_init_rejects_second_thread_token_in_one_tx" $
            pfails $
              runInit
                defaultInit
                  { iExtraOutputTokens = Just (singleton threadPolicy (TokenName "other") 1)
                  }
        , -- The catalogue is what makes a fraud category real; a UTxO under
          -- another policy is not the catalogue.
          testCase "ct_init_rejects_substituted_catalogue_reference" $
            pfails $ runInit defaultInit {iCataloguePolicy = otherPolicy}
        , -- The delegated proof must be about *this* category.
          testCase "ct_init_rejects_uncatalogued_fraud_category" $
            pfails $ runInit defaultInit {iAttestedCategory = otherScript}
        , testCase "ct_init_rejects_absent_membership_attestation" $
            pfails $ runInit defaultInit {iWithdrawRedeemer = False}
        , -- The hub oracle is where the state queue's policy id comes from.
          testCase "ct_init_rejects_substituted_hub_oracle" $
            pfails $ runInit defaultInit {iHubPolicy = otherPolicy}
        , -- The block must be a real queue entry, not any UTxO.
          testCase "ct_init_rejects_unregistered_block_reference" $
            pfails $ runInit defaultInit {iBlockPolicy = otherPolicy}
        , -- The token name binds the thread to one category and one block.
          testCase "rejects a token name naming another block" $
            pfails $ runInit defaultInit {iAssetNameHash = otherHeaderHash}
        , testCase "rejects a token name naming another category" $
            pfails $ runInit defaultInit {iAssetNameId = otherCategoryId}
        ]
    , testGroup
        "mint / Success"
        [ testCase "ct_success_accepts_thread_token_burn" $
            psucceeds $ runSuccess (toMint (threadToken (-1)))
        , testCase "ct_success_rejects_missing_thread_token_burn" $
            pfails $ runSuccess (toMint (singleton otherPolicy (TokenName "x") (-1)))
        , testCase "rejects a mint where a burn is required" $
            pfails $ runSuccess (toMint (threadToken 1))
        , -- Unlike cancellation, success permits other tokens alongside: the
          -- fraud-proof token is minted in the same transaction.
          testCase "permits the fraud-proof token minted alongside" $
            psucceeds $
              runSuccess (toMint (threadToken (-1) <> singleton otherPolicy (TokenName "fp") 1))
        ]
    , testGroup
        "mint / BurnForCancellation"
        [ testCase "ct_burn_for_cancellation_accepts_exact_burn" $
            psucceeds $ runCancellation (toMint (threadToken (-1)))
        , -- A cancellation earns nothing, so nothing may ride along with it —
          -- in particular no fraud-proof token.
          testCase "ct_burn_for_cancellation_rejects_extra_mint" $
            pfails $
              runCancellation
                (toMint (threadToken (-1) <> singleton otherPolicy (TokenName "fp") 1))
        , testCase "rejects a mint where a burn is required" $
            pfails $ runCancellation (toMint (threadToken 1))
        , testCase "rejects burning a token of another policy" $
            pfails $ runCancellation (toMint (singleton otherPolicy (TokenName "x") (-1)))
        ]
    ]

--------------------------------------------------------------------------------
-- Init
--------------------------------------------------------------------------------

data Init = Init
  { iOutputScript :: BS.ByteString
  , iAttestedCategory :: BS.ByteString
  , iStepData :: Maybe PD.Data
  , iSigners :: [BS.ByteString]
  , iExtraMint :: Maybe Value
  , iExtraOutputTokens :: Maybe Value
  , iCataloguePolicy :: CurrencySymbol
  , iHubPolicy :: CurrencySymbol
  , iBlockPolicy :: CurrencySymbol
  , iWithdrawRedeemer :: Bool
  , iAssetNameId :: BS.ByteString
  , iAssetNameHash :: BS.ByteString
  }

defaultInit :: Init
defaultInit =
  Init
    { iOutputScript = categoryScript
    , iAttestedCategory = categoryScript
    , iStepData = Nothing
    , iSigners = [prover]
    , iExtraMint = Nothing
    , iExtraOutputTokens = Nothing
    , iCataloguePolicy = cataloguePolicy
    , iHubPolicy = hubPolicy
    , iBlockPolicy = stateQueuePolicy
    , iWithdrawRedeemer = True
    , iAssetNameId = categoryId
    , iAssetNameHash = headerHash
    }

runInit :: forall s. Init -> Term s PUnit
runInit i =
  computationThreadMintValidator
    # pdata (pconstant cataloguePolicy)
    # pdata (pconstant hubPolicy)
    # pconstant ctx
  where
    threadName = TokenName (toBuiltin (iAssetNameId i <> iAssetNameHash i))
    outputValue =
      mkAdaValue 2_000_000
        <> singleton threadPolicy threadName 1
        <> maybe mempty id (iExtraOutputTokens i)
    minted = singleton threadPolicy threadName 1 <> maybe mempty id (iExtraMint i)
    stepDatum =
      PD.Constr
        0
        [ PD.B prover
        , maybe (PD.Constr 1 []) (\d -> PD.Constr 0 [d]) (iStepData i)
        ]
    firstStepOutput =
      TxOut
        (scriptHashAddress (ScriptHash (toBuiltin (iOutputScript i))))
        outputValue
        (OutputDatum (Datum (dataToBuiltinData stepDatum)))
        Nothing
    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoOutputs = [firstStepOutput]
        , txInfoReferenceInputs =
            [ catalogueRefIn (iCataloguePolicy i)
            , hubRefIn (iHubPolicy i)
            , blockRefIn (iBlockPolicy i)
            ]
        , txInfoMint = toMint minted
        , txInfoSignatories = map (PubKeyHash . toBuiltin) (iSigners i)
        , txInfoRedeemers =
            Map.unsafeFromList
              [ (Rewarding (ScriptCredential (ScriptHash (toBuiltin phasHash))), phasRedeemer)
              | iWithdrawRedeemer i
              ]
        }
    -- The `phas` validator's redeemer, restating the four arguments it proved.
    -- The port checks these against its own, so an attestation about another
    -- category cannot back this thread.
    phasRedeemer =
      Redeemer . dataToBuiltinData $
        PD.List
          [ PD.B catalogueRoot
          , PD.B (cborBytes categoryId)
          , PD.B (cborBytes (iAttestedCategory i))
          , PD.List []
          ]
    ctx = ScriptContext txInfo (Redeemer (dataToBuiltinData initRedeemer)) (MintingScript threadPolicy)
    initRedeemer =
      PD.Constr
        0
        [ PD.I 0 -- first_step_output_index
        , PD.B categoryId
        , PD.B (iOutputScript i)
        , PD.List [] -- membership proof
        , PD.I 0 -- catalogue ref input index
        , PD.I 0 -- inclusion proof redeemer index
        , PD.I 1 -- hub oracle ref input index
        , PD.I 2 -- fraudulent block ref input index
        ]

{- | The CBOR encoding of a bytestring, as @cbor.serialise@ produces it.

Recomputed here rather than taken from the term, so that a change to either
side's encoding fails a test instead of two copies agreeing. Byte strings under
24 bytes take a one-byte header; longer ones a length prefix.
-}
cborBytes :: BS.ByteString -> BS.ByteString
cborBytes b
  | n < 24 = BS.cons (0x40 + fromIntegral n) b
  | n < 256 = BS.pack [0x58, fromIntegral n] <> b
  | otherwise = error "cborBytes: fixture bytestrings stay under 256 bytes"
  where
    n = BS.length b

--------------------------------------------------------------------------------
-- Success and BurnForCancellation
--------------------------------------------------------------------------------

runSuccess :: forall s. MintValue -> Term s PUnit
runSuccess = runBurn 1

runCancellation :: forall s. MintValue -> Term s PUnit
runCancellation = runBurn 2

runBurn :: forall s. Integer -> MintValue -> Term s PUnit
runBurn tag minted =
  computationThreadMintValidator
    # pdata (pconstant cataloguePolicy)
    # pdata (pconstant hubPolicy)
    # pconstant ctx
  where
    base = buildScriptContext mempty
    txInfo = (scriptContextTxInfo base) {txInfoMint = minted}
    redeemer =
      PD.Constr (fromIntegral tag) [PD.B (categoryId <> headerHash)]
    ctx =
      ScriptContext
        txInfo
        (Redeemer (dataToBuiltinData redeemer))
        (MintingScript threadPolicy)

threadToken :: Integer -> Value
threadToken = singleton threadPolicy (TokenName (toBuiltin (categoryId <> headerHash)))

--------------------------------------------------------------------------------
-- Identities
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (concat (replicate 28 (hexByte n)))

hexByte :: Int -> String
hexByte x = [d (x `div` 16), d (x `mod` 16)]
  where
    d i = "0123456789abcdef" !! i

threadPolicy, cataloguePolicy, hubPolicy, stateQueuePolicy, otherPolicy :: CurrencySymbol
threadPolicy = policyFor 0x11
cataloguePolicy = policyFor 0x12
hubPolicy = policyFor 0x13
stateQueuePolicy = policyFor 0x14
otherPolicy = policyFor 0x15

categoryScript, otherScript :: BS.ByteString
categoryScript = BS.replicate 28 0x21
otherScript = BS.replicate 28 0x22

categoryId, otherCategoryId :: BS.ByteString
categoryId = BS.pack [0x00, 0x00, 0x00, 0x07]
otherCategoryId = BS.pack [0x00, 0x00, 0x00, 0x08]

headerHash, otherHeaderHash :: BS.ByteString
headerHash = BS.replicate 28 0xaa
otherHeaderHash = BS.replicate 28 0xbb

prover, otherProver :: BS.ByteString
prover = BS.replicate 28 0x31
otherProver = BS.replicate 28 0x32

catalogueRoot :: BS.ByteString
catalogueRoot = BS.replicate 32 0x12

-- | Aiken @env.plutarch_phas_validator_hash@, copied independently.
phasHash :: BS.ByteString
phasHash =
  BS.pack
    [ 0x1f, 0xc5, 0x9f, 0xf5, 0x4d, 0xa0, 0x2f, 0x25
    , 0x35, 0xd6, 0x4b, 0x40, 0xb6, 0x47, 0xa8, 0x82
    , 0x6c, 0x8b, 0x3d, 0x91, 0x4d, 0x7b, 0xa5, 0x25
    , 0x7f, 0x5b, 0x27, 0x21
    ]

toMint :: Value -> MintValue
toMint = UnsafeMintValue . getValue

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101")

--------------------------------------------------------------------------------
-- Reference inputs
--------------------------------------------------------------------------------

catalogueRefIn :: CurrencySymbol -> TxInInfo
catalogueRefIn policy =
  TxInInfo
    (outRefN 0)
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol policy)))
        ( mkAdaValue 2_000_000
            <> singleton policy (TokenName (toBuiltin ("MIDGARD_FRAUD_PROOF_CATALOGUE" :: BS.ByteString))) 1
        )
        (OutputDatum (Datum (dataToBuiltinData (PD.B catalogueRoot))))
        Nothing
    )

hubRefIn :: CurrencySymbol -> TxInInfo
hubRefIn policy =
  TxInInfo
    (outRefN 1)
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol policy)))
        ( mkAdaValue 2_000_000
            <> singleton policy (TokenName (toBuiltin ("MIDGARD_HUB_ORACLE" :: BS.ByteString))) 1
        )
        (OutputDatum (Datum hubDatum))
        Nothing
    )

-- | A state-queue node: @MBLC ++ header_hash@, carrying a @StateQueueNode@.
blockRefIn :: CurrencySymbol -> TxInInfo
blockRefIn policy =
  TxInInfo
    (outRefN 2)
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol policy)))
        (mkAdaValue 2_000_000 <> singleton policy (TokenName (toBuiltin ("MBLC" <> headerHash))) 1)
        (OutputDatum (Datum (dataToBuiltinData element)))
        Nothing
    )
  where
    element = PD.Constr 0 [PD.Constr 1 [node], PD.Constr 1 []]
    node = PD.Constr 0 [headerData, PD.B ""]

headerData :: PD.Data
headerData =
  PD.Constr
    0
    ( replicate 9 (PD.B (BS.replicate 32 0x01))
        <> replicate 7 (PD.I 0)
        <> [PD.I 100, PD.I 200, PD.I 0, PD.I 0, PD.I 0, PD.I 0]
        <> [PD.B (BS.replicate 28 0x02)]
        <> [PD.B prover]
        <> [PD.I 1]
    )

hubDatum :: BuiltinData
hubDatum =
  dataToBuiltinData $
    PD.Constr
      0
      ( [ PD.B (cs (policyFor 0x41)) -- registered_operators
        , PD.B (cs (policyFor 0x42)) -- active_operators
        , PD.B (cs (policyFor 0x43)) -- retired_operators
        , PD.B (cs (policyFor 0x44)) -- scheduler
        , PD.B (cs stateQueuePolicy) -- state_queue
        ]
          <> [PD.B (cs (policyFor (0x45 + i))) | i <- [0 .. 6]]
          <> replicate 13 addressData
          <> [PD.B (cs (policyFor 0x4f))]
      )
  where
    cs = fromBuiltin . unCurrencySymbol
    addressData =
      PD.Constr 0 [PD.Constr 1 [PD.B (cs (policyFor 0x42))], PD.Constr 1 []]
