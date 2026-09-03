{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.DaAttestationHandlers
Description : Tests for the dispatch wiring of @validators/da-attestation.ak@.

The three layers beneath these handlers are covered separately. What is tested
here is the wiring: that a burn-side spend binds to the /right/ mint redeemer
and to /this/ input.

@validate_burn_binding@ is the piece worth testing first. Two spend redeemers
carry the same single field and differ only in which mint constructor may
satisfy them, so nothing about their shape stops one being used for the other —
only the @expect_rescue@ flag does. If that selection were dropped, an
attestation could be destroyed under conditions neither branch actually checked:
a @BurnForStateQueue@ satisfied by a rescue authorisation would burn an
attestation without attaching anything to the state queue, and a
@BurnForRescue@ satisfied by an apply would refund one without proving it was
stranded.
-}
module Testing.DaAttestationHandlers (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), singleton)
import PlutusLedgerApi.V3 (
  Address,
  OutputDatum (NoOutputDatum),
  Redeemer (..),
  ScriptHash (..),
  ScriptPurpose (Minting),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
  ToData,
  toBuiltinData,
 )
import PlutusTx.Builtins (builtinDataToData, dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.V3 (PRedeemer, PScriptPurpose, PTxInInfo, PTxOutRef)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Validators.DaAttestation (pvalidateBurnBinding)
import Testing.Eval (passertEval, pfails)
import Testing.ScriptContextBuilder (currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "DA Attestation Handler Tests"
    [ testGroup
        "validateBurnBinding"
        [ testCase "a state-queue burn binds to an apply redeemer" $
            holds $ bind Apply False 0
        , testCase "a rescue burn binds to a rescue redeemer" $
            holds $ bind Rescue True 0
        , -- The cross pair. Nothing about the two spend redeemers' shape keeps
          -- them apart; only the expect_rescue flag does.
          testCase "a state-queue burn rejects a rescue authorisation" $
            pfails $ bind Rescue False 0
        , testCase "a rescue burn rejects an apply authorisation" $
            pfails $ bind Apply True 0
        , -- A burn deferring to Init would authorise destruction with a
          -- creation.
          testCase "rejects a burn deferring to Init" $
            pfails $ bind InitR False 0
        , testCase "rejects a rescue burn deferring to Init" $
            pfails $ bind InitR True 0
        , -- The mint redeemer must name this very input, not merely be of the
          -- right kind: otherwise one authorisation could cover a different
          -- attestation spent in the same transaction.
          testCase "rejects a mint redeemer naming another input" $
            pfails $ bind Apply False 1
        , testCase "rejects a redeemer index that is not the policy's" $
            pfails $ bindAt 1 Apply False 0
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

data Kind = Apply | Rescue | InitR

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

{- | Two attestation inputs are present, so "names this input" is a real
question rather than one there is only one answer to.
-}
bind :: forall s. Kind -> Bool -> Integer -> Term s PBool
bind = bindAt 0

bindAt :: forall s. Integer -> Kind -> Bool -> Integer -> Term s PBool
bindAt redeemerIndex kind expectRescue boundIndex =
  pvalidateBurnBinding
    (inputsT [attestationIn ownRef, attestationIn otherRef])
    (redeemersT [(Minting attestationPolicy, Redeemer (dataToBuiltinData mintRedeemer))])
    (pconstant ownRef)
    (pconstant redeemerIndex)
    (pconstant expectRescue)
  where
    mintRedeemer = case kind of
      -- Init: four fields, tag 0.
      InitR -> PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, PD.I 0]
      -- ApplyToStateQueue: five fields, tag 1; the first is the bound input.
      Apply -> PD.Constr 1 [PD.I boundIndex, PD.I 0, PD.I 0, PD.I 0, PD.I 0]
      -- RescueStrandedAttestation: three fields, tag 2.
      Rescue -> PD.Constr 2 [PD.I boundIndex, PD.I 0, PD.I 0]

attestationIn :: TxOutRef -> TxInInfo
attestationIn ref =
  TxInInfo
    ref
    ( TxOut
        (addressOf attestationPolicy)
        (mkAdaValue 2_000_000 <> singleton attestationPolicy attName 1)
        NoOutputDatum
        Nothing
    )

--------------------------------------------------------------------------------
-- Identities and plumbing
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (concat (replicate 28 (hexByte n)))

hexByte :: Int -> String
hexByte x = [d (x `div` 16), d (x `mod` 16)]
  where
    d i = "0123456789abcdef" !! i

attestationPolicy :: CurrencySymbol
attestationPolicy = policyFor 0x12

addressOf :: CurrencySymbol -> Address
addressOf cs = scriptHashAddress (ScriptHash (unCurrencySymbol cs))

attName :: TokenName
attName = TokenName (toBuiltin ("DAAT" <> BS.replicate 28 0xaa))

ownRef, otherRef :: TxOutRef
ownRef = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101") 0
otherRef = TxOutRef (TxId "0202020202020202020202020202020202020202020202020202020202020202") 0

inputsT :: forall s. [TxInInfo] -> Term s (PBuiltinList (PAsData PTxInInfo))
inputsT xs = punsafeCoerce (pasList # pconstant @PData (PD.List (map toPD xs)))

redeemersT ::
  forall s.
  [(ScriptPurpose, Redeemer)] ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)))
redeemersT entries =
  punsafeCoerce
    ( pasMap
        #$ pconstant @PData
          (PD.Map [(toPD p, builtinDataToData (getRedeemer r)) | (p, r) <- entries])
    )

toPD :: ToData a => a -> PD.Data
toPD = builtinDataToData . toBuiltinData
