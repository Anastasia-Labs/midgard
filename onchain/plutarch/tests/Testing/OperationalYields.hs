{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Testing.OperationalYields (tests) where

import Data.ByteString qualified as BS
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore.Data qualified as D
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, singleton)
import PlutusLedgerApi.V3
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins (dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.AvailabilityChallenge (PParametersV1)
import Midgard.StateQueueYield
import Midgard.Validators.AvailabilityChallenge (availabilityChallengeValidator)
import Midgard.Validators.StateQueue (stateQueueMintValidator)
import Testing.Eval (passertEval, pfails, psucceeds)
import Testing.ScriptContextBuilder (buildScriptContext, mkAdaValue)

tests :: TestTree
tests =
  testGroup
    "Operational Yields"
    [ testGroup
        "authenticated zero withdrawal"
        [ testCase "accepts the role NFT, reference script, zero withdrawal and unique redeemer" $
            pass $
              handshake goodTx 0
        , testCase "allows unrelated policies, withdrawals and redeemers" $
            pass $
              handshake
                ( goodTx
                    { txInfoReferenceInputs =
                        [ reference
                            { txInInfoResolved =
                                (txInInfoResolved reference)
                                  { txOutValue = txOutValue (txInInfoResolved reference) <> singleton otherPolicy (TokenName "other") 4
                                  }
                            }
                        ]
                    , txInfoWdrl = Map.unsafeFromList [(credential, 0), (PubKeyCredential $ PubKeyHash "other", 12)]
                    , txInfoRedeemers = Map.unsafeFromList [(Rewarding credential, fieldless), (Minting otherPolicy, fieldless)]
                    }
                )
                0
        , rejects "cross-arm role" $ replaceValue (singleton authPolicy (TokenName "StateQueueMergeYield") 1)
        , rejects "wrong authentication policy" $ replaceValue (singleton otherPolicy role 1)
        , rejects "NFT quantity two" $ replaceValue (singleton authPolicy role 2)
        , rejects "absent NFT" $ replaceValue mempty
        , rejects "second role under the authentication policy" $
            replaceValue (singleton authPolicy role 1 <> singleton authPolicy (TokenName "other") 1)
        , rejects "absent reference script" $
            goodTx
              { txInfoReferenceInputs =
                  [ reference
                      { txInInfoResolved = (txInInfoResolved reference) {txOutReferenceScript = Nothing}
                      }
                  ]
              }
        , rejects "substituted withdrawal script" $ goodTx {txInfoWdrl = Map.unsafeFromList [(otherCredential, 0)]}
        , rejects "nonzero withdrawal" $ goodTx {txInfoWdrl = Map.unsafeFromList [(credential, 1)]}
        , rejects "absent withdrawal" $ goodTx {txInfoWdrl = Map.empty}
        , rejects "duplicate withdrawal" $ goodTx {txInfoWdrl = Map.unsafeFromList [(credential, 0), (credential, 0)]}
        , rejects "absent withdrawal redeemer" $ goodTx {txInfoRedeemers = Map.empty}
        , rejects "substituted withdrawal redeemer" $ goodTx {txInfoRedeemers = Map.unsafeFromList [(Rewarding otherCredential, fieldless)]}
        , rejects "duplicate withdrawal redeemer" $ goodTx {txInfoRedeemers = Map.unsafeFromList [(Rewarding credential, fieldless), (Rewarding credential, fieldless)]}
        , testCase "rejects negative reference index" $ pfails $ handshake goodTx (-1)
        , testCase "rejects missing reference index" $ pfails $ handshake goodTx 1
        ]
    , testGroup
        "each mint arm selects its own authenticated role"
        [ testGroup
            label
            [ testCase "accepts exact role" $ psucceeds $ runMint family tag $ txFor armRole
            , testCase "rejects cross-arm role substitution" $ pfails $ runMint family tag $ txFor "wrong-role"
            , testCase "rejects missing withdrawal redeemer" $
                pfails $
                  runMint family tag $
                    (txFor armRole) {txInfoRedeemers = Map.empty}
            ]
        | (family, tag, label, armRole) <- arms
        ]
    , testGroup
        "reward binds only its applied mint policy"
        [ testCase "accepts unique policy with unrelated mint" $
            pass $
              yielded [(Minting ownPolicy, fieldless), (Minting otherPolicy, fieldless)] fieldless
        , testCase "rejects absent policy" $ pfails $ yielded [(Minting otherPolicy, fieldless)] fieldless
        , testCase "rejects duplicate policy" $ pfails $ yielded [(Minting ownPolicy, fieldless), (Minting ownPolicy, fieldless)] fieldless
        , testCase "rejects prover-selected withdrawal payload" $
            pfails $
              yielded [(Minting ownPolicy, fieldless)] (Redeemer $ dataToBuiltinData $ D.Constr 0 [D.I 1])
        , testCase "rejects wrong withdrawal constructor" $
            pfails $
              yielded [(Minting ownPolicy, fieldless)] (Redeemer $ dataToBuiltinData $ D.Constr 1 [])
        ]
    ]
  where
    rejects label tx = testCase ("rejects " <> label) $ pfails $ handshake tx 0
    pass :: (forall s. Term s PBool) -> Assertion
    pass term = passertEval term

authPolicy, otherPolicy, ownPolicy :: CurrencySymbol
authPolicy = CurrencySymbol $ toBuiltin $ BS.replicate 28 0xaa
otherPolicy = CurrencySymbol $ toBuiltin $ BS.replicate 28 0xcc
ownPolicy = CurrencySymbol $ toBuiltin $ BS.replicate 28 0xdd

scriptHash :: ScriptHash
scriptHash = ScriptHash $ toBuiltin $ BS.replicate 28 0xbb

credential, otherCredential :: Credential
credential = ScriptCredential scriptHash
otherCredential = ScriptCredential $ ScriptHash $ toBuiltin $ BS.replicate 28 0xee

role :: TokenName
role = TokenName "StateQueueCommitYield"

fieldless :: Redeemer
fieldless = Redeemer $ dataToBuiltinData $ D.Constr 0 []

reference :: TxInInfo
reference =
  TxInInfo
    (TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x11) 0)
    ( TxOut
        (scriptHashAddress scriptHash)
        (mkAdaValue 2_000_000 <> singleton authPolicy role 1)
        NoOutputDatum
        (Just scriptHash)
    )

goodTx :: TxInfo
goodTx =
  (scriptContextTxInfo $ buildScriptContext mempty)
    { txInfoReferenceInputs = [reference]
    , txInfoWdrl = Map.unsafeFromList [(credential, 0)]
    , txInfoRedeemers = Map.unsafeFromList [(Rewarding credential, fieldless)]
    }

replaceValue :: Value -> TxInfo
replaceValue value =
  goodTx
    { txInfoReferenceInputs =
        [ reference
            { txInInfoResolved = (txInInfoResolved reference) {txOutValue = mkAdaValue 2_000_000 <> value}
            }
        ]
    }

txFor :: BS.ByteString -> TxInfo
txFor name = replaceValue $ singleton authPolicy (TokenName $ toBuiltin name) 1

handshake :: forall s. TxInfo -> Integer -> Term s PBool
handshake tx index =
  prequireAuthenticatedZeroYield
    # pconstant tx
    # pconstant authPolicy
    # pconstant role
    # pconstant index
    #== pconstant scriptHash

yielded :: forall s. [(ScriptPurpose, Redeemer)] -> Redeemer -> Term s PBool
yielded redeemers payload =
  pgetYieldedMintRedeemer
    # pconstant (ScriptContext (goodTx {txInfoRedeemers = Map.unsafeFromList redeemers}) payload (RewardingScript credential))
    # pconstant ownPolicy
    #== pdata (pconstant fieldless)

-- These are the target constructor tags and exact role bytes, independently
-- enumerated from state-queue-yield.ak and availability-challenge-yield.ak.
arms :: [(Bool, Integer, String, BS.ByteString)]
arms =
  [ (True, 2, "commit", "StateQueueCommitYield")
  , (True, 3, "fraud removal", "StateQueueFraudRemovalYield")
  , (True, 4, "unattested timeout", "StateQueueUnattestedYield")
  , (True, 5, "unavailable timeout", "StateQueueUnavailableYield")
  , (True, 6, "merge", "StateQueueMergeYield")
  , (False, 0, "bond", "AvailabilityChallengeBondYield")
  , (False, 1, "open", "AvailabilityChallengeOpenYield")
  , (False, 2, "settle", "AvailabilityChallengeSettleYield")
  , (False, 3, "close", "AvailabilityChallengeCloseYield")
  , (False, 4, "timeout", "AvailabilityChallengeExpiryYield")
  ]

runMint :: forall s. Bool -> Integer -> TxInfo -> Term s PUnit
runMint stateQueue tag tx =
  if stateQueue
    then
      stateQueueMintValidator
        # pdata (pconstant otherPolicy)
        # pdata (pconstant scriptHash)
        # pdata (pconstant otherPolicy)
        # pdata (pconstant $ scriptHashAddress scriptHash)
        # pdata (pconstant otherPolicy)
        # pdata (pconstant otherPolicy)
        # pdata (pconstant otherPolicy)
        # pdata (pconstant otherPolicy)
        # pdata (pconstant otherPolicy)
        # pdata (pconstant otherPolicy)
        # pdata (pconstant authPolicy)
        # pconstant context
    else
      availabilityChallengeValidator
        # pdata (pconstant otherPolicy)
        # (pdata $ pconstant authPolicy)
        # punsafeCoerce @(PAsData PParametersV1) (pconstant @PData $ D.Constr 0 [])
        # pconstant context
  where
    -- Only the yield index is inspected by a mint arm; the rewarding arm owns
    -- all operational fields. Supplying those fields here keeps the wire arity exact.
    arity =
      if stateQueue
        then [9, 6, 3, 4, 19] !! fromIntegral (tag - 2)
        else [7, 10, 6, 8, 7] !! fromIntegral tag
    payload = Redeemer $ dataToBuiltinData $ D.Constr tag $ replicate arity $ D.I 0
    context = ScriptContext tx payload $ MintingScript ownPolicy
