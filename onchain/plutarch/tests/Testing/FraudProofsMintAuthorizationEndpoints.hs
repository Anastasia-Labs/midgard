{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsMintAuthorizationEndpoints (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (ScriptContext (..), ScriptHash (..), TokenName (..), TxInfo (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.Validators.FraudProofs.MintAuthorization
import Plutarch.Prelude
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Mint-authorization endpoints"
    [ testCase "step 01 binds an accepted transaction" $ psucceeds $ step01 acceptedContext
    , testCase "step 01 rejects an honestly rejected transaction" $ pfails $ step01 rejectedContext
    , testCase "step 01 cancels under the prover signature" $ psucceeds $ step01 $ cancellationContext Nothing True
    , testCase "step 01 rejects an unsigned cancellation" $ pfails $ step01 $ cancellationContext Nothing False
    , testCase "step 05 finalizes a direction-A verdict" $ psucceeds $ step05 $ finalizeContext $ Just $ terminalState 0
    , testCase "step 05 finalizes a direction-B verdict" $ psucceeds $ step05 $ finalizeContext $ Just $ terminalState 1
    , testCase "step 05 rejects a missing state" $ pfails $ step05 $ finalizeContext Nothing
    , testCase "step 05 rejects an out-of-domain direction" $ pfails $ step05 $ finalizeContext $ Just $ terminalState 2
    , testCase "step 05 cancels under the prover signature" $ psucceeds $ step05 $ cancellationContext (Just $ terminalState 0) True
    , testCase "step 05 rejects an unsigned cancellation" $ pfails $ step05 $ cancellationContext (Just $ terminalState 0) False
    ]

step01, step05 :: forall s. ScriptContext -> Term s PUnit
step01 ctx =
  mintAuthorizationStep01Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant hubOracleHash)
    # pconstant ctx
step05 ctx =
  mintAuthorizationStep05Validator
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant ctPolicy)
    # pconstant ctx

acceptedSource, acceptedRoot :: BS.ByteString
acceptedSource = sourceCborWithValidity txScriptSpend 0
acceptedRoot = singleEntryPhasRoot txScriptSpendId acceptedSource

step01Context :: BS.ByteString -> BS.ByteString -> ScriptContext
step01Context source root =
  spendContext
    (stepDatum Nothing)
    (PD.Constr 1 [PD.Constr 0 [inclusionArgs txScriptSpendId source root]])
    [threadInput]
    [ stepOutput nextScript $
        Just $
          PD.Constr
            0
            [ PD.B txScriptSpendId
            , PD.B $ witnessSetHashOf txScriptSpend
            , PD.I $ tValidityStart txScriptSpend
            , PD.I $ tValidityEnd txScriptSpend
            ]
    ]
    (referenceInputsWithTransactionsRoot $ commitCountedRoot transactionsDomain root l2Count)
    [phasEntry root txScriptSpendId source]
    mempty

acceptedContext, rejectedContext :: ScriptContext
acceptedContext = step01Context acceptedSource acceptedRoot
rejectedContext = step01Context (sourceCborOf txScriptSpend) (singleEntryPhasRoot txScriptSpendId $ sourceCborOf txScriptSpend)

terminalState :: Integer -> PD.Data
terminalState direction = PD.Constr 0 [PD.B $ BS.replicate 28 0x42, PD.I direction]

finalizeContext :: Maybe PD.Data -> ScriptContext
finalizeContext state =
  spendContext
    (stepDatum state)
    (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0]])
    [threadInput]
    [convictionOutput fraudProofAddress threadName]
    []
    [fraudProofMintEntry threadName]
    (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)

cancellationContext :: Maybe PD.Data -> Bool -> ScriptContext
cancellationContext state signedByProver =
  let context =
        spendContext
          (stepDatum state)
          cancelRedeemer
          [threadInput]
          []
          []
          [cancelMintEntry threadName]
          mempty
   in if signedByProver then context else withoutSignatories context

withoutSignatories :: ScriptContext -> ScriptContext
withoutSignatories (ScriptContext txInfo redeemer scriptInfo) =
  ScriptContext txInfo {txInfoSignatories = []} redeemer scriptInfo
