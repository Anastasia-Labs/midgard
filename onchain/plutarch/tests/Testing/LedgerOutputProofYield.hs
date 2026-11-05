{-# LANGUAGE OverloadedStrings #-}

module Testing.LedgerOutputProofYield (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Midgard.FraudProofs.NativeTx.Codec (pencodeDefiniteBytes)
import Midgard.LedgerOutputProofRaw qualified as Raw
import Midgard.LedgerOutputProofRoles qualified as Roles
import Midgard.LedgerOutputProofStages qualified as Stages
import Midgard.LedgerOutputProofYield qualified as Yield
import Midgard.ValidationSemanticYield qualified as Semantic
import Midgard.ValidationTrace qualified as Trace
import Plutarch.LedgerApi.V3 qualified as P
import Plutarch.Prelude
import PlutusCore.Data qualified as D
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V3
import PlutusTx.AssocMap qualified as Map
import Test.Tasty
import Test.Tasty.HUnit
import Testing.Eval (passertEvalNoTraceWithoutHoistChecks, pfailsNoTraceWithoutHoistChecks)
import Testing.ScriptContextBuilder (buildScriptContext, mkAdaValue)

tests :: TestTree
tests =
    testGroup
        "Ledger output yield handshake"
        [ testCase "reads the exact unique dispatcher action" $
            passertEvalNoTraceWithoutHoistChecks $
                pmatch (dispatch goodTx 0) $ \claim ->
                    Yield.pclaim'preProgramCounter claim
                        #== 9
                        #&& Yield.pclaim'prePriorLedgerRoot claim
                        #== pconstant (BS.replicate 32 3)
                        #&& Yield.pclaim'nextControlCbor claim
                        #== pconstant frameCbor
                        #&& Yield.pclaim'witness claim
                        #== pconstant (D.Constr 0 [])
        , testCase "rejects another stage role" $ pfailsNoTraceWithoutHoistChecks $ dispatch goodTx 1
        , testCase "rejects two matching dispatcher inputs" $ pfailsNoTraceWithoutHoistChecks $ dispatch (goodTx{txInfoInputs = [input, input]}) 0
        , testCase "rejects a foreign dispatcher input" $ pfailsNoTraceWithoutHoistChecks $ dispatch (goodTx{txInfoInputs = [input{txInInfoResolved = (txInInfoResolved input){txOutAddress = scriptHashAddress otherHash}}]}) 0
        , testCase "rejects a missing spend redeemer" $ pfailsNoTraceWithoutHoistChecks $ dispatch (goodTx{txInfoRedeemers = Map.empty}) 0
        , testCase "rejects a duplicate spend redeemer" $ pfailsNoTraceWithoutHoistChecks $ dispatch (goodTx{txInfoRedeemers = Map.unsafeFromList [(Spending ref, action), (Spending ref, action)]}) 0
        , testCase "rejects a redeemer for another input" $ pfailsNoTraceWithoutHoistChecks $ dispatch (goodTx{txInfoRedeemers = Map.unsafeFromList [(Spending $ TxOutRef (TxId "other") 1, action)]}) 0
        , testCase "rejects a missing inline datum" $ pfailsNoTraceWithoutHoistChecks $ dispatch (goodTx{txInfoInputs = [input{txInInfoResolved = (txInInfoResolved input){txOutDatum = NoOutputDatum}}]}) 0
        , testCase "requires the nine-field action" $ pfailsNoTraceWithoutHoistChecks $ dispatch (goodTx{txInfoRedeemers = Map.unsafeFromList [(Spending ref, redeemer $ D.Constr 1 [D.Constr 0 $ init actionFields])]}) 0
        , testCase "attestation sees the same dispatcher claim" $
            passertEvalNoTraceWithoutHoistChecks $
                pmatch (attestation goodTx) $
                    \claim -> Yield.pclaim'claimedScalar claim #== pconstant (D.Constr 1 [])
        , testCase "attests exactly the advanced control bytes" $
            passertEvalNoTraceWithoutHoistChecks $
                Yield.pattest # dispatch goodTx 0 # pcon (PJust $ pcon $ Stages.PAdvanced $ Raw.popen # pconstant frameCbor)
        , testCase "refuses a substituted advanced control" $
            passertEvalNoTraceWithoutHoistChecks $
                pnot # (Yield.pattest # dispatch goodTx 0 # pcon (PJust $ pcon $ Stages.PAdvanced $ Raw.preplace # (Raw.popen # pconstant frameCbor) # 2 # pconstant (D.I 1)))
        , testCase "refuses a failed stage" $
            passertEvalNoTraceWithoutHoistChecks $
                pnot # (Yield.pattest # dispatch goodTx 0 # pcon PNothing)
        , testCase "accepts the exact terminal rejection" $
            passertEvalNoTraceWithoutHoistChecks $
                Yield.pattest # rejectionClaim # pcon (PJust $ pcon Stages.PInvalidOutput)
        , testCase "rejection refuses a substituted reason" $
            passertEvalNoTraceWithoutHoistChecks $
                pnot # (Yield.pattest # rejectionClaim # pcon (PJust $ pcon Stages.PInvalidReferenceScript))
        , testCase "rejection binds the preceding program counter" $
            passertEvalNoTraceWithoutHoistChecks $
                pmatch rejectionClaim $
                    \c -> pnot # (Yield.pattest # pcon c{Yield.pclaim'preProgramCounter = 10} # pcon (PJust $ pcon Stages.PInvalidOutput))
        , testCase "rejection binds the preceding ledger root" $
            passertEvalNoTraceWithoutHoistChecks $
                pmatch rejectionClaim $
                    \c -> pnot # (Yield.pattest # pcon c{Yield.pclaim'prePriorLedgerRoot = pconstant "wrong"} # pcon (PJust $ pcon Stages.PInvalidOutput))
        , testCase "rejection requires the empty control channel" $
            passertEvalNoTraceWithoutHoistChecks $
                pmatch rejectionClaim $
                    \c -> pnot # (Yield.pattest # pcon c{Yield.pclaim'nextControlCbor = pconstant frameCbor} # pcon (PJust $ pcon Stages.PInvalidOutput))
        , testCase "rejection does not add a ledger-delta clause absent upstream" $
            passertEvalNoTraceWithoutHoistChecks $
                pmatch rejectionClaim $
                    \c -> Yield.pattest # pcon c{Yield.pclaim'claimedSuccessor = replaceSuccessor (Yield.pclaim'claimedSuccessor c) 14 (pconstant $ D.B "different delta")} # pcon (PJust $ pcon Stages.PInvalidOutput)
        , testCase "rejection refuses an accepted verdict" $
            passertEvalNoTraceWithoutHoistChecks $
                pmatch rejectionClaim $
                    \c -> pnot # (Yield.pattest # pcon c{Yield.pclaim'claimedSuccessor = replaceSuccessor (Yield.pclaim'claimedSuccessor c) 12 (pconstant $ D.Constr 1 [])} # pcon (PJust $ pcon Stages.PInvalidOutput))
        , testCase "rejects window witness with foreign constructor" $ pfailsNoTraceWithoutHoistChecks $ Yield.pwindowWitness # pconstant (D.Constr 7 [D.B "x"])
        , testCase "rejects no-witness with extra fields" $ pfailsNoTraceWithoutHoistChecks $ Yield.pwindowWitness # pconstant (D.Constr 0 [D.I 0])
        , testCase "value witness rejects the obsolete five-field layout" $ pfailsNoTraceWithoutHoistChecks $ Yield.pvalueWitness # pconstant (D.Constr 2 [D.I 0, D.B "", D.B "", D.I 1, D.List []])
        , testCase "semantic dispatch returns its checked state and all extra fields" $
            passertEvalNoTraceWithoutHoistChecks $
                pmatch (Semantic.puniqueSemanticDispatchV1 # dispatchers # pconstant goodTx) $
                    \result -> plength # Semantic.pdispatch'extra result #== 6
        , testCase "semantic dispatch rejects an ill-typed machine state" $
            pfailsNoTraceWithoutHoistChecks $
                Semantic.puniqueSemanticDispatchV1 # dispatchers # pconstant (goodTx{txInfoInputs = [input{txInInfoResolved = (txInInfoResolved input){txOutDatum = OutputDatum $ Datum $ dataToBuiltinData $ datumWith $ D.Constr 0 (D.B "not an integer" : tail machineFields)}}]})
        , testCase "role order requires the matching reference indices" $ passertEvalNoTraceWithoutHoistChecks $ handshake [0, 1]
        , testCase "short yield index list fails" $ pfailsNoTraceWithoutHoistChecks $ handshake [0]
        , testCase "long yield index list refuses" $ passertEvalNoTraceWithoutHoistChecks $ pnot # handshake [0, 1, 0]
        , testCase "swapped yield indices fail" $ pfailsNoTraceWithoutHoistChecks $ handshake [1, 0]
        , testCase "out-of-range stage role fails" $ pfailsNoTraceWithoutHoistChecks $ Roles.pstageRole # 24
        , testCase "negative descriptor role fails" $ pfailsNoTraceWithoutHoistChecks $ Roles.pdescriptorRole # (-1)
        , testCase "integer and bytes stages require their scalar attestation" $
            passertEvalNoTraceWithoutHoistChecks $
                (Roles.pstageAttestationRoles # 5 #== pcons # pdata Roles.pscalarIntegerAttestationRole # pnil)
                    #&& (Roles.pstageAttestationRoles # 7 #== Roles.pstageAttestationRoles # 5)
                    #&& (Roles.pstageAttestationRoles # 17 #== pcons # pdata Roles.pscalarBytesAttestationRole # pnil)
                    #&& (Roles.pstageAttestationRoles # 18 #== Roles.pstageAttestationRoles # 17)
                    #&& pnull
                    # (Roles.pstageAttestationRoles # 23)
        ]

frameCbor :: BS.ByteString
frameCbor = Base16.decodeLenient $ "91010000015820" <> BS.concat (replicate 32 "00") <> "00d87a80d87a800080d87a80d87a80d87a80d87a80d87a80d87a80d87a80"

machineFields :: [D.Data]
machineFields = [D.I 1, D.B "event", D.B "tx", D.B "commitment", D.B "context", D.Constr 0 [], D.B $ BS.replicate 32 3, D.Constr 7 [], D.I 9, D.B "work", D.I 0, D.I 0, D.Constr 0 [], D.B "rejection", D.B "delta"]
machine :: D.Data
machine = D.Constr 0 machineFields
datumWith :: D.Data -> D.Data
datumWith pre = D.Constr 0 [D.B "prover", D.Constr 0 [D.Constr 0 [D.I 1, D.Constr 0 [D.I 1, pre, D.B "operator", D.B "challenger"], D.B "evidence"]]]
actionFields :: [D.Data]
actionFields = [D.I 0, D.I 0, D.Constr 0 [D.B "work", machine], D.Constr 0 [], D.B frameCbor, D.B frameCbor, D.Constr 1 [], D.I 0, D.List []]
redeemer :: D.Data -> Redeemer
redeemer = Redeemer . dataToBuiltinData
action :: Redeemer
action = redeemer $ D.Constr 1 [D.Constr 0 actionFields]
scriptHash, otherHash :: ScriptHash
scriptHash = ScriptHash $ toBuiltin $ BS.replicate 28 0x11
otherHash = ScriptHash $ toBuiltin $ BS.replicate 28 0x22
ref :: TxOutRef
ref = TxOutRef (TxId "thread") 0
input :: TxInInfo
input = TxInInfo ref $ TxOut (scriptHashAddress scriptHash) (mkAdaValue 2_000_000) (OutputDatum $ Datum $ dataToBuiltinData $ datumWith machine) Nothing
goodTx :: TxInfo
goodTx = (scriptContextTxInfo $ buildScriptContext mempty){txInfoInputs = [input], txInfoRedeemers = Map.unsafeFromList [(Spending ref, action)]}

dispatchers :: forall s. Term s (PBuiltinList (PAsData P.PScriptHash))
dispatchers = pcons # pdata (pconstant scriptHash) # pnil
dispatch :: forall s. TxInfo -> Integer -> Term s Yield.PClaim
dispatch tx role = pmatch (pconstant tx) $ \P.PTxInfo{P.ptxInfo'inputs, P.ptxInfo'redeemers} ->
    Yield.pdispatch # dispatchers # pconstant role # pfromData ptxInfo'inputs # (pto $ pto $ pfromData ptxInfo'redeemers)
attestation :: forall s. TxInfo -> Term s Yield.PClaim
attestation tx = pmatch (pconstant tx) $ \P.PTxInfo{P.ptxInfo'inputs, P.ptxInfo'redeemers} ->
    Yield.pdispatchAttestation # dispatchers # pfromData ptxInfo'inputs # (pto $ pto $ pfromData ptxInfo'redeemers)

handshake :: forall s. [Integer] -> Term s PBool
handshake indices = Semantic.prequireSemanticYieldsV1 # pconstant tx # pconstant policy # roles # (pmap # plam pdata # pconstant @(PBuiltinList PInteger) indices)
  where
    policy = CurrencySymbol "auth"
    names = ["V1VtLopDatumAttachIntegerYield", "V1VtLopScalarIntegerYield"]
    hashes = [scriptHash, otherHash]
    references = [TxInInfo (TxOutRef (TxId "ref") i) $ TxOut (scriptHashAddress hash) (mkAdaValue 2_000_000 <> singleton policy (TokenName name) 1) NoOutputDatum (Just hash) | (i, name, hash) <- zip3 [0, 1] names hashes]
    tx = goodTx{txInfoReferenceInputs = references, txInfoWdrl = Map.unsafeFromList [(ScriptCredential hash, 0) | hash <- hashes], txInfoRedeemers = Map.unsafeFromList [(Rewarding $ ScriptCredential hash, redeemer $ D.Constr 0 []) | hash <- hashes]}
    roles = pcons # pdata (Roles.pstageRole # 5) # (Roles.pstageAttestationRoles # 5)

replaceSuccessor :: forall s. Term s PData -> Term s PInteger -> Term s PData -> Term s PData
replaceSuccessor original index replacement = pmatch (pasConstr # original) $ \(PBuiltinPair tag fields) ->
    pforgetData $ pconstrBuiltin # tag # (pmap # plam (\i -> pif (i #== index) replacement (pelemAt # i # fields)) # pconstant @(PBuiltinList PInteger) [0 .. 14])

rejectionClaim :: forall s. Term s Yield.PClaim
rejectionClaim = pmatch (dispatch goodTx 0) $ \c ->
    plet (pconstant @PByteString "E_INVALID_OUTPUT") $ \code ->
        plet (pconstant @PByteString "\x84\x02" <> (pencodeDefiniteBytes # code) <> (pencodeDefiniteBytes # Yield.pclaim'prePriorLedgerRoot c) <> pconstant "\x41\x80") $ \terminalWitness ->
            plet (replaceSuccessor (pconstant machine) 7 (pconstant $ D.Constr 14 [])) $ \phase ->
                plet (replaceSuccessor phase 12 (pconstant $ D.Constr 2 [])) $ \verdict ->
                    plet (replaceSuccessor verdict 13 (pforgetData $ pdata $ Trace.phashRejectionCode # code)) $ \reason ->
                        plet (replaceSuccessor reason 9 (pforgetData $ pdata $ Trace.phashWorkWitness # pcon Trace.PTerminal # 10 # terminalWitness)) $ \successor ->
                            pcon c{Yield.pclaim'claimedSuccessor = successor, Yield.pclaim'nextControlCbor = pconstant ""}
