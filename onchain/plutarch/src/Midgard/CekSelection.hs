-- | Exact execution-selection wire openings and unique yielding spend.
module Midgard.CekSelection where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.BoundedItem qualified as Item
import Midgard.CekProof qualified as Proof
import Midgard.ComputationThread (PStepDatum (..))
import Midgard.ValidationMachine qualified as VM
import Midgard.ValidationMerkle (PFrontierPeak (..))
import Midgard.ValidationResolutionData
import Midgard.ValidationResolver qualified as Resolver
import Midgard.Common.Utils (pheadSingleton)
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

-- EnvelopeFacts has the same three-field wire layout as ProgramEnvelope.
data PAction s = PAction
    { paction'inputIndex :: Term s (PAsData PInteger)
    , paction'outputIndex :: Term s (PAsData PInteger)
    , paction'transition :: Term s (PAsData VM.PValidationOneStepWitnessV1)
    , paction'auxiliary :: Term s PData
    , paction'route :: Term s (PAsData Resolver.PCekMaterialRouteV1)
    , paction'yields :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
    , paction'envelope :: Term s (PAsData Proof.PProgramEnvelopeV1)
    , paction'material :: Term s (PAsData Proof.PCekMaterialPartitionFacts)
    , paction'beginTraversal :: Term s (PAsData PBool)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PAction)

pauthenticateRole, psuccessorRole, pprogramRole, pdataRole :: forall s. Term s PByteString
pauthenticateRole = pconstant "V1VtCekSelAuthYield"
psuccessorRole = pconstant "V1VtCekSelSuccYield"
pprogramRole = pconstant "V1VtCekSelMatProgYield"
pdataRole = pconstant "V1VtCekSelMatDataYield"

bytesList :: forall s. Term s PData -> Term s (PAsData (PBuiltinList (PAsData PByteString)))
bytesList raw = pdata $ pmap # plam (\x -> pdata $ pasByteStr # x) # (pasList # raw)
integers :: forall s. Term s PData -> Term s (PAsData (PBuiltinList (PAsData PInteger)))
integers raw = pdata $ pmap # plam (\x -> pdata $ pasInt # x) # (pasList # raw)

pdecodeRoute :: forall s. Term s PData -> Term s Resolver.PCekMaterialRouteV1
pdecodeRoute raw = pmatch (pasConstr # raw) $ \(PBuiltinPair tag f) ->
    pif (tag #== 0 #&& pnull # f) (pcon Resolver.PNoCekMaterial) $
        pif (tag #== 1 #&& plength # f #== 2) (pcon $ Resolver.PDirectCekMaterial (bytesField f 0) (bytesField f 1)) $
            pif (tag #== 2 #&& plength # f #== 2) (pcon $ Resolver.PSinglePublicationCekMaterial (bytesField f 0) (integerField f 1)) $
                pif (tag #== 3 #&& plength # f #== 2) (pcon $ Resolver.PMinimumMultiOutputCekMaterial (bytesField f 0) (integers $ pelemAt # 1 # f)) $
                    pif (tag #== 4 #&& plength # f #== 1) (pcon $ Resolver.PIncrementalCekMaterial (bytesField f 0)) perror

pdecodeAction :: forall s. Term s (PData :--> PAction)
pdecodeAction = phoistAcyclic $ plam $ \raw -> plet (recordFields 9 raw) $ \f ->
    plet (recordFields 3 $ pelemAt # 6 # f) $ \e ->
        plet (recordFields 6 $ pelemAt # 7 # f) $ \m ->
            pcon $
                PAction
                    (integerField f 0)
                    (integerField f 1)
                    (pdata $ decodeTransition $ pelemAt # 2 # f)
                    (pelemAt # 3 # f)
                    (pdata $ pdecodeRoute $ pelemAt # 4 # f)
                    (integers $ pelemAt # 5 # f)
                    (pdata $ pcon $ Proof.PProgramEnvelopeV1 (bytesField e 0) (integerField e 1) (integerField e 2))
                    (pdata $ pcon $ Proof.PCekMaterialPartitionFacts (integerField m 0) (integerField m 1) (integerField m 2) (integerField m 3) (bytesList $ pelemAt # 4 # m) (bytesList $ pelemAt # 5 # m))
                    (enumField 2 $ pelemAt # 8 # f)

pselectionLanguage :: forall s. Term s (PData :--> PInteger)
pselectionLanguage = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 11) (pasInt # (pelemAt # 1 # fields)) perror

prouteEnvelope :: forall s. Term s (Resolver.PCekMaterialRouteV1 :--> PByteString)
prouteEnvelope = phoistAcyclic $ plam $ \route -> pmatch route $ \case
    Resolver.PDirectCekMaterial envelope _ -> pfromData envelope
    Resolver.PSinglePublicationCekMaterial envelope _ -> pfromData envelope
    Resolver.PMinimumMultiOutputCekMaterial envelope _ -> pfromData envelope
    _ -> perror

pdecodeAuxiliary :: forall s. Term s (PData :--> VM.PValidationAuxiliaryWitnessV1)
pdecodeAuxiliary = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag f) ->
    pif
        (tag #== 11 #&& plength # f #== 16)
        ( plet (recordFields 8 $ pelemAt # 15 # f) $ \c ->
            pcon $
                VM.PNativeExecutionScanWitness
                    (integerField f 0)
                    (integerField f 1)
                    (integerField f 2)
                    (integerField f 3)
                    (bytesField f 4)
                    (bytesField f 5)
                    (bytesList $ pelemAt # 6 # f)
                    (integerField f 7)
                    (integerField f 8)
                    (bytesField f 9)
                    (integerField f 10)
                    (bytesField f 11)
                    (bytesList $ pelemAt # 12 # f)
                    (bytesField f 13)
                    (bytesList $ pelemAt # 14 # f)
                    ( pdata $
                        pcon $
                            Item.PChunkProofV1
                                (integerField c 0)
                                (integerField c 1)
                                (integerField c 2)
                                (integerField c 3)
                                (integerField c 4)
                                (bytesField c 5)
                                (pdata $ pmap # plam (\x -> plet (recordFields 2 x) $ \peak -> pdata $ pcon $ PFrontierPeak (integerField peak 0) (bytesField peak 1)) # (pasList # (pelemAt # 6 # c)))
                                (bytesList $ pelemAt # 7 # c)
                    )
        )
        perror

pdecodeStep :: forall s. Term s PData -> Term s PStepDatum
pdecodeStep raw = plet (recordFields 2 raw) $ \f ->
    pcon $
        PStepDatum
            (pdata $ pcon $ PPubKeyHash $ pasByteStr # (phead # f))
            ( pmatch (pasConstr # (pelemAt # 1 # f)) $ \(PBuiltinPair tag values) ->
                pif (tag #== 1 #&& pnull # values) (pcon PDNothing) $
                    pif
                        (tag #== 0 #&& plength # values #== 1)
                        (pcon $ PDJust $ pdata $ pforgetData $ pdata $ decodePrepared $ phead # values)
                        perror
            )

puniqueDispatch :: forall s. Term s (PScriptHash :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)) :--> PPair PStepDatum PAction)
puniqueDispatch = phoistAcyclic $ plam $ \hash inputs redeemers -> P.do
    PTxInInfo{ptxInInfo'outRef, ptxInInfo'resolved} <-
        pmatch $
            pfromData $
                pheadSingleton
                    # ( pfilter
                            # plam
                                ( \input -> P.do
                                    PTxInInfo{ptxInInfo'resolved = resolved} <- pmatch $ pfromData input
                                    PTxOut{ptxOut'address} <- pmatch resolved
                                    PAddress credential _ <- pmatch ptxOut'address
                                    credential #== pcon (PScriptCredential $ pdata hash)
                                )
                            # inputs
                      )
    PTxOut{ptxOut'datum} <- pmatch ptxInInfo'resolved
    raw <- plet $ pmatch ptxOut'datum $ \case POutputDatum dat -> pto dat; _ -> perror
    PBuiltinPair _ redeemer <-
        pmatch $
            pheadSingleton
                # (pfilter # plam (\pair -> pmatch pair $ \(PBuiltinPair purpose _) -> pfromData purpose #== pcon (PSpending ptxInInfo'outRef)) # redeemers)
    PBuiltinPair tag fields <- pmatch $ pasConstr # (pto $ pfromData redeemer)
    pif
        (tag #== 1 #&& plength # fields #== 1)
        (pcon $ PPair (pdecodeStep raw) (pdecodeAction # (phead # fields)))
        perror
