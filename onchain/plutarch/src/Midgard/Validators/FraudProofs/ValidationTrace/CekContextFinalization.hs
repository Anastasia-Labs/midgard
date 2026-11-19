-- | Authenticated context finalization and the five script-info routes.
module Midgard.Validators.FraudProofs.ValidationTrace.CekContextFinalization where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.CekContextChain qualified as Chain
import Midgard.CekContextStep qualified as Step
import Midgard.CekContextWire qualified as Wire
import Midgard.CekData qualified as Data
import Midgard.CekSelection (bytesList)
import Midgard.ComputationThread (PStepDatum (..))
import Midgard.ScriptContext qualified as ScriptContext
import Midgard.ScriptProof qualified as Script
import Midgard.ValidationMachine qualified as VM
import Midgard.ValidationMerkle qualified as Merkle
import Midgard.ValidationResolutionData (bytesField, integerField, recordFields)
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectState, pstep)
import Midgard.Validators.FraudProofs.ValidationTrace.CekContext (Hop, continue, hop)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.V3
import Plutarch.Prelude

data PPending s = PPending
    { ppending'staged :: Term s PData
    , ppending'items :: Term s (PAsData Data.PDataSequenceSummaryV1)
    , ppending'redeemer :: Term s (PAsData Data.PDataSummaryV1)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PPending)

pdecodePending :: forall s. Term s (PData :--> PPending)
pdecodePending = phoistAcyclic $ plam $ \raw -> plet (recordFields 3 raw) $ \f ->
    pcon $ PPending (phead # f) (pdata $ Wire.pdecodeDataSequenceSummary # (pelemAt # 1 # f)) (pdata $ Wire.pdecodeDataSummary # (pelemAt # 2 # f))

pfinalized :: forall s. Term s (PPending :--> Chain.PStaged :--> Data.PDataSummaryV1 :--> Chain.PVerified)
pfinalized = phoistAcyclic $ plam $ \pending staged info -> pmatch pending $ \p -> pmatch staged $ \s ->
    pmatch (pfromData $ Chain.pstaged'context s) $ \c ->
        pcon $
            Chain.PVerified
                (ppending'staged p)
                ( pdata $
                    pcon $
                        Chain.PContinueContext $
                            pdata $
                                pcon
                                    c
                                        { VM.pcekContext'stage = pdata 11
                                        , VM.pcekContext'redeemerContextControlHash = pdata $ VM.phashCekContextPartsControlV1 # (pcon $ VM.PCekContextPartsControlV1 (ppending'items p) (ppending'redeemer p) (pdata info))
                                        }
                )

authenticateValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
authenticateValidator = plam $ \routes policy ctx -> pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PData policy datum redeemer ownRef tx $ \raw ->
        plet (Chain.pdecodeAction # raw) $ \action -> pmatch action $ \a ->
            plet (pexpectDatum datum) $ \step -> pmatch step $ \d ->
                plet (Chain.pdecodeStaged # pexpectState (pstep'data d)) $ \staged -> pmatch staged $ \s ->
                    pmatch (pfromData $ Chain.pstaged'context s) $ \c -> pmatch (pfromData $ Chain.pstaged'native s) $ \native ->
                        plet (pfromData (VM.pcekContext'languageTag c) #== 128) $ \midgard ->
                            plet
                                ( pif
                                    (pnot # midgard #&& pfromData (VM.pcekContext'purposeKind c) #== 0)
                                    ( plet (Step.pauxiliaryFields 20 5 $ Chain.paction'auxiliary a) $ \f ->
                                        plet (pfromData $ integerField f 1) $ \index -> plet (pfromData $ bytesField f 2) $ \key -> plet (pfromData $ bytesField f 3) $ \descriptor ->
                                            pif
                                                ( index
                                                    #== pfromData (VM.pcekContext'purposeIndex c)
                                                    #&& key
                                                    #== pfromData (VM.pcekContext'subject c)
                                                    #&& Merkle.pverifyMembership
                                                    # pfromData (Step.pnative'resolvedInputCount native)
                                                    # pfromData (Step.pnative'resolvedItemPeaks native)
                                                    # index
                                                    # (Script.presolvedContextItemLeafHash # 0 # index # key # descriptor)
                                                    # pfromData (bytesList $ pelemAt # 4 # f)
                                                )
                                                (Wire.pdecodeCekRedeemerContextControl # (phead # f))
                                                perror
                                    )
                                    (Wire.pdecodeCekRedeemerContextControl # (phead # Step.pauxiliaryFields 19 1 (Chain.paction'auxiliary a)))
                                )
                                $ \redeemers -> pmatch redeemers $ \r ->
                                    pmatch (pfromData $ VM.pcekRedeemer'currentRedeemer r) $ \summary ->
                                        pif
                                            ( pand'List
                                                [ pfromData (VM.pcekContext'stage c) #== 10
                                                , Chain.pauxiliaryIsBound # pfromData (Chain.pstaged'bound s) # Chain.paction'auxiliary a
                                                , VM.pcekRedeemerContextControlIsWellFormed # pfromData (Step.pnative'redeemerCount native) # redeemers
                                                , VM.phashCekRedeemerContextControlV1 # redeemers #== pfromData (VM.pcekContext'redeemerContextControlHash c)
                                                , VM.pcekRedeemer'cursor r #== Step.pnative'redeemerCount native
                                                , pfromData (VM.pcekRedeemer'activeScanHash r) #== pconstant ""
                                                , plengthBS # pfromData (Data.psummary'root summary) #== 32
                                                ]
                                            )
                                            ( continue
                                                policy
                                                step
                                                action
                                                ownRef
                                                tx
                                                (pelemAt # (pif midgard 4 $ pfromData $ VM.pcekContext'purposeKind c) # pfromData routes)
                                                (pforgetData $ pdata $ pcon $ PPending (pforgetData $ pdata staged) (VM.pcekRedeemer'mapItems r) (VM.pcekRedeemer'currentRedeemer r))
                                            )
                                            perror

summaryHop :: forall s. (Term s VM.PCekContextControlV1 -> Term s PBool) -> (Term s VM.PCekContextControlV1 -> Term s PData -> Term s (PMaybe Data.PDataSummaryV1)) -> Hop s
summaryHop accepts summarize = hop $ \raw action -> pmatch action $ \a ->
    plet (pdecodePending # raw) $ \pending -> pmatch pending $ \p ->
        plet (Chain.pdecodeStaged # ppending'staged p) $ \staged -> pmatch staged $ \s ->
            plet (pfromData $ Chain.pstaged'context s) $ \context -> pmatch context $ \c ->
                pif
                    (pfromData (VM.pcekContext'stage c) #== 10 #&& accepts context #&& Chain.pauxiliaryIsBound # pfromData (Chain.pstaged'bound s) # Chain.paction'auxiliary a)
                    ( pmatch (summarize context $ Chain.paction'auxiliary a) $ \case
                        PNothing -> perror
                        PJust info -> pforgetData $ pdata $ pfinalized # pending # staged # info
                    )
                    perror

spendValidator :: forall s. Hop s
spendValidator =
    summaryHop
        (\context -> pmatch context $ \c -> pfromData (VM.pcekContext'languageTag c) #/= 128 #&& pfromData (VM.pcekContext'purposeKind c) #== 0)
        ( \context auxiliary -> pmatch context $ \c -> plet (Step.pauxiliaryFields 20 5 auxiliary) $ \f ->
            ScriptContext.pcardanoSpendScriptInfoFromDescriptorV1 # pfromData (VM.pcekContext'subject c) # pfromData (bytesField f 3)
        )

purposeValidator :: forall s. Integer -> Hop s
purposeValidator kind =
    summaryHop
        (\context -> pmatch context $ \c -> pfromData (VM.pcekContext'languageTag c) #/= 128 #&& pfromData (VM.pcekContext'purposeKind c) #== pconstant kind)
        (\context _ -> pmatch context $ \c -> ScriptContext.pscriptPurposeSummaryV1 # pconstant kind # pfromData (VM.pcekContext'scriptHash c) # pfromData (VM.pcekContext'subject c) # pconstant False)

mintValidator, withdrawValidator, observeValidator, midgardValidator :: forall s. Hop s
mintValidator = purposeValidator 1
withdrawValidator = purposeValidator 2
observeValidator = purposeValidator 3
midgardValidator =
    summaryHop
        (\context -> pmatch context $ \c -> pfromData (VM.pcekContext'languageTag c) #== 128)
        (\context _ -> pmatch context $ \c -> ScriptContext.pscriptPurposeSummaryV1 # pfromData (VM.pcekContext'purposeKind c) # pfromData (VM.pcekContext'scriptHash c) # pfromData (VM.pcekContext'subject c) # pconstant True)
