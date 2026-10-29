-- | Context binding and return stages around the shared redeemer-item machine.
module Midgard.Validators.FraudProofs.ValidationTrace.CekContextItem where

import Midgard.CekContextChain qualified as Chain
import Midgard.CekContextItem qualified as Carrier
import Midgard.CekContextItemWire qualified as Wire
import Midgard.CekContextRedeemer qualified as Redeemer
import Midgard.CekContextStep qualified as Step
import Midgard.CekContextWire qualified as ContextWire
import Midgard.CekData qualified as Data
import Midgard.ComputationThread (PStepDatum (..))
import Midgard.RedeemerItemProof qualified as Item
import Midgard.ScriptProof qualified as Script
import Midgard.ValidationMachine qualified as VM
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectState, pstep)
import Midgard.Validators.FraudProofs.ValidationTrace.CekContext (Hop, continue, hop)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.V3
import Plutarch.Prelude

bindValidator :: forall s. Hop s
bindValidator = plam $ \nextHash policy ctx -> pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PData policy datum redeemer ownRef tx $ \raw ->
        plet (Carrier.pdecodeAction # raw) $ \action -> pmatch action $ \a ->
            plet (pexpectDatum datum) $ \step -> pmatch step $ \d ->
                plet (Chain.pdecodeStaged # pexpectState (pstep'data d)) $ \staged -> pmatch staged $ \s ->
                    pmatch (pfromData $ Chain.pstaged'context s) $ \context -> pmatch (pfromData $ Chain.pstaged'native s) $ \native ->
                        plet (Step.pauxiliaryFields 18 3 $ Carrier.paction'auxiliary a) $ \f ->
                            plet (Wire.pdecodeRedeemerItemProofControl # (pelemAt # 1 # f)) $ \control -> pmatch control $ \c ->
                                plet (Script.predeemerItemLeafHash # pfromData (Item.predeemerControl'itemIndex c) # pfromData (Item.predeemerControl'itemCommitment c)) $ \leaf ->
                                    plet
                                        ( pif
                                            (pfromData (VM.pcekContext'stage context) #== 0)
                                            ( pmatch (VM.predeemerTagForPurposeKindV1 # pfromData (VM.pcekContext'purposeKind context)) $ \case
                                                PNothing -> perror
                                                PJust purpose -> pmatch (pasConstr # (phead # f)) $ \(PBuiltinPair tag fields) ->
                                                    pif
                                                        ( pand'List
                                                            [ tag #== 1
                                                            , pnull # fields
                                                            , pfromData (Item.predeemerControl'mode c) #== Item.pmodeDescriptor
                                                            , pfromData (Item.predeemerControl'expectedPurposeTag c) #== purpose
                                                            , Item.predeemerControl'expectedPointerIndex c #== VM.pcekContext'purposeIndex context
                                                            , leaf #== pfromData (VM.pcekContext'redeemerLeaf context)
                                                            ]
                                                        )
                                                        (pfromData $ VM.pcekContext'redeemerContextControlHash context)
                                                        perror
                                            )
                                            ( plet (Step.pauxiliaryFields 0 1 $ phead # f) $ \currentFields ->
                                                plet (ContextWire.pdecodeCekRedeemerContextControl # (phead # currentFields)) $ \current -> pmatch current $ \r ->
                                                    pif
                                                        ( pand'List
                                                            [ pfromData (VM.pcekContext'stage context) #== 9
                                                            , VM.pcekRedeemerContextControlIsWellFormed # pfromData (Step.pnative'redeemerCount native) # current
                                                            , VM.phashCekRedeemerContextControlV1 # current #== pfromData (VM.pcekContext'redeemerContextControlHash context)
                                                            , pfromData (Item.predeemerControl'itemIndex c) #== pfromData (Step.pnative'redeemerCount native) - pfromData (VM.pcekRedeemer'cursor r) - 1
                                                            , leaf #== pfromData (VM.pcekRedeemer'activeRedeemerLeaf r)
                                                            ]
                                                        )
                                                        (pfromData $ VM.pcekRedeemer'activeScanHash r)
                                                        perror
                                            )
                                        )
                                        $ \expected ->
                                            pif
                                                ( Chain.pauxiliaryIsBound
                                                    # pfromData (Chain.pstaged'bound s)
                                                    # Carrier.paction'auxiliary a
                                                    #&& Item.predeemerControl'itemCount c
                                                    #== Step.pnative'redeemerCount native
                                                    #&& Item.phashControlV1
                                                    # control
                                                    #== expected
                                                )
                                                ( continue
                                                    policy
                                                    step
                                                    (Carrier.pcontextAction # action)
                                                    ownRef
                                                    tx
                                                    nextHash
                                                    ( pforgetData $
                                                        pdata $
                                                            pcon $
                                                                Carrier.PPending
                                                                    (pforgetData $ pdata staged)
                                                                    (pdata control)
                                                                    (pdata $ pblake2b_256 # (pserialiseData # (pelemAt # 2 # f)))
                                                                    (Carrier.paction'claimedNext a)
                                                    )
                                                )
                                                perror

returnValidator :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
returnValidator = plam $ \routes policy ctx -> pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PData policy datum redeemer ownRef tx $ \raw ->
        plet (Chain.pdecodeAction # raw) $ \action -> pmatch action $ \a ->
            plet (pexpectDatum datum) $ \step -> pmatch step $ \d ->
                plet (Carrier.pdecodeVerified # pexpectState (pstep'data d)) $ \verified -> pmatch verified $ \v ->
                    pmatch (Chain.pdecodeStaged # Carrier.pverified'staged v) $ \s ->
                        pmatch (pfromData $ Chain.pstaged'context s) $ \c -> pmatch (pfromData $ Carrier.pverified'next v) $ \item ->
                            plet
                                ( pif
                                    (pfromData (VM.pcekContext'stage c) #== 0)
                                    (pif (pfromData (Item.predeemerControl'stage item) #== Item.pstageTerminal) 1 0)
                                    ( pif
                                        (pfromData (VM.pcekContext'stage c) #== 9)
                                        (pif (pfromData (Item.predeemerControl'stage item) #/= Item.pstageTerminal) 2 (pif (pfromData (Item.predeemerControl'mode item) #== Item.pmodeDescriptor) 3 4))
                                        perror
                                    )
                                )
                                $ \route ->
                                    pif
                                        (Chain.pauxiliaryIsBound # pfromData (Chain.pstaged'bound s) # Chain.paction'auxiliary a)
                                        (continue policy step action ownRef tx (pelemAt # route # pfromData routes) (pforgetData $ pdata verified))
                                        perror

hashValidator, finalizeValidator :: forall s. Hop s
hashValidator = hop $ \raw _ -> plet (Carrier.pdecodeVerified # raw) $ \verified -> pmatch verified $ \v ->
    pmatch (pfromData $ Carrier.pverified'next v) $ \item ->
        pif
            (pfromData (Item.predeemerControl'stage item) #/= Item.pstageTerminal)
            (pforgetData $ pdata $ pcon $ Carrier.PHashResult (pforgetData $ pdata verified) (pdata $ Item.phashControlV1 # pfromData (Carrier.pverified'next v)))
            perror
finalizeValidator = hop $ \raw _ -> plet (Carrier.pdecodeVerified # raw) $ \verified -> pmatch verified $ \v ->
    pmatch (pfromData $ Carrier.pverified'next v) $ \item ->
        pif
            (pfromData (Item.predeemerControl'stage item) #== Item.pstageTerminal #&& pfromData (Item.predeemerControl'mode item) #== Item.pmodeData)
            ( pmatch (Item.pfinalizeV1 # pfromData (Carrier.pverified'next v)) $ \case
                PNothing -> perror
                PJust summary -> pforgetData $ pdata $ pcon $ Carrier.PValueResult (pforgetData $ pdata verified) (pdata summary)
            )
            perror

selectionContinueValidator, selectionFinishValidator :: forall s. Hop s
selectionContinueValidator = hop $ \raw action -> pmatch action $ \a -> pmatch (Carrier.pdecodeHashResult # raw) $ \result ->
    pmatch (Carrier.pdecodeVerified # Carrier.phash'verified result) $ \v -> pmatch (Chain.pdecodeStaged # Carrier.pverified'staged v) $ \s ->
        pmatch (pfromData $ Chain.pstaged'context s) $ \c -> pmatch (pfromData $ Carrier.pverified'next v) $ \item ->
            pif
                ( Chain.pauxiliaryIsBound
                    # pfromData (Chain.pstaged'bound s)
                    # Chain.paction'auxiliary a
                    #&& pfromData (VM.pcekContext'stage c)
                    #== 0
                    #&& pfromData (Item.predeemerControl'stage item)
                    #/= Item.pstageTerminal
                )
                (pforgetData $ pdata $ pcon $ Chain.PVerified (Carrier.pverified'staged v) (pdata $ pcon $ Chain.PContinueContext $ pdata $ pcon c{VM.pcekContext'redeemerContextControlHash = Carrier.phash'next result}))
                perror
selectionFinishValidator = hop $ \raw action -> pmatch action $ \a -> pmatch (Carrier.pdecodeVerified # raw) $ \v ->
    pmatch (Chain.pdecodeStaged # Carrier.pverified'staged v) $ \s -> pmatch (pfromData $ Chain.pstaged'context s) $ \c ->
        pmatch (pfromData $ Carrier.pverified'next v) $ \item ->
            pif
                ( Chain.pauxiliaryIsBound
                    # pfromData (Chain.pstaged'bound s)
                    # Chain.paction'auxiliary a
                    #&& pfromData (VM.pcekContext'stage c)
                    #== 0
                    #&& pfromData (Item.predeemerControl'stage item)
                    #== Item.pstageTerminal
                )
                ( pforgetData $
                    pdata $
                        pcon $
                            Chain.PVerified
                                (Carrier.pverified'staged v)
                                ( pdata $
                                    pcon $
                                        Chain.PContinueContext $
                                            pdata $
                                                pcon
                                                    c
                                                        { VM.pcekContext'stage = pdata 1
                                                        , VM.pcekContext'redeemerContextControlHash = pdata $ VM.phashCekRedeemerContextControlV1 # VM.pinitialCekRedeemerContextControlV1
                                                        , VM.pcekContext'executionMemoryLimit = Item.predeemerControl'executionMemory item
                                                        , VM.pcekContext'executionCpuLimit = Item.predeemerControl'executionSteps item
                                                        }
                                )
                )
                perror

pemptySummary :: forall s. Term s Data.PDataSummaryV1
pemptySummary = pcon $ Data.PDataSummaryV1 (pdata $ pconstant "") (pdata 0) (pdata 0)

-- The three data returns authenticate the same owned openings before applying
-- their distinct terminal/nonterminal updates.
dataReturn :: forall s. (Term s PData -> (Term s Carrier.PVerified -> Term s PData -> Term s PData) -> Term s PData) -> (Term s Item.PRedeemerItemProofControlV1 -> Term s VM.PCekContextControlV1 -> Term s VM.PCekRedeemerContextControlV1 -> Term s PData -> Term s VM.PCekRedeemerContextControlV1) -> Hop s
dataReturn opening update = hop $ \raw action -> pmatch action $ \a -> opening raw $ \verified payload -> pmatch verified $ \v ->
    plet (Chain.pdecodeStaged # Carrier.pverified'staged v) $ \staged -> pmatch staged $ \s ->
        plet (pfromData $ Chain.pstaged'context s) $ \context -> pmatch context $ \c ->
            plet (pfromData $ Carrier.pverified'next v) $ \nextItem -> pmatch nextItem $ \item ->
                plet (Step.pauxiliaryFields 18 3 $ Chain.paction'auxiliary a) $ \f ->
                    plet (ContextWire.pdecodeCekRedeemerContextControl # (phead # Step.pauxiliaryFields 0 1 (phead # f))) $ \current -> pmatch current $ \r ->
                        pmatch (Wire.pdecodeRedeemerItemProofControl # (pelemAt # 1 # f)) $ \oldItem ->
                            pmatch (pfromData $ VM.pcekRedeemer'activePurpose r) $ \purpose ->
                                pif
                                    ( Chain.pauxiliaryIsBound
                                        # pfromData (Chain.pstaged'bound s)
                                        # Chain.paction'auxiliary a
                                        #&& pfromData (VM.pcekContext'stage c)
                                        #== 9
                                        #&& pif
                                            (pfromData (Item.predeemerControl'mode item) #== Item.pmodeDescriptor)
                                            (pfromData (Item.predeemerControl'expectedPurposeTag oldItem) #== 6 #&& pfromData (VM.pcekContext'languageTag c) #== 3 #&& pfromData (VM.pcekRedeemer'activePurpose r) #== pemptySummary)
                                            (plengthBS # pfromData (Data.psummary'root purpose) #== 32)
                                    )
                                    (pforgetData $ pdata $ pcon $ Chain.PVerified (Carrier.pverified'staged v) (pdata $ pcon $ Chain.PContinueContext $ pdata $ Redeemer.pcontextSuccessor # staged # update nextItem context current payload))
                                    perror

dataContinueValidator, dataFinishDescriptorValidator, dataFinishValueValidator :: forall s. Hop s
dataContinueValidator =
    dataReturn
        (\raw next -> pmatch (Carrier.pdecodeHashResult # raw) $ \result -> next (Carrier.pdecodeVerified # Carrier.phash'verified result) (pforgetData $ Carrier.phash'next result))
        ( \nextItem _ current payload -> pmatch nextItem $ \item -> pmatch current $ \r ->
            pif
                (pfromData (Item.predeemerControl'stage item) #/= Item.pstageTerminal)
                (pcon r{VM.pcekRedeemer'activeScanHash = pdata $ pasByteStr # payload})
                perror
        )
dataFinishDescriptorValidator =
    dataReturn
        (\raw next -> next (Carrier.pdecodeVerified # raw) raw)
        ( \nextItem _ current _ -> pmatch nextItem $ \item -> pmatch current $ \r ->
            pif
                (pfromData (Item.predeemerControl'stage item) #== Item.pstageTerminal #&& pfromData (Item.predeemerControl'mode item) #== Item.pmodeDescriptor)
                ( pcon
                    r
                        { VM.pcekRedeemer'cursor = pdata $ pfromData (VM.pcekRedeemer'cursor r) + 1
                        , VM.pcekRedeemer'activeScanHash = pdata $ pconstant ""
                        , VM.pcekRedeemer'activeRedeemerLeaf = pdata $ pconstant ""
                        , VM.pcekRedeemer'activePurpose = pdata pemptySummary
                        }
                )
                perror
        )
dataFinishValueValidator =
    dataReturn
        (\raw next -> pmatch (Carrier.pdecodeValueResult # raw) $ \result -> next (Carrier.pdecodeVerified # Carrier.pvalue'verified result) (pforgetData $ Carrier.pvalue'summary result))
        ( \nextItem context current payload -> pmatch nextItem $ \item -> pmatch context $ \c -> pmatch current $ \r ->
            plet (ContextWire.pdecodeDataSummary # payload) $ \summary ->
                pif
                    (pfromData (Item.predeemerControl'stage item) #== Item.pstageTerminal #&& pfromData (Item.predeemerControl'mode item) #/= Item.pmodeDescriptor)
                    ( pcon
                        r
                            { VM.pcekRedeemer'cursor = pdata $ pfromData (VM.pcekRedeemer'cursor r) + 1
                            , VM.pcekRedeemer'mapItems = pdata $ Data.pprependDataPairSummaryV1 # pfromData (VM.pcekRedeemer'activePurpose r) # summary # pfromData (VM.pcekRedeemer'mapItems r)
                            , VM.pcekRedeemer'activeScanHash = pdata $ pconstant ""
                            , VM.pcekRedeemer'activeRedeemerLeaf = pdata $ pconstant ""
                            , VM.pcekRedeemer'activePurpose = pdata pemptySummary
                            , VM.pcekRedeemer'currentRedeemer = pif (VM.pcekRedeemer'activeRedeemerLeaf r #== VM.pcekContext'redeemerLeaf c) (pdata summary) (VM.pcekRedeemer'currentRedeemer r)
                            }
                    )
                    perror
        )
