-- | The four physical redeemer-selection context stages.
module Midgard.Validators.FraudProofs.ValidationTrace.CekContextRedeemer where

import Midgard.CekContextChain qualified as Chain
import Midgard.CekContextRedeemer qualified as Redeemer
import Midgard.CekContextStep qualified as Step
import Midgard.CekContextWire qualified as Wire
import Midgard.CekSelection (bytesList)
import Midgard.RedeemerItemProof qualified as Item
import Midgard.ScriptContext qualified as ScriptContext
import Midgard.ScriptProof qualified as Script
import Midgard.ValidationMachine qualified as VM
import Midgard.ValidationMerkle qualified as Merkle
import Midgard.ValidationResolutionData (bytesField, integerField)
import Midgard.Validators.FraudProofs.ValidationTrace.CekContext (Hop, hop)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

authenticateValidator :: forall s. Hop s
authenticateValidator = hop $ \raw action -> pmatch action $ \a ->
    plet (Chain.pdecodeStaged # raw) $ \staged -> pmatch staged $ \s ->
        pmatch (pfromData $ Chain.pstaged'context s) $ \c -> pmatch (pfromData $ Chain.pstaged'native s) $ \native ->
            plet (Step.pauxiliaryFields 17 12 $ Chain.paction'auxiliary a) $ \f ->
                plet (Wire.pdecodeCekRedeemerContextControl # (phead # f)) $ \control -> pmatch control $ \r ->
                    plet (pfromData $ integerField f 1) $ \index -> plet (pfromData $ integerField f 2) $ \count ->
                        plet (pfromData $ bytesField f 4) $ \commitment -> plet (Script.predeemerItemLeafHash # index # commitment) $ \leaf ->
                            plet (pfromData $ integerField f 6) $ \frontier ->
                                plet (pfromData $ integerField f 7) $ \kind -> plet (pfromData $ integerField f 8) $ \purposeIndex ->
                                    plet (pfromData $ bytesField f 9) $ \scriptHash -> plet (pfromData $ bytesField f 10) $ \subject ->
                                        pif
                                            ( pand'List
                                                [ pfromData (VM.pcekContext'stage c) #== 9
                                                , Chain.pauxiliaryIsBound # pfromData (Chain.pstaged'bound s) # Chain.paction'auxiliary a
                                                , VM.pcekRedeemerContextControlIsWellFormed # pfromData (Step.pnative'redeemerCount native) # control
                                                , VM.phashCekRedeemerContextControlV1 # control #== pfromData (VM.pcekContext'redeemerContextControlHash c)
                                                , pfromData (VM.pcekRedeemer'activeScanHash r) #== pconstant ""
                                                , index #== pfromData (Step.pnative'redeemerCount native) - pfromData (VM.pcekRedeemer'cursor r) - 1
                                                , count #== pfromData (Step.pnative'redeemerCount native)
                                                , Merkle.pverifyMembership # pfromData (Step.pnative'redeemerCount native) # pfromData (Step.pnative'redeemerPeaks native) # index # leaf # pfromData (bytesList $ pelemAt # 5 # f)
                                                , frontier #>= 0
                                                , frontier #< pfromData (Step.pnative'purposeCount native)
                                                , Merkle.pverifyMembership # pfromData (Step.pnative'purposeCount native) # pfromData (Step.pnative'purposePeaks native) # frontier # (Script.ppurposeLeafHash # kind # purposeIndex # scriptHash # subject) # pfromData (bytesList $ pelemAt # 11 # f)
                                                ]
                                            )
                                            ( pforgetData $
                                                pdata $
                                                    pcon $
                                                        Redeemer.PSelectionPending
                                                            (pforgetData $ pdata staged)
                                                            (pdata control)
                                                            (pdata index)
                                                            (pdata count)
                                                            (integerField f 3)
                                                            (pdata commitment)
                                                            (pdata leaf)
                                                            (pdata kind)
                                                            (pdata purposeIndex)
                                                            (pdata scriptHash)
                                                            (pdata subject)
                                            )
                                            perror

initializeValidator :: forall s. Hop s
initializeValidator = hop $ \raw action -> pmatch action $ \a ->
    plet (Redeemer.pdecodeSelection # raw) $ \selection -> pmatch selection $ \s ->
        pmatch (Chain.pdecodeStaged # Redeemer.pselection'staged s) $ \staged -> pmatch (pfromData $ Chain.pstaged'context staged) $ \c ->
            pif
                (pfromData (VM.pcekContext'stage c) #== 9 #&& Chain.pauxiliaryIsBound # pfromData (Chain.pstaged'bound staged) # Chain.paction'auxiliary a)
                ( pmatch (ScriptContext.pscriptPurposeSummaryV1 # pfromData (Redeemer.pselection'purposeKind s) # pfromData (Redeemer.pselection'scriptHash s) # pfromData (Redeemer.pselection'subject s) # (pfromData (VM.pcekContext'languageTag c) #== 128)) $ \purpose ->
                    let next summary = pforgetData $ pdata $ pcon $ Redeemer.PPurposePending (pdata selection) (pdata summary)
                     in case purpose of
                            PNothing -> pif (pfromData (Redeemer.pselection'purposeKind s) #== 3 #&& pfromData (VM.pcekContext'languageTag c) #== 3) (next $ pcon PDNothing) perror
                            PJust summary -> next $ pcon $ PDJust $ pdata summary
                )
                perror

hashValidator :: forall s. Hop s
hashValidator = hop $ \raw _ -> pmatch (Redeemer.pdecodePurpose # raw) $ \pending ->
    pmatch (pfromData $ Redeemer.ppurpose'selection pending) $ \selection ->
        plet (pmatch (pfromData $ Redeemer.ppurpose'purpose pending) $ \case PDNothing -> Item.pmodeDescriptor; PDJust _ -> Item.pmodeData) $ \mode ->
            pmatch (VM.predeemerTagForPurposeKindV1 # pfromData (Redeemer.pselection'purposeKind selection)) $ \case
                PNothing -> perror
                PJust kind ->
                    pforgetData $
                        pdata $
                            pcon $
                                Redeemer.PInitialPending
                                    (Redeemer.ppurpose'selection pending)
                                    (Redeemer.ppurpose'purpose pending)
                                    (pdata $ Item.phashControlV1 # (Item.pinitialControlV1 # mode # pfromData (Redeemer.pselection'itemIndex selection) # pfromData (Redeemer.pselection'itemCount selection) # pfromData (Redeemer.pselection'totalLength selection) # pfromData (Redeemer.pselection'itemCommitment selection) # kind # pfromData (Redeemer.pselection'purposeIndex selection)))

finishValidator :: forall s. Hop s
finishValidator = hop $ \raw action -> pmatch action $ \a -> pmatch (Redeemer.pdecodeInitial # raw) $ \pending ->
    pmatch (pfromData $ Redeemer.pinitial'selection pending) $ \selection ->
        plet (Chain.pdecodeStaged # Redeemer.pselection'staged selection) $ \staged -> pmatch staged $ \s ->
            pmatch (pfromData $ Chain.pstaged'context s) $ \c -> pmatch (pfromData $ Redeemer.pselection'control selection) $ \current ->
                pif
                    (pfromData (VM.pcekContext'stage c) #== 9 #&& Chain.pauxiliaryIsBound # pfromData (Chain.pstaged'bound s) # Chain.paction'auxiliary a)
                    ( pforgetData $
                        pdata $
                            pcon $
                                Chain.PVerified
                                    (Redeemer.pselection'staged selection)
                                    ( pdata $
                                        pcon $
                                            Chain.PContinueContext $
                                                pdata $
                                                    Redeemer.pcontextSuccessor
                                                        # staged
                                                        # pcon
                                                            current
                                                                { VM.pcekRedeemer'activeScanHash = Redeemer.pinitial'hash pending
                                                                , VM.pcekRedeemer'activeRedeemerLeaf = Redeemer.pselection'redeemerLeaf selection
                                                                , VM.pcekRedeemer'activePurpose = pmatch (pfromData $ Redeemer.pinitial'purpose pending) $ \case
                                                                    PDNothing -> VM.pcekRedeemer'activePurpose current
                                                                    PDJust summary -> summary
                                                                }
                                    )
                    )
                    perror
