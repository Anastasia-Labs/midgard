-- | Observer authentication through the field door and the separate ordered fold.
module Midgard.Validators.FraudProofs.ValidationTrace.CekContextObserver where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.BoundedCollection (pmaxTxSizeDerivedItemCount)
import Midgard.CekContextChain qualified as Chain
import Midgard.CekContextStep qualified as Step
import Midgard.CekData qualified as Data
import Midgard.ComputationThread (PStepDatum (..))
import Midgard.FraudProofs.NativeTx.Compact qualified as Compact
import Midgard.FraudProofs.NativeTx.Types (PNativeTxBodyCompact (..), PNativeTxCompact (..), PVerifiedMidgardNativeTxCompact (..))
import Midgard.NativeTxFieldAccess qualified as Field
import Midgard.ValidationMachine qualified as VM
import Midgard.ValidationMachineFieldDoor qualified as Door
import Midgard.ValidationResolutionData (bytesField, integerField, recordFields)
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectState, pstep)
import Midgard.Validators.FraudProofs.ValidationTrace.CekContext (Hop, continue, hop)
import Plutarch.LedgerApi.V3
import Plutarch.Prelude

data POpening s = PEmptyObservers | PFinishedObservers | PObserverItem (Term s (PAsData PInteger)) (Term s (PAsData PByteString))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct POpening)
data PPending s = PPending {ppending'staged :: Term s PData, ppending'opening :: Term s (PAsData POpening)}
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PPending)

pdecodeOpening :: forall s. Term s (PData :--> POpening)
pdecodeOpening = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag f) ->
    pif (tag #== 0 #&& pnull # f) (pcon PEmptyObservers) $
        pif (tag #== 1 #&& pnull # f) (pcon PFinishedObservers) $
            pif (tag #== 2 #&& plength # f #== 2) (pcon $ PObserverItem (integerField f 0) (bytesField f 1)) perror
pdecodePending :: forall s. Term s (PData :--> PPending)
pdecodePending = phoistAcyclic $ plam $ \raw -> plet (recordFields 2 raw) $ \f ->
    pcon $ PPending (phead # f) (pdata $ pdecodeOpening # (pelemAt # 1 # f))

pdecodeCarriage :: forall s. Term s (PData :--> Field.PFieldCarriageV1)
pdecodeCarriage = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag f) ->
    pif (tag #== 0 #&& plength # f #== 1) (pcon $ Field.PInline $ bytesField f 0) $
        pif (tag #== 1 #&& plength # f #== 1) (pcon $ Field.PRawUtxo $ integerField f 0) $
            pif
                (tag #== 2 #&& plength # f #== 2)
                (pcon $ Field.PCertified (integerField f 0) (pdata $ pmap # plam (\x -> pdata $ pasInt # x) # (pasList # (pelemAt # 1 # f))))
                perror

pnextObserver :: forall s. Term s (Chain.PStaged :--> POpening :--> Chain.PSuccessor)
pnextObserver = phoistAcyclic $ plam $ \staged opening -> pmatch staged $ \s ->
    plet (pfromData $ Chain.pstaged'context s) $ \context -> pmatch context $ \c ->
        plet (pfromData $ VM.pcekContext'observerItems c) $ \items -> pmatch items $ \i ->
            plet (pfromData (VM.pcekContext'languageTag c) #== 128) $ \midgard ->
                let finished = pcon c{VM.pcekContext'stage = pdata 6, VM.pcekContext'observerSummary = pdata $ VM.pfinalizeCekObserverItemsV1 # items # midgard}
                 in pcon $ Chain.PContinueContext $ pdata $ pmatch opening $ \case
                        PEmptyObservers -> pif (VM.pobserverContextIsPristine # context) finished perror
                        PFinishedObservers -> pif (pfromData (VM.pcekContext'observerCount c) #> 0 #&& Data.pseq'length i #== VM.pcekContext'observerCount c) finished perror
                        PObserverItem count hash ->
                            pif
                                ( pfromData count
                                    #> 0
                                    #&& pfromData count
                                    #<= pmaxTxSizeDerivedItemCount
                                    #&& plengthBS
                                    # pfromData hash
                                    #== 28
                                    #&& pif (pfromData (Data.pseq'length i) #== 0) (pfromData (VM.pcekContext'previousObserver c) #== pconstant "") (pfromData hash #< pfromData (VM.pcekContext'previousObserver c))
                                )
                                (pcon c{VM.pcekContext'observerCount = count, VM.pcekContext'observerItems = pdata $ VM.pprependCekObserverItemV1 # pfromData hash # midgard # items, VM.pcekContext'previousObserver = hash})
                                perror

authenticateValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
authenticateValidator = plam $ \nextHash policy certificate ctx -> pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PData policy datum redeemer ownRef tx $ \raw ->
        plet (Chain.pdecodeAction # raw) $ \action -> pmatch action $ \a ->
            plet (pexpectDatum datum) $ \step -> pmatch step $ \d ->
                plet (Chain.pdecodeStaged # pexpectState (pstep'data d)) $ \staged -> pmatch staged $ \s ->
                    pmatch (pfromData $ Chain.pstaged'bound s) $ \bound -> pmatch (pfromData $ Chain.pstaged'native s) $ \native ->
                        pmatch (pfromData $ Chain.pstaged'context s) $ \context -> pmatch (pfromData $ VM.pcekContext'observerItems context) $ \items ->
                            pmatch (Compact.pverifyNativeTxProofSourceV1 # pfromData (Chain.pbound'transactionId bound) # pfromData (Step.pnative'compactCbor native) # pfromData (Step.pnative'witnessSetCompactCbor native) # pfromData (Step.pnative'fieldPreimageLengthsCbor native)) $ \(PPair source witnesses) ->
                                pmatch source $ \verified -> pmatch (pverified'txCompact verified) $ \compact -> pmatch (pcompact'body compact) $ \body ->
                                    pif
                                        (pfromData (VM.pcekContext'stage context) #== 5 #&& Chain.pauxiliaryIsBound # pfromData (Chain.pstaged'bound s) # Chain.paction'auxiliary a #&& pverified'version verified #== 1)
                                        ( plet
                                            ( pif
                                                (pbodyCompact'requiredObserversHash body #== Field.pemptyFieldCommitment)
                                                (pif (Step.pnoAuxiliary $ Chain.paction'auxiliary a) (pcon PEmptyObservers) perror)
                                                ( pif
                                                    (pfromData (VM.pcekContext'observerCount context) #> 0 #&& Data.pseq'length items #== VM.pcekContext'observerCount context)
                                                    (pif (Step.pnoAuxiliary $ Chain.paction'auxiliary a) (pcon PFinishedObservers) perror)
                                                    ( plet (Step.pauxiliaryFields 1 3 $ Chain.paction'auxiliary a) $ \f ->
                                                        plet (pdecodeCarriage # (pelemAt # 2 # f)) $ \carriage -> pmatch tx $ \t ->
                                                            plet (pcon $ Door.PMachineFieldDoorV1 (pfromData $ ptxInfo'referenceInputs t) certificate) $ \door ->
                                                                plet (pif (pfromData (VM.pcekContext'observerCount context) #== 0) (Door.pmachineFieldCount # door # source # witnesses # 3 # carriage) (pfromData $ VM.pcekContext'observerCount context)) $ \count ->
                                                                    plet (count - pfromData (Data.pseq'length items) - 1) $ \index ->
                                                                        pif
                                                                            (pfromData (integerField f 0) #== 3 #&& pfromData (integerField f 1) #== index #&& index #>= 0)
                                                                            ( plet (Door.popenMachineFieldItem # door # source # witnesses # 3 # index # carriage) $ \item ->
                                                                                pif
                                                                                    (Door.pmachineFieldItemCount # item #== count #&& Door.pmachineFieldItemLength # item #== 28)
                                                                                    (pcon $ PObserverItem (pdata count) (pdata $ Door.pmachineFieldItemBytes # item))
                                                                                    perror
                                                                            )
                                                                            perror
                                                    )
                                                )
                                            )
                                            $ \opening ->
                                                continue policy step action ownRef tx nextHash (pforgetData $ pdata $ pcon $ PPending (pforgetData $ pdata staged) (pdata opening))
                                        )
                                        perror

foldValidator :: forall s. Hop s
foldValidator = hop $ \raw action -> pmatch action $ \a -> pmatch (pdecodePending # raw) $ \pending ->
    plet (Chain.pdecodeStaged # ppending'staged pending) $ \staged -> pmatch staged $ \s -> pmatch (pfromData $ Chain.pstaged'context s) $ \c ->
        pif
            (pfromData (VM.pcekContext'stage c) #== 5 #&& Chain.pauxiliaryIsBound # pfromData (Chain.pstaged'bound s) # Chain.paction'auxiliary a)
            (pforgetData $ pdata $ pcon $ Chain.PVerified (ppending'staged pending) (pdata $ pnextObserver # staged # pfromData (ppending'opening pending)))
            perror
