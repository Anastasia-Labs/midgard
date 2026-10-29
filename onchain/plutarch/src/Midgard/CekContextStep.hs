-- | Authenticated native facts consumed by the CEK context physical stages.
module Midgard.CekContextStep where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.CekConstant qualified as Constant
import Midgard.CekContextWire qualified as Wire
import Midgard.CekData qualified as Data
import Midgard.CekMachine qualified as Machine
import Midgard.CekProof qualified as Proof
import Midgard.CekSelection (bytesList)
import Midgard.FraudProofs.NativeTx.Compact qualified as Compact
import Midgard.FraudProofs.NativeTx.Types (PNativeTxBodyCompact (..), PNativeTxCompact (..))
import Midgard.ScriptContext qualified as Context
import Midgard.ValidationMachine qualified as VM
import Midgard.ValidationMerkle (PFrontierPeak)
import Midgard.ValidationMerkle qualified as Merkle
import Midgard.ValidationResolutionData (bytesField, integerField)
import Plutarch.Prelude

data PNativeFacts s = PNativeFacts
    { pnative'compactCbor :: Term s (PAsData PByteString)
    , pnative'witnessSetCompactCbor :: Term s (PAsData PByteString)
    , pnative'fieldPreimageLengthsCbor :: Term s (PAsData PByteString)
    , pnative'resolvedInputCount :: Term s (PAsData PInteger)
    , pnative'spendInputCount :: Term s (PAsData PInteger)
    , pnative'resolvedItemPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
    , pnative'signerCount :: Term s (PAsData PInteger)
    , pnative'signerFrontierCommitment :: Term s (PAsData PByteString)
    , pnative'redeemerCount :: Term s (PAsData PInteger)
    , pnative'redeemerPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
    , pnative'purposeCount :: Term s (PAsData PInteger)
    , pnative'purposePeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
    , pnative'outputCount :: Term s (PAsData PInteger)
    , pnative'outputDescriptorPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
    , pnative'mintCount :: Term s (PAsData PInteger)
    , pnative'mintPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PNativeFacts)

pnativeFacts :: forall s. Term s (VM.PNativeScriptsControlV1 :--> PNativeFacts)
pnativeFacts = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
    pcon $
        PNativeFacts
            (VM.pnativeControl'compactCbor c)
            (VM.pnativeControl'witnessSetCompactCbor c)
            (VM.pnativeControl'fieldPreimageLengthsCbor c)
            (VM.pnativeControl'resolvedInputCount c)
            (VM.pnativeControl'spendInputCount c)
            (VM.pnativeControl'resolvedItemPeaks c)
            (VM.pnativeControl'signerCount c)
            (VM.pnativeControl'signerFrontierCommitment c)
            (VM.pnativeControl'redeemerCount c)
            (VM.pnativeControl'redeemerPeaks c)
            (VM.pnativeControl'purposeCount c)
            (VM.pnativeControl'purposePeaks c)
            (VM.pnativeControl'outputCount c)
            (VM.pnativeControl'outputDescriptorPeaks c)
            (VM.pnativeControl'mintCount c)
            (VM.pnativeControl'mintPeaks c)

pauxiliaryFields :: forall s. Term s PInteger -> Term s PInteger -> Term s PData -> Term s (PBuiltinList PData)
pauxiliaryFields expected count raw = pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== expected #&& plength # fields #== count) fields perror
pnoAuxiliary :: forall s. Term s PData -> Term s PBool
pnoAuxiliary raw = pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) -> tag #== 0 #&& pnull # fields

pnextResolved :: forall s. Term s (VM.PCekContextControlV1 :--> PNativeFacts :--> PData :--> PInteger :--> VM.PCekContextControlV1)
pnextResolved = phoistAcyclic $ plam $ \context native auxiliary sourceKind -> pmatch context $ \c -> pmatch native $ \n ->
    plet (pif (sourceKind #== 1) (pfromData (pnative'resolvedInputCount n) - pfromData (pnative'spendInputCount n)) (pfromData $ pnative'spendInputCount n)) $ \count ->
        plet (pfromData $ pif (sourceKind #== 1) (VM.pcekContext'referenceItems c) (VM.pcekContext'spendItems c)) $ \current -> pmatch current $ \items ->
            pif
                (pfromData (Data.pseq'length items) #== count)
                (pif (pnoAuxiliary auxiliary) (pcon c{VM.pcekContext'stage = pdata $ pif (sourceKind #== 1) 2 3}) perror)
                ( plet (pauxiliaryFields 13 5 auxiliary) $ \f ->
                    pif
                        (pfromData (integerField f 0) #== sourceKind)
                        ( pmatch
                            ( Context.pprependResolvedDescriptorTxInInfoV1
                                # pfromData (pnative'resolvedInputCount n)
                                # pfromData (pnative'resolvedItemPeaks n)
                                # pfromData (pnative'spendInputCount n)
                                # pfromData (integerField f 0)
                                # pfromData (integerField f 1)
                                # pfromData (bytesField f 2)
                                # pfromData (bytesField f 3)
                                # pfromData (bytesList $ pelemAt # 4 # f)
                                # (pfromData (VM.pcekContext'languageTag c) #== 128)
                                # current
                            )
                            $ \case
                                PNothing -> perror
                                PJust next -> pif (sourceKind #== 1) (pcon c{VM.pcekContext'referenceItems = pdata next}) (pcon c{VM.pcekContext'spendItems = pdata next})
                        )
                        perror
                )

pnextOutput :: forall s. Term s (VM.PCekContextControlV1 :--> PNativeFacts :--> PData :--> VM.PCekContextControlV1)
pnextOutput = phoistAcyclic $ plam $ \context native auxiliary -> pmatch context $ \c -> pmatch native $ \n -> pmatch (pfromData $ VM.pcekContext'outputItems c) $ \items ->
    pif
        (Data.pseq'length items #== pnative'outputCount n)
        (pif (pnoAuxiliary auxiliary) (pcon c{VM.pcekContext'stage = pdata 4}) perror)
        ( plet (pauxiliaryFields 14 3 auxiliary) $ \f -> pmatch
            ( Context.pprependOutputDescriptorV1
                # pfromData (pnative'outputCount n)
                # pfromData (pnative'outputDescriptorPeaks n)
                # pfromData (integerField f 0)
                # pfromData (bytesField f 1)
                # pfromData (bytesList $ pelemAt # 2 # f)
                # (pfromData (VM.pcekContext'languageTag c) #== 128)
                # pfromData (VM.pcekContext'outputItems c)
            )
            $ \case
                PNothing -> perror
                PJust next -> pcon c{VM.pcekContext'outputItems = pdata next}
        )

pnextSigner :: forall s. Term s (VM.PCekContextControlV1 :--> PNativeFacts :--> PData :--> VM.PCekContextControlV1)
pnextSigner = phoistAcyclic $ plam $ \context native auxiliary -> pmatch context $ \c -> pmatch native $ \n -> pmatch (pfromData $ VM.pcekContext'signerItems c) $ \items ->
    pif
        (Data.pseq'length items #== pnative'signerCount n)
        (pif (pnoAuxiliary auxiliary) (pcon c{VM.pcekContext'stage = pdata 5}) perror)
        ( plet (pauxiliaryFields 15 4 auxiliary) $ \f -> pmatch
            ( Context.pprependSignerV1
                # pfromData (pnative'signerCount n)
                # pfromData (pnative'signerFrontierCommitment n)
                # (pmap # plam (\raw -> pdata $ Wire.pdecodeFrontierPeak # raw) # (pasList # (pelemAt # 0 # f)))
                # pfromData (integerField f 1)
                # pfromData (bytesField f 2)
                # pfromData (bytesList $ pelemAt # 3 # f)
                # pfromData (VM.pcekContext'signerItems c)
            )
            $ \case
                PNothing -> perror
                PJust next -> pcon c{VM.pcekContext'signerItems = pdata next}
        )

pnextMintInit :: forall s. Term s (VM.PCekContextControlV1 :--> PNativeFacts :--> PData :--> VM.PCekContextControlV1)
pnextMintInit = phoistAcyclic $ plam $ \context native auxiliary -> pmatch context $ \c -> pmatch native $ \n ->
    pif
        (pnoAuxiliary auxiliary)
        ( pif
            (pfromData (pnative'mintCount n) #== 0)
            (pcon c{VM.pcekContext'stage = pdata 9, VM.pcekContext'mintSummary = pdata $ Data.pmapDataSummaryV1 # Data.pemptyDataPairSummaryV1})
            (pcon c{VM.pcekContext'stage = pdata 8})
        )
        perror

pnextAssemble :: forall s. Term s (VM.PCekContextControlV1 :--> PByteString :--> PData :--> VM.PCekContextControlV1)
pnextAssemble = phoistAcyclic $ plam $ \context transactionId auxiliary -> pmatch context $ \c ->
    plet (Wire.pdecodeCekContextPartsControl # (phead # pauxiliaryFields 21 1 auxiliary)) $ \control -> pmatch control $ \parts ->
        pif
            (VM.phashCekContextPartsControlV1 # control #== pfromData (VM.pcekContext'redeemerContextControlHash c))
            ( pcon
                c
                    { VM.pcekContext'stage = pdata 12
                    , VM.pcekContext'redeemerContextControlHash =
                        pdata $
                            VM.phashCekTxInfoAssemblyControlV1
                                # ( pcon $
                                        VM.PCekTxInfoAssemblyControlV1
                                            ( pdata $
                                                Context.ptxInfoTailFieldsSummaryV1
                                                    # (pfromData (VM.pcekContext'languageTag c) #== 128)
                                                    # pfromData (VM.pcekContext'observerSummary c)
                                                    # pfromData (VM.pcekContext'signerItems c)
                                                    # pfromData (VM.pcekContext'mintSummary c)
                                                    # pfromData (VM.pcekParts'redeemerItems parts)
                                                    # transactionId
                                            )
                                            (VM.pcekParts'redeemer parts)
                                            (VM.pcekParts'scriptInfo parts)
                                  )
                    }
            )
            perror

pseedHash :: forall s. Term s (VM.PCekContextControlV1 :--> PInteger :--> PData :--> PByteString)
pseedHash = phoistAcyclic $ plam $ \context cursor auxiliary -> pmatch context $ \c ->
    plet (Wire.pdecodeCekFinalContextControl # (phead # pauxiliaryFields 23 1 auxiliary)) $ \control -> pmatch control $ \final ->
        pif
            (VM.phashCekFinalContextControlV1 # control #== pfromData (VM.pcekContext'redeemerContextControlHash c))
            ( plet (Context.pscriptContextSummaryV1 # pfromData (VM.pcekFinal'txInfo final) # pfromData (VM.pcekFinal'redeemer final) # pfromData (VM.pcekFinal'scriptInfo final)) $ \summary ->
                Machine.phashStateV1
                    # ( pcon $
                            Machine.PMachineStateV1
                                (pdata Machine.pmodeCompute)
                                (pdata cursor)
                                (pdata $ Proof.phashApplicationTermV1 # pfromData (VM.pcekContext'programTermRoot c) # (Proof.phashContextConstantTermV1 # (Constant.psemanticDataConstantRootV1 # summary)))
                                (pdata Proof.pemptyEnvironmentRootV1)
                                (pdata Proof.pemptyContinuationRootV1)
                                (pdata 0)
                                (pdata 0)
                                (pdata 0)
                      )
            )
            perror

pnextTxInfo :: forall s. Term s (VM.PCekContextControlV1 :--> PNativeFacts :--> PData :--> VM.PCekContextControlV1)
pnextTxInfo = phoistAcyclic $ plam $ \context native auxiliary -> pmatch context $ \c -> pmatch native $ \n ->
    plet (Wire.pdecodeCekTxInfoAssemblyControl # (phead # pauxiliaryFields 22 1 auxiliary)) $ \control -> pmatch control $ \assembly ->
        pif
            (VM.phashCekTxInfoAssemblyControlV1 # control #== pfromData (VM.pcekContext'redeemerContextControlHash c))
            ( pmatch (Compact.pdecodeNativeTxCompactV1 # pfromData (pnative'compactCbor n)) $ \compact -> pmatch (pcompact'body compact) $ \body ->
                plet
                    ( Context.ptxInfoFromTailSummaryV1
                        # (pfromData (VM.pcekContext'languageTag c) #== 128)
                        # pfromData (VM.pcekContext'spendItems c)
                        # pfromData (VM.pcekContext'referenceItems c)
                        # pfromData (VM.pcekContext'outputItems c)
                        # pbodyCompact'fee body
                        # pbodyCompact'validityIntervalStart body
                        # pbodyCompact'validityIntervalEnd body
                        # pfromData (VM.pcekContext'observerSummary c)
                        # pfromData (VM.pcekContext'mintSummary c)
                        # pfromData (VM.pcekAssembly'tailFields assembly)
                    )
                    $ \txInfo ->
                        pcon
                            c
                                { VM.pcekContext'stage = pdata 13
                                , VM.pcekContext'redeemerContextControlHash =
                                    pdata $
                                        VM.phashCekFinalContextControlV1
                                            # (pcon $ VM.PCekFinalContextControlV1 (pdata txInfo) (VM.pcekAssembly'redeemer assembly) (VM.pcekAssembly'scriptInfo assembly))
                                }
            )
            perror

porderIsValid :: forall s. Term s (VM.PCekContextControlV1 :--> PByteString :--> PByteString :--> PData :--> PBool)
porderIsValid = phoistAcyclic $ plam $ \context policy name previous -> pmatch context $ \c ->
    pmatch (pasConstr # previous) $ \(PBuiltinPair tag values) ->
        let none = tag #== 1 #&& pnull # values
         in pif
                (pfromData (VM.pcekContext'currentMintPolicy c) #== pconstant "")
                (none #&& pfromData (VM.pcekContext'mintCursor c) #== 0)
                ( pif
                    (policy #== pfromData (VM.pcekContext'currentMintPolicy c))
                    ( pif
                        (tag #== 0 #&& plength # values #== 1)
                        ( plet (pauxiliaryFields 0 3 $ phead # values) $ \headFields ->
                            plet (pfromData $ bytesField headFields 0) $ \assetName ->
                                name #< assetName #&& VM.pprependCekMintAssetSummary # assetName # pfromData (integerField headFields 1) # (Wire.pdecodeDataSequenceSummary # (pelemAt # 2 # headFields)) #== pfromData (VM.pcekContext'currentMintAssets c)
                        )
                        perror
                    )
                    (none #&& policy #< pfromData (VM.pcekContext'currentMintPolicy c))
                )

pnextMintItem :: forall s. Term s (VM.PCekContextControlV1 :--> PNativeFacts :--> PData :--> VM.PCekContextControlV1)
pnextMintItem = phoistAcyclic $ plam $ \context native auxiliary -> pmatch context $ \c -> pmatch native $ \n ->
    pif
        (VM.pcekContext'mintCursor c #== pnative'mintCount n)
        ( pif
            (pnoAuxiliary auxiliary)
            ( plet (VM.pfinalizeCurrentCekMintPolicy # context) $ \policies ->
                pcon
                    c
                        { VM.pcekContext'stage = pdata 9
                        , VM.pcekContext'currentMintPolicy = pdata $ pconstant ""
                        , VM.pcekContext'currentMintAssets = pdata Data.pemptyDataPairSummaryV1
                        , VM.pcekContext'mintPolicies = pdata policies
                        , VM.pcekContext'mintSummary = pdata $ Data.pmapDataSummaryV1 # policies
                        }
            )
            perror
        )
        ( plet (pauxiliaryFields 16 6 auxiliary) $ \f ->
            plet (pfromData $ bytesField f 1) $ \policy -> plet (pfromData $ bytesField f 2) $ \name -> plet (pfromData $ integerField f 3) $ \quantity ->
                pif
                    ( porderIsValid
                        # context
                        # policy
                        # name
                        # (pelemAt # 5 # f)
                        #&& Merkle.pverifyMembership
                        # pfromData (pnative'mintCount n)
                        # pfromData (pnative'mintPeaks n)
                        # pfromData (integerField f 0)
                        # (VM.pmintAssetLeafHash # policy # name # quantity)
                        # pfromData (bytesList $ pelemAt # 4 # f)
                    )
                    ( pif
                        (pfromData (VM.pcekContext'currentMintPolicy c) #== pconstant "")
                        ( pcon
                            c
                                { VM.pcekContext'mintCursor = pdata $ pfromData (VM.pcekContext'mintCursor c) + 1
                                , VM.pcekContext'currentMintPolicy = pdata policy
                                , VM.pcekContext'currentMintAssets = pdata $ VM.pprependCekMintAssetSummary # name # quantity # pfromData (VM.pcekContext'currentMintAssets c)
                                }
                        )
                        ( pif
                            (policy #== pfromData (VM.pcekContext'currentMintPolicy c))
                            ( pcon
                                c
                                    { VM.pcekContext'mintCursor = pdata $ pfromData (VM.pcekContext'mintCursor c) + 1
                                    , VM.pcekContext'currentMintAssets = pdata $ VM.pprependCekMintAssetSummary # name # quantity # pfromData (VM.pcekContext'currentMintAssets c)
                                    }
                            )
                            ( pif
                                (policy #< pfromData (VM.pcekContext'currentMintPolicy c))
                                ( plet (VM.pfinalizeCurrentCekMintPolicy # context) $ \policies ->
                                    pcon
                                        c
                                            { VM.pcekContext'mintCursor = pdata $ pfromData (VM.pcekContext'mintCursor c) + 1
                                            , VM.pcekContext'currentMintPolicy = pdata policy
                                            , VM.pcekContext'currentMintAssets = pdata $ VM.pprependCekMintAssetSummary # name # quantity # Data.pemptyDataPairSummaryV1
                                            , VM.pcekContext'mintPolicies = pdata policies
                                            }
                                )
                                perror
                            )
                        )
                    )
                    perror
        )
