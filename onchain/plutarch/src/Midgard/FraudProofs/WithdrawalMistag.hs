module Midgard.FraudProofs.WithdrawalMistag (
    PStep01State (..),
    PStep01Args (..),
    PStep02State (..),
    PStep02Args (..),
    PWithdrawalLedgerEvidenceV1 (..),
    PStep03State (..),
    PStep03Args (..),
    PStep04State (..),
    PStep04Args (..),
    PStep05Args (..),
    pwithdrawalMistagTestCategoryIdV1,
    pcategoryIsWithdrawalMistagV1,
    pchallengedHeaderHashOfV1,
    pcomputationThreadAssetNameV1,
    ptraceCoordinateIsExactV1,
    pwithdrawalSignatureIsValidV1,
    poutputCoreMatchesWithSignatureV1,
    poutputCoreMatchesV1,
    pwithdrawalLedgerOutrefKeyV1,
    pclassifyLedgerEvidenceV1,
    paddressPayloadLengthV1,
    pexactPayoutOutputBytesV1,
    ppayoutIsExactlyPayableWithRateV1,
    ppayoutIsExactlyPayableV1,
    pestablishMistagV1,
    pmistagFaultIsEstablishedV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_224, pblake2b_256, pverifyEd25519Signature)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
    PAddress (..),
    PCurrencySymbol (..),
    POutputDatum (..),
    PStakingCredential (..),
    PTokenName (..),
 )
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Types (PProof)
import Midgard.Common.Value (pfromAssetList)
import Midgard.FraudProofCatalogue (pidByteCount)
import Midgard.FraudProofs.NativeTx.Components (pencodeMidgardTxInput)
import Midgard.FraudProofs.NativeTx.Types (
    PMidgardAddress (..),
    PMidgardCredential (..),
    PMidgardTxInput (..),
    PMidgardTxOutput (..),
    PMidgardValue (..),
 )
import Midgard.LedgerOutput (pdecodeCanonicalOutput)
import Midgard.LedgerOutputCommitment (PLedgerOutputCommitmentV1 (..), pencodeLedgerOutputCommitment)
import Midgard.LedgerOutputDescriptor (pbuildV1)
import Midgard.LedgerState (
    PEventKey (..),
    PEventToStepValue (..),
    PTransitionPhase (..),
    PTransitionStep (..),
    PWithdrawalBody (..),
    PWithdrawalId,
    PWithdrawalInfo (..),
 )
import Midgard.MpfProof (pdoesNotHave, phasV1)
import Midgard.TransitionTrace (
    PIndexedTraceProof,
    PRootDomain (..),
    PRootMembershipProof (..),
    pverifyIndexedTraceProof,
    pverifyRootMembershipWithBytes,
 )
import Midgard.ValidationMachine (pcoinsPerUtxoByte, pminAdaLovelaceV1, poutputMeetsMinAdaV1)

-- | Aiken's pre-registration Q41 test category. Production allocates afresh.
pwithdrawalMistagTestCategoryIdV1 :: forall s. Term s PByteString
pwithdrawalMistagTestCategoryIdV1 = phexByteStr "00000014"

pcategoryIsWithdrawalMistagV1 :: forall s. Term s (PAsData PTokenName :--> PBool)
pcategoryIsWithdrawalMistagV1 = phoistAcyclic $ plam $ \assetName ->
    plet (pto $ pfromData assetName) $ \name ->
        plengthBS # name #== pidByteCount
            + 28
                #&& psliceBS
                # 0
                # pidByteCount
                # name
                #== pwithdrawalMistagTestCategoryIdV1

pchallengedHeaderHashOfV1 :: forall s. Term s (PAsData PTokenName :--> PByteString)
pchallengedHeaderHashOfV1 = phoistAcyclic $ plam $ \assetName ->
    psliceBS # pidByteCount # 28 # (pto $ pfromData assetName)

pcomputationThreadAssetNameV1 :: forall s. Term s (PByteString :--> PTokenName)
pcomputationThreadAssetNameV1 = phoistAcyclic $ plam $ \headerHash ->
    pcon $ PTokenName $ pwithdrawalMistagTestCategoryIdV1 <> headerHash

data PStep01State (s :: S) = PStep01State
    { pstep01State'challengedHeaderHash :: Term s (PAsData PByteString)
    , pstep01State'withdrawalId :: Term s (PAsData PWithdrawalId)
    , pstep01State'withdrawalInfoHash :: Term s (PAsData PByteString)
    , pstep01State'claimedValid :: Term s (PAsData PBool)
    , pstep01State'eventToStepRoot :: Term s (PAsData PByteString)
    , pstep01State'totalEventCount :: Term s (PAsData PInteger)
    , pstep01State'transitionTraceRoot :: Term s (PAsData PByteString)
    , pstep01State'transitionStepCount :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep01State)

data PStep01Args (s :: S) = PStep01Args
    { pstep01Args'inputIndex :: Term s (PAsData PInteger)
    , pstep01Args'outputIndex :: Term s (PAsData PInteger)
    , pstep01Args'hubRefInputIndex :: Term s (PAsData PInteger)
    , pstep01Args'stateQueueNodeRefInputIndex :: Term s (PAsData PInteger)
    , pstep01Args'committedWithdrawal :: Term s (PAsData PRootMembershipProof)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02State (s :: S) = PStep02State
    { pstep02State'challengedHeaderHash :: Term s (PAsData PByteString)
    , pstep02State'withdrawalId :: Term s (PAsData PWithdrawalId)
    , pstep02State'withdrawalInfoHash :: Term s (PAsData PByteString)
    , pstep02State'claimedValid :: Term s (PAsData PBool)
    , pstep02State'preUtxosRoot :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

data PStep02Args (s :: S) = PStep02Args
    { pstep02Args'inputIndex :: Term s (PAsData PInteger)
    , pstep02Args'outputIndex :: Term s (PAsData PInteger)
    , pstep02Args'withdrawalInfo :: Term s PData
    , pstep02Args'eventToStep :: Term s (PAsData PRootMembershipProof)
    , pstep02Args'transitionStep :: Term s (PAsData PIndexedTraceProof)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PWithdrawalLedgerEvidenceV1 (s :: S)
    = PPresentLedgerOutput
        { ppresentLedgerOutput'outputCbor :: Term s (PAsData PByteString)
        , ppresentLedgerOutput'membershipProof :: Term s (PAsData PProof)
        }
    | PAbsentLedgerOutput
        { pabsentLedgerOutput'nonMembershipProof :: Term s (PAsData PProof)
        }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PWithdrawalLedgerEvidenceV1)

data PStep03State (s :: S) = PStep03State
    { pstep03State'challengedHeaderHash :: Term s (PAsData PByteString)
    , pstep03State'withdrawalId :: Term s (PAsData PWithdrawalId)
    , pstep03State'withdrawalBodyHash :: Term s (PAsData PByteString)
    , pstep03State'claimedValid :: Term s (PAsData PBool)
    , pstep03State'outputPresent :: Term s (PAsData PBool)
    , pstep03State'coreValid :: Term s (PAsData PBool)
    , pstep03State'cardanoValueSize :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

data PStep03Args (s :: S) = PStep03Args
    { pstep03Args'inputIndex :: Term s (PAsData PInteger)
    , pstep03Args'outputIndex :: Term s (PAsData PInteger)
    , pstep03Args'withdrawalInfo :: Term s PData
    , pstep03Args'evidence :: Term s (PAsData PWithdrawalLedgerEvidenceV1)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PStep04State (s :: S) = PStep04State
    { pstep04State'challengedHeaderHash :: Term s (PAsData PByteString)
    , pstep04State'withdrawalId :: Term s (PAsData PWithdrawalId)
    , pstep04State'claimedValid :: Term s (PAsData PBool)
    , pstep04State'actualValid :: Term s (PAsData PBool)
    , pstep04State'exactOutputBytes :: Term s (PAsData PInteger)
    , pstep04State'requiredLovelace :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep04State)

data PStep04Args (s :: S) = PStep04Args
    { pstep04Args'inputIndex :: Term s (PAsData PInteger)
    , pstep04Args'outputIndex :: Term s (PAsData PInteger)
    , pstep04Args'withdrawalBody :: Term s PData
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

data PStep05Args (s :: S) = PStep05Args
    { pstep05Args'inputIndex :: Term s (PAsData PInteger)
    , pstep05Args'outputIndex :: Term s (PAsData PInteger)
    , pstep05Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep05Args)

ptraceCoordinateIsExactV1 ::
    forall s.
    Term s PStep01State ->
    Term s PRootMembershipProof ->
    Term s PIndexedTraceProof ->
    Term s PBool
ptraceCoordinateIsExactV1 inputState eventToStep transitionStep =
    P.do
        PStep01State
            { pstep01State'withdrawalId
            , pstep01State'eventToStepRoot
            , pstep01State'totalEventCount
            , pstep01State'transitionTraceRoot
            , pstep01State'transitionStepCount
            } <-
            pmatch inputState
        let eventKey = pcon $ PWithdrawalEventKey pstep01State'withdrawalId
        PRootMembershipProof{prootMembership'key, prootMembership'value} <- pmatch eventToStep
        PEventToStepValue{peventToStepValue'stepIndex, peventToStepValue'phase} <-
            pmatch $ pfromData $ punsafeCoerce @(PAsData PEventToStepValue) prootMembership'value
        PRootMembershipProof
            { prootMembership'key = transitionKey
            , prootMembership'value = transitionValue
            } <-
            pmatch transitionStep
        PTransitionStep
            { ptransitionStep'stepIndex
            , ptransitionStep'eventKey
            , ptransitionStep'phase
            } <-
            pmatch $ pfromData $ punsafeCoerce @(PAsData PTransitionStep) transitionValue
        pand'List
            [ pverifyRootMembershipWithBytes
                eventToStep
                (pdata $ pcon PEventToStepRootDomain)
                (pfromData pstep01State'eventToStepRoot)
                (pfromData pstep01State'totalEventCount)
                (pserialiseData # prootMembership'key)
                (pserialiseData # prootMembership'value)
            , pverifyIndexedTraceProof
                transitionStep
                (pfromData pstep01State'transitionTraceRoot)
                (pfromData pstep01State'transitionStepCount)
            , prootMembership'key #== pforgetData (pdata eventKey)
            , peventToStepValue'phase #== pdata (pcon PWithdrawal)
            , transitionKey #== pforgetData peventToStepValue'stepIndex
            , ptransitionStep'stepIndex #== peventToStepValue'stepIndex
            , ptransitionStep'eventKey #== pdata eventKey
            , ptransitionStep'phase #== pdata (pcon PWithdrawal)
            ]

pwithdrawalSignatureDomainV1 :: forall s. Term s PByteString
pwithdrawalSignatureDomainV1 = pconstant "MidgardWithdrawalV1"

pmaximumWithdrawalAssetCountV1 :: forall s. Term s PInteger
pmaximumWithdrawalAssetCountV1 = 100

pwithdrawalSignatureIsValidV1 :: forall s. Term s (PWithdrawalInfo :--> PBool)
pwithdrawalSignatureIsValidV1 = phoistAcyclic $ plam $ \info -> P.do
    PWithdrawalInfo{pwithdrawalInfo'body, pwithdrawalInfo'signature} <- pmatch info
    PWithdrawalBody{pwithdrawalBody'l2Owner} <- pmatch $ pfromData pwithdrawalInfo'body
    let signatureItems = pasList # pwithdrawalInfo'signature
    pif (plength # signatureItems #== 2) `flip` (pconstant False) $
        plet (pasByteStr #$ pelemAt # 0 # signatureItems) $ \verificationKey ->
            plet (pasByteStr #$ pelemAt # 1 # signatureItems) $ \signature ->
                plet (pblake2b_256 #$ pwithdrawalSignatureDomainV1 <> (pserialiseData # pforgetData pwithdrawalInfo'body)) $ \message ->
                    plengthBS
                        # verificationKey
                        #== 32
                        #&& plengthBS
                        # signature
                        #== 64
                        #&& pblake2b_224
                        # verificationKey
                        #== pasByteStr
                        # pwithdrawalBody'l2Owner
                        #&& pverifyEd25519Signature
                        # verificationKey
                        # message
                        # signature

pnativeValueToCardano :: forall s. Term s PMidgardValue -> Term s Value.PSortedValue
pnativeValueToCardano value = P.do
    PMidgardValue{pvalue'lovelace, pvalue'assets} <- pmatch value
    let ada = Value.psingletonSortedValue # Value.padaSymbol # Value.padaToken # pfromData pvalue'lovelace
    pfoldl
        # plam
            ( \acc item ->
                pmatch item $ \(PBuiltinPair unitD quantityD) ->
                    plet (pfromData unitD) $ \unit ->
                        plet (pfromData quantityD) $ \quantity ->
                            Value.punionWith
                                # plam (+)
                                # acc
                                # ( Value.psingletonSortedValue
                                        # pcon (PCurrencySymbol $ psliceBS # 0 # 28 # unit)
                                        # pcon (PTokenName $ psliceBS # 28 # (plengthBS # unit - 28) # unit)
                                        # quantity
                                  )
            )
        # ada
        # pto (pfromData pvalue'assets)

poutputCoreMatchesWithSignatureV1 ::
    forall s.
    Term s (PWithdrawalInfo :--> PMidgardTxOutput :--> PBool :--> PBool)
poutputCoreMatchesWithSignatureV1 = phoistAcyclic $ plam $ \info output signatureIsValid -> P.do
    PWithdrawalInfo{pwithdrawalInfo'body} <- pmatch info
    PWithdrawalBody{pwithdrawalBody'l2Owner, pwithdrawalBody'l2Value} <- pmatch $ pfromData pwithdrawalInfo'body
    PMidgardTxOutput{ptxOutput'address, ptxOutput'value} <- pmatch output
    PMidgardAddress{paddress'paymentCredential} <- pmatch $ pfromData ptxOutput'address
    PMidgardValue{pvalue'assets} <- pmatch $ pfromData ptxOutput'value
    let ownerMatches = pmatch (pfromData paddress'paymentCredential) $ \case
            PMidgardPubKeyCredential owner -> owner #== punsafeCoerce pwithdrawalBody'l2Owner
            PMidgardScriptCredential _ -> pconstant False
    ownerMatches
        #&& pnativeValueToCardano (pfromData ptxOutput'value)
        #== pfromAssetList
        # pfromData pwithdrawalBody'l2Value
        #&& plength
        # pto (pfromData pvalue'assets)
        #<= pmaximumWithdrawalAssetCountV1
        #&& signatureIsValid

poutputCoreMatchesV1 :: forall s. Term s (PWithdrawalInfo :--> PMidgardTxOutput :--> PBool)
poutputCoreMatchesV1 = phoistAcyclic $ plam $ \info output ->
    poutputCoreMatchesWithSignatureV1 # info # output # (pwithdrawalSignatureIsValidV1 # info)

poutrefOutputIndex :: forall s. Term s PData -> Term s PInteger
poutrefOutputIndex outref =
    pmatch (pasConstr # outref) $ \(PBuiltinPair _ fields) ->
        pasInt #$ pelemAt # 1 # fields

pwithdrawalLedgerOutrefKeyV1 :: forall s. Term s (PData :--> PByteString)
pwithdrawalLedgerOutrefKeyV1 = phoistAcyclic $ plam $ \outref ->
    pmatch (pasConstr # outref) $ \(PBuiltinPair _ fields) ->
        pencodeMidgardTxInput
            # pcon
                ( PMidgardTxInput
                    (pdata $ pasByteStr #$ pelemAt # 0 # fields)
                    (pdata $ pasInt #$ pelemAt # 1 # fields)
                )

pclassifyLedgerEvidenceV1 ::
    forall s.
    Term s PStep02State ->
    Term s PWithdrawalInfo ->
    Term s PWithdrawalLedgerEvidenceV1 ->
    Term s PStep03State
pclassifyLedgerEvidenceV1 inputState withdrawalInfo evidence = P.do
    PStep02State
        { pstep02State'challengedHeaderHash
        , pstep02State'withdrawalId
        , pstep02State'withdrawalInfoHash
        , pstep02State'claimedValid
        , pstep02State'preUtxosRoot
        } <-
        pmatch inputState
    PWithdrawalInfo{pwithdrawalInfo'body} <- pmatch withdrawalInfo
    PWithdrawalBody{pwithdrawalBody'l2Outref} <- pmatch $ pfromData pwithdrawalInfo'body
    let infoHash = pblake2b_256 #$ pserialiseData # pforgetData (pdata withdrawalInfo)
        bodyHash = pblake2b_256 #$ pserialiseData # pforgetData pwithdrawalInfo'body
        key = pwithdrawalLedgerOutrefKeyV1 # pwithdrawalBody'l2Outref
        result outputPresent coreValid cardanoValueSize =
            pcon $
                PStep03State
                    pstep02State'challengedHeaderHash
                    pstep02State'withdrawalId
                    (pdata bodyHash)
                    pstep02State'claimedValid
                    (pdata outputPresent)
                    (pdata coreValid)
                    (pdata cardanoValueSize)
    pif (infoHash #== pfromData pstep02State'withdrawalInfoHash) `flip` perror $
        pmatch evidence $ \case
            PAbsentLedgerOutput{pabsentLedgerOutput'nonMembershipProof} ->
                pif
                    ( pdoesNotHave
                        # pfromData pstep02State'preUtxosRoot
                        # key
                        # pfromData pabsentLedgerOutput'nonMembershipProof
                    )
                    (result (pconstant False) (pconstant False) 0)
                    perror
            PPresentLedgerOutput{ppresentLedgerOutput'outputCbor, ppresentLedgerOutput'membershipProof} ->
                plet (pfromData ppresentLedgerOutput'outputCbor) $ \outputCbor ->
                    pmatch (pbuildV1 # poutrefOutputIndex pwithdrawalBody'l2Outref # outputCbor) $ \case
                        PNothing -> perror
                        PJust descriptor ->
                            pif
                                ( phasV1
                                    # pfromData pstep02State'preUtxosRoot
                                    # key
                                    # (pencodeLedgerOutputCommitment # descriptor)
                                    # pfromData ppresentLedgerOutput'membershipProof
                                )
                                ( pmatch (pdecodeCanonicalOutput # outputCbor) $ \case
                                    PNothing -> perror
                                    PJust output ->
                                        pmatch descriptor $ \PLedgerOutputCommitmentV1{poutputCommitment'cardanoValueSize} ->
                                            result
                                                (pconstant True)
                                                (poutputCoreMatchesV1 # withdrawalInfo # output)
                                                (pfromData poutputCommitment'cardanoValueSize)
                                )
                                perror

pcborBytesHeadLength :: forall s. Term s (PInteger :--> PInteger)
pcborBytesHeadLength = phoistAcyclic $ plam $ \length' ->
    pif (length' #< 24) 1 $
        pif (length' #<= 255) 2 $
            pif (length' #<= 65_535) 3 $
                pif (length' #<= 4_294_967_295) 5 9

ppointerWordLength :: forall s. Term s (PInteger :--> PInteger)
ppointerWordLength = phoistAcyclic $ pfix $ \self -> plam $ \value ->
    pif (value #< 0) perror $
        pif (value #< 128) 1 (1 + self # (pdiv # value # 128))

paddressPayloadLengthV1 :: forall s. Term s (PAddress :--> PInteger)
paddressPayloadLengthV1 = phoistAcyclic $ plam $ \address -> P.do
    PAddress{paddress'stakingCredential} <- pmatch address
    29
        + pmatch
            paddress'stakingCredential
            ( \case
                PDNothing -> 0
                PDJust stakeD -> pmatch (pfromData stakeD) $ \case
                    PStakingHash _ -> 28
                    PStakingPtr slot txIndex certificateIndex ->
                        ppointerWordLength # pfromData slot
                            + ppointerWordLength # pfromData txIndex
                            + ppointerWordLength # pfromData certificateIndex
            )

pdatumOptionLength :: forall s. Term s POutputDatum -> Term s PInteger
pdatumOptionLength datum = pmatch datum $ \case
    PNoOutputDatum -> 0
    POutputDatumHash hashD ->
        pif (plengthBS # pto (pfromData hashD) #== 32) (1 + 1 + 1 + 2 + 32) perror
    POutputDatum datumD ->
        plet (pserialiseData # pto datumD) $ \bytes ->
            plet (plengthBS # bytes) $ \length' ->
                1 + 1 + 1 + 2 + pcborBytesHeadLength # length' + length'

pexactPayoutOutputBytesV1 :: forall s. Term s (PWithdrawalBody :--> PInteger :--> PInteger)
pexactPayoutOutputBytesV1 = phoistAcyclic $ plam $ \body cardanoValueSize -> P.do
    PWithdrawalBody{pwithdrawalBody'l1Address, pwithdrawalBody'l1Datum} <- pmatch body
    plet (paddressPayloadLengthV1 # pfromData pwithdrawalBody'l1Address) $ \addressLength ->
        1
            + 1
            + pcborBytesHeadLength # addressLength
            + addressLength
            + 1
            + cardanoValueSize
            + pdatumOptionLength pwithdrawalBody'l1Datum

ppayoutIsExactlyPayableWithRateV1 ::
    forall s.
    Term s (PWithdrawalBody :--> PInteger :--> PInteger :--> PBool)
ppayoutIsExactlyPayableWithRateV1 = phoistAcyclic $ plam $ \body cardanoValueSize coinsPerUtxoByte -> P.do
    PWithdrawalBody{pwithdrawalBody'l2Value} <- pmatch body
    let lovelace =
            Value.pvalueOf
                # (pfromAssetList # pfromData pwithdrawalBody'l2Value)
                # Value.padaSymbol
                # Value.padaToken
    cardanoValueSize
        #> 0
        #&& cardanoValueSize
        #<= 5_000
        #&& poutputMeetsMinAdaV1
        # coinsPerUtxoByte
        # (pexactPayoutOutputBytesV1 # body # cardanoValueSize)
        # lovelace

ppayoutIsExactlyPayableV1 :: forall s. Term s (PWithdrawalBody :--> PInteger :--> PBool)
ppayoutIsExactlyPayableV1 = phoistAcyclic $ plam $ \body cardanoValueSize ->
    ppayoutIsExactlyPayableWithRateV1 # body # cardanoValueSize # pcoinsPerUtxoByte

pestablishMistagV1 ::
    forall s.
    Term s PStep03State ->
    Term s PWithdrawalBody ->
    Term s PStep04State
pestablishMistagV1 inputState withdrawalBody = P.do
    PStep03State
        { pstep03State'challengedHeaderHash
        , pstep03State'withdrawalId
        , pstep03State'withdrawalBodyHash
        , pstep03State'claimedValid
        , pstep03State'outputPresent
        , pstep03State'coreValid
        , pstep03State'cardanoValueSize
        } <-
        pmatch inputState
    let bodyHash = pblake2b_256 #$ pserialiseData # pforgetData (pdata withdrawalBody)
        claimedValid = pfromData pstep03State'claimedValid
        payable = ppayoutIsExactlyPayableV1 # withdrawalBody # pfromData pstep03State'cardanoValueSize
        actualValid = pfromData pstep03State'outputPresent #&& pfromData pstep03State'coreValid #&& payable
        exactOutputBytes = pexactPayoutOutputBytesV1 # withdrawalBody # pfromData pstep03State'cardanoValueSize
    pif
        (bodyHash #== pfromData pstep03State'withdrawalBodyHash #&& pnot # (claimedValid #== actualValid))
        ( pcon $
            PStep04State
                pstep03State'challengedHeaderHash
                pstep03State'withdrawalId
                pstep03State'claimedValid
                (pdata actualValid)
                (pdata exactOutputBytes)
                (pdata $ pminAdaLovelaceV1 # pcoinsPerUtxoByte # exactOutputBytes)
        )
        perror

pmistagFaultIsEstablishedV1 :: forall s. Term s (PStep04State :--> PBool)
pmistagFaultIsEstablishedV1 = phoistAcyclic $ plam $ \state -> P.do
    PStep04State
        { pstep04State'claimedValid
        , pstep04State'actualValid
        , pstep04State'exactOutputBytes
        , pstep04State'requiredLovelace
        } <-
        pmatch state
    pnot
        # (pfromData pstep04State'claimedValid #== pfromData pstep04State'actualValid)
        #&& pfromData pstep04State'exactOutputBytes
        #> 0
        #&& pfromData pstep04State'requiredLovelace
        #>= 0
