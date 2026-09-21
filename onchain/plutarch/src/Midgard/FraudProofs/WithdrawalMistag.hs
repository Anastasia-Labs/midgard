module Midgard.FraudProofs.WithdrawalMistag (
    PStep01State (..),
    PStep01Payload (..),
    PStep02Payload (..),
    PStep03Payload (..),
    PStep04Payload (..),
    PAuthenticatedOutputProjection (..),
    pauthenticatedOutputProjection,
    poutputValueMatchesV1,
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

import Aiken.Cbor (pdeserialise)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.FraudProofs.NativeTx.Codec (pencodeDefiniteBytes)
import Midgard.ValidationMerkle (PBuiltFrontier (..), pbuildFrontier, pfrontierCommitment)
import Plutarch.Builtin.Crypto (pblake2b_224, pblake2b_256, pverifyEd25519Signature)
import Plutarch.Core.Internal.Builtins (pindexBS')
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
    PAddress (..),
    POutputDatum (..),
    PStakingCredential (..),
    PTokenName (..),
 )
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottRec (..))
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Types (PProof)
import Midgard.Common.Value (pfromAssetList)
import Midgard.FraudProofCatalogue (pidByteCount)
import Midgard.FraudProofs.NativeTx.Components (pencodeMidgardTxInput)
import Midgard.FraudProofs.NativeTx.Types (
    PMidgardTxInput (..),
 )
import Midgard.LedgerOutputCommitment (passetLeafHash)
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

data PStep01Payload (s :: S) = PStep01Payload
    { pstep01Payload'committedWithdrawal :: Term s (PAsData PRootMembershipProof)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep01Payload)

data PStep01Args (s :: S) = PStep01Args
    { pstep01Args'inputIndex :: Term s (PAsData PInteger)
    , pstep01Args'outputIndex :: Term s (PAsData PInteger)
    , pstep01Args'hubRefInputIndex :: Term s (PAsData PInteger)
    , pstep01Args'stateQueueNodeRefInputIndex :: Term s (PAsData PInteger)
    , pstep01Args'payload :: Term s PData
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

data PStep02Payload (s :: S) = PStep02Payload
    { pstep02Payload'withdrawalInfo :: Term s PData
    , pstep02Payload'eventToStep :: Term s (PAsData PRootMembershipProof)
    , pstep02Payload'transitionStep :: Term s (PAsData PIndexedTraceProof)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep02Payload)

data PStep02Args (s :: S) = PStep02Args
    { pstep02Args'inputIndex :: Term s (PAsData PInteger)
    , pstep02Args'outputIndex :: Term s (PAsData PInteger)
    , pstep02Args'payload :: Term s PData
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PWithdrawalLedgerEvidenceV1 (s :: S)
    = PPresentLedgerOutput
        { ppresentLedgerOutput'descriptorCbor :: Term s (PAsData PByteString)
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
    , pstep03State'ownerSignatureValid :: Term s (PAsData PBool)
    , pstep03State'cardanoValueSize :: Term s (PAsData PInteger)
    , pstep03State'outputLovelace :: Term s (PAsData PInteger)
    , pstep03State'outputAssetCount :: Term s (PAsData PInteger)
    , pstep03State'outputAssetFrontierCommitment :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

data PStep03Payload (s :: S) = PStep03Payload
    { pstep03Payload'withdrawalInfo :: Term s PData
    , pstep03Payload'evidence :: Term s (PAsData PWithdrawalLedgerEvidenceV1)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep03Payload)

data PStep03Args (s :: S) = PStep03Args
    { pstep03Args'inputIndex :: Term s (PAsData PInteger)
    , pstep03Args'outputIndex :: Term s (PAsData PInteger)
    , pstep03Args'payload :: Term s PData
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

data PStep04Payload (s :: S) = PStep04Payload
    { pstep04Payload'withdrawalBody :: Term s PData
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep04Payload)

data PStep04Args (s :: S) = PStep04Args
    { pstep04Args'inputIndex :: Term s (PAsData PInteger)
    , pstep04Args'outputIndex :: Term s (PAsData PInteger)
    , pstep04Args'payload :: Term s PData
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

data PAuthenticatedOutputProjection (s :: S)
    = PAuthenticatedOutputProjection
        (Term s PInteger)
        (Term s PInteger)
        (Term s PByteString)
        (Term s PInteger)
        (Term s PInteger)
        (Term s PByteString)
        (Term s PInteger)
        (Term s PByteString)
    deriving stock (Generic)
    deriving anyclass (SOP.Generic)
    deriving (PlutusType) via (DeriveAsScottRec PAuthenticatedOutputProjection)

-- The MPF authenticates the whole descriptor; only these eight projections are
-- decoded. The ignored descriptor fields are deliberately not revalidated.
pauthenticatedOutputProjection :: forall s. Term s (PByteString :--> PAuthenticatedOutputProjection)
pauthenticatedOutputProjection = phoistAcyclic $ plam $ \bytes -> P.do
    PJust dat <- pmatch $ pdeserialise # bytes
    items <- plet $ pasList # dat
    let int i = pasInt # (pelemAt # i # items)
        bs i = pasByteStr # (pelemAt # i # items)
    pif
        ( plength
            # items
            #== 16
            #&& int 0
            #== 1
            #&& int 1
            #>= 0
            #&& int 1
            #<= 65535
            #&& int 2
            #>= 0
            #&& plengthBS
            # bs 3
            #== 32
            #&& int 6
            #>= 0
            #&& int 8
            #>= 0
            #&& int 8
            #<= 5000
        )
        (pcon $ PAuthenticatedOutputProjection (int 1) (int 2) (bs 3) (int 8) (int 6) (bs 4) (int 5) (bs 7))
        perror

ppaymentOwner :: forall s. Term s (PByteString :--> PMaybe PByteString)
ppaymentOwner = phoistAcyclic $ plam $ \address ->
    plet (plengthBS # address) $ \len -> plet (pdiv # (pindexBS' # address # 0) # 16) $ \kind ->
        pif
            (pif (len #== 29) (kind #== 6 #|| kind #== 7) (len #== 57 #&& kind #>= 0 #&& kind #<= 3))
            (pif (kind #== 1 #|| kind #== 3 #|| kind #== 7) (pcon PNothing) (pcon $ PJust $ psliceBS # 1 # 28 # address))
            perror

type PNamedAsset = PBuiltinPair (PAsData PByteString) (PAsData PByteString)
type PAssetRun = PBuiltinList PNamedAsset

-- Merge runs preserve the target's bounded mixed-width asset ordering.
pmergeAssetPairs :: forall s. Term s (PAssetRun :--> PAssetRun :--> PAssetRun)
pmergeAssetPairs = phoistAcyclic $ pfix $ \self -> plam $ \left right ->
    pelimList
        ( \a restLeft ->
            pelimList
                ( \b restRight ->
                    pif
                        (pfromData (pfstBuiltin # a) #< pfromData (pfstBuiltin # b))
                        (pcons # a # (self # restLeft # right))
                        (pcons # b # (self # left # restRight))
                )
                left
                right
        )
        right
        left

pmergeAssetRuns :: forall s. Term s (PBuiltinList PAssetRun :--> PBuiltinList PAssetRun)
pmergeAssetRuns = phoistAcyclic $ pfix $ \self -> plam $ \runs ->
    pelimList (\a rest -> pelimList (\b tailRuns -> pcons # (pmergeAssetPairs # a # b) # (self # tailRuns)) runs rest) runs runs

psortedAssetRuns :: forall s. Term s (PBuiltinList PAssetRun :--> PAssetRun)
psortedAssetRuns = phoistAcyclic $ pfix $ \self -> plam $ \runs ->
    pelimList (\one rest -> pif (pnull # rest) one (self # (pmergeAssetRuns # runs))) pnil runs

passetPairsAreSorted :: forall s. Term s (PAssetRun :--> PBool)
passetPairsAreSorted = phoistAcyclic $ pfix $ \self -> plam $ \pairs ->
    pelimList (\a rest -> pelimList (\b _ -> pfromData (pfstBuiltin # a) #< pfromData (pfstBuiltin # b) #&& self # rest) (pconstant True) rest) (pconstant True) pairs

psortAssetPairs :: forall s. Term s (PAssetRun :--> PAssetRun)
psortAssetPairs = phoistAcyclic $ plam $ \pairs ->
    pif (passetPairsAreSorted # pairs) pairs (psortedAssetRuns # (pmap # plam (\x -> pcons # x # pnil) # pairs))

poutputValueMatchesV1 :: forall s. Term s (PWithdrawalBody :--> Value.PSortedValue :--> PInteger :--> PInteger :--> PByteString :--> PBool)
poutputValueMatchesV1 = phoistAcyclic $ plam $ \body value lovelace count commitment -> P.do
    PWithdrawalBody{pwithdrawalBody'l2Value} <- pmatch body
    validDomains <-
        plet $
            pall
                # plam
                    ( \entry -> P.do
                        PBuiltinPair policyD tokensD <- pmatch entry
                        let policy = pto $ pfromData policyD
                            tokens = pto $ pfromData tokensD
                        pif
                            (policy #== pconstant "")
                            (pelimList (\token rest -> pmatch token $ \(PBuiltinPair name quantity) -> pnull # rest #&& pto (pfromData name) #== pconstant "" #&& pfromData quantity #> 0) (pconstant False) tokens)
                            (plengthBS # policy #== 28 #&& pall # plam (\token -> pmatch token $ \(PBuiltinPair name quantity) -> plengthBS # pto (pfromData name) #<= 32 #&& pfromData quantity #> 0) # tokens)
                    )
                # pto (pfromData pwithdrawalBody'l2Value)
    pif
        validDomains
        ( P.do
            named <-
                plet $
                    psortAssetPairs
                        # ( pfoldr
                                # plam
                                    ( \entry rest -> P.do
                                        PBuiltinPair policyD tokensD <- pmatch entry
                                        let policy = pto $ pfromData policyD
                                        pif
                                            (policy #== pconstant "")
                                            rest
                                            ( pconcat
                                                # ( pmap
                                                        # plam
                                                            ( \token -> pmatch token $ \(PBuiltinPair nameD quantityD) ->
                                                                plet (pto $ pfromData nameD) $ \name -> ppairDataBuiltin # pdata (policy <> (pencodeDefiniteBytes # name)) # pdata (passetLeafHash # policy # name # pfromData quantityD)
                                                            )
                                                        # pto (pto (pfromData tokensD))
                                                  )
                                                # rest
                                            )
                                    )
                                # pnil
                                # pto (pto (pto value))
                          )
            pif
                (plength # named #== count)
                ( P.do
                    PBuiltFrontier _ peaks <- pmatch $ pbuildFrontier # (pmap # plam (\pair -> psndBuiltin # pair) # named)
                    Value.pvalueOf
                        # value
                        # Value.padaSymbol
                        # Value.padaToken
                        #== lovelace
                        #&& pfrontierCommitment
                        # count
                        # peaks
                        #== commitment
                        #&& count
                        #<= pmaximumWithdrawalAssetCountV1
                )
                (pconstant False)
        )
        (pconstant False)

poutputCoreMatchesWithSignatureV1 :: forall s. Term s (PWithdrawalInfo :--> PAuthenticatedOutputProjection :--> PBool :--> PBool)
poutputCoreMatchesWithSignatureV1 = phoistAcyclic $ plam $ \info output signature -> P.do
    PWithdrawalInfo{pwithdrawalInfo'body} <- pmatch info
    body@PWithdrawalBody{pwithdrawalBody'l2Owner, pwithdrawalBody'l2Value} <- pmatch $ pfromData pwithdrawalInfo'body
    PAuthenticatedOutputProjection _ _ _ _ count address lovelace frontier <- pmatch output
    ppaymentOwner
        # address
        #== pcon (PJust $ pasByteStr # pwithdrawalBody'l2Owner)
        #&& signature
        #&& poutputValueMatchesV1
        # pcon body
        # (pfromAssetList # pfromData pwithdrawalBody'l2Value)
        # lovelace
        # count
        # frontier

poutputCoreMatchesV1 :: forall s. Term s (PWithdrawalInfo :--> PAuthenticatedOutputProjection :--> PBool)
poutputCoreMatchesV1 = phoistAcyclic $ plam $ \info output -> poutputCoreMatchesWithSignatureV1 # info # output # (pwithdrawalSignatureIsValidV1 # info)

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
        result outputPresent ownerSignature cardanoValueSize lovelace assetCount frontier =
            pcon $
                PStep03State
                    pstep02State'challengedHeaderHash
                    pstep02State'withdrawalId
                    (pdata bodyHash)
                    pstep02State'claimedValid
                    (pdata outputPresent)
                    (pdata ownerSignature)
                    (pdata cardanoValueSize)
                    (pdata lovelace)
                    (pdata assetCount)
                    (pdata frontier)
    pif (infoHash #== pfromData pstep02State'withdrawalInfoHash) `flip` perror $
        pmatch evidence $ \case
            PAbsentLedgerOutput{pabsentLedgerOutput'nonMembershipProof} ->
                pif
                    ( pdoesNotHave
                        # pfromData pstep02State'preUtxosRoot
                        # key
                        # pfromData pabsentLedgerOutput'nonMembershipProof
                    )
                    (result (pconstant False) (pconstant False) 0 0 0 (pconstant ""))
                    perror
            PPresentLedgerOutput{ppresentLedgerOutput'descriptorCbor, ppresentLedgerOutput'membershipProof} ->
                plet (pfromData ppresentLedgerOutput'descriptorCbor) $ \descriptorBytes ->
                    pif
                        (phasV1 # pfromData pstep02State'preUtxosRoot # key # descriptorBytes # pfromData ppresentLedgerOutput'membershipProof)
                        ( P.do
                            PAuthenticatedOutputProjection index _ _ valueSize count address lovelace frontier <- pmatch $ pauthenticatedOutputProjection # descriptorBytes
                            PWithdrawalBody{pwithdrawalBody'l2Owner} <- pmatch $ pfromData pwithdrawalInfo'body
                            pif
                                (index #== poutrefOutputIndex pwithdrawalBody'l2Outref)
                                ( result
                                    (pconstant True)
                                    ( pif
                                        (count #> pmaximumWithdrawalAssetCountV1)
                                        (pconstant False)
                                        (ppaymentOwner # address #== pcon (PJust $ pasByteStr # pwithdrawalBody'l2Owner) #&& pwithdrawalSignatureIsValidV1 # withdrawalInfo)
                                    )
                                    valueSize
                                    lovelace
                                    count
                                    frontier
                                )
                                perror
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
        , pstep03State'ownerSignatureValid
        , pstep03State'cardanoValueSize
        , pstep03State'outputLovelace
        , pstep03State'outputAssetCount
        , pstep03State'outputAssetFrontierCommitment
        } <-
        pmatch inputState
    let bodyHash = pblake2b_256 #$ pserialiseData # pforgetData (pdata withdrawalBody)
        claimedValid = pfromData pstep03State'claimedValid
        valueSize = pfromData pstep03State'cardanoValueSize
    exactOutputBytes <- plet $ pexactPayoutOutputBytesV1 # withdrawalBody # valueSize
    actualValid <- plet $ pif
        (pfromData pstep03State'outputPresent #&& pfromData pstep03State'ownerSignatureValid)
        (P.do
            PWithdrawalBody {pwithdrawalBody'l2Value} <- pmatch withdrawalBody
            value <- plet $ pfromAssetList # pfromData pwithdrawalBody'l2Value
            poutputValueMatchesV1 # withdrawalBody # value
                # pfromData pstep03State'outputLovelace # pfromData pstep03State'outputAssetCount
                # pfromData pstep03State'outputAssetFrontierCommitment
                #&& valueSize #> 0 #&& valueSize #<= 5000
                #&& poutputMeetsMinAdaV1 # pcoinsPerUtxoByte # exactOutputBytes
                    # (Value.pvalueOf # value # Value.padaSymbol # Value.padaToken))
        (pconstant False)
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
