{-# LANGUAGE OverloadedStrings #-}

-- | Narrow machine twin for the mint-declared-asset-limit fraud proof.
module Midgard.FraudProofs.MintDeclaredAssetLimit (
  PBoundPolicyV1 (..),
  PPolicyHeaderV1 (..),
  PFoldStateV1 (..),
  PDecisionStateV1 (..),
  PAuthenticationStateV1 (..),
  PStep01Source (..),
  PStep01Args (..),
  PStep02ActionV1 (..),
  PStep03Args (..),
  PStep04Args (..),
  pmintFieldIndex,
  pmaxDistinctAssetCount,
  pstagedPolicyBudget,
  pstagedFoldBudget,
  pfoldPolicyCost,
  poutcomeScanning,
  poutcomeCrossing,
  poutcomeNonCrossing,
  pbindPolicyV1,
  ppolicyHeaderV1,
  pinitialFoldV1,
  pbeginPolicyV1,
  pconsumeAssetV1,
  pconsumeAssetsV1,
  pdecisionV1,
  pterminalContradictionV1,
  pencodeBoundPolicyV1,
  pencodeFoldStateV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.Utils ((#/=))
import Plutarch.Prelude

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.NativeTx.Codec (pdecodeCanonicalIntAt, pdecodeDefiniteArrayHeaderAt)
import Midgard.FraudProofs.NativeTx.Preimages (pcanonicalBytesKeyPrecedes, pdecodeCanonicalBytesAt, pdecodeCanonicalMapHeaderAt)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PMintDeclaredAssetLimit))
import Midgard.TransitionTrace (PRootMembershipProof)

data PBoundPolicyV1 (s :: S) = PBoundPolicyV1
  { pboundPolicy'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pboundPolicy'policyIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBoundPolicyV1)

data PPolicyHeaderV1 (s :: S) = PPolicyHeaderV1
  { ppolicyHeader'policyId :: Term s (PAsData PByteString)
  , ppolicyHeader'declaredCount :: Term s (PAsData PInteger)
  , ppolicyHeader'assetsOffset :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPolicyHeaderV1)

data PFoldStateV1 (s :: S) = PFoldStateV1
  { pfoldState'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pfoldState'policyIndex :: Term s (PAsData PInteger)
  , pfoldState'targetPolicyId :: Term s (PAsData PByteString)
  , pfoldState'targetDeclaredCount :: Term s (PAsData PInteger)
  , pfoldState'checkpointHash :: Term s (PAsData PByteString)
  , pfoldState'accumulatedCount :: Term s (PAsData PInteger)
  , pfoldState'previousPolicy :: Term s (PAsData PByteString)
  , pfoldState'activePolicy :: Term s (PAsData PByteString)
  , pfoldState'itemCursor :: Term s (PAsData PInteger)
  , pfoldState'assetsRemaining :: Term s (PAsData PInteger)
  , pfoldState'policyAssetCursor :: Term s (PAsData PInteger)
  , pfoldState'previousAsset :: Term s (PAsData PByteString)
  , pfoldState'outcome :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFoldStateV1)

data PDecisionStateV1 (s :: S) = PDecisionStateV1
  { pdecisionState'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pdecisionState'policyIndex :: Term s (PAsData PInteger)
  , pdecisionState'crossing :: Term s (PAsData PBool)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDecisionStateV1)

data PAuthenticationStateV1 (s :: S)
  = PBound (Term s (PAsData PBoundPolicyV1))
  | PGrammar (Term s (PAsData PBoundPolicyV1)) (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticationStateV1)

data PStep01Source (s :: S)
  = PAcceptedSource (Term s (PAsData PNativeTxInclusionCarriage))
  | PForcedSource (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PHeaderV1)) (Term s (PAsData PRootMembershipProof)) (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Source)

data PStep01Args (s :: S) = PStep01Args
  { pstep01Args'source :: Term s (PAsData PStep01Source)
  , pstep01Args'policyIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02ActionV1 (s :: S)
  = PAuthenticateDirect (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PFieldOpeningV1))
  | PStartGrammar (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PFieldOpeningV1)) (Term s (PAsData PInteger))
  | PResumeGrammar (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PFieldOpeningV1)) (Term s (PAsData PByteString)) (Term s (PAsData PInteger))
  | PFinishGrammar (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PFieldOpeningV1)) (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02ActionV1)

data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , pstep03Args'opening :: Term s (PAsData PFieldOpeningV1)
  , pstep03Args'checkpointBytes :: Term s (PAsData PByteString)
  , pstep03Args'budget :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

pmintFieldIndex, pmaxDistinctAssetCount, pstagedPolicyBudget, pstagedFoldBudget, pfoldPolicyCost, poutcomeScanning, poutcomeCrossing, poutcomeNonCrossing :: forall s. Term s PInteger
pmintFieldIndex = 5
pmaxDistinctAssetCount = 16_384
pstagedPolicyBudget = 24
pstagedFoldBudget = 192
pfoldPolicyCost = 8
poutcomeScanning = 0
poutcomeCrossing = 1
poutcomeNonCrossing = 2

pbindPolicyV1 :: forall s. Term s (Subject.PVerdictSubject :--> PInteger :--> PBoundPolicyV1)
pbindPolicyV1 = phoistAcyclic $ plam $ \subject policyIndex ->
  pif
    (Subject.psubjectIsCanonical # subject #&& policyIndex #>= 0)
    ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
        let bound = pcon $ PBoundPolicyV1 (pdata subject) (pdata policyIndex)
         in pif
              (pfromData psubject'direction #== 1)
              (plet (Subject.pbindExactRejectionReason # subject # pcon (PMintDeclaredAssetLimit $ pdata policyIndex)) $ \_ -> bound)
              bound
    )
    perror

ppolicyHeaderV1 :: forall s. Term s (PByteString :--> PPolicyHeaderV1)
ppolicyHeaderV1 = phoistAcyclic $ plam $ \item ->
  pmatch (pdecodeDefiniteArrayHeaderAt # item # 0) $ \(PPair offset itemCount) ->
    pif
      (itemCount #== 2)
      ( pmatch (pdecodeCanonicalBytesAt # item # offset) $ \(PPair headerOffset policyId) ->
          pmatch (pdecodeCanonicalMapHeaderAt # item # headerOffset) $ \(PPair assetsOffset declaredCount) ->
            pif
              (plengthBS # policyId #== 28 #&& declaredCount #> 0 #&& assetsOffset #< plengthBS # item)
              (pcon $ PPolicyHeaderV1 (pdata policyId) (pdata declaredCount) (pdata assetsOffset))
              perror
      )
      perror

pinitialFoldV1 :: forall s. Term s (PBoundPolicyV1 :--> PByteString :--> PByteString :--> PFoldStateV1)
pinitialFoldV1 = phoistAcyclic $ plam $ \bound targetItem checkpointHash ->
  pmatch (ppolicyHeaderV1 # targetItem) $ \PPolicyHeaderV1{..} ->
    pif
      (plengthBS # checkpointHash #== 32)
      ( pmatch bound $ \PBoundPolicyV1{..} ->
          pcon $
            PFoldStateV1
              pboundPolicy'subject
              pboundPolicy'policyIndex
              ppolicyHeader'policyId
              ppolicyHeader'declaredCount
              (pdata checkpointHash)
              (pdata 0)
              (pdata $ pconstant "")
              (pdata $ pconstant "")
              (pdata 0)
              (pdata 0)
              (pdata 0)
              (pdata $ pconstant "")
              (pdata poutcomeScanning)
      )
      perror

pbeginPolicyV1 :: forall s. Term s (PFoldStateV1 :--> PInteger :--> PByteString :--> PFoldStateV1)
pbeginPolicyV1 = phoistAcyclic $ plam $ \state itemIndex item -> pmatch state $ \s@PFoldStateV1{..} ->
  pif
    ( pfromData pfoldState'outcome
        #== poutcomeScanning
        #&& pfromData pfoldState'activePolicy
        #== pconstant ""
        #&& itemIndex
        #>= 0
        #&& itemIndex
        #<= pfromData pfoldState'policyIndex
        #&& pfromData pfoldState'accumulatedCount
        #>= 0
        #&& pfromData pfoldState'accumulatedCount
        #<= pmaxDistinctAssetCount
    )
    ( pmatch (ppolicyHeaderV1 # item) $ \PPolicyHeaderV1{..} ->
        let ordered = itemIndex #== 0 #|| pcanonicalBytesKeyPrecedes # pfromData pfoldState'previousPolicy # pfromData ppolicyHeader'policyId
            nextCount = pfromData pfoldState'accumulatedCount + pfromData ppolicyHeader'declaredCount
         in pif
              ordered
              ( pif
                  (itemIndex #== pfromData pfoldState'policyIndex)
                  ( pif
                      (pfromData ppolicyHeader'policyId #== pfromData pfoldState'targetPolicyId #&& pfromData ppolicyHeader'declaredCount #== pfromData pfoldState'targetDeclaredCount)
                      (pif (nextCount #> pmaxDistinctAssetCount) (pcon s{pfoldState'outcome = pdata poutcomeCrossing}) (popenPolicy state ppolicyHeader'policyId ppolicyHeader'declaredCount ppolicyHeader'assetsOffset))
                      perror
                  )
                  (pif (nextCount #<= pmaxDistinctAssetCount) (popenPolicy state ppolicyHeader'policyId ppolicyHeader'declaredCount ppolicyHeader'assetsOffset) perror)
              )
              perror
    )
    perror

popenPolicy :: forall s. Term s PFoldStateV1 -> Term s (PAsData PByteString) -> Term s (PAsData PInteger) -> Term s (PAsData PInteger) -> Term s PFoldStateV1
popenPolicy state policyId declaredCount assetsOffset = pmatch state $ \s ->
  pcon
    s
      { pfoldState'activePolicy = policyId
      , pfoldState'itemCursor = assetsOffset
      , pfoldState'assetsRemaining = declaredCount
      , pfoldState'policyAssetCursor = pdata 0
      , pfoldState'previousAsset = pdata $ pconstant ""
      }

type PConsumeResult = PPair PInteger (PPair PInteger (PPair PInteger (PPair PByteString (PPair PInteger PInteger))))

pconsumeAssetsV1 :: forall s. Term s (PFoldStateV1 :--> PInteger :--> PByteString :--> PInteger :--> PPair PFoldStateV1 PInteger)
pconsumeAssetsV1 = phoistAcyclic $ plam $ \state itemIndex item budget -> pmatch state $ \s@PFoldStateV1{..} ->
  pif
    (budget #== 0 #|| pfromData pfoldState'assetsRemaining #== 0)
    (pcon $ PPair state budget)
    ( pif
        ( pfromData pfoldState'outcome
            #== poutcomeScanning
            #&& plengthBS
            # pfromData pfoldState'activePolicy
            #== 28
            #&& pfromData pfoldState'assetsRemaining
            #> 0
            #&& pfromData pfoldState'policyAssetCursor
            #>= 0
        )
        ( pmatch
            ( pconsumeEntries
                # item
                # (plengthBS # item)
                # pfromData pfoldState'itemCursor
                # pfromData pfoldState'assetsRemaining
                # pfromData pfoldState'policyAssetCursor
                # pfromData pfoldState'previousAsset
                # pfromData pfoldState'accumulatedCount
                # budget
            )
            $ \(PPair cursor tail1) -> pmatch tail1 $ \(PPair remaining tail2) -> pmatch tail2 $ \(PPair assetCursor tail3) -> pmatch tail3 $ \(PPair previous tail4) -> pmatch tail4 $ \(PPair count left) ->
              pif
                (remaining #== 0)
                ( pcon $
                    PPair
                      ( pcon
                          s
                            { pfoldState'accumulatedCount = pdata count
                            , pfoldState'previousPolicy = pfoldState'activePolicy
                            , pfoldState'activePolicy = pdata $ pconstant ""
                            , pfoldState'itemCursor = pdata 0
                            , pfoldState'assetsRemaining = pdata 0
                            , pfoldState'policyAssetCursor = pdata 0
                            , pfoldState'previousAsset = pdata $ pconstant ""
                            , pfoldState'outcome = pdata $ pif (itemIndex #== pfromData pfoldState'policyIndex) poutcomeNonCrossing poutcomeScanning
                            }
                      )
                      left
                )
                ( pcon $
                    PPair
                      ( pcon
                          s
                            { pfoldState'accumulatedCount = pdata count
                            , pfoldState'itemCursor = pdata cursor
                            , pfoldState'assetsRemaining = pdata remaining
                            , pfoldState'policyAssetCursor = pdata assetCursor
                            , pfoldState'previousAsset = pdata previous
                            }
                      )
                      left
                )
        )
        perror
    )

pconsumeAssetV1 :: forall s. Term s (PFoldStateV1 :--> PInteger :--> PByteString :--> PFoldStateV1)
pconsumeAssetV1 = phoistAcyclic $ plam $ \state index item -> pmatch (pconsumeAssetsV1 # state # index # item # 1) $ \(PPair consumed _) -> consumed

pconsumeEntries :: forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PByteString :--> PInteger :--> PInteger :--> PConsumeResult)
pconsumeEntries = phoistAcyclic $ pfix $ \self -> plam $ \item itemLength cursor remaining assetCursor previous count budget ->
  pif
    (budget #== 0 #|| remaining #== 0)
    (pconsumeResult cursor remaining assetCursor previous count budget)
    ( pmatch (pdecodeCanonicalBytesAt # item # cursor) $ \(PPair quantityOffset assetName) ->
        pmatch (pdecodeCanonicalIntAt # item # quantityOffset) $ \(PPair nextCursor quantity) ->
          let nextCount = count + 1
              valid =
                plengthBS
                  # assetName
                  #<= 32
                  #&& quantity
                  #/= 0
                  #&& nextCount
                  #<= pmaxDistinctAssetCount
                  #&& (assetCursor #== 0 #|| pcanonicalBytesKeyPrecedes # previous # assetName)
                  #&& pif (remaining #== 1) (nextCursor #== itemLength) (nextCursor #< itemLength)
           in pif valid (self # item # itemLength # nextCursor # (remaining - 1) # (assetCursor + 1) # assetName # nextCount # (budget - 1)) perror
    )

pconsumeResult :: forall s. Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PByteString -> Term s PInteger -> Term s PInteger -> Term s PConsumeResult
pconsumeResult cursor remaining assetCursor previous count budget =
  pcon $ PPair cursor $ pcon $ PPair remaining $ pcon $ PPair assetCursor $ pcon $ PPair previous $ pcon $ PPair count budget

pdecisionV1 :: forall s. Term s (PFoldStateV1 :--> PDecisionStateV1)
pdecisionV1 = phoistAcyclic $ plam $ \state -> pmatch state $ \PFoldStateV1{..} ->
  pif
    (pfromData pfoldState'outcome #== poutcomeCrossing #|| pfromData pfoldState'outcome #== poutcomeNonCrossing)
    (pcon $ PDecisionStateV1 pfoldState'subject pfoldState'policyIndex (pdata $ pfromData pfoldState'outcome #== poutcomeCrossing))
    perror

pterminalContradictionV1 :: forall s. Term s (PDecisionStateV1 :--> PBool)
pterminalContradictionV1 = phoistAcyclic $ plam $ \state -> pmatch state $ \PDecisionStateV1{..} -> Subject.pterminalContradiction # pfromData pdecisionState'subject # pfromData pdecisionState'crossing

pencodeBoundPolicyV1 :: forall s. Term s (PBoundPolicyV1 :--> PByteString)
pencodeBoundPolicyV1 = phoistAcyclic $ plam $ \value -> pserialiseData # pforgetData (pdata value)

pencodeFoldStateV1 :: forall s. Term s (PFoldStateV1 :--> PByteString)
pencodeFoldStateV1 = phoistAcyclic $ plam $ \value -> pserialiseData # pforgetData (pdata value)
