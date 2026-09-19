module Midgard.FraudProofs.CommittedFieldShape (
  PCommittedFieldShapeVerdictV1 (..),
  PStep01Args (..),
  PStep02State (..),
  PStep02Args (..),
  pshapeVerdictAdmissible,
  pshapeVerdictNotAnEnvelope,
  pshapeVerdictFieldByteBound,
  pshapeVerdictWrongStride,
  pshapeVerdictCodeCount,
  pcommittedFieldShapeVerdictV1,
  pcommittedFieldShapeV1,
  pisCommittedFieldShapeViolationV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PTxInInfo)
import Plutarch.Prelude

import Midgard.FraudProofs.CanonicalDecodability (
  PCommittedFieldClaimV1 (..),
  penvelopeVerdictV1,
  pverdictGrammatical,
 )
import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (pfirstWitnessSetFieldIndex)
import Midgard.FraudProofs.NativeTx.Codec (pbyteAt)
import Midgard.FraudProofs.NativeTx.Types (PNativeTxWitnessSetCompact (..), PVerifiedMidgardNativeTxCompact)
import Midgard.NativeTxFieldAccess (
  pauthenticatedCommittedPreimage,
  pfieldCount,
  pfieldStride,
  pmaxTransactionAggregateFieldBytes,
  pwalkDerivedStride,
 )

data PCommittedFieldShapeVerdictV1 s = PCommittedFieldShapeVerdictV1
  { pshapeVerdict'fieldIndex :: Term s (PAsData PInteger)
  , pshapeVerdict'verdict :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCommittedFieldShapeVerdictV1)

data PStep01Args s = PStep01Args
  { pstep01Args'inclusion :: Term s (PAsData PNativeTxInclusionCarriage)
  , pstep01Args'claim :: Term s (PAsData PCommittedFieldClaimV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02State s = PStep02State
  { pstep02State'badTxId :: Term s (PAsData PByteString)
  , pstep02State'fieldIndex :: Term s (PAsData PInteger)
  , pstep02State'verdict :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

data PStep02Args s = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

pshapeVerdictAdmissible, pshapeVerdictNotAnEnvelope, pshapeVerdictFieldByteBound, pshapeVerdictWrongStride, pshapeVerdictCodeCount :: forall s. Term s PInteger
pshapeVerdictAdmissible = 0
pshapeVerdictNotAnEnvelope = 1
pshapeVerdictFieldByteBound = 2
pshapeVerdictWrongStride = 3
pshapeVerdictCodeCount = 4

-- | Slot-specific §7.4/§5.4 verdict, total over arbitrary preimage bytes.
pcommittedFieldShapeVerdictV1 :: forall s. Term s (PInteger :--> PByteString :--> PInteger)
pcommittedFieldShapeVerdictV1 = phoistAcyclic $ plam $ \fieldIndex preimage ->
  plet (pfieldStride # fieldIndex) $ \stride ->
    pmatch (pminimalArrayHeaderV1 # preimage) $ \case
      PNothing -> pshapeVerdictNotAnEnvelope
      PJust header -> pmatch header $ \(PPair headerLength count) ->
        pif
          (penvelopeVerdictV1 # preimage #/= pverdictGrammatical)
          pshapeVerdictNotAnEnvelope
          ( plet (plengthBS # preimage) $ \totalLength ->
              pif
                (totalLength #> pmaxTransactionAggregateFieldBytes)
                pshapeVerdictFieldByteBound
                ( pif
                    (stride #== pwalkDerivedStride)
                    pshapeVerdictAdmissible
                    ( pif
                        (headerLength + stride * count #== totalLength)
                        pshapeVerdictAdmissible
                        pshapeVerdictWrongStride
                    )
                )
          )

pminimalArrayHeaderV1 :: forall s. Term s (PByteString :--> PMaybe (PPair PInteger PInteger))
pminimalArrayHeaderV1 = phoistAcyclic $ plam $ \preimage ->
  plet (plengthBS # preimage) $ \totalLength ->
    pif
      (totalLength #== 0)
      (pcon PNothing)
      ( plet (pbyteAt # preimage # 0) $ \tag ->
          pif
            (tag #>= 128 #&& tag #<= 151)
            (pcon $ PJust $ pcon $ PPair 1 (tag - 128))
            ( pif
                (tag #== 152)
                ( pif
                    (totalLength #< 2)
                    (pcon PNothing)
                    ( plet (pbyteAt # preimage # 1) $ \count ->
                        pif
                          (count #< 24)
                          (pcon PNothing)
                          (pcon $ PJust $ pcon $ PPair 2 count)
                    )
                )
                ( pif
                    (tag #== 153)
                    ( pif
                        (totalLength #< 3)
                        (pcon PNothing)
                        ( plet (pbyteAt # preimage # 1 * 256 + pbyteAt # preimage # 2) $ \count ->
                            pif
                              (count #<= 255)
                              (pcon PNothing)
                              (pcon $ PJust $ pcon $ PPair 3 count)
                        )
                    )
                    (pcon PNothing)
                )
            )
      )

pcommittedFieldShapeV1 ::
  forall s.
  Term s PVerifiedMidgardNativeTxCompact ->
  Term s PCommittedFieldClaimV1 ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PCommittedFieldShapeVerdictV1
pcommittedFieldShapeV1 verified claim referenceInputs certificatePolicy =
  pmatch claim $ \case
    PBodyFieldClaim fieldIndexD carriageD ->
      pif
        (pfromData fieldIndexD #< pfirstWitnessSetFieldIndex)
        (verdict (pfromData fieldIndexD) unreadWitnessSet (pfromData carriageD))
        perror
    PWitnessFieldClaim fieldIndexD witnessSetD carriageD ->
      pif
        (pfromData fieldIndexD #>= pfirstWitnessSetFieldIndex)
        (verdict (pfromData fieldIndexD) (pfromData witnessSetD) (pfromData carriageD))
        perror
  where
    unreadWitnessSet = pcon $ PNativeTxWitnessSetCompact (pdata $ pconstant "") (pdata $ pconstant "") (pdata $ pconstant "")
    verdict fieldIndex witnessSet carriage =
      pcon $ PCommittedFieldShapeVerdictV1
        (pdata fieldIndex)
        ( pdata $
            pcommittedFieldShapeVerdictV1 # fieldIndex
              # ( pauthenticatedCommittedPreimage
                    # verified # witnessSet # fieldIndex # carriage # referenceInputs # certificatePolicy
                )
        )

pisCommittedFieldShapeViolationV1 :: forall s. Term s (PInteger :--> PInteger :--> PBool)
pisCommittedFieldShapeViolationV1 = phoistAcyclic $ plam $ \fieldIndex verdict ->
  pif
    (fieldIndex #>= 0 #&& fieldIndex #< pfieldCount #&& verdict #>= 0 #&& verdict #< pshapeVerdictCodeCount)
    (verdict #== pshapeVerdictFieldByteBound #|| verdict #== pshapeVerdictWrongStride)
    perror
