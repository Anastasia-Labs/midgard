module Midgard.FraudProofs.CanonicalDecodability (
  PCommittedFieldClaimV1 (..),
  PCommittedFieldVerdictV1 (..),
  PStep01Args (..),
  PStep02State (..),
  PStep02Args (..),
  pverdictGrammatical,
  pverdictMissingArrayHeader,
  pverdictMissingItemHeader,
  pverdictTrailingBytes,
  pverdictCodeCount,
  penvelopeVerdictV1,
  pcommittedFieldVerdictV1,
  pisCanonicalDecodabilityViolationV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PTxInInfo)
import Plutarch.Prelude

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (pfirstWitnessSetFieldIndex)
import Midgard.FraudProofs.NativeTx.Codec (pbyteAt)
import Midgard.FraudProofs.NativeTx.Types (PNativeTxWitnessSetCompact (..), PVerifiedMidgardNativeTxCompact)
import Midgard.NativeTxFieldAccess (PFieldCarriageV1, pauthenticatedCommittedPreimage, pfieldCount)

data PCommittedFieldClaimV1 s
  = PBodyFieldClaim
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldCarriageV1))
  | PWitnessFieldClaim
      (Term s (PAsData PInteger))
      (Term s (PAsData PNativeTxWitnessSetCompact))
      (Term s (PAsData PFieldCarriageV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCommittedFieldClaimV1)

data PCommittedFieldVerdictV1 s = PCommittedFieldVerdictV1
  { pcommittedVerdict'fieldIndex :: Term s (PAsData PInteger)
  , pcommittedVerdict'verdict :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCommittedFieldVerdictV1)

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

pverdictGrammatical, pverdictMissingArrayHeader, pverdictMissingItemHeader, pverdictTrailingBytes, pverdictCodeCount :: forall s. Term s PInteger
pverdictGrammatical = 0
pverdictMissingArrayHeader = 1
pverdictMissingItemHeader = 5
pverdictTrailingBytes = 10
pverdictCodeCount = 11

pverdictNotAnArrayHeader, pverdictNonMinimalArrayHeader, pverdictTruncatedArrayHeader :: forall s. Term s PInteger
pverdictNotAnArrayHeader = 2
pverdictNonMinimalArrayHeader = 3
pverdictTruncatedArrayHeader = 4

pverdictNotAnItemHeader, pverdictNonMinimalItemHeader, pverdictTruncatedItemHeader, pverdictTruncatedItemPayload :: forall s. Term s PInteger
pverdictNotAnItemHeader = 6
pverdictNonMinimalItemHeader = 7
pverdictTruncatedItemHeader = 8
pverdictTruncatedItemPayload = 9

-- | Total §5.1 grammar verdict over arbitrary bytes.
penvelopeVerdictV1 :: forall s. Term s (PByteString :--> PInteger)
penvelopeVerdictV1 = phoistAcyclic $ plam $ \preimage ->
  plet (plengthBS # preimage) $ \totalLength ->
    plet
      ( pfix $ \walk -> plam $ \offset remaining ->
          pif
            (remaining #<= 0)
            (pif (offset #== totalLength) pverdictGrammatical pverdictTrailingBytes)
            ( pif
                (offset #>= totalLength)
                pverdictMissingItemHeader
                ( plet (pbyteAt # preimage # offset) $ \tag ->
                    pif
                      (tag #>= 64 #&& tag #<= 87)
                      (advance walk (offset + 1) (tag - 64) remaining totalLength)
                      ( pif
                          (tag #== 88)
                          ( pif
                              (offset + 2 #> totalLength)
                              pverdictTruncatedItemHeader
                              ( plet (pbyteAt # preimage # (offset + 1)) $ \len ->
                                  pif
                                    (len #< 24)
                                    pverdictNonMinimalItemHeader
                                    (advance walk (offset + 2) len remaining totalLength)
                              )
                          )
                          ( pif
                              (tag #== 89)
                              ( pif
                                  (offset + 3 #> totalLength)
                                  pverdictTruncatedItemHeader
                                  ( plet (pbyteAt # preimage # (offset + 1) * 256 + pbyteAt # preimage # (offset + 2)) $ \len ->
                                      pif
                                        (len #<= 255)
                                        pverdictNonMinimalItemHeader
                                        (advance walk (offset + 3) len remaining totalLength)
                                  )
                              )
                              pverdictNotAnItemHeader
                          )
                      )
                )
            )
      )
      $ \walk ->
        pif
          (totalLength #== 0)
          pverdictMissingArrayHeader
          ( plet (pbyteAt # preimage # 0) $ \tag ->
              pif
                (tag #>= 128 #&& tag #<= 151)
                (walk # 1 # (tag - 128))
                ( pif
                    (tag #== 152)
                    ( pif
                        (totalLength #< 2)
                        pverdictTruncatedArrayHeader
                        ( plet (pbyteAt # preimage # 1) $ \count ->
                            pif (count #< 24) pverdictNonMinimalArrayHeader (walk # 2 # count)
                        )
                    )
                    ( pif
                        (tag #== 153)
                        ( pif
                            (totalLength #< 3)
                            pverdictTruncatedArrayHeader
                            ( plet (pbyteAt # preimage # 1 * 256 + pbyteAt # preimage # 2) $ \count ->
                                pif (count #<= 255) pverdictNonMinimalArrayHeader (walk # 3 # count)
                            )
                        )
                        pverdictNotAnArrayHeader
                    )
                )
          )
  where
    advance walk payloadOffset len remaining totalLength =
      pif
        (payloadOffset + len #> totalLength)
        pverdictTruncatedItemPayload
        (walk # (payloadOffset + len) # (remaining - 1))

pcommittedFieldVerdictV1 ::
  forall s.
  Term s PVerifiedMidgardNativeTxCompact ->
  Term s PCommittedFieldClaimV1 ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PCommittedFieldVerdictV1
pcommittedFieldVerdictV1 verified claim referenceInputs certificatePolicy =
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
      pcon $ PCommittedFieldVerdictV1
        (pdata fieldIndex)
        ( pdata $
            penvelopeVerdictV1
              # ( pauthenticatedCommittedPreimage
                    # verified # witnessSet # fieldIndex # carriage # referenceInputs # certificatePolicy
                )
        )

pisCanonicalDecodabilityViolationV1 :: forall s. Term s (PInteger :--> PInteger :--> PBool)
pisCanonicalDecodabilityViolationV1 = phoistAcyclic $ plam $ \fieldIndex verdict ->
  pif
    ( fieldIndex #>= 0 #&& fieldIndex #< pfieldCount
        #&& verdict #>= 0 #&& verdict #< pverdictCodeCount
    )
    (verdict #/= pverdictGrammatical)
    perror
