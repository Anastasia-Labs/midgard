{- | Canonical verdict subjects for non-interactive proof threads. An accepted
subject consumes already authenticated native carriage; a forced subject
verifies the counted root, native source, verdict and thread header here.
-}
module Midgard.FraudProofs.ProofThreadSubstrate (
  PVerdictSubject (..),
  psubjectIsCanonical,
  pencodeVerdictSubject,
  pbindAcceptedSubject,
  pbindForcedSubject,
  pbindForcedSubjectToThread,
  prejectionReasonOf,
  pbindExactRejectionReason,
  pterminalContradiction,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.FraudProofCatalogue (pidByteCount)
import Midgard.FraudProofs.NativeTx.Codec (pencodeDefiniteArrayHeader, pencodeDefiniteBytes)
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Types
import Midgard.LedgerState
import Midgard.RejectionReason (POperatorVerdictV1 (..), PRejectionReasonV1)
import Midgard.TransitionTrace
import Plutarch.Builtin.Crypto (pblake2b_224)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

{- | Field order is the target VerdictSubjectV1 Data ABI. The public byte
encoding below is a definite CBOR array, not this constructor encoding.
-}
data PVerdictSubject (s :: S) = PVerdictSubject
  { psubject'version :: Term s (PAsData PInteger)
  , psubject'direction :: Term s (PAsData PInteger)
  , psubject'sourceKind :: Term s (PAsData PInteger)
  , psubject'transactionId :: Term s (PAsData PByteString)
  , psubject'sourceKey :: Term s (PAsData PByteString)
  , psubject'rejectionReason :: Term s (PAsData (PMaybeData PRejectionReasonV1))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PVerdictSubject)

psubjectIsCanonical :: forall s. Term s (PVerdictSubject :--> PBool)
psubjectIsCanonical = phoistAcyclic $ plam $ \subject -> P.do
  PVerdictSubject {..} <- pmatch subject
  let direction = pfromData psubject'direction
      sourceKind = pfromData psubject'sourceKind
      key = pfromData psubject'sourceKey
      hasReason = pmatch (pfromData psubject'rejectionReason) $ \case
        PDNothing -> pconstant False
        PDJust _ -> pconstant True
  pfromData psubject'version
    #== 1
    #&& plengthBS
    # pfromData psubject'transactionId
    #== 32
    #&& pif
      (sourceKind #== 0)
      (direction #== 0 #&& key #== pconstant "" #&& pnot # hasReason)
      ( sourceKind
          #== 1
          #&& plengthBS
          # key
          #> 0
          #&& pif (direction #== 0) (pnot # hasReason) (direction #== 1 #&& hasReason)
      )

pencodeVerdictSubject :: forall s. Term s (PVerdictSubject :--> PByteString)
pencodeVerdictSubject = phoistAcyclic $ plam $ \subject -> P.do
  PVerdictSubject {..} <- pmatch subject
  pif
    (psubjectIsCanonical # subject)
    ( (pencodeDefiniteArrayHeader # 6)
        <> (pserialiseData # pforgetData psubject'version)
        <> (pserialiseData # pforgetData psubject'direction)
        <> (pserialiseData # pforgetData psubject'sourceKind)
        <> (pencodeDefiniteBytes # pfromData psubject'transactionId)
        <> (pencodeDefiniteBytes # pfromData psubject'sourceKey)
        <> pmatch
          (pfromData psubject'rejectionReason)
          ( \case
              PDNothing -> pencodeDefiniteArrayHeader # 0
              PDJust reason -> (pencodeDefiniteArrayHeader # 1) <> (pserialiseData # pforgetData reason)
          )
    )
    perror

pbindAcceptedSubject :: forall s. Term s (PVerifiedMidgardNativeTxCompact :--> PVerdictSubject)
pbindAcceptedSubject = phoistAcyclic $ plam $ \verified -> P.do
  PVerifiedMidgardNativeTxCompact {..} <- pmatch verified
  PNativeTxCompact {pcompact'validityCode} <- pmatch pverified'txCompact
  pif
    ( pverified'version
        #== pnativeTxVersionV1
        #&& plengthBS
        # pverified'txId
        #== 32
        #&& pcompact'validityCode
        #== 0
    )
    ( pcon $
        PVerdictSubject
          (pdata 1)
          (pdata 0)
          (pdata 0)
          (pdata pverified'txId)
          (pdata $ pconstant "")
          (pdata $ pcon PDNothing)
    )
    perror

pbindForcedSubject ::
  forall s.
  Term s (PHeaderV1 :--> PRootMembershipProof :--> PInteger :--> PVerdictSubject)
pbindForcedSubject = phoistAcyclic $ plam $ \header membership direction -> P.do
  PHeaderV1 {pheader'protocolVersion, pheader'forcedTransactionsRoot, pheader'forcedTransactionCount} <- pmatch header
  PRootMembershipProof {prootMembership'key, prootMembership'value} <- pmatch membership
  PForcedInclusionTxV1 {..} <- pmatch $ punsafeCoerceData @PForcedInclusionTxV1 prootMembership'value
  PNativeTxProofSourceV1 {..} <- pmatch $ pfromData pforcedTx'source
  PPair verified _ <-
    pmatch $
      pverifyNativeTxProofSourceV1
        # pfromData pforcedTx'txId
        # pfromData pnativeSource'compactCbor
        # pfromData pnativeSource'witnessSetCompactCbor
        # pfromData pnativeSource'fieldPreimageLengthsCbor
  PVerifiedMidgardNativeTxCompact {..} <- pmatch verified
  PNativeTxCompact {pcompact'validityCode} <- pmatch pverified'txCompact
  keyBytes <- plet $ pserialiseData # prootMembership'key
  pif
    ( pfromData pheader'protocolVersion
        #== 1
        #&& pverifyRootMembershipWithBytes
          membership
          (pdata $ pcon PForcedTransactionsV1RootDomain)
          (pfromData pheader'forcedTransactionsRoot)
          (pfromData pheader'forcedTransactionCount)
          keyBytes
          (pserialiseData # prootMembership'value)
        #&& pverified'version
        #== pnativeTxVersionV1
        #&& pverified'txId
        #== pfromData pforcedTx'txId
    )
    ( plet
        ( pmatch (pfromData pforcedTx'verdict) $ \case
            PForcedTxValid -> pif (direction #== 0 #&& pcompact'validityCode #== 0) (pcon PDNothing) perror
            PForcedTxInvalid reason ->
              pif
                (direction #== 1 #&& pcompact'validityCode #== 1)
                (pcon $ PDJust $ pdata $ punsafeCoerceData @PRejectionReasonV1 reason)
                perror
        )
        $ \reason ->
          pcon $
            PVerdictSubject
              (pdata 1)
              (pdata direction)
              (pdata 1)
              pforcedTx'txId
              (pdata keyBytes)
              (pdata reason)
    )
    perror

pbindForcedSubjectToThread ::
  forall s.
  Term s (PByteString :--> PHeaderV1 :--> PRootMembershipProof :--> PInteger :--> PVerdictSubject)
pbindForcedSubjectToThread = phoistAcyclic $ plam $ \assetName header membership direction ->
  pif
    ( (pblake2b_224 #$ pserialiseData # pforgetData (pdata header))
        #== psliceBS
        # pidByteCount
        # (plengthBS # assetName)
        # assetName
    )
    (pbindForcedSubject # header # membership # direction)
    perror

prejectionReasonOf :: forall s. Term s (PVerdictSubject :--> PRejectionReasonV1)
prejectionReasonOf = phoistAcyclic $ plam $ \subject ->
  pif
    (psubjectIsCanonical # subject)
    ( pmatch subject $ \PVerdictSubject {psubject'rejectionReason} ->
        pmatch (pfromData psubject'rejectionReason) $ \case
          PDNothing -> perror
          PDJust reason -> pfromData reason
    )
    perror

pbindExactRejectionReason ::
  forall s.
  Term s (PVerdictSubject :--> PRejectionReasonV1 :--> PRejectionReasonV1)
pbindExactRejectionReason = phoistAcyclic $ plam $ \subject expected ->
  plet (prejectionReasonOf # subject) $ \actual ->
    pif ((pserialiseData # pforgetData (pdata actual)) #== (pserialiseData # pforgetData (pdata expected))) actual perror

pterminalContradiction :: forall s. Term s (PVerdictSubject :--> PBool :--> PBool)
pterminalContradiction = phoistAcyclic $ plam $ \subject fault ->
  pif
    (psubjectIsCanonical # subject)
    ( pmatch subject $ \PVerdictSubject {psubject'direction} ->
        pif
          (pfromData psubject'direction #== 0)
          fault
          (plet (prejectionReasonOf # subject) $ \_ -> pnot # fault)
    )
    perror

punsafeCoerceData :: forall a s. (PIsData a) => Term s PData -> Term s a
punsafeCoerceData = pfromData . punsafeCoerce
