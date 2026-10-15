module Midgard.FraudProofs.ValueUnion (
  PConservationClaim (..),
  PComplete (..),
  PChunkCarriage (..),
  PBalanceState (..),
  PFoldState (..),
  PPendingContribution (..),
  PAssetDeltaWitness (..),
  PSourceState (..),
  PEventState (..),
  PInputs (..),
  PSelectedInput (..),
  PAssetCursor (..),
  PFieldGrammar (..),
  PFieldCursor (..),
  POutputItem (..),
  POutputScan (..),
  PMintPolicy (..),
  PMintCursor (..),
  pemptyDeltaRoot,
  papplyContribution,
  pterminalClaimHolds,
  pauthenticatedChunkHashes,
  popenChunks,
  ppolicyHeader,
  passet,
  padvanceOutput,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.Common.Types (PProof)
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pdecodeCanonicalIntAt, pexpectByte)
import Midgard.FraudProofs.NativeTx.Preimages (pcanonicalBytesKeyPrecedes, pdecodeCanonicalBytesAt, pdecodeCanonicalMapHeaderAt)
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput)
import Midgard.FraudProofs.ValueNotPreserved (PClaimedAssetV1 (..), PClaimedImbalanceDirectionV1, PStep04State (..), pvalueNotPreservedFaultIsEstablishedV1)
import Midgard.LedgerOutputScan qualified as Scan
import Midgard.LedgerState (PEventKey)
import Midgard.MpfProof qualified as Mpf
import Midgard.NativeTxFieldAccess (PFieldViewV1 (..), pchunkBytesK, pfieldReadRange)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (POutputDatum (..), PTxInInfo (..), PTxOut (..))
import Plutarch.MerkleTree.Merkling (pnull_hash)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

data PConservationClaim s = PAcceptedImbalance (Term s (PAsData PClaimedAssetV1)) (Term s (PAsData PClaimedImbalanceDirectionV1)) | PForcedConservation
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PConservationClaim)

data PComplete s = PComplete
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PComplete)

data PChunkCarriage s
  = PInlineChunks (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PReferencedChunks (Term s (PAsData (PBuiltinList (PAsData PInteger))))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PChunkCarriage)

data PBalanceState s = PBalanceState
  { pbalanceState'transactionId :: Term s (PAsData (PByteString))
  , pbalanceState'claim :: Term s (PAsData (PConservationClaim))
  , pbalanceState'preUtxosRoot :: Term s (PAsData (PByteString))
  , pbalanceState'lovelaceDelta :: Term s (PAsData (PInteger))
  , pbalanceState'assetDeltaRoot :: Term s (PAsData (PByteString))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBalanceState)

data PFoldState s = PFoldState
  { pfoldState'balance :: Term s (PAsData (PBalanceState))
  , pfoldState'continuation :: Term s (PData)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFoldState)

data PPendingContribution s = PPendingContribution
  { ppendingContribution'unit :: Term s (PAsData (PByteString))
  , ppendingContribution'quantity :: Term s (PAsData (PInteger))
  , ppendingContribution'returnScriptHash :: Term s (PAsData (PByteString))
  , ppendingContribution'nextContinuation :: Term s (PData)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPendingContribution)

data PAssetDeltaWitness s = PAssetDeltaWitness
  { passetDeltaWitness'delta :: Term s (PAsData (PInteger))
  , passetDeltaWitness'proof :: Term s (PAsData (PProof))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAssetDeltaWitness)

data PSourceState s = PSourceState
  { psourceState'transactionId :: Term s (PAsData (PByteString))
  , psourceState'claim :: Term s (PAsData (PConservationClaim))
  , psourceState'fee :: Term s (PAsData (PInteger))
  , psourceState'eventKey :: Term s (PAsData (PEventKey))
  , psourceState'eventRoot :: Term s (PAsData (PByteString))
  , psourceState'eventCount :: Term s (PAsData (PInteger))
  , psourceState'traceRoot :: Term s (PAsData (PByteString))
  , psourceState'traceCount :: Term s (PAsData (PInteger))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSourceState)

data PEventState s = PEventState
  { peventState'transactionId :: Term s (PAsData (PByteString))
  , peventState'claim :: Term s (PAsData (PConservationClaim))
  , peventState'fee :: Term s (PAsData (PInteger))
  , peventState'eventKey :: Term s (PAsData (PEventKey))
  , peventState'traceRoot :: Term s (PAsData (PByteString))
  , peventState'traceCount :: Term s (PAsData (PInteger))
  , peventState'stepIndex :: Term s (PAsData (PInteger))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PEventState)

data PInputs s = PInputs
  { pinputs'cursor :: Term s (PAsData (PInteger))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PInputs)

data PSelectedInput s = PSelectedInput
  { pselectedInput'input :: Term s (PAsData (PMidgardTxInput))
  , pselectedInput'cursor :: Term s (PAsData (PInteger))
  , pselectedInput'selectorHash :: Term s (PAsData (PByteString))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSelectedInput)

data PAssetCursor s = PAssetCursor
  { passetCursor'count :: Term s (PAsData (PInteger))
  , passetCursor'frontierCommitment :: Term s (PAsData (PByteString))
  , passetCursor'cursor :: Term s (PAsData (PInteger))
  , passetCursor'quantitySign :: Term s (PAsData (PInteger))
  , passetCursor'nextScriptHash :: Term s (PAsData (PByteString))
  , passetCursor'nextContinuation :: Term s (PData)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAssetCursor)

data PFieldGrammar s = PFieldGrammar
  { pfieldGrammar'fieldIndex :: Term s (PAsData (PInteger))
  , pfieldGrammar'checkpointHash :: Term s (PMaybeData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFieldGrammar)

data PFieldCursor s = PFieldCursor
  { pfieldCursor'fieldIndex :: Term s (PAsData (PInteger))
  , pfieldCursor'checkpointHash :: Term s (PAsData (PByteString))
  , pfieldCursor'grammarScriptHash :: Term s (PAsData (PByteString))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFieldCursor)

data POutputItem s = POutputItem
  { poutputItem'fieldTotalLength :: Term s (PAsData (PInteger))
  , poutputItem'fieldChunkHashes :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , poutputItem'index :: Term s (PAsData (PInteger))
  , poutputItem'offset :: Term s (PAsData (PInteger))
  , poutputItem'length :: Term s (PAsData (PInteger))
  , poutputItem'checkpointHash :: Term s (PAsData (PByteString))
  , poutputItem'nextCheckpointHash :: Term s (PAsData (PByteString))
  , poutputItem'selectorHash :: Term s (PAsData (PByteString))
  , poutputItem'grammarScriptHash :: Term s (PAsData (PByteString))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POutputItem)

data POutputScan s = POutputScan
  { poutputScan'item :: Term s (PAsData (POutputItem))
  , poutputScan'control :: Term s (PAsData (Scan.PLedgerOutputScanControlV1))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POutputScan)

data PMintPolicy s = PMintPolicy
  { pmintPolicy'offset :: Term s (PAsData (PInteger))
  , pmintPolicy'length :: Term s (PAsData (PInteger))
  , pmintPolicy'nextCheckpointHash :: Term s (PAsData (PByteString))
  , pmintPolicy'policyId :: Term s (PAsData (PByteString))
  , pmintPolicy'cursor :: Term s (PAsData (PInteger))
  , pmintPolicy'remaining :: Term s (PAsData (PInteger))
  , pmintPolicy'previousAsset :: Term s (PMaybeData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintPolicy)

data PMintCursor s = PMintCursor
  { pmintCursor'field :: Term s (PAsData (PFieldCursor))
  , pmintCursor'previousPolicy :: Term s (PMaybeData PByteString)
  , pmintCursor'active :: Term s (PMaybeData PMintPolicy)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintCursor)

pemptyDeltaRoot :: forall s. Term s PByteString
pemptyDeltaRoot = pnull_hash

papplyContribution :: forall s. Term s (PByteString :--> PByteString :--> PInteger :--> PInteger :--> PProof :--> PByteString)
papplyContribution = phoistAcyclic $ plam $ \root unit quantity old proof ->
  pif
    (plengthBS # unit #>= 28 #&& plengthBS # unit #<= 60 #&& quantity #/= 0 #&& Mpf.pproofHasAtMostSteps # proof # 64)
    ( P.do
        let next = old + quantity
        PJust result <-
          pmatch $
            pif
              (old #== 0)
              (Mpf.pinsertRoot # root # unit # (pcborInt quantity) # proof)
              ( pif
                  (next #== 0)
                  (Mpf.pdeleteRootPairedFold # root # unit # (pcborInt old) # proof)
                  (Mpf.pupdateRoot # root # unit # (pcborInt old) # (pcborInt next) # proof)
              )
        result
    )
    perror

pterminalClaimHolds :: forall s. Term s (PBalanceState :--> PMaybeData PAssetDeltaWitness :--> PBool)
pterminalClaimHolds = phoistAcyclic $ plam $ \state witness -> pmatch state $ \s ->
  pmatch (pfromData $ pbalanceState'claim s) $ \case
    PForcedConservation -> pmatch witness $ \case
      PDNothing -> pfromData (pbalanceState'lovelaceDelta s) #== 0 #&& pfromData (pbalanceState'assetDeltaRoot s) #== pemptyDeltaRoot
      _ -> perror
    PAcceptedImbalance asset direction -> P.do
      delta <- plet $ pmatch (pfromData asset) $ \case
        PAdaAsset -> pmatch witness $ \case PDNothing -> pfromData $ pbalanceState'lovelaceDelta s; _ -> perror
        PTokenAsset policy name -> P.do
          PDJust evidence <- pmatch witness
          PAssetDeltaWitness delta proof <- pmatch $ pfromData evidence
          pif (Mpf.phasV1 # pfromData (pbalanceState'assetDeltaRoot s) # (pfromData policy <> pfromData name) # (pcborInt (pfromData delta)) # pfromData proof) (pfromData delta) perror
      pvalueNotPreservedFaultIsEstablishedV1 # pcon (PStep04State (pbalanceState'transactionId s) asset direction (pdata delta))

phashes :: forall s. Term s (PByteString :--> PInteger :--> PBuiltinList PByteString)
phashes = phoistAcyclic $ pfix $ \self -> plam $ \bytes offset ->
  pif (offset #== plengthBS # bytes) pnil $
    plet (plengthBS # bytes - offset) $ \remaining ->
      plet (pif (remaining #< pchunkBytesK) remaining pchunkBytesK) $ \size ->
        pcons # (pblake2b_256 # (psliceBS # offset # size # bytes)) # (self # bytes # (offset + size))

pauthenticatedChunkHashes :: forall s. Term s (PFieldViewV1 :--> PBuiltinList PByteString)
pauthenticatedChunkHashes = phoistAcyclic $ plam $ \view -> pmatch view $ \case
  PWholeView bytes _ _ -> phashes # bytes # 0
  PProvisionalWholeView bytes _ _ -> phashes # bytes # 0
  PChunkedView _ digests _ _ -> digests

plengthsExact :: forall s. Term s (PBuiltinList PByteString :--> PInteger :--> PBool)
plengthsExact = phoistAcyclic $ pfix $ \self -> plam $ \chunks remaining ->
  pelimList
    ( \chunk rest -> plet (pif (remaining #< pchunkBytesK) remaining pchunkBytesK) $ \size ->
        remaining #> 0 #&& plengthBS # chunk #== size #&& self # rest # (remaining - size)
    )
    (remaining #== 0)
    chunks

popenChunks :: forall s. Term s (PChunkCarriage :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList PByteString :--> PInteger :--> PFieldViewV1)
popenChunks = phoistAcyclic $ plam $ \carriage refs digests total -> P.do
  chunks <- plet $ pmatch carriage $ \case
    PInlineChunks chunks -> pmap # plam pfromData # pfromData chunks
    PReferencedChunks indices ->
      pmap
        # plam
          ( \index ->
              pif
                (pfromData index #>= 0)
                ( P.do
                    PTxInInfo _ output <- pmatch $ pfromData $ pelemAt # pfromData index # refs
                    PTxOut{ptxOut'datum} <- pmatch output
                    POutputDatum datum <- pmatch ptxOut'datum
                    pasByteStr # pto datum
                )
                perror
          )
        # pfromData indices
  pif
    (plength # chunks #== plength # digests #&& plengthsExact # chunks # total)
    (pcon $ PChunkedView chunks digests 0 0)
    perror

ppolicyHeader :: forall s. Term s (PByteString :--> PMaybeData PByteString :--> PPair PByteString (PPair PInteger PInteger))
ppolicyHeader = phoistAcyclic $ plam $ \bytes previous -> P.do
  PPair offset policy <- pmatch $ pdecodeCanonicalBytesAt # bytes # (pexpectByte # bytes # 0 # 130)
  PPair cursor count <- pmatch $ pdecodeCanonicalMapHeaderAt # bytes # offset
  let ordered = pmatch previous $ \case PDNothing -> pconstant True; PDJust before -> pcanonicalBytesKeyPrecedes # pfromData before # policy
  pif (plengthBS # policy #== 28 #&& ordered #&& count #> 0) (pcon $ PPair policy $ pcon $ PPair cursor count) perror

passet :: forall s. Term s (PByteString :--> PMaybeData PByteString :--> PPair PByteString (PPair PInteger PInteger))
passet = phoistAcyclic $ plam $ \bytes previous -> P.do
  PPair offset name <- pmatch $ pdecodeCanonicalBytesAt # bytes # 0
  PPair consumed quantity <- pmatch $ pdecodeCanonicalIntAt # bytes # offset
  let ordered = pmatch previous $ \case PDNothing -> pconstant True; PDJust before -> pcanonicalBytesKeyPrecedes # pfromData before # name
  pif (plengthBS # name #<= 32 #&& ordered #&& quantity #/= 0) (pcon $ PPair name $ pcon $ PPair quantity consumed) perror

padvanceOutput :: forall s. Term s (PFieldViewV1 :--> PInteger :--> PInteger :--> Scan.PLedgerOutputScanControlV1 :--> PInteger :--> Scan.PLedgerOutputScanControlV1)
padvanceOutput = phoistAcyclic $ pfix $ \self -> plam $ \view offset len control budget ->
  pif (budget #== 0 #|| Scan.pterminalIsExactV1 # control # len) control $
    pmatch (Scan.pfinishV1 # control # len) $ \case
      PJust complete -> complete
      PNothing -> P.do
        c <- pmatch control
        let cursor = pfromData $ Scan.pscan'cursor c
            remaining = len - cursor
            size = pif (remaining #< 132) remaining 132
        pif
          (remaining #> 0)
          ( P.do
              window <- plet $ pfieldReadRange # view # (offset + cursor) # size
              PJust next <- pmatch $ Scan.pstepV1 # control # len # window # 0
              self # view # offset # len # next # (budget - 1)
          )
          perror
