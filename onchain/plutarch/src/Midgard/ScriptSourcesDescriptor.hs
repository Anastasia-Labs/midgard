{- |
Module      : Midgard.ScriptSourcesDescriptor
Description : Narrow ScriptSources redeemer-descriptor claims.

This is the Plutarch representation of the target Aiken
@midgard/script-sources-descriptor.ak@ boundary.  Descriptor steps deliberately
exclude the optional data-traversal control: the shared rewarding validator
authenticates only header/tail descriptor work, while the spending validators
consume the claimed successor.
-}
module Midgard.ScriptSourcesDescriptor (
  PDescriptorControl (..),
  PDescriptorStepClaim (..),
  pfullControl,
  pnarrowControl,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Midgard.BoundedItem (PChunkProofV1)
import Midgard.RedeemerItemProof qualified as RedeemerItemProof

data PDescriptorControl (s :: S) = PDescriptorControl
  { pdescriptorControl'version :: Term s (PAsData PInteger)
  , pdescriptorControl'mode :: Term s (PAsData PInteger)
  , pdescriptorControl'stage :: Term s (PAsData PInteger)
  , pdescriptorControl'itemIndex :: Term s (PAsData PInteger)
  , pdescriptorControl'itemCount :: Term s (PAsData PInteger)
  , pdescriptorControl'totalLength :: Term s (PAsData PInteger)
  , pdescriptorControl'itemCommitment :: Term s (PAsData PByteString)
  , pdescriptorControl'expectedPurposeTag :: Term s (PAsData PInteger)
  , pdescriptorControl'expectedPointerIndex :: Term s (PAsData PInteger)
  , pdescriptorControl'purposeTag :: Term s (PAsData PInteger)
  , pdescriptorControl'pointerIndex :: Term s (PAsData PInteger)
  , pdescriptorControl'dataOffset :: Term s (PAsData PInteger)
  , pdescriptorControl'dataLength :: Term s (PAsData PInteger)
  , pdescriptorControl'executionMemory :: Term s (PAsData PInteger)
  , pdescriptorControl'executionSteps :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDescriptorControl)

data PDescriptorStepClaim (s :: S) = PDescriptorStepClaim
  { pdescriptorClaim'control :: Term s (PAsData PDescriptorControl)
  , pdescriptorClaim'openTail :: Term s (PAsData PBool)
  , pdescriptorClaim'chunkProof :: Term s (PAsData PChunkProofV1)
  , pdescriptorClaim'nextChunkProof :: Term s (PAsData (PMaybeData PChunkProofV1))
  , pdescriptorClaim'claimedNext :: Term s (PAsData PDescriptorControl)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDescriptorStepClaim)

pfullControl :: forall s. Term s (PDescriptorControl :--> RedeemerItemProof.PRedeemerItemProofControlV1)
pfullControl = phoistAcyclic $ plam $ \control ->
  pmatch control $ \c ->
    pcon $
      RedeemerItemProof.PRedeemerItemProofControlV1
        (pdescriptorControl'version c)
        (pdescriptorControl'mode c)
        (pdescriptorControl'stage c)
        (pdescriptorControl'itemIndex c)
        (pdescriptorControl'itemCount c)
        (pdescriptorControl'totalLength c)
        (pdescriptorControl'itemCommitment c)
        (pdescriptorControl'expectedPurposeTag c)
        (pdescriptorControl'expectedPointerIndex c)
        (pdescriptorControl'purposeTag c)
        (pdescriptorControl'pointerIndex c)
        (pdescriptorControl'dataOffset c)
        (pdescriptorControl'dataLength c)
        (pdescriptorControl'executionMemory c)
        (pdescriptorControl'executionSteps c)
        (pdata $ pcon PDNothing)

pnarrowControl :: forall s. Term s (RedeemerItemProof.PRedeemerItemProofControlV1 :--> PDescriptorControl)
pnarrowControl = phoistAcyclic $ plam $ \control ->
  pmatch control $ \c ->
    pmatch (pfromData $ RedeemerItemProof.predeemerControl'traversal c) $ \case
      PDNothing ->
        pcon $
          PDescriptorControl
            (RedeemerItemProof.predeemerControl'version c)
            (RedeemerItemProof.predeemerControl'mode c)
            (RedeemerItemProof.predeemerControl'stage c)
            (RedeemerItemProof.predeemerControl'itemIndex c)
            (RedeemerItemProof.predeemerControl'itemCount c)
            (RedeemerItemProof.predeemerControl'totalLength c)
            (RedeemerItemProof.predeemerControl'itemCommitment c)
            (RedeemerItemProof.predeemerControl'expectedPurposeTag c)
            (RedeemerItemProof.predeemerControl'expectedPointerIndex c)
            (RedeemerItemProof.predeemerControl'purposeTag c)
            (RedeemerItemProof.predeemerControl'pointerIndex c)
            (RedeemerItemProof.predeemerControl'dataOffset c)
            (RedeemerItemProof.predeemerControl'dataLength c)
            (RedeemerItemProof.predeemerControl'executionMemory c)
            (RedeemerItemProof.predeemerControl'executionSteps c)
      PDJust _ -> perror
