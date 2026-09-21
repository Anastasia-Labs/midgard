-- | Shared control carriers and executor selection for both item-proof callers.
module Midgard.ScriptSourcesItemNormalization where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.CekDataTraverse qualified as Traverse
import Midgard.RedeemerItemProof qualified as Item
import Midgard.ScriptSourcesRedeemerNormalization qualified as Envelope
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

data PTraversalChecked s = PTraversalChecked
  { ptraversal'checkingNext :: Term s (PAsData PBool)
  , ptraversal'envelope :: Term s (PAsData Envelope.PPreparedScriptSourcesRedeemerEnvelopeV1)
  , ptraversal'currentControl :: Term s (PAsData Item.PRedeemerItemProofControlV1)
  , ptraversal'claimedNext :: Term s PData
  , ptraversal'witnessHash :: Term s (PAsData PByteString)
  , ptraversal'checkedOptionalTraversalCbor :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTraversalChecked)
data PCurrentChecked s = PCurrentChecked
  { pcurrent'envelope :: Term s (PAsData Envelope.PPreparedScriptSourcesRedeemerEnvelopeV1)
  , pcurrent'control :: Term s (PAsData Item.PRedeemerItemProofControlV1)
  , pcurrent'claimedNext :: Term s PData
  , pcurrent'witnessHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCurrentChecked)
data PControlsChecked s = PControlsChecked
  { pcontrols'envelope :: Term s (PAsData Envelope.PPreparedScriptSourcesRedeemerEnvelopeV1)
  , pcontrols'current :: Term s (PAsData Item.PRedeemerItemProofControlV1)
  , pcontrols'next :: Term s (PAsData Item.PRedeemerItemProofControlV1)
  , pcontrols'witnessHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PControlsChecked)
data PExecutionOutput s = PExecutionOutput
  { poutput'deploymentId :: Term s (PAsData PByteString)
  , poutput'family :: Term s (PAsData PInteger)
  , poutput'executorScriptHash :: Term s (PAsData PByteString)
  , poutput'settlementScriptHash :: Term s (PAsData PByteString)
  , poutput'attestation :: Term s PData
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PExecutionOutput)
data PTraversalExecution s = PTraversalExecution
  { pexecution'output :: Term s (PAsData PExecutionOutput)
  , pexecution'current :: Term s (PAsData Traverse.PDataTraverseControlV1)
  , pexecution'next :: Term s (PAsData Traverse.PDataTraverseControlV1)
  , pexecution'action :: Term s (PAsData Traverse.PDataTraverseActionV1)
  , pexecution'sourceBytes :: Term s (PAsData (PMaybeData PByteString))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTraversalExecution)
data POuterExecution s = POuterExecution
  { pouter'output :: Term s (PAsData PExecutionOutput)
  , pouter'current :: Term s (PAsData Item.PRedeemerItemProofControlV1)
  , pouter'next :: Term s (PAsData Item.PRedeemerItemProofControlV1)
  , pouter'action :: Term s (PAsData Item.PRedeemerItemProofActionV1)
  , pouter'sourceBytes :: Term s (PAsData (PMaybeData PByteString))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POuterExecution)

pauxiliaryItemFields :: forall s. Term s (PData :--> PPair PData PData)
pauxiliaryItemFields = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag f) ->
  pif (tag #== 18 #&& plength # f #== 3)
    (pmatch (pasConstr # (phead # f)) $ \(PBuiltinPair option fields) ->
      pif (option #== 1 #&& pnull # fields) (pcon $ PPair (pelemAt # 1 # f) (pelemAt # 2 # f)) perror) perror

pcheckedOptionalTraversalCbor :: forall s. Term s (Item.PRedeemerItemProofControlV1 :--> PByteString)
pcheckedOptionalTraversalCbor = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pmatch (pfromData $ Item.predeemerControl'traversal c) $ \case
    PDNothing -> pconstant "\xd8\x7a\x80"
    PDJust inner -> pconstant "\xd8\x79\x9f" <> (Traverse.pencodeControlV1 # pfromData inner) <> pconstant "\xff"

prawDataHash :: forall s. Term s (PData :--> PByteString)
prawDataHash = phoistAcyclic $ plam $ \raw -> pblake2b_256 # (pserialiseData # raw)

pexecutionOutput :: forall s. Term s (Envelope.PPreparedScriptSourcesRedeemerEnvelopeV1 :--> PByteString :--> PExecutionOutput)
pexecutionOutput = phoistAcyclic $ plam $ \state witnessHash -> pmatch state $ \s ->
  pcon $ PExecutionOutput (Envelope.penvelope'deploymentId s) (Envelope.penvelope'actionFamily s)
    (Envelope.penvelope'semanticExecutorScriptHash s) (Envelope.penvelope'settlementScriptHash s)
    (pforgetData $ pdata $ pcon $ Envelope.PScriptSourcesRedeemerExecutionAttestedStateV1
      (pdata Envelope.pversion) (pdata Envelope.pexecutionAttestedDomain) (Envelope.penvelope'deploymentId s)
      (pdata $ Envelope.pbaseProvenanceIdentityV1 # state) (Envelope.penvelope'envelopeBinderScriptHash s)
      (Envelope.penvelope'traversalNormalizerScriptHash s) (Envelope.penvelope'outerNormalizerScriptHash s)
      (Envelope.penvelope'semanticExecutorScriptHash s) (Envelope.penvelope'settlementScriptHash s)
      (Envelope.penvelope'actionFamily s) (Envelope.penvelope'canonicalActionHash s) (pdata witnessHash)
      (Envelope.penvelope'currentPendingItemControlHash s) (Envelope.penvelope'expectedNextItemControlHash s)
      (Envelope.penvelope'expectedNextItemControlHash s) (Envelope.penvelope'redeemerCount s) (Envelope.penvelope'redeemerTotalCount s))

pexecutionOutputIsExact :: forall s. Term s (PExecutionOutput :--> PByteString :--> PInteger :--> PByteString :--> PByteString :--> PData :--> PBool)
pexecutionOutputIsExact = phoistAcyclic $ plam $ \output deployment family inputHash outputHash outputState -> pmatch output $ \o ->
  pfromData (poutput'deploymentId o) #== deployment #&& pfromData (poutput'family o) #== family
    #&& pfromData (poutput'executorScriptHash o) #== inputHash #&& pfromData (poutput'settlementScriptHash o) #== outputHash
    #&& poutput'attestation o #== outputState

pexecutorIndex :: forall s. Term s (PData :--> PData :--> PInteger :--> PInteger)
pexecutorIndex = phoistAcyclic $ plam $ \current witness family -> pmatch (pasConstr # witness) $ \(PBuiltinPair witnessTag witnessFields) ->
  pif (witnessTag #== 0 #&& plength # witnessFields #== 3)
    (pmatch (pasConstr # (phead # witnessFields)) $ \(PBuiltinPair actionTag actionFields) ->
      pif (family #== 0) 0 $ pif (family #== 1) 1 $ pif (family #== 2) 2 $ pif (family #== 3) 3 $
      pif (family #== 8) 16 $ pif (family #== 9) (pif (actionTag #== 0) 17 (pif (actionTag #== 1) 18 perror)) $
      pif (actionTag #== 2 #&& plength # actionFields #== 1)
        (pmatch (pasConstr # (phead # actionFields)) $ \(PBuiltinPair traversalTag _) ->
          pif (family #== 4) (pif (traversalTag #>= 1 #&& traversalTag #<= 4) (traversalTag + 3) perror) $
          pif (family #== 6) 10 $
          pmatch (pasConstr # current) $ \(PBuiltinPair controlTag controlFields) ->
          pif (controlTag #== 0)
            (pmatch (pasConstr # (pelemAt # 15 # controlFields)) $ \(PBuiltinPair optionTag optionFields) ->
              pif (optionTag #== 0 #&& plength # optionFields #== 1)
                (pmatch (pasConstr # (phead # optionFields)) $ \(PBuiltinPair innerTag innerFields) ->
                  plet (pasInt # (pelemAt # 1 # innerFields)) $ \stage ->
                  pif (innerTag #== 0)
                    (pif (family #== 5) (pif (stage #== 1 #|| stage #== 2) (stage + 7) perror)
                      (pif (family #== 7 #&& stage #>= 1 #&& stage #<= 5) (stage + 10) perror)) perror) perror) perror) perror) perror
