{-# LANGUAGE OverloadedStrings #-}

module Midgard.FraudProofs.DaHashPreimage (
  PSourceEnvelopeV1 (..),
  PCompactInspectionV1 (..),
  PVerdictV1 (..),
  pinspectSourceEnvelopeV1,
  pinspectCompactV1,
  pwitnessSetIsCanonicalV1,
  pfieldLengthsAreCanonicalV1,
  padjudicateCommittedSourceLeafV1,
  pisDaHashPreimageViolationV1,
  PStep02State (..),
  PStep02Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils ((#/=))
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottRec (..))

import Aiken.Cbor (pdeserialise)
import Midgard.FraudProofs.NativeTx.Compact (
  pencodeNativeTxBodyCompact,
  pencodeNativeTxCompactV1,
  pencodeNativeTxFieldPreimageLengthsV1,
  pencodeNativeTxWitnessSetCompact,
  pnativeTxIdForVersion,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxFieldPreimageLengthsV1 (..),
  PNativeTxWitnessSetCompact (..),
  pnativeTxVersionV1,
 )

-- | Aiken @rule.SourceEnvelopeV1@. Internal Scott representation.
data PSourceEnvelopeV1 (s :: S) = PSourceEnvelopeV1
  { psourceEnvelope'embeddedTxId :: Term s PByteString
  , psourceEnvelope'compactCbor :: Term s PByteString
  , psourceEnvelope'witnessSetCompactCbor :: Term s PByteString
  , psourceEnvelope'fieldPreimageLengthsCbor :: Term s PByteString
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PSourceEnvelopeV1)

-- | Aiken @rule.CompactInspectionV1@. Internal Scott representation.
data PCompactInspectionV1 (s :: S) = PCompactInspectionV1
  { pcompactInspection'derivedTxId :: Term s PByteString
  , pcompactInspection'witnessSetHash :: Term s PByteString
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PCompactInspectionV1)

-- | Ordered Aiken @rule.VerdictV1@ constructors; constructor order is ABI.
data PVerdictV1 (s :: S)
  = PMalformedSource
  | PKeyMismatch
  | PMalformedProofSource
  | PDerivedIdMismatch
  | PNoViolation
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PVerdictV1)

pdataIsConstr, pdataIsList, pdataIsInt, pdataIsBytes :: forall s. Term s (PData :--> PBool)
pdataIsConstr = phoistAcyclic $ plam $ \d ->
  pchooseData # d # pconstant True # pconstant False # pconstant False # pconstant False # pconstant False
pdataIsList = phoistAcyclic $ plam $ \d ->
  pchooseData # d # pconstant False # pconstant False # pconstant True # pconstant False # pconstant False
pdataIsInt = phoistAcyclic $ plam $ \d ->
  pchooseData # d # pconstant False # pconstant False # pconstant False # pconstant True # pconstant False
pdataIsBytes = phoistAcyclic $ plam $ \d ->
  pchooseData # d # pconstant False # pconstant False # pconstant False # pconstant False # pconstant True

-- | Total structural decoder for exact @Data(L2TransactionSourceV1)@.
pinspectSourceEnvelopeV1 :: forall s. Term s (PByteString :--> PMaybe PSourceEnvelopeV1)
pinspectSourceEnvelopeV1 = phoistAcyclic $ plam $ \sourceCbor ->
  pmatch (pdeserialise # sourceCbor) $ \case
    PNothing -> pcon PNothing
    PJust sourceData ->
      pif (pdataIsConstr # sourceData)
        ( pmatch (pasConstr # sourceData) $ \(PBuiltinPair sourceTag sourceFields) ->
            pif (sourceTag #== 0 #&& plength # sourceFields #== 2)
              ( plet (pelemAt # 0 # sourceFields) $ \txIdData ->
                plet (pelemAt # 1 # sourceFields) $ \proofSourceData ->
                pif (pdataIsConstr # proofSourceData)
                  ( pmatch (pasConstr # proofSourceData) $ \(PBuiltinPair proofTag proofFields) ->
                      pif (proofTag #== 0 #&& plength # proofFields #== 3)
                        ( plet (pelemAt # 0 # proofFields) $ \compactData ->
                          plet (pelemAt # 1 # proofFields) $ \witnessData ->
                          plet (pelemAt # 2 # proofFields) $ \lengthsData ->
                          pif
                            ( pdataIsBytes # txIdData
                                #&& pdataIsBytes # compactData
                                #&& pdataIsBytes # witnessData
                                #&& pdataIsBytes # lengthsData
                            )
                            ( plet (pasByteStr # txIdData) $ \txId ->
                              pif
                                (plengthBS # txId #== 32 #&& pserialiseData # sourceData #== sourceCbor)
                                ( pcon $ PJust $ pcon $
                                    PSourceEnvelopeV1
                                      txId
                                      (pasByteStr # compactData)
                                      (pasByteStr # witnessData)
                                      (pasByteStr # lengthsData)
                                )
                                (pcon PNothing)
                            )
                            (pcon PNothing)
                        )
                        (pcon PNothing)
                  )
                  (pcon PNothing)
              )
              (pcon PNothing)
        )
        (pcon PNothing)

-- | Total decoder for the exact twelve-field compact body Data list.
pinspectCompactBodyV1 :: forall s. Term s (PData :--> PMaybe PNativeTxBodyCompact)
pinspectCompactBodyV1 = phoistAcyclic $ plam $ \bodyData ->
  pif (pdataIsList # bodyData)
    ( plet (pasList # bodyData) $ \fields ->
      pif (plength # fields #== 12)
        ( plet (pelemAt # 0 # fields) $ \spendData ->
          plet (pelemAt # 1 # fields) $ \referenceData ->
          plet (pelemAt # 2 # fields) $ \outputsData ->
          plet (pelemAt # 3 # fields) $ \feeData ->
          plet (pelemAt # 4 # fields) $ \startData ->
          plet (pelemAt # 5 # fields) $ \endData ->
          plet (pelemAt # 6 # fields) $ \observersData ->
          plet (pelemAt # 7 # fields) $ \signersData ->
          plet (pelemAt # 8 # fields) $ \mintData ->
          plet (pelemAt # 9 # fields) $ \integrityData ->
          plet (pelemAt # 10 # fields) $ \auxiliaryData ->
          plet (pelemAt # 11 # fields) $ \networkData ->
          pif
            ( pdataIsBytes # spendData
                #&& pdataIsBytes # referenceData
                #&& pdataIsBytes # outputsData
                #&& pdataIsInt # feeData
                #&& pdataIsInt # startData
                #&& pdataIsInt # endData
                #&& pdataIsBytes # observersData
                #&& pdataIsBytes # signersData
                #&& pdataIsBytes # mintData
                #&& pdataIsBytes # integrityData
                #&& pdataIsBytes # auxiliaryData
                #&& pdataIsInt # networkData
            )
            ( plet (pasByteStr # spendData) $ \spend ->
              plet (pasByteStr # referenceData) $ \reference ->
              plet (pasByteStr # outputsData) $ \outputs ->
              plet (pasInt # feeData) $ \fee ->
              plet (pasByteStr # observersData) $ \observers ->
              plet (pasByteStr # signersData) $ \signers ->
              plet (pasByteStr # mintData) $ \mint ->
              plet (pasByteStr # integrityData) $ \integrity ->
              plet (pasByteStr # auxiliaryData) $ \auxiliary ->
              plet (pasInt # networkData) $ \network ->
              pif
                ( plengthBS # spend #== 32
                    #&& plengthBS # reference #== 32
                    #&& plengthBS # outputs #== 32
                    #&& fee #>= 0
                    #&& plengthBS # observers #== 32
                    #&& plengthBS # signers #== 32
                    #&& plengthBS # mint #== 32
                    #&& plengthBS # integrity #== 32
                    #&& plengthBS # auxiliary #== 32
                    #&& (network #== 0 #|| network #== 1 #|| network #== 255)
                )
                ( pcon $ PJust $ pcon $
                    PNativeTxBodyCompact
                      spend
                      reference
                      outputs
                      fee
                      (pasInt # startData)
                      (pasInt # endData)
                      observers
                      signers
                      mint
                      integrity
                      auxiliary
                      network
                )
                (pcon PNothing)
            )
            (pcon PNothing)
        )
        (pcon PNothing)
    )
    (pcon PNothing)

-- | Total compact inspection followed by exact V1 native re-encoding.
pinspectCompactV1 :: forall s. Term s (PByteString :--> PMaybe PCompactInspectionV1)
pinspectCompactV1 = phoistAcyclic $ plam $ \compactCbor ->
  pmatch (pdeserialise # compactCbor) $ \case
    PNothing -> pcon PNothing
    PJust compactData ->
      pif (pdataIsList # compactData)
        ( plet (pasList # compactData) $ \fields ->
          pif (plength # fields #== 4)
            ( plet (pelemAt # 0 # fields) $ \versionData ->
              plet (pelemAt # 1 # fields) $ \bodyData ->
              plet (pelemAt # 2 # fields) $ \witnessHashData ->
              plet (pelemAt # 3 # fields) $ \validityData ->
              pif
                (pdataIsInt # versionData #&& pdataIsBytes # witnessHashData #&& pdataIsInt # validityData)
                ( pmatch (pinspectCompactBodyV1 # bodyData) $ \case
                    PNothing -> pcon PNothing
                    PJust body ->
                      plet (pasInt # versionData) $ \version ->
                      plet (pasByteStr # witnessHashData) $ \witnessHash ->
                      plet (pasInt # validityData) $ \validity ->
                      pif
                        ( version #== pnativeTxVersionV1
                            #&& plengthBS # witnessHash #== 32
                            #&& (validity #== 0 #|| validity #== 1)
                        )
                        ( plet (pcon $ PNativeTxCompact body witnessHash validity) $ \compact ->
                          pif (pencodeNativeTxCompactV1 # compact #== compactCbor)
                            ( pcon $ PJust $ pcon $
                                PCompactInspectionV1
                                  (pnativeTxIdForVersion # pnativeTxVersionV1 # (pencodeNativeTxBodyCompact # body))
                                  witnessHash
                            )
                            (pcon PNothing)
                        )
                        (pcon PNothing)
                )
                (pcon PNothing)
            )
            (pcon PNothing)
        )
        (pcon PNothing)

-- | Exact canonical compact witness-set inspection.
pwitnessSetIsCanonicalV1 :: forall s. Term s (PByteString :--> PBool)
pwitnessSetIsCanonicalV1 = phoistAcyclic $ plam $ \witnessCbor ->
  pmatch (pdeserialise # witnessCbor) $ \case
    PNothing -> pconstant False
    PJust witnessData ->
      pif (pdataIsList # witnessData)
        ( plet (pasList # witnessData) $ \fields ->
          pif (plength # fields #== 3)
            ( plet (pelemAt # 0 # fields) $ \addressData ->
              plet (pelemAt # 1 # fields) $ \scriptData ->
              plet (pelemAt # 2 # fields) $ \redeemerData ->
              pif
                (pdataIsBytes # addressData #&& pdataIsBytes # scriptData #&& pdataIsBytes # redeemerData)
                ( plet (pasByteStr # addressData) $ \address ->
                  plet (pasByteStr # scriptData) $ \script ->
                  plet (pasByteStr # redeemerData) $ \redeemer ->
                  pif
                    (plengthBS # address #== 32 #&& plengthBS # script #== 32 #&& plengthBS # redeemer #== 32)
                    ( pencodeNativeTxWitnessSetCompact
                        # pcon (PNativeTxWitnessSetCompact (pdata address) (pdata script) (pdata redeemer))
                        #== witnessCbor
                    )
                    (pconstant False)
                )
                (pconstant False)
            )
            (pconstant False)
        )
        (pconstant False)

-- | Exact canonical nine-field preimage-length inspection.
pfieldLengthsAreCanonicalV1 :: forall s. Term s (PByteString :--> PBool)
pfieldLengthsAreCanonicalV1 = phoistAcyclic $ plam $ \lengthsCbor ->
  pmatch (pdeserialise # lengthsCbor) $ \case
    PNothing -> pconstant False
    PJust lengthsData ->
      pif (pdataIsList # lengthsData)
        ( plet (pasList # lengthsData) $ \fields ->
          pif (plength # fields #== 9)
            ( plet (pelemAt # 0 # fields) $ \spendData ->
              plet (pelemAt # 1 # fields) $ \referenceData ->
              plet (pelemAt # 2 # fields) $ \outputsData ->
              plet (pelemAt # 3 # fields) $ \observersData ->
              plet (pelemAt # 4 # fields) $ \signersData ->
              plet (pelemAt # 5 # fields) $ \mintData ->
              plet (pelemAt # 6 # fields) $ \scriptData ->
              plet (pelemAt # 7 # fields) $ \addressData ->
              plet (pelemAt # 8 # fields) $ \redeemersData ->
              pif
                ( pdataIsInt # spendData
                    #&& pdataIsInt # referenceData
                    #&& pdataIsInt # outputsData
                    #&& pdataIsInt # observersData
                    #&& pdataIsInt # signersData
                    #&& pdataIsInt # mintData
                    #&& pdataIsInt # scriptData
                    #&& pdataIsInt # addressData
                    #&& pdataIsInt # redeemersData
                )
                ( plet (pasInt # spendData) $ \spend ->
                  plet (pasInt # referenceData) $ \reference ->
                  plet (pasInt # outputsData) $ \outputs ->
                  plet (pasInt # observersData) $ \observers ->
                  plet (pasInt # signersData) $ \signers ->
                  plet (pasInt # mintData) $ \mint ->
                  plet (pasInt # scriptData) $ \script ->
                  plet (pasInt # addressData) $ \address ->
                  plet (pasInt # redeemersData) $ \redeemers ->
                  pif
                    ( spend #>= 0
                        #&& reference #>= 0
                        #&& outputs #>= 0
                        #&& observers #>= 0
                        #&& signers #>= 0
                        #&& mint #>= 0
                        #&& script #>= 0
                        #&& address #>= 0
                        #&& redeemers #>= 0
                    )
                    ( pencodeNativeTxFieldPreimageLengthsV1
                        # pcon
                          ( PNativeTxFieldPreimageLengthsV1
                              spend
                              reference
                              outputs
                              observers
                              signers
                              mint
                              address
                              script
                              redeemers
                          )
                        #== lengthsCbor
                    )
                    (pconstant False)
                )
                (pconstant False)
            )
            (pconstant False)
        )
        (pconstant False)

-- | Total Q44 verdict, in the Aiken rule's stable precedence order.
padjudicateCommittedSourceLeafV1 :: forall s. Term s (PByteString :--> PByteString :--> PVerdictV1)
padjudicateCommittedSourceLeafV1 = phoistAcyclic $ plam $ \committedTxId committedLeafValue ->
  pmatch (pinspectSourceEnvelopeV1 # committedLeafValue) $ \case
    PNothing -> pcon PMalformedSource
    PJust source -> pmatch source $ \PSourceEnvelopeV1 {psourceEnvelope'embeddedTxId, psourceEnvelope'compactCbor, psourceEnvelope'witnessSetCompactCbor, psourceEnvelope'fieldPreimageLengthsCbor} ->
      pif (psourceEnvelope'embeddedTxId #/= committedTxId) (pcon PKeyMismatch) $
        pmatch (pinspectCompactV1 # psourceEnvelope'compactCbor) $ \case
          PNothing -> pcon PMalformedProofSource
          PJust compact -> pmatch compact $ \PCompactInspectionV1 {pcompactInspection'derivedTxId, pcompactInspection'witnessSetHash} ->
            pif
              ( pnot # (pwitnessSetIsCanonicalV1 # psourceEnvelope'witnessSetCompactCbor)
                  #|| pblake2b_256 # psourceEnvelope'witnessSetCompactCbor #/= pcompactInspection'witnessSetHash
                  #|| pnot # (pfieldLengthsAreCanonicalV1 # psourceEnvelope'fieldPreimageLengthsCbor)
              )
              (pcon PMalformedProofSource)
              ( pif
                  (pcompactInspection'derivedTxId #/= psourceEnvelope'embeddedTxId)
                  (pcon PDerivedIdMismatch)
                  (pcon PNoViolation)
              )

pisDaHashPreimageViolationV1 :: forall s. Term s (PVerdictV1 :--> PBool)
pisDaHashPreimageViolationV1 = phoistAcyclic $ plam $ \verdict -> verdict #/= pcon PNoViolation

-- | Aiken @da_hash_preimage/step_02.State@.
data PStep02State (s :: S) = PStep02State
  { pstep02State'verdict :: Term s (PAsData PVerdictV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Aiken @da_hash_preimage/step_02.Args@.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)
