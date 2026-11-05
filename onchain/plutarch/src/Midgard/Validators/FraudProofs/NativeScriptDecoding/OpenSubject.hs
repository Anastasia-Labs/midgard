module Midgard.Validators.FraudProofs.NativeScriptDecoding.OpenSubject (
  nativeScriptDecodingOpenSubjectValidator,
) where

import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pcontinue)
import Midgard.FraudProofs.FieldOpening (
  PNativeTxAnchorV1 (..),
  popenedFieldView,
  preferenceInputsFieldIndex,
  pspendInputsFieldIndex,
 )
import Midgard.FraudProofs.NativeScriptDecoding.Engine
import Midgard.FraudProofs.NativeScriptDecoding.Step03 (POpenSubjectArgs (..))
import Midgard.FraudProofs.NativeTx.Components (pencodeMidgardTxInput)
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput (..))
import Midgard.NativeTxMachineWalk (pspendInputAt, pspendInputCount)
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)

nativeScriptDecodingOpenSubjectValidator ::
  forall s.
  Term s
    ( PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
nativeScriptDecodingOpenSubjectValidator = plam $
  \bindDescriptorScriptHash step04ScriptHash computationThreadPolicy fieldCertificatePolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @POpenSubjectArgs computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
        POpenSubjectArgs {popenSubjectArgs'inputIndex, popenSubjectArgs'outputIndex, popenSubjectArgs'fieldOpening} <- pmatch args
        PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
        pcontinue
          computationThreadPolicy
          (pexpectDatum datum)
          (pfromData popenSubjectArgs'inputIndex)
          (pfromData popenSubjectArgs'outputIndex)
          ownOutRef
          (pfromData ptxInfo'inputs)
          (pfromData ptxInfo'outputs)
          $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
            state <- plet $ pexpectStateAs @PScanThreadStateV1 inputState
            st <- pmatch state
            sourceKind <- plet $ pfromData (pscanState'outpointSourceKind st)
            cursor <- plet $ pfromData (pscanState'outpointCursor st)
            subjectOutpoint <- plet $
              pif
                ((sourceKind #/= poutpointSourceSpend #&& sourceKind #/= poutpointSourceReference) #|| cursor #< 0)
                (pmatch (pfromData popenSubjectArgs'fieldOpening) $ \case
                  PDNothing -> pcon PNothing
                  PDJust _ -> perror)
                (pmatch (pfromData popenSubjectArgs'fieldOpening) $ \case
                  PDNothing -> perror
                  PDJust openingD ->
                    plet
                      ( popenedFieldView
                          # pfromData openingD
                          # pcon (PBodyAnchor $ pscanState'verifiedTxId st)
                          # pif (sourceKind #== poutpointSourceSpend) pspendInputsFieldIndex preferenceInputsFieldIndex
                          # pfromData ptxInfo'referenceInputs
                          # fieldCertificatePolicy
                      ) $ \view ->
                        pif (cursor #>= pspendInputCount # view)
                          (pcon PNothing)
                          (pcon $ PJust $ pspendInputAt # view # cursor))
            pexpecting
              ( pand'List
                  [ pfromData (pscanState'machineStateHash st) #== pconstant ""
                  , pfromData (pscanState'refusalClass st) #== pclassPending
                  , pfromData (pscanState'outpointKeyHash st) #== pconstant ""
                  , pfromData (pscanState'referenceScriptLanguage st) #== planguageUnbound
                  , pfromData (pscanState'outputIndex st) #== -1
                  , pfromData (pscanState'totalLength st) #== -1
                  , pfromData (pscanState'itemCommitment st) #== pconstant ""
                  ]
              ) $
              pmatch subjectOutpoint $ \case
                PNothing ->
                  pexpecting (pfromData (pscanState'direction st) #== pdirectionWrongfulRejection) $
                    pexpecting (outputScriptHash #== step04ScriptHash) $
                      plet (pscanStateWithRefusalClassV1 # state # prefusalClassMalformed) $ \expected ->
                        pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)
                PJust outpoint -> pmatch outpoint $ \o ->
                  plet (pencodeMidgardTxInput # outpoint) $ \outpointCbor ->
                  plet (popenedSubjectScanStateV1 # state # outpointCbor # pfromData (ptxInput'outputIndex o)) $ \expected ->
                    pexpecting (outputScriptHash #== bindDescriptorScriptHash) $
                      pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)
