module Midgard.Validators.FraudProofs.NativeScriptDecoding.BindDescriptor (
  nativeScriptDecodingBindDescriptorValidator,
) where

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.BoundedItem (PChunkProofV1 (..))
import Midgard.FraudProofs.Common (pcontinue)
import Midgard.FraudProofs.NativeScriptDecoding.Engine
import Midgard.FraudProofs.NativeScriptDecoding.Step03 (PBindDescriptorArgs (..))
import Midgard.LedgerOutputCommitment (
  PLedgerOutputCommitmentV1 (..),
  pdecodeLedgerOutputCommitment,
  pverifyReferenceScriptChunk,
 )
import Midgard.MpfProof (phasV1)
import Midgard.NativeScriptScan (pencodeStructureControlV1)
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)

nativeScriptDecodingBindDescriptorValidator ::
  forall s.
  Term s
    ( PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
nativeScriptDecodingBindDescriptorValidator = plam $
  \advanceScriptHash step04ScriptHash computationThreadPolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PBindDescriptorArgs computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
        PBindDescriptorArgs
          { pbindDescriptorArgs'inputIndex
          , pbindDescriptorArgs'outputIndex
          , pbindDescriptorArgs'outpointKeyCbor
          , pbindDescriptorArgs'descriptorCbor
          , pbindDescriptorArgs'ledgerMembershipProof
          , pbindDescriptorArgs'firstChunkProof
          } <- pmatch args
        PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
        pcontinue
          computationThreadPolicy
          (pexpectDatum datum)
          (pfromData pbindDescriptorArgs'inputIndex)
          (pfromData pbindDescriptorArgs'outputIndex)
          ownOutRef
          (pfromData ptxInfo'inputs)
          (pfromData ptxInfo'outputs)
          $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
            state <- plet $ pexpectStateAs @PScanThreadStateV1 inputState
            st <- pmatch state
            outpointKey <- plet $ pfromData pbindDescriptorArgs'outpointKeyCbor
            descriptorCbor <- plet $ pfromData pbindDescriptorArgs'descriptorCbor
            descriptor <- plet $ pdecodeLedgerOutputCommitment # descriptorCbor
            d <- pmatch descriptor
            bound <- plet $ pboundDescriptorScanStateV1 # state # descriptor
            pexpecting
              ( pand'List
                  [ pfromData (pscanState'machineStateHash st) #== pconstant ""
                  , pfromData (pscanState'refusalClass st) #== pclassPending
                  , pfromData (pscanState'outpointKeyHash st) #/= pconstant ""
                  , pfromData (pscanState'referenceScriptLanguage st) #== planguageUnbound
                  , pfromData (pscanState'outputIndex st) #>= 0
                  , pfromData (pscanState'totalLength st) #== -1
                  , pfromData (pscanState'itemCommitment st) #== pconstant ""
                  , pblake2b_256 # outpointKey #== pfromData (pscanState'outpointKeyHash st)
                  , pfromData (poutputCommitment'outputIndex d) #== pfromData (pscanState'outputIndex st)
                  , pfromData (poutputCommitment'totalLength d) #> 0
                  , phasV1
                      # pfromData (pscanState'priorLedgerRoot st)
                      # outpointKey
                      # descriptorCbor
                      # pfromData pbindDescriptorArgs'ledgerMembershipProof
                  ]
              ) $
              pif (pfromData (poutputCommitment'referenceScriptLanguage d) #== 0)
                (pmatch (pfromData pbindDescriptorArgs'firstChunkProof) $ \case
                  PDNothing -> perror
                  PDJust firstProofD -> plet (pfromData firstProofD) $ \firstProof -> pmatch firstProof $ \proofFields ->
                    pexpecting
                      ( pfromData (pchunkProof'chunkIndex proofFields) #== 0
                          #&& pverifyReferenceScriptChunk # descriptor # firstProof
                      ) $
                      pmatch
                        (pbindMachineV1
                          # pfromData (pchunkProof'chunk proofFields)
                          # pfromData (poutputCommitment'referenceScriptTotalLength d)) $ \case
                          PMachineBoundV1 controlD ->
                            plet (pencodeStructureControlV1 # pfromData controlD) $ \controlCbor ->
                            plet (pscanStateWithMachineHashV1 # bound # (phashMachineControlV1 # controlCbor)) $ \expected ->
                              pexpecting (outputScriptHash #== advanceScriptHash) $
                                pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)
                          PMachineBindMalformedV1 ->
                            pexpecting (pfromData (pscanState'direction st) #== pdirectionWrongfulAcceptance) $
                              plet (pscanStateWithRefusalClassV1 # bound # prefusalClassMalformed) $ \expected ->
                                pexpecting (outputScriptHash #== step04ScriptHash) $
                                  pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)
                          PMachineBindNonNativeV1 _ -> perror)
                (pmatch (pfromData pbindDescriptorArgs'firstChunkProof) $ \case
                  PDJust _ -> perror
                  PDNothing ->
                    pexpecting (pfromData (pscanState'direction st) #== pdirectionWrongfulRejection) $
                      plet (pscanStateWithRefusalClassV1 # bound # prefusalClassMalformed) $ \expected ->
                        pexpecting (outputScriptHash #== step04ScriptHash) $
                          pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True))
