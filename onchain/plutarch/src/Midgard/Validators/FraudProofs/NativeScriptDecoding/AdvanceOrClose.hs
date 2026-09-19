module Midgard.Validators.FraudProofs.NativeScriptDecoding.AdvanceOrClose (
  nativeScriptDecodingAdvanceOrCloseValidator,
) where

import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pcontinue)
import Midgard.FraudProofs.NativeScriptDecoding.Engine
import Midgard.FraudProofs.NativeScriptDecoding.Step03 (PAdvanceOrCloseArgs (..))
import Midgard.NativeScriptScan qualified as Scan
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)

nativeScriptDecodingAdvanceOrCloseValidator ::
  forall s.
  Term s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
nativeScriptDecodingAdvanceOrCloseValidator = plam $
  \step04ScriptHash computationThreadPolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PAdvanceOrCloseArgs computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
        PAdvanceOrCloseArgs
          { padvanceArgs'inputIndex
          , padvanceArgs'outputIndex
          , padvanceArgs'controlCbor
          , padvanceArgs'chunkProof
          , padvanceArgs'nextChunkProof
          , padvanceArgs'frames
          , padvanceArgs'stepBudget
          } <- pmatch args
        PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
        pcontinue
          computationThreadPolicy
          (pexpectDatum datum)
          (pfromData padvanceArgs'inputIndex)
          (pfromData padvanceArgs'outputIndex)
          ownOutRef
          (pfromData ptxInfo'inputs)
          (pfromData ptxInfo'outputs)
          $ \ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
            state <- plet $ pexpectStateAs @PScanThreadStateV1 inputState
            st <- pmatch state
            controlCbor <- plet $ pfromData padvanceArgs'controlCbor
            control <- plet $ Scan.pdecodeStructureControlV1 # controlCbor
            frames <- plet $ pfromData padvanceArgs'frames
            stepBudget <- plet $ pfromData padvanceArgs'stepBudget
            let validateTransition expectedScript expectedState =
                  pexpecting (outputScriptHash #== expectedScript) $
                    pexpecting (outputStateData #== pforgetData (pdata expectedState)) (pconstant True)
                closeWith refusalClass =
                  validateTransition step04ScriptHash (pscanStateWithRefusalClassV1 # state # refusalClass)
                noProofs =
                  pfromData padvanceArgs'chunkProof #== pcon PDNothing
                    #&& pfromData padvanceArgs'nextChunkProof #== pcon PDNothing
            pexpecting
              ( pand'List
                  [ pfromData (pscanState'refusalClass st) #== pclassPending
                  , pfromData (pscanState'outpointKeyHash st) #/= pconstant ""
                  , pfromData (pscanState'referenceScriptLanguage st) #== 0
                  , pfromData (pscanState'outputIndex st) #>= 0
                  , pfromData (pscanState'totalLength st) #> 0
                  , pfromData (pscanState'itemCommitment st) #/= pconstant ""
                  , pfromData (pscanState'machineStateHash st) #/= pconstant ""
                  , phashMachineControlV1 # controlCbor #== pfromData (pscanState'machineStateHash st)
                  ]
              ) $
              pif
                ( pfromData (pscanState'direction st) #== pdirectionWrongfulRejection
                    #&& Scan.pstructureTerminalIsExactV1 # control
                )
                ( pexpecting
                    (noProofs #&& pnull # frames #&& stepBudget #== 0)
                    (closeWith prefusalClassMalformed)
                )
                ( pmatch (pfromData padvanceArgs'chunkProof) $ \case
                    PDNothing ->
                      pexpecting (pfromData padvanceArgs'nextChunkProof #== pcon PDNothing) $
                        handleScan
                          ownScriptHash
                          step04ScriptHash
                          state
                          st
                          outputScriptHash
                          outputStateData
                          control
                          (pcon PNothing)
                          frames
                          stepBudget
                    PDJust chunkProofD ->
                      pmatch control $ \controlFields ->
                        let nextProof = pmatch (pfromData padvanceArgs'nextChunkProof) $ \case
                              PDNothing -> pcon PNothing
                              PDJust nextProofD -> pcon $ PJust $ pfromData nextProofD
                            window =
                              pauthenticatedScanWindowV1
                                # pfromData (pscanState'outputIndex st)
                                # pfromData (pscanState'totalLength st)
                                # pfromData (pscanState'itemCommitment st)
                                # pfromData (Scan.pstructure'cursor controlFields)
                                # pfromData chunkProofD
                                # nextProof
                         in handleScan
                              ownScriptHash
                              step04ScriptHash
                              state
                              st
                              outputScriptHash
                              outputStateData
                              control
                              (pcon $ PJust window)
                              frames
                              stepBudget
                )

handleScan ::
  forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PScriptHash) ->
  Term s PScanThreadStateV1 ->
  PScanThreadStateV1 s ->
  Term s (PAsData PScriptHash) ->
  Term s PData ->
  Term s Scan.PNativeScriptStructureControlV1 ->
  Term s (PMaybe PScanWindowV1) ->
  Term s (PBuiltinList (PAsData Scan.PNativeScriptFrameV1)) ->
  Term s PInteger ->
  Term s PBool
handleScan ownScriptHash step04ScriptHash state st outputScriptHash outputStateData control window frames stepBudget =
  pmatch (pbudgetedScanV1 # control # window # frames # stepBudget) $ \case
    PScanAdvancedV1 nextControlD ->
      plet (pfromData nextControlD) $ \nextControl ->
        pif
          ( pfromData (pscanState'direction st) #== pdirectionWrongfulRejection
              #&& Scan.pstructureTerminalIsExactV1 # nextControl
          )
          (validate step04ScriptHash $ pscanStateWithRefusalClassV1 # state # prefusalClassMalformed)
          ( plet (Scan.pencodeStructureControlV1 # nextControl) $ \nextControlCbor ->
              validate ownScriptHash $ pscanStateWithMachineHashV1 # state # (phashMachineControlV1 # nextControlCbor)
          )
    PScanRefusedV1 refusalClassD ->
      pexpecting (pfromData (pscanState'direction st) #== pdirectionWrongfulAcceptance) $
        validate step04ScriptHash $ pscanStateWithRefusalClassV1 # state # pfromData refusalClassD
  where
    validate expectedScript expectedState =
      pexpecting (outputScriptHash #== expectedScript) $
        pexpecting (outputStateData #== pforgetData (pdata expectedState)) (pconstant True)
