module Midgard.Validators.FraudProofs.MintAuthorization (
  mintAuthorizationStep01Validator,
  mintAuthorizationStep02Validator,
  mintAuthorizationStep03Validator,
  mintAuthorizationStep04Validator,
  mintAuthorizationStep05Validator,
  mintAuthorizationEvaluateValidator,
  mintAuthorizationWitnessScanValidator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_224, pblake2b_256)
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottRec (..))
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofCatalogue (pidByteCount)
import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (
  PFieldOpeningV1,
  PNativeTxAnchorV1 (..),
  paddressWitnessesFieldIndex,
  pfoldOpenedField,
  pmintFieldIndex,
  popenedCommittedPreimage,
  popenedFieldView,
  popenedFieldWalk,
  preferenceInputsFieldIndex,
  pscriptWitnessesFieldIndex,
 )
import Midgard.FraudProofs.MintAuthorization (
  PClaimEvidence (..),
  PStep01Args (..),
  PStep02Args (..),
  PStep02State (..),
  PStep03Args (..),
  PStep03State (..),
  PStep04Args (..),
  PStep04State (..),
  PStep05Args (..),
  PStep05State (..),
 )
import Midgard.FraudProofs.MintAuthorization.Engine (
  PNativeScriptVerdictV1 (..),
  pdirectionScriptAbsent,
  pdirectionScriptUnsatisfied,
  pevaluateNativeScriptV1,
  ppolicyIdOfMintItemV1,
 )
import Midgard.FraudProofs.MintAuthorizationScan qualified as Machines
import Midgard.FraudProofs.NativeScriptDecoding.Engine (pverifyCommittedPreStateV1)
import Midgard.FraudProofs.NativeTx.Components (
  pdecodeMidgardAddressWitnessCbor,
  pdecodeMidgardVersionedScriptAt,
  pencodeMidgardTxInput,
  pencodeMidgardVersionedScript,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddressWitness (..),
  PMidgardScriptLanguage (..),
  PMidgardVersionedScript (..),
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.FraudProofs.StructuredDataCarriage qualified as Carriage
import Midgard.LedgerOutputCommitment (
  PLedgerOutputCommitmentV1 (..),
  pdecodeLedgerOutputCommitment,
 )
import Midgard.LedgerState (PEventKey (..))
import Midgard.MpfProof (phasV1)
import Midgard.NativeTxFieldAccess (pdecodeFieldArrayHeader, pfieldCommitment, pfieldItemAt, pfieldItemCount)
import Midgard.NativeTxMachineWalk (pspendInputAt)
import Midgard.ScriptProof (pversionedScriptHash)
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstateIsAbsent, pstep)

mintAuthorizationStep01Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
mintAuthorizationStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep01Args{pstep01Args'carriage} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      ppassNativeTxToNextStepCarried
        computationThreadPolicy
        hubOracle
        datum
        (pfromData pstep01Args'carriage)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'referenceInputs)
        (pfromData ptxInfo'outputs)
        (pto $ pto $ pfromData ptxInfo'redeemers)
        $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData _header badTxId badTxView -> P.do
          PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch badTxView
          PNativeTxCompact{pcompact'body, pcompact'witnessSetHash, pcompact'validityCode} <- pmatch pverified'txCompact
          PNativeTxBodyCompact{pbodyCompact'validityIntervalStart, pbodyCompact'validityIntervalEnd} <- pmatch pcompact'body
          expectedState <-
            plet $
              pcon $
                PStep02State
                  (pdata badTxId)
                  (pdata pcompact'witnessSetHash)
                  (pdata pbodyCompact'validityIntervalStart)
                  (pdata pbodyCompact'validityIntervalEnd)
          pexpecting (pstateIsAbsent inputState) $
            pexpecting (pcompact'validityCode #== 0) $
              pexpecting (outputScriptHash #== step02ScriptHash) $
                pexpecting (outputStateData #== pforgetData (pdata expectedState)) (pconstant True)

data PPreparedMintArgs s
  = PPreparedMintArgs
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
      (Term s (PMaybe PClaimEvidence))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PPreparedMintArgs)

mintAuthorizationStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mintAuthorizationStep02Validator = plam $ \nextHash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PStep02Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
      PPreparedMintArgs inputIndex outputIndex opening claim <- pmatch $ pmatch args $ \case
        PStep02Args i o header event transition policy direction mint ->
          pcon $ PPreparedMintArgs i o mint $ pcon $ PJust $ pcon $ PClaimEvidence header event transition policy direction
        PPublishedArgs i o evidence mint ->
          pcon $ PPreparedMintArgs i o mint $ pcon $ PJust $ punsafeCoerce @PClaimEvidence $ Carriage.presolve # evidence # pfromData ptxInfo'referenceInputs
        PAdvanceMintScan i o mint -> pcon $ PPreparedMintArgs i o mint $ pcon PNothing
      pcontinue
        threadPolicy
        (pexpectDatum datum)
        (pfromData inputIndex)
        (pfromData outputIndex)
        ownRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \ownHash name _ prior outputHash outputState -> P.do
          state <- plet $ pexpectStateAs @PStep02State prior
          txId <- plet $ pmatch state $ \case
            PStep02State{pstep02State'badTxId} -> pstep02State'badTxId
            PMintScanState{pmintState'badTxId} -> pmintState'badTxId
          bytes <- plet $ popenedCommittedPreimage # pfromData opening # pcon (PBodyAnchor txId) # pmintFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy
          frozen <- plet $ pmatch claim $ \case
            PNothing -> pmatch state $ \case scan@PMintScanState{} -> pcon scan; _ -> perror
            PJust evidence -> P.do
              PStep02State badId witness start end <- pmatch state
              PClaimEvidence header event transition policy direction <- pmatch evidence
              priorRoot <- plet $ pverifyCommittedPreStateV1 # pfromData header # pcon (PL2TransactionEventKey badId) # pfromData event # pfromData transition
              pexpecting (pblake2b_224 # (pserialiseData # pforgetData header) #== psliceBS # pidByteCount # (plengthBS # pto (pfromData name) - pidByteCount) # pto (pfromData name)) $
                pexpecting (pfromData direction #== pdirectionScriptAbsent #|| pfromData direction #== pdirectionScriptUnsatisfied) $
                  pcon $
                    PMintScanState badId witness start end (pdata priorRoot) policy direction (pdata $ pfieldCommitment # bytes) (pdata $ Machines.pinitialMint # bytes # pfromData policy)
          scan@PMintScanState{pmintState'badTxId, pmintState'witnessSetHash, pmintState'validityStart, pmintState'validityEnd, pmintState'priorLedgerRoot, pmintState'policyIndex, pmintState'direction, pmintState'fieldHash, pmintState'control} <- pmatch frozen
          pexpecting (pfieldCommitment # bytes #== pfromData pmintState'fieldHash) $ P.do
            next <- plet $ Machines.padvanceMint # pfromData pmintState'control # bytes # pfromData pmintState'policyIndex # 32
            Machines.PMintControl{Machines.pmint'policyId, Machines.pmint'itemIndex, Machines.pmint'itemCount} <- pmatch next
            pif
              (Machines.pmintComplete # next # (plengthBS # bytes))
              (outputHash #== nextHash #&& outputState #== pforgetData (pdata $ pcon $ PStep03State pmint'policyId pmintState'direction pmintState'badTxId pmintState'witnessSetHash pmintState'validityStart pmintState'validityEnd pmintState'priorLedgerRoot))
              ( next
                  #/= pfromData pmintState'control
                  #&& pfromData pmint'itemIndex
                  #< pfromData pmint'itemCount
                  #&& outputHash
                  #== ownHash
                  #&& outputState
                  #== pforgetData (pdata $ pcon scan{pmintState'control = pdata next})
              )

mintAuthorizationStep03Validator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
mintAuthorizationStep03Validator = plam $ \step04ScriptHash step05ScriptHash evaluateHash witnessScanHash computationThreadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep03Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args ->
      pmatch args $ \case
        PWitnessAbsence inputIndexD outputIndexD openingD -> P.do
          PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
          pcontinue
            computationThreadPolicy
            (pexpectDatum datum)
            (pfromData inputIndexD)
            (pfromData outputIndexD)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
              PStep03State
                { pstep03State'policyId
                , pstep03State'direction
                , pstep03State'badTxId
                , pstep03State'badTxWitnessSetHash
                , pstep03State'priorLedgerRoot
                } <-
                pmatch $ pexpectStateAs @PStep03State inputState
              scriptWalk <-
                plet $
                  popenedFieldWalk
                    # pfromData openingD
                    # pcon
                      ( PWitnessAnchor
                          { pwitnessAnchor'txId = pstep03State'badTxId
                          , pwitnessAnchor'witnessSetHash = pstep03State'badTxWitnessSetHash
                          }
                      )
                    # pscriptWitnessesFieldIndex
                    # pfromData ptxInfo'referenceInputs
                    # certificatePolicy
              policyPresent <-
                plet $
                  pfoldOpenedField @PBool
                    # scriptWalk
                    # pconstant False
                    # plam
                      ( \found _index item -> P.do
                          PPair _offset scriptWitness <- pmatch $ pdecodeMidgardVersionedScriptAt # item # 0
                          pexpecting (pencodeMidgardVersionedScript # scriptWitness #== item) $
                            found #|| pversionedScriptHash # scriptWitness #== pfromData pstep03State'policyId
                      )
              expectedState <-
                plet $
                  pcon $
                    PStep04State
                      pstep03State'policyId
                      pstep03State'badTxId
                      pstep03State'priorLedgerRoot
                      (pdata 0)
              pexpecting (pfromData pstep03State'direction #== pdirectionScriptAbsent) $
                pexpecting (pnot # policyPresent) $
                  pexpecting (outputScriptHash #== step04ScriptHash) $
                    pexpecting (outputStateData #== pforgetData (pdata expectedState)) (pconstant True)
        PEvaluateUnsatisfied inputIndexD outputIndexD scriptBytesD openingD -> P.do
          PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
          pcontinue
            computationThreadPolicy
            (pexpectDatum datum)
            (pfromData inputIndexD)
            (pfromData outputIndexD)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
              PStep03State
                { pstep03State'policyId
                , pstep03State'direction
                , pstep03State'badTxId
                , pstep03State'badTxWitnessSetHash
                , pstep03State'validityIntervalStart
                , pstep03State'validityIntervalEnd
                } <-
                pmatch $ pexpectStateAs @PStep03State inputState
              scriptBytes <- plet $ pfromData scriptBytesD
              nativeScript <-
                plet $
                  pcon $
                    PMidgardVersionedScript
                      (pdata $ pcon PNativeCardanoScript)
                      (pdata scriptBytes)
              addressWalk <-
                plet $
                  popenedFieldWalk
                    # pfromData openingD
                    # pcon
                      ( PWitnessAnchor
                          { pwitnessAnchor'txId = pstep03State'badTxId
                          , pwitnessAnchor'witnessSetHash = pstep03State'badTxWitnessSetHash
                          }
                      )
                    # paddressWitnessesFieldIndex
                    # pfromData ptxInfo'referenceInputs
                    # certificatePolicy
              signerHashes <-
                plet $
                  pfoldOpenedField @(PBuiltinList PByteString)
                    # addressWalk
                    # pnil
                    # plam
                      ( \acc _index item ->
                          pmatch (pdecodeMidgardAddressWitnessCbor # item) $ \witness ->
                            pcons # (pblake2b_224 # pfromData (paddressWitness'verificationKey witness)) # acc
                      )
              pexpecting (pfromData pstep03State'direction #== pdirectionScriptUnsatisfied)
                $ pexpecting (pversionedScriptHash # nativeScript #== pfromData pstep03State'policyId)
                $ pmatch
                  ( pevaluateNativeScriptV1
                      # scriptBytes
                      # signerHashes
                      # pfromData pstep03State'validityIntervalStart
                      # pfromData pstep03State'validityIntervalEnd
                  )
                $ \case
                  PScriptEvaluatedV1 satisfiedD ->
                    pexpecting (pnot # pfromData satisfiedD) $
                      pexpecting (outputScriptHash #== step05ScriptHash) $
                        pexpecting
                          ( outputStateData
                              #== pforgetData
                                ( pdata $
                                    pcon $
                                      PStep05State
                                        pstep03State'policyId
                                        (pdata pdirectionScriptUnsatisfied)
                                )
                          )
                          (pconstant True)
                  _ -> perror
        PStartAbsence inputIndex outputIndex indices opening -> P.do
          PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch txInfo
          pcontinue
            computationThreadPolicy
            (pexpectDatum datum)
            (pfromData inputIndex)
            (pfromData outputIndex)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ _ _ prior outputHash outputState -> P.do
              PStep03State policy direction txId witness _ _ root <- pmatch $ pexpectStateAs @PStep03State prior
              parts <- plet $ Machines.pchunks # pfromData indices # pfromData ptxInfo'referenceInputs
              bytes <- plet $ Machines.ppayload # parts
              let len = plengthBS # bytes
              pexpecting (pfromData direction #== pdirectionScriptAbsent #&& len #> 0 #&& len #<= 32768) $
                pexpecting (bytes #== popenedCommittedPreimage # pfromData opening # pcon (PWitnessAnchor txId witness) # pscriptWitnessesFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy) $ P.do
                  PPair cursor count <- pmatch $ pdecodeFieldArrayHeader # bytes
                  let expected =
                        pcon $
                          Machines.PWitnessScanState
                            policy
                            txId
                            root
                            (pdata len)
                            (pdata $ pmap # plam (\part -> pdata $ pblake2b_256 # pfromData part) # parts)
                            (pdata cursor)
                            (pdata 0)
                            (pdata count)
                  outputHash #== witnessScanHash #&& outputState #== pforgetData (pdata expected)
        PStartUnsatisfied scriptLength inputIndex outputIndex indices opening -> P.do
          PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch txInfo
          pcontinue
            computationThreadPolicy
            (pexpectDatum datum)
            (pfromData inputIndex)
            (pfromData outputIndex)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ _ _ prior outputHash outputState -> P.do
              PStep03State policy direction txId witness start end _ <- pmatch $ pexpectStateAs @PStep03State prior
              parts <- plet $ Machines.prawChunks # pfromData indices # pfromData ptxInfo'referenceInputs
              raw <- plet $ Machines.ppayload # parts
              let len = pfromData scriptLength
                  rawLength = plengthBS # raw
                  script = psliceBS # 0 # len # raw
                  signerBytes = psliceBS # len # (rawLength - len) # raw
              pexpecting (pfromData direction #== pdirectionScriptUnsatisfied #&& len #> 0 #&& len #<= 32768 #&& rawLength #> len #&& rawLength - len #<= 32768) $
                pexpecting (pversionedScriptHash # pcon (PMidgardVersionedScript (pdata $ pcon PNativeCardanoScript) (pdata script)) #== pfromData policy) $
                  pexpecting (signerBytes #== popenedCommittedPreimage # pfromData opening # pcon (PWitnessAnchor txId witness) # paddressWitnessesFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy) $ P.do
                    PPair headerLength count <- pmatch $ pdecodeFieldArrayHeader # signerBytes
                    let expected =
                          pcon $
                            Machines.PEvaluateState
                              policy
                              scriptLength
                              (pdata rawLength)
                              (pdata $ len + headerLength)
                              (pdata count)
                              (pdata 0)
                              (pdata $ pmap # plam (\part -> pdata $ pblake2b_256 # pfromData part) # parts)
                              (pdata pnil)
                              start
                              end
                              (pdata 0)
                              (pdata 0)
                              (pdata $ pconstant "")
                              (pdata 0)
                              (pdata (-1))
                    plengthBS # signerBytes #== headerLength + 103 * count #&& outputHash #== evaluateHash #&& outputState #== pforgetData (pdata expected)

mintAuthorizationStep04Validator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
mintAuthorizationStep04Validator = plam $ \step05ScriptHash computationThreadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep04Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args ->
      pmatch args $ \case
        PResolveNext inputIndexD outputIndexD openingD descriptorCborD proofD -> P.do
          PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
          pcontinue
            computationThreadPolicy
            (pexpectDatum datum)
            (pfromData inputIndexD)
            (pfromData outputIndexD)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
              PStep04State
                { pstep04State'policyId
                , pstep04State'badTxId
                , pstep04State'priorLedgerRoot
                , pstep04State'refCursor
                } <-
                pmatch $ pexpectStateAs @PStep04State inputState
              referenceView <-
                plet $
                  popenedFieldView
                    # pfromData openingD
                    # pcon (PBodyAnchor{pbodyAnchor'txId = pstep04State'badTxId})
                    # preferenceInputsFieldIndex
                    # pfromData ptxInfo'referenceInputs
                    # certificatePolicy
              cursor <- plet $ pfromData pstep04State'refCursor
              outpoint <- plet $ pspendInputAt # referenceView # cursor
              outpointKey <- plet $ pencodeMidgardTxInput # outpoint
              descriptorCbor <- plet $ pfromData descriptorCborD
              descriptor <- plet $ pdecodeLedgerOutputCommitment # descriptorCbor
              d <- pmatch descriptor
              expectedState <-
                plet $
                  pcon $
                    PStep04State
                      pstep04State'policyId
                      pstep04State'badTxId
                      pstep04State'priorLedgerRoot
                      (pdata $ cursor + 1)
              pexpecting (cursor #>= 0 #&& cursor #< pfieldItemCount # referenceView)
                $ pexpecting
                  (phasV1 # pfromData pstep04State'priorLedgerRoot # outpointKey # descriptorCbor # pfromData proofD)
                $ pexpecting
                  ( pfromData (poutputCommitment'referenceScriptLanguage d)
                      #== -1
                      #|| pnot
                      # (pfromData (poutputCommitment'referenceScriptHash d) #== pfromData pstep04State'policyId)
                  )
                $ pexpecting (outputScriptHash #== ownScriptHash)
                $ pexpecting (outputStateData #== pforgetData (pdata expectedState)) (pconstant True)
        PAdvanceComplete inputIndexD outputIndexD openingD -> P.do
          PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
          pcontinue
            computationThreadPolicy
            (pexpectDatum datum)
            (pfromData inputIndexD)
            (pfromData outputIndexD)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
              PStep04State
                { pstep04State'policyId
                , pstep04State'badTxId
                , pstep04State'refCursor
                } <-
                pmatch $ pexpectStateAs @PStep04State inputState
              referenceView <-
                plet $
                  popenedFieldView
                    # pfromData openingD
                    # pcon (PBodyAnchor{pbodyAnchor'txId = pstep04State'badTxId})
                    # preferenceInputsFieldIndex
                    # pfromData ptxInfo'referenceInputs
                    # certificatePolicy
              pexpecting (pfromData pstep04State'refCursor #== pfieldItemCount # referenceView) $
                pexpecting (outputScriptHash #== step05ScriptHash) $
                  pexpecting
                    ( outputStateData
                        #== pforgetData
                          ( pdata $
                              pcon $
                                PStep05State
                                  pstep04State'policyId
                                  (pdata pdirectionScriptAbsent)
                          )
                    )
                    (pconstant True)

mintAuthorizationStep05Validator ::
  forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mintAuthorizationStep05Validator = plam $ \fraudProofPolicy fraudProofAddress computationThreadPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep05Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep05Args{pstep05Args'inputIndex, pstep05Args'outputIndex, pstep05Args'fraudProofMintRedeemerIndex} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      pfinalize
        computationThreadPolicy
        fraudProofPolicy
        fraudProofAddress
        (pexpectDatum datum)
        (pfromData pstep05Args'inputIndex)
        (pfromData pstep05Args'outputIndex)
        (pfromData pstep05Args'fraudProofMintRedeemerIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        (pto $ pto $ pfromData ptxInfo'redeemers)
        $ \_ownScriptHash _threadName _prover inputState -> P.do
          PStep05State{pstep05State'direction} <- pmatch $ pexpectStateAs @PStep05State inputState
          pexpecting
            ( pfromData pstep05State'direction
                #== pdirectionScriptAbsent
                #|| pfromData pstep05State'direction
                #== pdirectionScriptUnsatisfied
            )
            (pconstant True)

mintAuthorizationEvaluateValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mintAuthorizationEvaluateValidator = plam $ \nextHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @Machines.PEvaluateAction threadPolicy datum redeemer ownRef tx $ \action -> P.do
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
      PPair i o <- pmatch $ pmatch action $ \case
        Machines.PEvaluateAdvance i o _ _ -> pcon $ PPair i o
        Machines.PEvaluateFinalize i o -> pcon $ PPair i o
      pcontinue threadPolicy (pexpectDatum datum) (pfromData i) (pfromData o) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ prior outputHash outputState -> P.do
        state <- plet $ pexpectStateAs @Machines.PEvaluateState prior
        pmatch action $ \case
          Machines.PEvaluateAdvance _ _ indices operations -> P.do
            bytes <- plet $ Machines.pauthenticatePayload # state # (Machines.prawChunks # pfromData indices # pfromData ptxInfo'referenceInputs)
            next <- plet $ Machines.padvanceEvaluation # state # bytes # pfromData operations
            outputHash #== ownHash #&& outputState #== pforgetData (pdata next)
          Machines.PEvaluateFinalize _ _ -> pmatch state $ \s ->
            Machines.pevaluate'signerIndex s
              #== Machines.pevaluate'signerCount s
              #&& Machines.pevaluate'cursor s
              #== Machines.pevaluate'scriptLength s
              #&& pfromData (Machines.pevaluate'stackRoot s)
              #== pconstant ""
              #&& pfromData (Machines.pevaluate'stackDepth s)
              #== 0
              #&& pfromData (Machines.pevaluate'result s)
              #== 0
              #&& outputHash
              #== nextHash
              #&& outputState
              #== pforgetData (pdata $ pcon $ PStep05State (Machines.pevaluate'policyId s) (pdata 1))

mintAuthorizationWitnessScanValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mintAuthorizationWitnessScanValidator = plam $ \nextHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @Machines.PWitnessScanAction threadPolicy datum redeemer ownRef tx $ \action -> P.do
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
      PPair i o <- pmatch $ pmatch action $ \case
        Machines.PWitnessAdvance i o _ -> pcon $ PPair i o
        Machines.PWitnessFinalize i o -> pcon $ PPair i o
      pcontinue threadPolicy (pexpectDatum datum) (pfromData i) (pfromData o) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ prior outputHash outputState -> P.do
        state <- plet $ pexpectStateAs @Machines.PWitnessScanState prior
        s <- pmatch state
        pmatch action $ \case
          Machines.PWitnessAdvance _ _ indices ->
            pexpecting (pfromData (Machines.pwitness'itemIndex s) #< pfromData (Machines.pwitness'itemCount s)) $ P.do
              bytes <- plet $ Machines.pauthenticateWitness # state # (Machines.pchunks # pfromData indices # pfromData ptxInfo'referenceInputs)
              next <- plet $ Machines.padvanceWitness # state # bytes # 16
              outputHash #== ownHash #&& outputState #== pforgetData (pdata next)
          Machines.PWitnessFinalize _ _ ->
            Machines.pwitness'itemIndex s
              #== Machines.pwitness'itemCount s
              #&& Machines.pwitness'cursor s
              #== Machines.pwitness'fieldLength s
              #&& outputHash
              #== nextHash
              #&& outputState
              #== pforgetData (pdata $ pcon $ PStep04State (Machines.pwitness'policyId s) (Machines.pwitness'badTxId s) (Machines.pwitness'priorLedgerRoot s) (pdata 0))
