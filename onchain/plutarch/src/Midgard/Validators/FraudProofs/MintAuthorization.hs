module Midgard.Validators.FraudProofs.MintAuthorization (
  mintAuthorizationStep01Validator,
  mintAuthorizationStep02Validator,
  mintAuthorizationStep03Validator,
  mintAuthorizationStep04Validator,
  mintAuthorizationStep05Validator,
) where

import Plutarch.Builtin.Crypto (pblake2b_224)
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofCatalogue (pidByteCount)
import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (
  PNativeTxAnchorV1 (..),
  paddressWitnessesFieldIndex,
  pfoldOpenedField,
  pmintFieldIndex,
  popenedFieldView,
  popenedFieldWalk,
  preferenceInputsFieldIndex,
  pscriptWitnessesFieldIndex,
 )
import Midgard.FraudProofs.MintAuthorization (
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
import Midgard.LedgerOutputCommitment (
  PLedgerOutputCommitmentV1 (..),
  pdecodeLedgerOutputCommitment,
 )
import Midgard.LedgerState (PEventKey (..))
import Midgard.MpfProof (phasV1)
import Midgard.NativeTxFieldAccess (pfieldItemAt, pfieldItemCount)
import Midgard.NativeTxMachineWalk (pspendInputAt)
import Midgard.ScriptProof (pversionedScriptHash)
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstateIsAbsent, pstep)

mintAuthorizationStep01Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
mintAuthorizationStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep01Args {pstep01Args'carriage} <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
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
          PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch badTxView
          PNativeTxCompact {pcompact'body, pcompact'witnessSetHash, pcompact'validityCode} <- pmatch pverified'txCompact
          PNativeTxBodyCompact {pbodyCompact'validityIntervalStart, pbodyCompact'validityIntervalEnd} <- pmatch pcompact'body
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

mintAuthorizationStep02Validator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
mintAuthorizationStep02Validator = plam $ \step03ScriptHash computationThreadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep02Args
        { pstep02Args'inputIndex
        , pstep02Args'outputIndex
        , pstep02Args'header
        , pstep02Args'eventToStepMembership
        , pstep02Args'transitionStepMembership
        , pstep02Args'policyIndex
        , pstep02Args'direction
        , pstep02Args'mintOpening
        } <-
        pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
      pcontinue
        computationThreadPolicy
        (pexpectDatum datum)
        (pfromData pstep02Args'inputIndex)
        (pfromData pstep02Args'outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_ownScriptHash threadName _prover inputState outputScriptHash outputStateData -> P.do
          PStep02State
            { pstep02State'badTxId
            , pstep02State'badTxWitnessSetHash
            , pstep02State'validityIntervalStart
            , pstep02State'validityIntervalEnd
            } <-
            pmatch $ pexpectStateAs @PStep02State inputState
          header <- plet $ pfromData pstep02Args'header
          threadNameBytes <- plet $ pto $ pfromData threadName
          eventKey <- plet $ pcon $ PL2TransactionEventKey pstep02State'badTxId
          priorLedgerRoot <-
            plet $
              pverifyCommittedPreStateV1
                # header
                # eventKey
                # pfromData pstep02Args'eventToStepMembership
                # pfromData pstep02Args'transitionStepMembership
          mintView <-
            plet $
              popenedFieldView
                # pfromData pstep02Args'mintOpening
                # pcon (PBodyAnchor {pbodyAnchor'txId = pstep02State'badTxId})
                # pmintFieldIndex
                # pfromData ptxInfo'referenceInputs
                # certificatePolicy
          policyIndex <- plet $ pfromData pstep02Args'policyIndex
          direction <- plet $ pfromData pstep02Args'direction
          policyId <- plet $ ppolicyIdOfMintItemV1 #$ pfieldItemAt # mintView # policyIndex
          expectedState <-
            plet $
              pcon $
                PStep03State
                  (pdata policyId)
                  (pdata direction)
                  pstep02State'badTxId
                  pstep02State'badTxWitnessSetHash
                  pstep02State'validityIntervalStart
                  pstep02State'validityIntervalEnd
                  (pdata priorLedgerRoot)
          pexpecting
            ( (pblake2b_224 #$ pserialiseData # pforgetData pstep02Args'header)
                #== psliceBS
                # pidByteCount
                # (plengthBS # threadNameBytes - pidByteCount)
                # threadNameBytes
            )
            $ pexpecting (policyIndex #>= 0 #&& policyIndex #< pfieldItemCount # mintView)
            $ pexpecting (direction #== pdirectionScriptAbsent #|| direction #== pdirectionScriptUnsatisfied)
            $ pexpecting (outputScriptHash #== step03ScriptHash)
            $ pexpecting (outputStateData #== pforgetData (pdata expectedState)) (pconstant True)

mintAuthorizationStep03Validator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
mintAuthorizationStep03Validator = plam $ \step04ScriptHash step05ScriptHash computationThreadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep03Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args ->
      pmatch args $ \case
        PWitnessAbsence inputIndexD outputIndexD openingD -> P.do
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
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
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
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
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
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
                    # pcon (PBodyAnchor {pbodyAnchor'txId = pstep04State'badTxId})
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
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
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
                    # pcon (PBodyAnchor {pbodyAnchor'txId = pstep04State'badTxId})
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
      PStep05Args {pstep05Args'inputIndex, pstep05Args'outputIndex, pstep05Args'fraudProofMintRedeemerIndex} <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
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
          PStep05State {pstep05State'direction} <- pmatch $ pexpectStateAs @PStep05State inputState
          pexpecting
            ( pfromData pstep05State'direction
                #== pdirectionScriptAbsent
                #|| pfromData pstep05State'direction
                #== pdirectionScriptUnsatisfied
            )
            (pconstant True)
