{- |
Module      : Midgard.Validators.FraudProofs.InvalidRange
Description : Plutarch port of @validators/fraud-proofs/invalid-range/step-0{1,2}.ak@.

The invalid-range fraud proof (spec §5.1.1): a committed transaction one or both
of whose validity bounds fall outside the block's own time range — or whose range
is unsatisfiable outright.

Two validators. Step-01 binds the transaction, normalises its interval against
'Midgard.Env.pposixTimeNone' and pairs it with the header's @start_time@ and
@end_time@; step-02 adjudicates the pair.

=== Only bounded ends are adjudicated

An /unbounded/ end is not a fault. A transaction with no upper bound does not
"exceed" the block's end — it simply never asserted one — so @FromNegInf@ is
checked on its upper bound alone and @ToPosInf@ on its lower. @Always@ asserts
nothing at all, and step-02 __aborts__ on it rather than returning @False@: a
thread that reached step-02 with an unbounded range was built on a premise the
family cannot be about, and the abort says so where a refusal would look like an
ordinary failed proof.

=== Exclusive on the wire, inclusive in the type

A native body's @validity_interval_end@ is /exclusive/ and every bounded
constructor of @NormalizedTimeRange@ holds an /inclusive/ upper, so step-01
subtracts one. That is also why step-02's upper test is @>=@ against the block's
@end_time@ while its lower test is @<@ against @start_time@: the block's range is
read as inclusive-lower, exclusive-upper, which the Aiken source flags as an
assumption pending a spec clarification. The port keeps the asymmetry rather than
tidying it, because tidying it would change which transactions are convictable.
-}
module Midgard.Validators.FraudProofs.InvalidRange (
  invalidRangeStep01Validator,
  invalidRangeStep02Validator,
) where

import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import DesignPatterns.ValidityRangeNormalization (PNormalizedTimeRange (..))
import Midgard.Env qualified as Env
import Midgard.FraudProofs.Common (pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.InvalidRange (PStep02Args (..), PStep02State (..))
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.LedgerState (PHeaderV1 (..))
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pexpecting,
  pstateIsAbsent,
  pstep,
 )

{- | Aiken @validators/fraud-proofs/invalid-range/step-01.ak@'s
@normalize_native_validity_range@.

The two sentinels decide the shape, and the exclusive upper becomes inclusive on
the way in. A bounded range whose lower exceeds its upper is @InvalidRange@ rather
than a @ClosedRange@ nothing satisfies — which is what lets step-02 convict it
without a separate emptiness test.
-}
pnormalizeNativeValidityRange ::
  forall (s :: S). Term s (PNativeTxBodyCompact :--> PNormalizedTimeRange)
pnormalizeNativeValidityRange = phoistAcyclic $
  plam $ \body -> P.do
    PNativeTxBodyCompact {pbodyCompact'validityIntervalStart, pbodyCompact'validityIntervalEnd} <-
      pmatch body
    lower <- plet pbodyCompact'validityIntervalStart
    exclusiveUpper <- plet pbodyCompact'validityIntervalEnd
    lowerAbsent <- plet $ lower #== Env.pposixTimeNone
    upperAbsent <- plet $ exclusiveUpper #== Env.pposixTimeNone
    pif
      lowerAbsent
      ( pif
          upperAbsent
          (pcon PAlways)
          (pcon (PFromNegInf {pntr'upperOnly = pdata (exclusiveUpper - 1)}))
      )
      ( pif
          upperAbsent
          (pcon (PToPosInf {pntr'lowerOnly = pdata lower}))
          ( plet (exclusiveUpper - 1) $ \upper ->
              pif
                (lower #> upper)
                (pcon PInvalidRange)
                (pcon (PClosedRange {pntr'lower = pdata lower, pntr'upper = pdata upper}))
          )
      )

{- | Aiken @validators/fraud-proofs/invalid-range/step-01.ak@.

Binds the transaction and writes the block's bounds alongside its normalised
range.

Unlike its siblings this step requires the thread's incoming state to be
__absent__ — Aiken writes @expect None = m_input_state_data@ where the other
families bind it and ignore it. The port keeps the check where Aiken has it.
-}
invalidRangeStep01Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-02's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
invalidRangeStep01Validator = plam $
  \step02ValidatorScriptHash computationThreadTokenPolicyId hubOracle ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \carriage -> P.do
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <-
            pmatch txInfo
          ppassNativeTxToNextStepCarried
            computationThreadTokenPolicyId
            hubOracle
            datum
            carriage
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'referenceInputs)
            (pfromData ptxInfo'outputs)
            (pto (pto (pfromData ptxInfo'redeemers)))
            $ \_ownScriptHash
               _threadTokenAssetName
               _fraudProver
               mInputStateData
               outputScriptHash
               outputStateData
               header
               _badTxId
               badTxView -> P.do
                PHeaderV1 {pheader'startTime, pheader'endTime} <- pmatch (pfromData header)
                PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch badTxView
                PNativeTxCompact {pcompact'body} <- pmatch pverified'txCompact
                pexpecting (pstateIsAbsent mInputStateData)
                  $ pexpecting (outputScriptHash #== step02ValidatorScriptHash)
                  $ pexpecting
                    ( outputStateData
                        #== pforgetData
                          ( pdata
                              ( pcon
                                  ( PStep02State
                                      { pstep02State'blockValidFrom = pheader'startTime
                                      , pstep02State'blockValidTo = pheader'endTime
                                      , pstep02State'badTxNormalizedValidityRange =
                                          pdata (pnormalizeNativeValidityRange # pcompact'body)
                                      }
                                  )
                              )
                          )
                    )
                    (pconstant True)

{- | Aiken @validators/fraud-proofs/invalid-range/step-02.ak@.

The conviction, a case analysis over the four shapes a normalised range can take
that assert anything at all.
-}
invalidRangeStep02Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- fraud proof token policy
        :--> PAsData PAddress -- fraud proof token address
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PScriptContext
        :--> PUnit
    )
invalidRangeStep02Validator = plam $
  \fraudProofTokenPolicyId fraudProofTokenAddress computationThreadTokenPolicyId ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PStep02Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PStep02Args
            { pstep02Args'inputIndex
            , pstep02Args'outputIndex
            , pstep02Args'fraudProofMintRedeemerIndex
            } <-
            pmatch args
          PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
          pfinalize
            computationThreadTokenPolicyId
            fraudProofTokenPolicyId
            fraudProofTokenAddress
            (pexpectDatum datum)
            (pfromData pstep02Args'inputIndex)
            (pfromData pstep02Args'outputIndex)
            (pfromData pstep02Args'fraudProofMintRedeemerIndex)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            (pto (pto (pfromData ptxInfo'redeemers)))
            $ \_ownScriptHash _threadTokenAssetName _fraudProver mInputStateData -> P.do
              PStep02State
                { pstep02State'blockValidFrom
                , pstep02State'blockValidTo
                , pstep02State'badTxNormalizedValidityRange
                } <-
                pmatch (pexpectStateAs @PStep02State mInputStateData)
              blockValidFrom <- plet $ pfromData pstep02State'blockValidFrom
              blockValidTo <- plet $ pfromData pstep02State'blockValidTo
              pexpecting
                ( pmatch (pfromData pstep02State'badTxNormalizedValidityRange) $ \case
                    PClosedRange {pntr'lower, pntr'upper} ->
                      pfromData pntr'lower
                        #< blockValidFrom
                        #|| pfromData pntr'upper
                        #>= blockValidTo
                    PFromNegInf {pntr'upperOnly} -> pfromData pntr'upperOnly #>= blockValidTo
                    PToPosInf {pntr'lowerOnly} -> pfromData pntr'lowerOnly #< blockValidFrom
                    -- Aiken's `fail @"The tx does not have an invalid time
                    -- range"`: an unbounded range is not a fault, so a thread
                    -- reaching here was built on a false premise.
                    PAlways -> perror
                    PInvalidRange -> pconstant True
                )
                (pconstant True)
