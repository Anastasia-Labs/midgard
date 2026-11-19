{- |
Module      : Midgard.Validators.FraudProofs.Step
Description : The shape every fraud-proof step validator has in common.

Roughly 160 dispatch validators sit under @validators/fraud-proofs/@ and every
one of them opens the same way: a spending validator, a @Cancel@ arm that is
identical everywhere, and a @Continue@ arm carrying the step's own payload. The
Aiken tree repeats that opening in each file because Aiken has no way not to; the
port factors it here, so a step module contains its own guards and nothing else.

Nothing in this module is family-specific, and nothing family-specific belongs in
it. The line is: if a change here would be wrong for /any/ step, it does not go
here.
-}
module Midgard.Validators.FraudProofs.Step (
  pstep,
  pdispatch,
  pexpecting,
  pexpectDatum,
  pexpectState,
  pexpectStateAs,
  pstateIsAbsent,
) where

import Data.Kind (Type)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptContext (..),
  PScriptInfo (..),
  PTxInfo (..),
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.ComputationThread (PStepDatum, PStepRedeemer (..))
import Midgard.FraudProofs.Common (pcancel)

{- | Aiken's @spend(datum, redeemer, own_out_ref, tx)@ plus @else(_) { fail }@.

Every fraud-proof step is a spending validator, so a purpose that is not
@spend@ never reaches the dispatch.
-}
pstep ::
  forall (s :: S).
  Term s PScriptContext ->
  ( Term s (PMaybeData PStepDatum) ->
    Term s PStepRedeemer ->
    Term s PTxOutRef ->
    Term s PTxInfo ->
    Term s PBool
  ) ->
  Term s PUnit
pstep ctx k = P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  pmatch pscriptContext'scriptInfo $ \case
    PSpendingScript ownOutRef mDatum ->
      pif
        ( k
            (punsafeCoerce @(PMaybeData PStepDatum) mDatum)
            (pfromData (punsafeCoerce @(PAsData PStepRedeemer) (pto pscriptContext'redeemer)))
            ownOutRef
            pscriptContext'txInfo
        )
        (pconstant ())
        perror
    _ -> perror

{- | @when redeemer is { Cancel .. -> cancel(..); Continue payload -> k payload }@.

Cancelling is identical in every step of every family: it abandons the thread and
burns its token, and the computation-thread policy is what checks that. Only the
@Continue@ payload differs, so that is what the continuation receives — typed at
whatever the step's own @Args@ is, or at a carriage for the steps that pass a
transaction on.
-}
pdispatch ::
  forall (s :: S) (payload :: S -> Type).
  (PIsData payload) =>
  Term s (PAsData PCurrencySymbol) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PStepRedeemer ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  (Term s payload -> Term s PBool) ->
  Term s PBool
pdispatch computationThreadTokenPolicyId datum redeemer ownOutRef txInfo k =
  pmatch redeemer $ \case
    PCancel {pcancel'inputIndex, pcancel'computationThreadMintRedeemerIndex} -> P.do
      PTxInfo {ptxInfo'inputs, ptxInfo'redeemers, ptxInfo'signatories} <- pmatch txInfo
      pcancel
        computationThreadTokenPolicyId
        datum
        (pfromData pcancel'inputIndex)
        ownOutRef
        (pfromData pcancel'computationThreadMintRedeemerIndex)
        (pfromData ptxInfo'inputs)
        (pto (pto (pfromData ptxInfo'redeemers)))
        (pfromData ptxInfo'signatories)
    PContinue {pcontinue'data} ->
      k (pfromData (punsafeCoerce @(PAsData payload) pcontinue'data))

-- | Aiken's @expect None = m_input_state_data@.
pstateIsAbsent :: forall (s :: S). Term s (PMaybeData PData) -> Term s PBool
pstateIsAbsent mData = pmatch mData $ \case
  PDJust _ -> pconstant False
  PDNothing -> pconstant True

-- | Aiken's @expect Some(step_datum) = datum@.
pexpectDatum :: forall (s :: S). Term s (PMaybeData PStepDatum) -> Term s PStepDatum
pexpectDatum mDatum = pmatch mDatum $ \case
  PDJust d -> pfromData d
  PDNothing -> perror

-- | Aiken's @expect Some(State { .. }) = m_input_state_data@.
pexpectState :: forall (s :: S). Term s (PMaybeData PData) -> Term s PData
pexpectState mData = pmatch mData $ \case
  PDJust d -> pfromData d
  PDNothing -> perror

{- | Aiken's @expect Some(State { .. }) = m_input_state_data@ at a known state
type.

The coercion is the port's standing substitute for Aiken's typed @expect@: a
malformed state fails when a field is read rather than at the pattern. Every
field of every step's state is read on every path that reaches it, so the two
agree on which transactions pass and can differ only in which error is reported.
-}
pexpectStateAs ::
  forall (a :: S -> Type) (s :: S). Term s (PMaybeData PData) -> Term s a
pexpectStateAs mData = punsafeCoerce (pexpectState mData)

-- | Aiken's @expect cond@ — evaluate to @value@ when @cond@ holds, abort otherwise.
pexpecting :: forall (a :: S -> Type) (s :: S). Term s PBool -> Term s a -> Term s a
pexpecting cond value = pif cond value perror
