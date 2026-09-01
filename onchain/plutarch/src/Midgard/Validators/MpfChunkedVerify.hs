{- |
Module      : Midgard.Validators.MpfChunkedVerify
Description : Plutarch port of @validators/mpf-chunked-verify.ak@.

The merkelized verifier for published-chunk MPF proof carriage (issue #545).

=== Why this is a withdraw script and not step-local code

The proof walk is the expensive part of the carriage, in script bytes as well as
in execution units. Compiled into every fault-proof step that can open a trie it
added about 3.7 kB to each step's spending script — which the step transaction
carries whichever route the prover takes, so it would have made the
/redeemer-carried/ route markedly more exhaustible: the measured ceiling fell
from branch level 21–23 to 8–9. Remediating one route by degrading the other is
not a remediation.

So the walk lives here, in one merkelized validator shared by every family,
exactly as @phas.ak@ and @pexcludes.ak@ already host the single-transaction
walks. A step on the direct route does not attach this script at all and is
byte-for-byte the size it was; a step on the chunked route attaches it in place
of @phas@/@pexcludes@ and carries no proof.

=== What it proves, and what it does not

The claim in the redeemer names a root, a key, a terminal and the order of the
publication UTxOs. This validator reassembles the proof from those UTxOs' inline
datums — reading @reference_inputs@, which it has and the delegating step also
has — and runs the complete walk.

It authenticates __nothing about the claim itself__. Binding the root to a
challenged header and the key to the step's own evidence is the delegating
step's job, and the step does it by requiring this exact claim in this exact
redeemer
('Midgard.FraudProofs.ChunkedInclusion.pdelegatedChunkMembership' and its
non-membership twin compare the whole claim). A verifier that accepted a claim
on its own terms would prove nothing; a verifier that tried to authenticate one
would need the step's evidence, which is what putting the walk here avoids
carrying.
-}
module Midgard.Validators.MpfChunkedVerify (mpfChunkedVerifyStakeValidator) where

import Plutarch.LedgerApi.V3 (
  PScriptContext (..),
  PScriptInfo (..),
  PTxInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.ChunkedInclusion (
  PChunkedProofClaim (..),
  PProofTerminal (..),
  PPublishedProofCarriage (..),
  ppublishedChunkMembershipByDigest,
  ppublishedChunkNonMembership,
 )

{- | Aiken @validators/mpf-chunked-verify.ak@ — @withdraw@, with
@else(_) { fail }@.

The own credential is unread, exactly as in Aiken: this validator is merkelized,
so which stake credential invoked it is the delegating step's concern and not
its own.
-}
mpfChunkedVerifyStakeValidator ::
  forall (s :: S). Term s (PScriptContext :--> PUnit)
mpfChunkedVerifyStakeValidator = plam $ \ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  -- `else(_) { fail }`: no purpose but `withdraw` reaches the walk.
  pmatch pscriptContext'scriptInfo $ \case
    PRewardingScript _ownCredential -> P.do
      PTxInfo {ptxInfo'referenceInputs} <- pmatch pscriptContext'txInfo
      PChunkedProofClaim
        { pclaim'mode
        , pclaim'merkleRoot
        , pclaim'keyBytes
        , pclaim'valueHash
        , pclaim'orderedChunkReferenceInputIndices
        } <-
        pmatch $
          pfromData $
            punsafeCoerce @(PAsData PChunkedProofClaim) (pto pscriptContext'redeemer)
      carriage <-
        plet $
          pcon
            ( PPublishedProofCarriage
                { pcarriage'orderedChunkReferenceInputIndices =
                    pclaim'orderedChunkReferenceInputIndices
                }
            )
      referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
      verified <-
        plet $
          pmatch (pfromData pclaim'mode) $ \case
            PMembership ->
              ppublishedChunkMembershipByDigest
                # referenceInputs
                # carriage
                # pfromData pclaim'merkleRoot
                # pfromData pclaim'keyBytes
                # pfromData pclaim'valueHash
            PNonMembership ->
              ppublishedChunkNonMembership
                # referenceInputs
                # carriage
                # pfromData pclaim'merkleRoot
                # pfromData pclaim'keyBytes
      pif verified (pconstant ()) perror
    _ -> perror
