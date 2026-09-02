# `observersForbiddenOnUntaggedNetwork` V1 size and transition sketch

- Frozen catalogue identity: `observersForbiddenOnUntaggedNetwork`, category
  ID `00000024`.
- Exact rejection constructor: `ObserversForbiddenOnUntaggedNetwork`.
- Machine predicate: `observer_count > 0 && network_id == 255`, evaluated by
  phase-A script preconditions after the authenticated observer field has been
  counted.

## Transition topology

The family uses two narrow validators after generic computation-thread
`Init`:

1. `step_01` binds the accepted or forced transaction source, the exact typed
   rejection reason for the forced arm, and the compact transaction's network
   scalar. Its successor datum contains only the verdict subject (including
   transaction identity) and network scalar; step 02 re-derives the positional
   field-3 commitment from the transaction-ID-bound compact bytes.
2. `step_02` authenticates the retained field-3 preimage through the shared
   carriage door, derives whether the canonical observer array is empty, and
   closes exactly one polarity. Wrongful acceptance requires a non-empty
   observer array and network scalar `255`. Wrongful rejection requires the
   negation: either the observer array is empty, or it is non-empty and the
   network scalar is tagged. It burns the computation thread and mints the
   permanent proof token.

Both validators support the shared canonical cancellation arm. Every datum is
constant-size; observer bytes are carried only by the step-02 redeemer or
authenticated publication references. The subject, transaction ID, field
index `3`, observer commitment, network scalar, direction, and forced reason
are rebound at every transition.

## Parameters and reference roles

- Step 01 parameters, in order: step-02 script hash, computation-thread policy
  ID, hub-oracle script hash.
- Step 02 parameters, in order: permanent fraud-proof policy ID, permanent
  token address data, computation-thread policy ID, field-preimage certificate
  policy ID.
- Family publications: step 01 and step 02.
- Shared references: computation-thread mint, fraud-proof mint,
  PHAS-membership withdrawal, chunk verification withdrawal, PHAS exclusion
  withdrawal, field-preimage certificate mint, and the canonical removal
  roster.

## Maximum shape and fit gate

The publication frontier is measured before lifecycle work because the
source-binding validator is expected to be the larger script. The observer
frontier is the 505-item canonical field used by the existing field-door
boundary: a three-byte array header plus 505 canonical 28-byte observer byte
strings, totaling 15,153 bytes. It forces certified carriage and covers the
largest observer-only preimage accepted by the shared bounded field surface.
The adjacent empty frontier is canonical `[]`; the tagged non-empty forced
frontier uses the same authenticated observer bytes with network scalar `0`.

The real testnet-blueprint Lucid gate must execute:

- accepted non-empty/untagged proof;
- forced empty/untagged contradiction;
- forced non-empty/tagged contradiction;
- honest accepted and wrongful-rejection mutations;
- cancellation at both physical stages;
- restart/reconciliation at the bound cursor;
- permanent mint and mutation-leased removal;
- signed publication and lifecycle measurements under ordinary Van Rossem
  limits with local evaluation enabled.

Reference publications target at most 15,872 signed bytes. All signed
transactions must remain below 16,384 bytes, 16,500,000 memory, and
10,000,000,000 CPU without raised protocol parameters or oversized flags.
The final machine-readable artifact is
`docs/fault-proofs/size-plans/observers-forbidden-on-untagged-network-v1-fit-ledger.json`
and binds the fresh blueprint digest and a canonical ledger digest.

## Production ownership

The family runner configuration contains only manifest, blueprint/deployment,
header, Lucid/signer, authenticated public L1/retained-DA source,
decision-digest, mutation-lease coordinator, and immutable reference UTxOs.
The package reconstructs and selects evidence, publishes carriage and
certificates, journals preflight and intent before submission, reconciles the
exact transaction and bound cursor after restart, and drives terminal mint and
removal. No evidence, stage, submit, observe, or journal callback is accepted.

Central catalogue, manifest, classifier, runtime, complete-replay, and watcher
registration remain outside this family-local slice. The central classifier
must route this family before any downstream parser that would discard the
accepted untagged transaction needed to prove machine precedence.

## Measured implementation status

The two-step implementation is complete and measured against the testnet
blueprint digest recorded in the fit ledger. Applied step 01 publishes at
14,794 bytes (1,078-byte reserve margin); applied step 02 publishes at 7,318
bytes (8,554-byte reserve margin). The maximum 505-observer field uses two
certificate-backed chunks; its largest publication is exactly 15,872 bytes,
still 512 bytes below the ordinary ledger maximum. The terminal proof mint is
1,361 bytes with 15,023 bytes of ledger margin, and mutation-leased removal is
2,060 bytes with 14,324 bytes of margin.

The family exports `ManifestBoundObserversForbiddenWorkflowConfigV1`,
`ManifestBoundObserversForbiddenWorkflowV1`,
`createManifestBoundObserversForbiddenWorkflowV1`,
`executeManifestBoundObserversForbiddenWorkflowV1`, and
`createObserversForbiddenProductionWorkflowRunnerSurfaceV1`. The central
serial integration must add the frozen category and these ordered deployment
roles: `fraudProofObserversForbiddenOnUntaggedNetwork`, then
`fraudProofObserversForbiddenOnUntaggedNetworkStep02`.
