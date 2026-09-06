# `observersForbiddenOnUntaggedNetwork` V1 size and transition sketch

- Frozen catalogue identity: `observersForbiddenOnUntaggedNetwork`, category
  ID `00000024`.
- Exact rejection constructor: `ObserversForbiddenOnUntaggedNetwork`.
- Machine predicate: the observer arm of phase-A script preconditions,
  `script_integrity_hash != zero && observer_count > 0 && network_id == 255`.
  The machine reaches that arm only when Plutus evaluation is required and
  the integrity hash is present (a zero hash either needs no evaluation or is
  rejected first as `ScriptIntegrityHashMissing`); a present hash itself
  requires evaluation, so the guard collapses to the hash alone. The family
  twin `forbidden_observers_hold_v1` is exactly this conjunction. Without the
  hash guard a native-only transaction carrying observers on scalar 255, which
  canonical validation accepts, would convict an honest operator.

## Transition topology

The family uses two narrow validators after generic computation-thread
`Init`:

1. `step_01` binds the accepted or forced transaction source, the exact typed
   rejection reason for the forced arm, and the compact transaction's network
   scalar. Its successor datum contains only the verdict subject (including
   transaction identity) and network scalar; step 02 re-derives the positional
   field-3 commitment from the transaction-ID-bound compact bytes.
2. `step_02` re-authenticates the compact body against the bound transaction
   id, reads its integrity hash and network scalar from that body,
   authenticates the retained field-3 preimage through the shared carriage
   door, counts the canonical observer array, and closes exactly one
   polarity. Wrongful acceptance requires a present integrity hash, a
   non-empty observer array, and network scalar `255`. Wrongful rejection
   requires the negation: the integrity hash is absent, or the observer array
   is empty, or it is non-empty and the network scalar is tagged. It burns
   the computation thread and mints the permanent proof token.

Both validators support the shared canonical cancellation arm. Every datum is
constant-size; observer bytes are carried only by the step-02 redeemer or
authenticated publication references. The subject, transaction ID, field
index `3`, observer commitment, network scalar, direction, and forced reason
are rebound at every transition; the integrity hash is read from the compact
body step 02 authenticates, never from the prover.

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
frontier uses the same 505 authenticated observer bytes with network scalar
`1`; the native-only forced frontier is one observer on scalar `255` under
the zero integrity hash.

The real testnet-blueprint Lucid gate executes, in
`demo/midgard-fault-proofs/tests/observers-forbidden-on-untagged-network-lifecycle.test.ts`:

- accepted maximum non-empty/untagged proof under a present hash, driven by
  the production actuator from its admitted artifact;
- forced empty/untagged contradiction;
- forced maximum non-empty/tagged contradiction;
- forced native-only (absent hash) contradiction;
- honest accepted refusals for both the native-only and the empty polarity;
- honest forced-rejection refusal (observers on `255` under a present hash);
- typed-reason mutation (a leaf typed `NetworkIdMismatch` claimed as this
  family), subject-coordinate and direction mutation, network-scalar
  substitution;
- every authentication seam: transaction membership, forced leaf header and
  membership, successor script, compact source, published carriage,
  certificate, and chunk order;
- cancellation at both physical stages;
- permanent mint and mutation-leased removal in every successful direction;
- signed publication and lifecycle measurements under ordinary Van Rossem
  limits with local evaluation enabled, closed by
  `assertCompleteLifecycleCoverage`.

Reference publications target at most 15,872 signed bytes. All signed
transactions must remain below 16,384 bytes, 16,500,000 memory, and
10,000,000,000 CPU without raised protocol parameters or oversized flags.
The final machine-readable artifact is
`docs/fault-proofs/size-plans/observers-forbidden-on-untagged-network-v1-fit-ledger.json`
in the shared `midgard-van-rossem-fit-ledger-v1` schema; it binds the fresh
blueprint digest and a canonical ledger digest, and
`observers-forbidden-on-untagged-network-fit-ledger.test.ts` pins both.

## Production ownership

The family runner configuration contains only manifest, blueprint/deployment,
header, Lucid/signer, authenticated public L1/retained-DA source,
decision-digest, mutation-lease coordinator, and immutable reference UTxOs.
The package reconstructs and selects evidence, publishes carriage and
certificates, journals preflight and intent before submission, reconciles the
exact transaction and bound cursor after restart, and drives terminal mint and
removal. No evidence, stage, submit, observe, or journal callback is accepted.
The journaled artifact carries the network scalar and the integrity hash it
was classified under; admission re-derives the evidence and refuses an
artifact that no longer closes its contradiction.

Central catalogue, manifest, classifier, runtime, complete-replay, and watcher
registration remain outside this family-local slice. The central classifier
must route this family before any downstream parser that would discard the
accepted untagged transaction needed to prove machine precedence.

## Measured implementation status

The two-step implementation is complete and measured against the testnet
blueprint digest recorded in the fit ledger. Applied step 01 publishes at
14,794 bytes (1,078-byte reserve margin); applied step 02 publishes at 7,435
bytes (8,437-byte reserve margin). The maximum 505-observer field uses two
certificate-backed chunks in both directions; its largest publication is
exactly 15,872 bytes, still 512 bytes below the ordinary ledger maximum. The
accepted terminal proof mint is 1,361 bytes with 15,023 bytes of ledger
margin; the forced maximum proof mint is 1,360 bytes. Mutation-leased removal
is 2,048 bytes with 14,336 bytes of margin in every direction, and it carries
the smallest execution margins of the ledger: 14,497,280 memory units and
9,317,259,662 CPU units remaining.

The family exports `ManifestBoundObserversForbiddenWorkflowConfig`,
`ManifestBoundObserversForbiddenWorkflow`,
`createManifestBoundObserversForbiddenWorkflow`,
`executeManifestBoundObserversForbiddenWorkflow`, and
`createObserversForbiddenWorkflowRunnerSurface`. The central serial
integration carries the frozen category and these ordered deployment roles:
`fraudProofObserversForbiddenOnUntaggedNetwork`, then
`fraudProofObserversForbiddenOnUntaggedNetworkStep02`.


### Canonical absence correction — 2026-09-05

Execution against the real host replay exposed an error in the original
plan's all-zero integrity-hash sentinel. The canonical codec and
`script_language_views_v1` both encode absence as Blake2b-256 of CBOR null
(`f6`), namely
`01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53`.
The Phase-A predicate, both dedicated family predicates and their host
classifiers now use that same commitment. An all-zero hash is a present,
incorrect integrity hash; it must not be reclassified as absence. This
replaces the zero-sentinel assumption above without a compatibility branch.
