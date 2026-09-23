# Remaining Non-Interactive Fault-Proof Gaps

Status: Active

Last reviewed: 2026-09-12 against the current Aiken, SDK, watcher, node, and
technical-specification sources. NIFP-01–03 closure recommendations aligned with
the selected list proposal on 2026-09-21; findings and open status are unchanged.

## Purpose and closure rule

This document tracks concrete protocol statements that remain impossible, only
partially provable, or incompletely routed through the non-interactive
fault-proof system. It is intentionally separate from the 55/55 source-category
inventory: a category may exist and be installed while failing to cover every
invalid statement in its advertised class.

An entry is closed only when all of the following are true:

1. the specification defines the authoritative fact and its lifetime;
2. an L1 validator can authenticate the fact without trusting the challenger,
   operator, or watcher's database;
3. the catalogue, applied validators, off-chain derivation, and watcher route
   implement the proof;
4. tests cover the faulty case, the corresponding honest refusal at the same
   evidence frontier, maximum supported shape, rollback, and the relevant
   challenge-window boundary; and
5. deployment identity and measured transaction-fit evidence have been rebuilt
   where validator or encoding changes require it.

The severities below describe protocol-coverage risk, not implementation effort.

## Summary

| ID      | Severity | Status                      | Gap                                                                                                 |
| ------- | -------- | --------------------------- | --------------------------------------------------------------------------------------------------- |
| NIFP-01 | High     | Open                        | `fabricatedWithdrawal` cannot prove every nonexistent withdrawal identity                           |
| NIFP-02 | High     | Open                        | `fabricatedDeposit` has the same incomplete nonexistence witness                                    |
| NIFP-03 | Medium   | Open                        | Event-backed proof evidence does not have a closed on-chain lifetime                                |
| NIFP-04 | High     | Open                        | A fabricated or substituted forced transaction has no exact L1-to-block proof                       |
| NIFP-05 | High     | Open                        | A forbidden auxiliary-data hash has no fault-proof route                                            |
| NIFP-06 | Medium   | Open                        | Malformed MidgardV1 program envelopes have no direct total proof route                              |
| NIFP-07 | Medium   | Open                        | `mintItemNonCanonical` cannot adjudicate wrongful forced rejection                                  |
| NIFP-08 | High     | Open                        | Malformed raw leaves outside `transactions_root` have no total canonicality route                   |
| NIFP-09 | Medium   | Ownership decision required | The 5,000-byte output-Value limit is neither covered by a fault proof nor explicitly assigned to DA |

## NIFP-01 — Fabricated withdrawal nonexistence is not universal

Severity: **High**  
Confidence: **High**

### Summary

The `fabricatedWithdrawal` family's absence arm proves one useful subset of
nonexistence: the challenged `WithdrawalId` names an L1 output that still exists
unspent. Because authentic event creation must spend that output, its continued
existence proves that no authentic event used that identity.

It cannot prove an arbitrary nonexistent identity. In particular, no current arm
can start from an ID whose transaction never existed, whose output index never
existed, or whose output existed but was spent by an unrelated transaction. A
Plutus validator cannot receive a nonexistent or already-spent UTxO as a
reference input, so these are not alternate encodings of the implemented
`AbsentWithdrawalIdentity` witness.

The other arm requires a currently live withdrawal-event UTxO and therefore only
handles a real event whose committed content can be compared. Together the two
arms do not exhaust the possible identities a malicious block can place in
`withdrawals_root`.

### Evidence

- [`WithdrawalEvidenceV1`](../../onchain/aiken/lib/midgard/fraud-proofs/fabricated-withdrawal/step-02.ak#L93-L102)
  contains only `AbsentWithdrawalIdentity` and `PresentWithdrawalEvent`.
- The absence arm requires a reference input whose `output_reference` equals the
  committed ID
  ([lines 138–151](../../onchain/aiken/lib/midgard/fraud-proofs/fabricated-withdrawal/step-02.ak#L138-L151)).
- The presence arm requires the live hub-registered withdrawal NFT and datum
  ([lines 153–178](../../onchain/aiken/lib/midgard/fraud-proofs/fabricated-withdrawal/step-02.ak#L153-L178)).
- The witness-staking specification already cautions that current
  non-registration proves only the absence of a _live_ witness, not that an event
  never existed
  ([lines 41–45](../../technical-spec/2-user-event-protocol/5-witness-staking-script.tex#L41-L45)).

### Recommended fix

Implement the selected [authenticated list design](event-history-design.md):
every real withdrawal is admitted atomically into the deployed withdrawal list.
Authenticate original content, eligibility time and all other facts consumed by
proofs. Current-list absence must establish ineligibility for the challenged
interval, with evidence captured after that interval and no backdated admission.
Retirement must preserve every event still needed by a permitted challenge.

Add list presence and gap/exact-key-filler absence to the entire fabricated
withdrawal workflow. A later malicious reuse of a legitimately retired ID is
rejected through absence; this does not require a permanent record or assert
that the ID never existed. Captured facts must remain usable across proof stages
when pointer updates spend and recreate list nodes.

Use the proposal's lifecycle-specific acceptance matrix for arbitrary IDs,
content substitution, timing, honest refusal, rollback, retirement and challenge
deadlines. An exact unspent nonce is not required by the new universal absence
path. Do not retain the old path merely as a compatibility obligation for an
undeployed interface. These recommendations do not claim implementation or
closure of this entry.

## NIFP-02 — Fabricated deposit nonexistence is not universal

Severity: **High**  
Confidence: **High**

### Summary

`fabricatedDeposit` mirrors the withdrawal construction and has the same gap.
Its absence proof requires the exact `DepositId` output to remain unspent; its
presence proof requires a live deposit-event NFT. It therefore cannot prove
every arbitrary nonexistent deposit identity committed by a malicious block.

### Evidence

- [`DepositEvidenceV1`](../../onchain/aiken/lib/midgard/fraud-proofs/fabricated-deposit/step-02.ak#L77-L83)
  exposes only the unspent-output and live-event alternatives.
- The verifier requires exact live output-reference equality in the absence arm
  and a hub-authenticated live deposit NFT in the presence arm
  ([lines 112–160](../../onchain/aiken/lib/midgard/fraud-proofs/fabricated-deposit/step-02.ak#L112-L160)).
- The witness-staking conformance warning applies to deposits, withdrawals, and
  transaction orders because the script is parameterized by the generic user
  event ID
  ([lines 8–21](../../technical-spec/2-user-event-protocol/5-witness-staking-script.tex#L8-L21)).

### Recommended fix

Use the same authenticated list primitives as NIFP-01 in a separately deployed
or domain-bound deposit list. Keep event-kind decoding and policy authentication
separate. Authenticate original deposited Value as well as payload and time,
including every affected staged projection proof.

Closure covers the same lifecycle-specific identity/content/timing cases as
withdrawals, plus asset substitution, pointer-update preservation, finalized
absorption and later reuse of retired IDs. The current deposit lifecycle has no
general early refund; this work does not introduce one. Implementing only the
currently provable unspent-nonce case does not close this entry.

## NIFP-03 — Event-proof lifetime is not closed

Severity: **Medium**  
Confidence: **High**

### Summary

Several proof routes authenticate an L1 event by reading its live event UTxO and
NFT as a reference input. Normal settlement or refund burns that NFT and spends
the UTxO. The fabricated-event families retain a datum hash _after step 02 has
run_, which lets an already-open computation thread continue, but it does not let
a challenger start step 02 after the event has disappeared.

The watcher now retains original event bytes off chain. That is necessary for
discovery and preimage reconstruction, but it is not an L1 authentication route:
an on-chain validator cannot trust an archival database merely because the
watcher retained it.

The scope is broader than the two fabricated-event families. Transition-trace
omission and event-window witnesses also read live deposit, withdrawal, or
transaction-order event UTxOs. Each affected proof needs evidence that remains
both retrievable _and on-chain authentic_ until the last block that could depend
on the event is no longer challengeable.

### Evidence

- Fabricated-withdrawal step 02 notes that the event UTxO may disappear, but the
  retained hash is created only after that step reads the live event
  ([lines 41–52](../../onchain/aiken/lib/midgard/fraud-proofs/fabricated-withdrawal/step-02.ak#L41-L52)).
- Step 03 opens the already-retained commitment; it supplies no independent L1
  history authentication
  ([lines 60–85](../../onchain/aiken/lib/midgard/fraud-proofs/fabricated-withdrawal/step-03.ak#L60-L85)).
- Transition-trace event helpers require live NFT-bearing reference inputs
  ([lines 1501–1575](../../onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak#L1501-L1575)),
  and the omission/window arms call those helpers
  ([lines 1610–1657](../../onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak#L1610-L1657),
  [1661–1764](../../onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak#L1661-L1764)).
- The existing coverage matrix already records the general data-lifetime
  requirement
  ([lines 79–81](coverage-matrix.md#L79-L81)).
- Watcher persistence retains spent protocol UTxOs and immutable original event
  bytes while a challenge may need them
  ([lines 13–25](../midgard/decisions/watcher-persistence.md#L13-L25)),
  but explicitly treats archive indexes as navigation rather than validation
  authority.

### Recommended fix

For deposits and withdrawals, implement the selected
[list retention and staged-proof design](event-history-design.md):

1. atomically authenticate each admitted event's identity, original payload,
   eligibility time and original assets in the appropriate list;
2. preserve those facts when list pointers change, and retain the Order and any
   external data while required for a permitted challenge;
3. capture authenticated facts into proof threads so later stages do not depend
   on an obsolete output reference;
4. authorize settlement/retirement against the actual finalized frontier and
   proof deadlines, rather than elapsed time or payment alone; and
5. prove that later pending headers reusing a retired ID remain challengeable
   through current-list absence without retaining the old payload forever.

The acceptance matrix distinguishes pointer-update consumption, finalized order
retirement and an expired challenge opportunity. Off-chain archives support
retrieval/recovery but are never independent L1 authority. Public retrieval and
adversarial witness churn must be verified through all permitted proof stages.

Forced-order omission/window evidence has a separate unresolved lifetime. Keep
that portion of NIFP-03 open after deposit/withdrawal work is delivered, and
preserve its existing behavior when shared helpers change. This two-list design
does not specify a forced-order history redesign or close NIFP-04.

## NIFP-04 — Fabricated or substituted forced transactions

Severity: **High**  
Confidence: **High**

### Summary

There is no catalogue family named for a fabricated forced transaction. Current
transition-trace checks can prove that a due authentic order was omitted and can
bind a committed forced leaf to its own trace entry, but the due/in-window
one-step binding does not compare the committed forced source against the
authentic L1 transaction-order event. Consequently, an extra forced leaf under a
fabricated key, or substituted content under an authentic order key, lacks an
exact non-interactive L1-to-block proof.

### Evidence

- The source catalogue has no `fabricatedForcedTransaction` category
  ([catalogue status](catalogue-status.md#source-inventory)).
- Forced one-step binding checks root membership, event key, and phase, but no
  authentic L1 order value
  ([lines 754–776](../../onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak#L754-L776)).
- `OmittedDueForcedTransaction` authenticates the live order and proves only key
  nonmembership in `forced_transactions_root`
  ([lines 1632–1656](../../onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak#L1632-L1656)).
- The out-of-window forced arm does construct and compare an expected source,
  demonstrating the missing comparison at the due/in-window frontier
  ([lines 1716–1762](../../onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak#L1716-L1762)).

### Recommended fix

Add either a dedicated `fabricatedForcedTransaction` family or explicit
transition-trace subclaims that:

1. authenticate the L1 transaction-order event through the retained event-history
   route from NIFP-03;
2. prove exact equality of the immutable submitted transaction body, witnesses,
   source commitment, transaction ID, and order ID between L1 and the block;
3. keep the operator verdict as a separately authenticated operator claim that
   the execution dispute may overturn;
4. prove nonmembership in event history for a wholly fabricated order key; and
5. cover same-key source substitution, extra nonexistent key, wrong order/event
   kind, honest equality, settlement, and rollback.

Implement the accepted validity-free forced-source decision described under
“Blocking prerequisites” before fixing the exact equality encoding.

## NIFP-05 — Forbidden auxiliary-data hash

Severity: **High**  
Confidence: **High**

### Summary

Canonical V1 requires `auxiliary_data_hash` to equal the empty-null root. The node
rejects any other value, but the on-chain compact-body parser accepts every
32-byte auxiliary hash and there is no dedicated category for this static fault.
Because the protocol carries no auxiliary-data preimage, an execution dispute is
not an appropriate substitute.

### Evidence

- The specification requires the exact empty-null root and fail-closed rejection
  ([lines 23–30](../../technical-spec/5-ledger-rules/2-custom-midgard-ledger-rules.tex#L23-L30)).
- The node enforces it during consensus admission
  ([lines 377–382](../../demo/midgard-core/src/consensus-validation.ts#L377-L382)).
- The `daHashPreimage` compact-body inspection accepts any 32-byte value
  ([lines 176–205](../../onchain/aiken/lib/midgard/fraud-proofs/da-hash-preimage/rule.ak#L176-L205)).
- No `auxiliaryDataHashForbidden` category exists in the source catalogue.

### Recommended fix

Add a bounded static family, for example `auxiliaryDataHashForbidden`, that opens
the authenticated compact transaction body, proves
`auxiliary_data_hash != EMPTY_NULL_ROOT`, and supports both normal wrongful
acceptance and the corresponding typed forced-transaction rejection polarity.
Add the typed rejection reason before relying on the forced direction. Test the
exact empty-null value, a one-bit difference, arbitrary 32-byte values, malformed
lengths handled by the existing shape family, normal and forced paths, and honest
refusal.

## NIFP-06 — MidgardV1 program-envelope canonicality

Severity: **Medium**  
Confidence: **High**

### Summary

The node rejects malformed or out-of-bound MidgardV1 CEK program envelopes in
script witnesses and output reference scripts. Existing script-decoding
non-interactive families deliberately classify non-native scripts as “no
fault,” while the execution-source binding accepts the Midgard language tag
without proving the canonical envelope. A malformed envelope can therefore fall
between static decoding and execution adjudication.

### Evidence

- Node admission decodes every MidgardV1 witness and reference-script envelope
  and returns `E_SCRIPT_PROGRAM_ENCODING` on failure
  ([lines 516–538](../../demo/midgard-core/src/consensus-validation.ts#L516-L538),
  [565–581](../../demo/midgard-core/src/consensus-validation.ts#L565-L581)).
- Witness-script decoding returns `result_no_fault` for non-native scripts
  ([lines 178–186](../../onchain/aiken/lib/midgard/fraud-proofs/witness-script-decoding/rule.ak#L178-L186)).
- Output-reference-script decoding does the same
  ([lines 322–329](../../onchain/aiken/lib/midgard/fraud-proofs/output-reference-script-decoding/rule.ak#L322-L329)).
- Execution-source binding checks language tag `0` and basic lengths, but not the
  canonical program-envelope decoder
  ([lines 205–250](../../onchain/aiken/lib/midgard/fraud-proofs/execution-source-script-decoding/rule.ak#L205-L250)).

### Recommended fix

Add a total, chunkable program-envelope canonicality family with variants for a
transaction witness, an output reference script, and a resolved predecessor
reference script. It should prove exact canonical decoding, version, declared
node/material bounds, field lengths, and rejection of trailing or alternate
encodings. Add the matching typed forced rejection reason and both wrongful
acceptance/rejection directions. Leave separately retained CEK graph-material
availability to its admission/liveness rule; this family concerns the committed
envelope bytes.

## NIFP-07 — Mint noncanonical wrongful rejection

Severity: **Medium**  
Confidence: **High**

### Summary

`mintItemNonCanonical` proves malformed mint content only for wrongful
acceptance. The family explicitly refuses the wrongful-rejection direction
because the forced rejection-reason schema has no typed arm for this statement.
The operator therefore cannot encode this as the truthful reason for rejecting a
forced transaction whose mint field has this fault; substituting a different
reason does not let this family adjudicate the actual statement.

### Evidence

- `bind_item_v1` documents the missing typed rejection arm and requires
  `direction_wrongful_acceptance`
  ([lines 41–50](../../onchain/aiken/lib/midgard/fraud-proofs/mint-item-non-canonical/rule.ak#L41-L50)).

### Recommended fix

Add a stable `MintItemNonCanonical` constructor to the canonical forced rejection
reason, preserve constructor-order/deployment migration rules, and extend the
family to both polarities:

- wrongful acceptance succeeds exactly when the committed mint item is
  noncanonical;
- wrongful rejection succeeds exactly when the item is canonical at the accused
  coordinate; and
- wrong reason, wrong coordinate, malformed surrounding field, and honest
  decisions are refused.

Rebuild the catalogue identity, applied validators, manifest, SDK schemas,
watcher classifier, and cross-language vectors together.

## NIFP-08 — Malformed raw leaves outside `transactions_root`

Severity: **High**  
Confidence: **Medium-high**

### Summary

`daHashPreimage` provides a total raw-leaf route for `transactions_root`, allowing
a challenger to bind bytes before typed decoding. Other header roots are consumed
primarily through typed membership witnesses. If a malicious commitment contains
a leaf whose key or value cannot decode to the expected deposit, withdrawal,
forced-transaction, event-to-step, or transition-trace schema, the typed proof
itself cannot carry that malformed leaf and no generic raw-leaf family currently
classifies it.

This is a content fault, not one of the byte-size limits assigned to DA by ADR 0006.

### Evidence

- `daHashPreimage` is explicitly scoped to a raw `transactions_root` leaf
  ([step-01 header](../../onchain/aiken/lib/midgard/fraud-proofs/da-hash-preimage/step-01.ak#L1-L8),
  [rule header](../../onchain/aiken/lib/midgard/fraud-proofs/da-hash-preimage/rule.ak#L1-L8)).
- Transition/event proofs accept typed root-membership values, for example the
  typed forced membership in the one-step binding
  ([lines 754–764](../../onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak#L754-L764)).
- ADR 0006 explicitly excludes malformed encodings and other content faults from
  its DA-only decision
  ([lines 20–28](decisions/0006-da-attestation-transaction-size-admission.md#L20-L28)).

### Recommended fix

Add a total raw-root canonicality substrate parameterized by an allowlisted root
domain and its canonical leaf schema, or add equivalent root-specific families.
The proof must bind raw key/value bytes and leaf index to the counted root before
attempting bounded canonical decoding. Cover at least `deposits_root`,
`withdrawals_root`, `forced_transactions_root`, `event_to_step_root`, and
`transition_trace_root`, including malformed key, malformed value, noncanonical
but decodable value, trailing bytes, count/index boundaries, and honest leaves.

If the intended security model instead makes DA attesters authoritative for
these schemas, record a new explicit trust decision that supersedes ADR 0006's
content exclusion and add exact malformed-leaf refusal tests. Do not silently
infer that broader trust from a DA signature.

## NIFP-09 — Output-Value 5,000-byte ownership is unassigned

Severity: **Medium**  
Status: **Ownership decision required**  
Confidence: **High**

### Summary

Canonical V1 limits the Cardano CBOR encoding of each output Value to 5,000
bytes, and the node rejects larger values. The on-chain ledger-output scanner
records `cardano_value_size` but its terminal predicate requires only a positive
size. ADR 0006 assigns full-transaction, nine field-preimage, and aggregate
transaction bytes to DA; it does not name this nested semantic limit.

### Evidence

- The 5,000-byte limit is normative
  ([lines 71–81](../../technical-spec/1-ledger-state/6-transaction.tex#L71-L81)).
- Node consensus validation rejects an oversized output Value
  ([lines 549–558](../../demo/midgard-core/src/consensus-validation.ts#L549-L558)).
- The terminal output scanner checks only `cardano_value_size > 0`
  ([lines 831–884](../../onchain/aiken/lib/midgard/ledger-output-scan-v1.ak#L831-L884)).
- ADR 0006's enumerated DA-owned limits do not include the nested Value size
  ([lines 8–24](decisions/0006-da-attestation-transaction-size-admission.md#L8-L24)).

### Recommended fix

Choose and record one owner:

- **DA-owned:** amend ADR 0006 and the DA rules to name every output's canonical
  Cardano Value size, then add exact-5,000 and 5,001-byte attester refusal tests
  for normal and forced transactions and prove that no signature is emitted; or
- **fault-proof-owned:** add an `outputValueSizeExceeded` static family using the
  authenticated ledger-output scan result, with normal/forced polarities and
  exact adjacent-boundary tests.

Until one route is normative and tested, the rule is enforced by the node but is
not closed under the optimistic dispute model.

## Existing documentation for withdrawal, deposit, and event gaps

The repository contains partial requirements and a proposed
[event-history architecture](event-history-design.md), which records the selected
deposit/withdrawal lists, current flow, remaining implementation decisions,
retention rules and lifecycle-specific acceptance criteria. The architecture is
selected but not implemented or verified; these gaps remain open.
The earlier documentation establishes the following narrower guarantees:

| Document                                                                                                                                                                                                                      | What it already establishes                                                                                                   | What remains                                                                                                |
| ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------- |
| [Witness staking script](../../technical-spec/2-user-event-protocol/5-witness-staking-script.tex#L8-L45)                                                                                                                      | Defines register/unregister endpoints and explicitly warns that non-registration proves only “not live,” not “never existed.” | No event-kind-aware historical nonmembership commitment or challenge-horizon registration rule.             |
| [Coverage matrix](coverage-matrix.md#L70-L92)                                                                                                                                                                                 | Records data lifetime as a cross-cutting gap.                                                                                 | Does not identify affected proof arms or prescribe an on-chain authentication mechanism.                    |
| [GOAL_SPEC Q39/Q40/Q42](../exec-plans/GOAL_SPEC.md#L948-L951)                                                                                                                                                                 | Requires fabricated deposit/withdrawal content fidelity and cross-block survival.                                             | Requirement list, not a complete history design or implementation receipt.                                  |
| [Watcher persistence decision](../midgard/decisions/watcher-persistence.md#L13-L25)                                                                                                                                           | Retains spent UTxOs, original event bytes, and proof dependencies off chain.                                                  | Archive indexes are not L1 validation authority; validators still need an authenticated history commitment. |
| Fabricated [deposit](../../onchain/aiken/lib/midgard/fraud-proofs/fabricated-deposit/step-02.ak#L10-L36) and [withdrawal](../../onchain/aiken/lib/midgard/fraud-proofs/fabricated-withdrawal/step-02.ak#L10-L52) family notes | Explain the unspent-nonce proof and the post-step-02 datum-hash handoff.                                                      | Do not cover arbitrary nonexistent IDs or initiation after event consumption.                               |

This tracker is the authority for the remaining gap and recommended closure. The
linked documents remain the authorities for their narrower protocol,
persistence, and implementation facts.

## Blocking prerequisites that are not new catalogue entries

### Forced-submission source identity

The forced path currently contains two validity claims: one embedded in the
submitted proof source and one in the operator verdict. The accepted decision is
to remove validity from the forced-specific submitted source and make the verdict
the sole operator claim. That design is recorded but explicitly not implemented
or deployed
([decision status and design](../midgard/decisions/forced-inclusion-submission-verdict.md#L1-L29)).
NIFP-04's exact source-equality proof should target that replacement encoding,
not cement the current mutable-validity workaround.

### Forced CEK material availability

Missing retained CEK graph material for a forced transaction is an
admission/liveness and cancellation problem, not a post-commit static statement
about a program that can necessarily be evaluated. Keep its owner under the
`forced-program-material-availability` consensus rule rather than creating a
misleading non-interactive proof category. NIFP-06 still applies when the
committed program _envelope itself_ is malformed.

## Explicitly excluded or closed findings

- **Transaction and field-preimage sizes:** ADR 0006 assigns the full canonical
  transaction, each of its nine committed field preimages, and aggregate block
  transaction bytes to fail-closed DA admission. They are not open catalogue
  gaps. NIFP-09 is listed separately because the nested 5,000-byte Value limit is
  not in that decision.
- **Negative output values and ADA minting:** these are structurally
  unrepresentable in canonical typed values; malformed encodings are handled at
  raw/canonicality boundaries. They do not require semantic categories for
  impossible typed statements.
- **Post-state UTxO network ID:** the `networkId` family now includes
  `OutputNetworkUtxo` and its SDK workflow, so the earlier post-state-output gap
  is closed in current source
  ([fault type](../../onchain/aiken/lib/midgard/fraud-proofs/network-id/step-01.ak#L13-L20)).
- **Deployment and acceptance gaps:** missing Preprod evidence, transaction fit,
  economics, and public-data retrieval remain release blockers tracked by the
  testing and execution documents. They are not additional protocol categories
  unless they reveal that a statement is impossible to prove.

## Maintenance

Update this document whenever a gap is added, its design is accepted, its source
lands, or acceptance closes it. A source implementation moves an entry to
“Implemented; acceptance pending”; it does not delete the entry. Remove it from
the open summary only after the closure rule at the top is satisfied, and link
the implementation, tests, measured fit evidence, and deployment identity in the
entry before marking it closed.
