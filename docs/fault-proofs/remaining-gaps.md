# Remaining Non-Interactive Fault-Proof Gaps

Status: Active

Last reviewed: 2026-11-23 against the Aiken, SDK, watcher and node sources
(entry status and evidence pointers).

## Purpose and closure rule

This document tracks concrete protocol statements that remain impossible, only
partially provable, or incompletely routed through the non-interactive
fault-proof system. It is separate from the
[catalogue inventory](catalogue-status.md): a category may exist and be
installed while failing to cover every invalid statement in its advertised
class.

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

| ID      | Severity | Status                          | Gap                                                                               |
| ------- | -------- | ------------------------------- | --------------------------------------------------------------------------------- |
| NIFP-01 | High     | Implemented; acceptance pending | Authenticated withdrawal nonexistence                                             |
| NIFP-02 | High     | Implemented; acceptance pending | Authenticated deposit nonexistence                                                |
| NIFP-03 | Medium   | Partly implemented              | Evidence lifetime: deposit/withdrawal implemented; forced-order evidence open     |
| NIFP-04 | High     | Open                            | A fabricated or substituted forced transaction has no exact L1-to-block proof     |
| NIFP-05 | High     | Open                            | A forbidden auxiliary-data hash has no fault-proof route                          |
| NIFP-06 | Medium   | Partly implemented              | Malformed Plutus program envelopes in a resolved predecessor reference script     |
| NIFP-07 | Medium   | Implemented; acceptance pending | `mintItemNonCanonical` wrongful forced rejection                                  |
| NIFP-08 | High     | Open                            | Malformed raw leaves outside `transactions_root` have no total canonicality route |
| NIFP-09 | Medium   | Implemented; acceptance pending | The 5,000-byte output-Value limit, owned by the ledger-output fault proofs        |

## NIFP-01 and NIFP-02 — Deposit and withdrawal nonexistence

Every real deposit and withdrawal is admitted into an authenticated sorted
event-history list (`onchain/aiken/lib/midgard/event-history/`, validators
`onchain/aiken/validators/user-events/history.ak`, `history-data.ak` and
`history-retirement.ak`). The `AbsentWithdrawalIdentity` and
`AbsentDepositIdentity` arms of the fabricated-event step 02 read that list
through `history_ref_input_index` and prove absence with a list gap, so an
arbitrary nonexistent or retired ID is challengeable without the original
output still being unspent.

Acceptance pending: a live deployment, public witness availability under list
churn through every proof stage, the maximum fit under final parameters, and
the recovery matrix.

## NIFP-03 — Event-proof lifetime

Deposit and withdrawal evidence is captured from the history list into the
proof thread, so a challenge can start after the event UTxO is spent; this
part shares the NIFP-01/02 acceptance.

Forced-order evidence remains open. The transition-trace
`OmittedDueForcedTransaction` arm reads the live transaction-order datum
(`get_tx_order_datum_with_nft` in
`onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak`), so it
cannot start once that order is consumed. Off-chain archives support retrieval
but are never L1 authority
([watcher verification boundaries](../midgard/decisions/watcher-verification-boundaries.md)).
Closing it needs on-chain authenticated forced-order history that lasts until
the last dependent block is no longer challengeable.

## NIFP-04 — Fabricated or substituted forced transactions

There is no `fabricatedForcedTransaction` category. The transition-trace
`validate_forced_transaction_one_step_binding` (same `proof.ak`) checks root
membership, event key and phase, but not the authentic L1 transaction-order
value, so an extra forced leaf under a fabricated key, or substituted content
under an authentic key, lacks an exact L1-to-block proof. The out-of-window
forced arm does build and compare an expected source.

Fix: a dedicated family or transition-trace subclaims that authenticate the L1
order through forced-order history (NIFP-03), prove exact equality of the
submitted body, witnesses, source commitment, transaction ID and order ID, keep
the operator verdict as a separate claim the execution dispute may overturn,
and prove history nonmembership for a wholly fabricated key. The submitted
source is the validity-free `ForcedInclusionTxV1 { tx_id, submitted_source,
verdict }` in `onchain/aiken/lib/midgard/ledger-state.ak`; the equality proof
targets that encoding.

## NIFP-05 — Forbidden auxiliary-data hash

Canonical V1 requires `auxiliary_data_hash` to equal the empty-null root, and
the node rejects any other value
(`demo/midgard-core/src/consensus-validation.validate-midgard-consensus-tx.ts`).
On chain, the `daHashPreimage` compact-body parser
(`onchain/aiken/lib/midgard/fraud-proofs/da-hash-preimage/rule.ak`) accepts any
32-byte value, and no category or typed rejection reason covers it. The
protocol carries no auxiliary-data preimage, so an execution dispute is not a
substitute.

Fix: a bounded static family that opens the authenticated compact body and
proves `auxiliary_data_hash != EMPTY_NULL_ROOT`, with a typed rejection reason
for the forced direction, tested at the exact value, a one-bit difference and
honest refusal.

## NIFP-06 — Plutus program-envelope canonicality

Rejection arm 48 `PlutusScriptUndecodable` and the shared decode record
(`onchain/aiken/lib/midgard/script-decode-v1.ak`) let `witnessScriptDecoding`
and `outputReferenceScriptDecoding` prove an undecodable Plutus script in a
transaction witness or an output reference script, in both directions.
`executionSourceScriptDecoding` covers only the native-script arms, so a
malformed Plutus envelope in a resolved predecessor reference script has no
confirmed total route.

## NIFP-07 — Mint noncanonical wrongful rejection

Rejection arm 47 `MintItemMalformed` exists and no machine step emits it.
`mintItemNonCanonical` step 01 hands a forced rejection carrying it to step 04
as an unreachable claim (`unreachable_claim_state_v1` in
`onchain/aiken/lib/midgard/fraud-proofs/mint-item-non-canonical/rule.ak`),
which contradicts the rejection. Tests:
`onchain/aiken/validators/fraud-proofs/mint-item-non-canonical/forced.test.ak`
and `demo/midgard-fault-proofs/tests/mint-item-non-canonical-forced*.test.ts`.
The branch is sound only while no machine step emits the reason.

## NIFP-08 — Malformed raw leaves outside `transactions_root`

`daHashPreimage` is a total raw-leaf route for `transactions_root` only. Other
header roots are consumed through typed membership witnesses, so a leaf whose
key or value cannot decode to the expected deposit, withdrawal,
forced-transaction, event-to-step, or transition-trace schema cannot be carried
by the typed proof, and no generic raw-leaf family classifies it. This is a
content fault, which
[ADR 0006](decisions/0006-da-attestation-transaction-size-admission.md)
excludes from DA admission.

Fix: a raw-root canonicality substrate parameterized by an allowlisted root
domain and its leaf schema, or root-specific families, binding raw key/value
bytes and leaf index to the counted root before bounded decoding. If DA
attesters are instead to be authoritative for these schemas, record a decision
that supersedes ADR 0006's content exclusion.

## NIFP-09 — Output-Value 5,000-byte limit

The ledger-output scan refuses an output Value over
`max_cardano_value_cbor_bytes` (5,000,
`onchain/aiken/lib/midgard/ledger-output-commitment-v1.ak`), so
`transactionOutputNonCanonical` and `resolvedOutputNonCanonical` convict it
under their existing arms. The tx-order mint refuses such a forced order;
`demo/midgard-fault-proofs/tests/transaction-output-non-canonical-value-cap-forced.test.ts`
convicts a forced leaf accepting a 5,001-byte Value and refuses to convict its
rejection.

## Explicitly excluded findings

- **Transaction and field-preimage sizes:** ADR 0006 assigns the full canonical
  transaction, each of its nine committed field preimages, and aggregate block
  transaction bytes to fail-closed DA admission.
- **Negative output values and ADA minting:** structurally unrepresentable in
  canonical typed values; malformed encodings are handled at raw/canonicality
  boundaries.
- **Post-state UTxO network ID and minimum Ada:** every output that enters the
  ledger comes from an accepted transaction (a normal `TxIsValid` transaction or
  a `ForcedTxValid` forced leaf), and `networkId` and `minAda` check those
  outputs at the transaction level. Deposits are projected faithfully under
  `transitionTrace`, and genesis is empty.
- **Forced CEK material availability:** the tx-order mint admits only
  native-script forced transactions with no redeemers, so a forced transaction
  executes no program.
- **Deployment and acceptance gaps:** Preprod evidence, transaction fit,
  economics and public-data retrieval are release work in the
  [release acceptance plan](execution-plan.md), not protocol categories.

## Maintenance

Update this document whenever a gap is added, its design is accepted, its source
lands, or acceptance closes it. A source implementation moves an entry to
"Implemented; acceptance pending"; it does not delete the entry. Remove it only
after the closure rule above is satisfied, linking the implementation, tests,
fit evidence and deployment identity in the entry first.
