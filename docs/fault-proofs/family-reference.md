# Fault-proof family semantics

This reference explains the semantic boundaries between fault families. The
[catalogue](catalogue-status.md) owns the complete category inventory; the
[off-chain reference](offchain-reference.md) identifies the SDK and installed
workflow authorities. Exact physical step counts, parameters and schemas belong
to source and the applied deployment manifest.

Every installed family must authenticate its evidence, reach permanent proof
mint and removal, and refuse honest evidence. [Testing status](testing-status.md)
and the [remaining acceptance plan](execution-plan.md) own release obligations;
source presence and retained measurements do not establish live/preprod readiness.
Family-specific CLI aliases are not prerequisites when the supported workflow
already exposes the operation; use the [challenger runbook](challenger-runbook.md).

## Canonical-decodability fault

The family proves that an operator-accepted normal transaction commits a field
preimage that is not a canonical envelope. Step 01 binds the transaction under
`transactions_root`, requires validity code zero, authenticates the exact field
bytes, and derives the envelope verdict. Step 02 convicts only a nonzero
in-range verdict. A grammatical field cannot convict. This two-step family does
not adjudicate wrongful forced rejection; typed rejection families own that
separate direction.

[Validator source](../../onchain/aiken/validators/fraud-proofs/canonical-decodability).

## Committed-field-shape fault

The family proves that an authenticated committed field violates the canonical
fixed-stride, item-count, or aggregate-size shape required by the native-V1
transaction format. It operates on the exact committed field bytes and does
not accept caller-asserted lengths or verdicts.

[Validator source](../../onchain/aiken/validators/fraud-proofs/committed-field-shape).

## Cross-block duplicate-event fault

The family proves that the same authenticated L1 deposit, withdrawal, or forced-order event
identity was applied by two different L2 blocks. It compares a live challenged
state-queue block with a distinct confirmed settlement block and requires the
same event key in the same counted-root domain.

Two distinct withdrawal ids spending the same L2 output are handled by
`doubleWithdraw`; duplicate still-live events and due-window violations remain
transition-trace concerns. This family does not treat an unauthenticated
off-chain archive as evidence.

[Validator source](../../onchain/aiken/validators/fraud-proofs/cross-block-duplicate-event).

## Double-withdraw fault

The family proves that one block commits two distinct payable withdrawal events
which drain the same L2 output reference. Both leaves must be tagged
`WithdrawalIsValid`; an honest non-payable duplicate does not convict.

The technical specification's [DOUBLE-WITHDRAW rule](../../technical-spec/5-ledger-rules/1-cardano-ledger-rules.tex)
still omits the two `WithdrawalIsValid` preconditions from its displayed formula.
That is an outstanding specification reconciliation item; the implemented
predicate described here includes both.

The family is same-block only. Reuse of the same event in two blocks belongs to
`crossBlockDuplicateEvent`, while a later spend after settlement is handled by
the state-transition/input-validity machinery.

[Validator source](../../onchain/aiken/validators/fraud-proofs/double-withdraw).

## Input-set-uniqueness fault

An operator-accepted committed transaction violates the canonical input-set
rules when:

- the spend-input field contains the same output reference twice;
- the reference-input field contains the same output reference twice; or
- one output reference appears in both fields.

The family is intra-transaction. Empty spend sets belong to `zeroInput`, and
cross-transaction double spends belong to `doubleSpend`.

Wrongful forced `DuplicateInput` rejection uses the exhaustive authenticated
spend/reference union scan, with bounded continuations and checkpointed order.
A pair of unequal items alone cannot establish uniqueness.

[Validator source](../../onchain/aiken/validators/fraud-proofs/input-set-uniqueness).

## L2-transaction-mistag fault

A normal transaction under `transactions_root` is an acceptance verdict in
canonical V1. Its compact `validity_code` must be zero. A non-zero value marks
the transaction as an invalid no-op and censors its state transition; the
committed scalar itself is the fault.

This is distinct from forced transactions. Forced leaves carry an explicit
operator verdict and use the validation-dispute/transition paths to prove
whether the transaction should execute or remain a no-op. D-S9 is resolved for
canonical V1: valid forced transactions apply the authenticated accepted ledger
delta, invalid forced transactions are exact no-ops, and either wrong verdict
is challengeable.

[Validator source](../../onchain/aiken/validators/fraud-proofs/l2-tx-mistag).

## Minimum-Ada fault

The registered `minAda` family uses five spending scripts and authenticated
step-02 transaction/UTxO withdrawals. It proves accepted transaction output
underfunding, newly introduced post-state UTxO underfunding, and wrongful forced
`OutputBelowMinAda` rejection for the exact authenticated output index.

Step 01 binds accepted, forced, or post-UTxO evidence. The transaction withdrawal
certifies output-field grammar and advances an authenticated indexed walk in
bounded batches. Step 03 scans the selected canonical output in bounded batches,
then applies the compiled minimum-Ada formula to its exact byte length and
lovelace. Accepted evidence requires underfunding; wrongful rejection requires
sufficiency, including the exact floor. The post-UTxO route authenticates the
committed descriptor, applies the same predicate, and proves predecessor
non-membership. Step 05 mints permanent evidence for registered-chain removal.

Source artifacts preserve submitted full transaction bytes and authenticate the
invalid adjudication committed by a forced leaf. Installed complete replay and
`createMinAdaWorkflowRunner` support both directions and resume step-02/03
selfloops from authenticated L1 checkpoints. Direct submit helpers complete their
bounded continuations when confirmation is enabled; without confirmation they
return the submitted continuation's `nextStepIndex` for later observation.

[Validator source](../../onchain/aiken/validators/fraud-proofs/min-ada).

## Min-fee fault

For the exact canonical native-V1 transaction size and the challenged header's
non-negative fee schedule, the family proves:

```text
fee < min_fee_a * canonical_tx_size + min_fee_b
```

`onchain/aiken/lib/midgard/fraud-proofs/native-tx/compact.ak` is the formula
authority shared by the standalone family and validation machine. Equality is
honest and cannot convict on the accepted-invalid route. The forced direction
binds `FeeBelowMinimum` and proves the authenticated fee meets or exceeds that
minimum.

[Validator source](../../onchain/aiken/validators/fraud-proofs/min-fee).

## Mint-authorization fault

An operator-accepted committed transaction contains a non-ADA mint/burn entry
for policy `H`, but authorization is unsatisfied in one of two ways:

1. no script source with versioned hash `H` exists in the transaction witnesses
   or resolved reference-input scripts; or
2. a native script whose versioned hash is `H` evaluates to false against the
   committed signer frontier and validity interval.

ADA is structurally unmintable in the canonical mint grammar. The family does
not treat malformed/guardrail-exceeded native scripts as unauthorized; those
belong to the structural-decoding family for the authenticated script source.

[Validator source](../../onchain/aiken/validators/fraud-proofs/mint-authorization).

## Missing-native-script transaction fault

The family proves that an operator-accepted transaction spends an output locked
by a Cardano native-script credential while the corresponding native script is
absent from the transaction's authenticated script-witness collection. A
present matching script, a non-native script credential, or unauthenticated
transaction/script bytes cannot convict.

[Validator source](../../onchain/aiken/validators/fraud-proofs/missing-native-script-tx).

## Missing-native-script UTxO fault

The family proves that an operator-accepted transaction spends a predecessor
output whose authenticated script credential names a Cardano native script
absent from the spending transaction's script witnesses. It proves predecessor
ledger membership, derives the credential from the committed output descriptor,
and binds the supplied native-script bytes to that credential before scanning
the authenticated witness field. Forged predecessor roots, keys, credentials,
or script preimages cannot convict.

[Validator source](../../onchain/aiken/validators/fraud-proofs/missing-native-script-utxo).

## Missing-signature fault

The accepted-invalid route selects a required signer from committed field 4,
binds its verification key hash, and proves the corresponding address witness
absent from committed field 7. It does not resolve a spent output's payment
credential; `spendInputSignerMissing` owns that reason. The forced route binds
`RequiredSignerUnsigned` and its exact signer coordinate, then proves that the
coordinate is impossible or that a matching valid signature exists. Unbound
signer/witness bytes cannot convict.

[Validator source](../../onchain/aiken/validators/fraud-proofs/missing-signature).

## Native-script-decoding fault

The native structural scan proves both accepted-undecodable and rejected-decodable claims. See the [native-script decoding design](native-script-decoding-fault-thread-design-v1.md) for the authenticated source, bounded scan, and terminal rules.

[Validator source](../../onchain/aiken/validators/fraud-proofs/native-script-decoding).

## Native-script-invalid fault

The family proves that an operator accepted a transaction containing a selected
native script that evaluates false under the authenticated validity interval
and address-witness signer set. A satisfied script, a non-native witness, or a
mutated signer/evaluator checkpoint cannot convict.

The forced direction contradicts the exact `WitnessNativeScriptFalse` reason
with a satisfied authenticated script. Direct and bounded staged signer/evaluator
paths share the same predicate.

[Validator source](../../onchain/aiken/validators/fraud-proofs/native-script-invalid).

## Value-not-preserved fault

An operator-accepted committed transaction fails value preservation for at
least one asset. The family uses a bounded single-asset claim: the prover names
one asset and the imbalance direction, and the chain verifies only that asset's
equation. The prover finds an unbalanced asset off-chain.

- ADA: `sum(inputs) - sum(outputs) - fee == 0`
- non-ADA asset `u`: `sum(inputs_u) + mint_u - sum(outputs_u) == 0`

The claim convicts only when the final delta is non-zero and its sign matches
the claimed inflation/deflation direction. ADA minting and negative output
quantities are structurally unrepresentable in canonical V1. Malformed committed
bytes are covered by `mintItemNonCanonical` and `transactionOutputNonCanonical`;
the Q24/Q25 decoder controls alone do not prove adversarial-byte coverage.

The installed conservation route also authenticates the selected event
pre-state, including prior effects in the same block. Wrongful forced rejection
requires an exhaustive input/output/mint union fold: zero deltas are deleted
from a signed-delta MPF, and only an empty terminal map plus Ada/fee equality
proves conservation. Each asset occurrence is authenticated and consumed once.

[Validator source](../../onchain/aiken/validators/fraud-proofs/value-not-preserved).

## Withdrawal-mistag fault

The family proves that a committed withdrawal's payable/refund tag disagrees
with the validity recomputed from authenticated evidence:

```text
claimed_valid != actual_valid
```

It covers both a valid withdrawal marked invalid and an invalid withdrawal
marked valid, including owner, value, signature, native-asset, and exact payout
feasibility checks. It does not convict merely because a different invalid
reason label could also apply; all invalid labels select the same refund/no-op
semantics.

This is a standalone single-party family. Transition trace proves consistency
with the committed tag, not the truth of the tag itself.

[Validator source](../../onchain/aiken/validators/fraud-proofs/withdrawal-mistag).

## Withdrawn-input fault

The family proves that an operator-accepted transaction spends an L2 output
which a payable withdrawal committed by the same block already consumes. The
transaction and withdrawal memberships are authenticated against the same
challenged header. An invalid/non-payable withdrawal cannot convict.

This is distinct from an input absent from the prior UTxO state (`no-input`), a
cross-transaction in-block double spend (`double-spend`), an intra-transaction
duplicate input (`inputSetUniqueness`), and use as a reference input
(`withdrawnReferenceInput`).

[Validator source](../../onchain/aiken/validators/fraud-proofs/withdrawn-input).

## Withdrawn-reference-input fault

The family proves that an operator-accepted transaction references an L2 output
which a payable withdrawal committed by the same block consumes. The
transaction and withdrawal memberships are authenticated against the same
challenged header. An invalid/non-payable withdrawal or a different output
reference cannot convict.

Actual spending of the withdrawn output is handled by `withdrawnInput`; a
reference input absent from the ledger is handled by
`noReferenceInput`; an existing transaction with an out-of-range reference
output index belongs to `referenceInputNoIdx`.

[Validator source](../../onchain/aiken/validators/fraud-proofs/withdrawn-reference-input).

## Mint-item non-canonicity

`mintItemNonCanonical` (`00000036`) proves that an accepted transaction's
nonempty field-5 item violates §5.6. The accepted source can be either the
transactions root or a `ForcedTxValid` leaf. It does not adjudicate wrongful
rejection: the current machine has no typed malformed-mint rejection reason.

Its statement owns incorrect item shape, policy IDs other than 28 bytes
(including the empty ADA policy), empty asset maps, asset names over 32 bytes,
zero quantities, nonminimal/invalid signed integer encodings, truncated or
trailing bytes, and duplicate/descending asset or adjacent policy keys.
Canonical negative quantities are burns and never convict.

The ownership boundaries are:

- `canonicalDecodability`: outer field envelope grammar.
- `committedFieldShape`: field byte limit; an oversized field cannot enter this proof.
- `fieldItemWidthIllegal`: empty mint item; step 02 refuses it.
- `mintDeclaredAssetLimit` and `distinctAssetAccumulationLimit`: declared and
  accumulated asset counts. This scanner does not impose either count limit.
- `mintAuthorization` and the script-source families: authorization and execution
  of well-formed mint statements.

An invalid count and a malformed body may coexist in one transaction; those are
independent statements. This family never convicts solely because an asset count
exceeds a semantic limit, a script is absent, or value is not preserved.

Step 01 authenticates the accepted subject and selected ordinal. Step 02 opens
field 5 and certifies at most 32 item envelopes per transaction. Its datum
retains the next ordinal/byte offset, selected item extent, and preceding policy
key, so a late selection never re-walks its prefix. Each round authenticates the
bounded field preimage once. Only after the entire envelope is certified does it
retain the selected item length/hash and 4095-byte chunk hashes for step 03. A malformed predecessor must be
challenged at its own ordinal. Step 03 scans at most 32 entries using the current
chunk and its successor, retaining a cursor across transactions. A token crossing
a chunk boundary remains authenticated. Step 04 mints the permanent proof only
for a noncanonical result, followed by the shared queue-removal path.

The SDK registers the four scripts and their deployment/reference identities.
The fault-proofs package exports accepted/forced binding, field opening,
resumable scanning, finalization, cancellation, and retained-DA detection.
The watcher installs the manifest-bound runner, retained-DA replay, reference
roster, durable journal, and resumable proof-thread indexer topology. This is
source installation, not live deployment or release-acceptance evidence.
The Lucid suites exercise the registered scripts with local UPLC evaluation,
including a 32764-byte item in the maximum 32768-byte certified field, a fault
at policy index 909 in a 32763-byte field, journal recovery after each physical
continuation, honest
mint/burn refusal, forged evidence, cancellation at all four steps, and removal.
