# 0001 — Publish field evidence once and reference it from proof transactions

- Status: Accepted
- Date: 2026-07-24
- Scope: canonical V1; mandatory pre-activation proof completion

## Context

The current canonical V1 implementation limits a general independently revealed
transaction-field preimage to 9,215 bytes. That limit comes from placing the
field preimage and the rest of a one-step witness in the same L1 transaction:

```text
16,384-byte L1 transaction floor
− 7,168-byte proof-overhead reservation
− 1-byte strict margin
= 9,215-byte field preimage
```

This is a limit of the self-contained proof-transaction layout, not a
fundamental limit on a field that can be made available to Plutus through a
reference input.

Cardano's `maxTxSize` is an updatable protocol parameter. The official
[protocol-parameter guide](https://docs.cardano.org/about-cardano/explore-more/parameter-guide)
defines it as the maximum serialized transaction size. The Cardano
[constitutional guardrails](https://cardano.org/constitution/) permit a value
as high as 32,768 bytes, but that is a guardrail, not the current value.

Live Koios queries on 2026-07-24 reported:

| Network | Epoch | `max_tx_size` | `max_block_size` | Max tx execution |
| --- | ---: | ---: | ---: | ---: |
| mainnet | 645 | 16,384 bytes | 90,112 bytes | 16.5M memory / 10B steps |
| preprod | 302 | 16,384 bytes | 90,112 bytes | 16.5M memory / 10B steps |

Query endpoints:

- `https://api.koios.rest/api/v1/epoch_params?limit=1&order=epoch_no.desc`
- `https://preprod.koios.rest/api/v1/epoch_params?limit=1&order=epoch_no.desc`

Midgard must use the parameters of the exact deployment network. It must not
confuse the 90,112-byte block-body limit or the constitutional 32KiB guardrail
with the currently available transaction limit.

## Serialization measurement

The reproducible measurement is:

```sh
cd demo/midgard-core
node scripts/measure-evidence-publication-envelope.mjs
```

It uses the repository's Cardano Multiplatform Library version and constructs
canonical Conway transactions with:

- one ordinary funding input;
- an evidence output at a script address;
- a change output;
- an inline constructor datum containing exactly `(evidence_bytes,
  evidence_hash)`;
- a finite TTL; and
- a real vkey witness.

The state-token case additionally includes:

- one minted evidence state token in the evidence output;
- one mint redeemer with the deployment's maximum execution-unit integers;
- one collateral input and total-collateral field;
- one reference input for the minting policy;
- one required signer; and
- a script-data hash.

Measured results:

| Publication shape | Maximum raw evidence | Datum bytes | Transaction bytes |
| --- | ---: | ---: | ---: |
| simple signed, one input and two outputs | 15,603 | 16,130 | 16,384 |
| state token + reference policy + collateral + witness | 15,345 | 15,864 | 16,384 |

Adding one raw evidence byte produces a 16,385-byte transaction in both
fixtures, demonstrating the exact serialization boundary.

The accepted release-safe starting bound for the state-token shape is
**14,848 raw evidence bytes (14.5KiB)**. It serializes to 15,871 bytes, leaving
513 bytes below the current 16,384-byte network limit. This reserve covers
small framing changes, output-value variation, and builder differences. It is
not permission to add unmeasured fields. The final implemented bound must be
regenerated against the exact publication transaction and deployment
parameters.

An exact 16KiB raw evidence item cannot fit in one publication transaction
while `maxTxSize` is 16KiB: the transaction must also serialize the datum
constructor, datum byte-string chunking, output, change, input, fee, validity
bound, and witness.

## Decision

Large transaction-field evidence will use a publish-once/reference-many
protocol:

```text
publication transaction
  └─ creates an authenticated evidence UTxO containing the field

proof transaction 0..n
  └─ references the evidence UTxO and supplies compact proof arguments
```

The field bytes are not repeatedly placed in every proof redeemer. A proof
transaction identifies the evidence UTxO as a reference input, authenticates
its state token and datum, and uses the referenced bytes for the relevant
bounded proof step.

Reference-input out-refs contribute to serialized proof-transaction size; the
resolved evidence output does not get copied into the transaction body.
Reading, decoding, and hashing that output still consumes Plutus memory and
CPU and therefore remains subject to generated execution-unit measurements.

## Evidence UTxO authentication

The default design uses a dedicated evidence state token and an evidence
spending script.

The evidence datum contains exactly:

```text
EvidenceDatum {
  evidence_bytes,
  evidence_hash,
}
```

The state-token minting policy enforces:

1. exactly one token for the evidence identity is minted;
2. exactly one output carries that token;
3. that output is locked by the exact evidence spending-script hash;
4. the output datum has the exact supported version and two-field shape;
5. `evidence_hash` is the domain-separated hash of `evidence_bytes`;
6. the raw evidence and complete publication transaction fit their compiled
   byte bounds; and
7. unknown versions, extra fields, duplicate tokens, datum hashes in place of
   inline data, or ambiguous encodings reject.

If hashing the maximum evidence and performing all mint-policy checks cannot
fit the reserved execution budget with adequate margin, publication becomes
two transactions:

1. an evidence-hash certification transaction validates
   `hash(evidence_bytes) == evidence_hash` and creates the authenticated state;
2. subsequent proof transactions reference that certified evidence UTxO and
   do not recompute the complete field hash.

The certification transition must remain trustless and L1-enforced. Moving
hashing to another transaction is permitted; replacing it with an off-chain
assertion is not.

The evidence spending validator must preserve availability for every block
whose challenge window can still use the evidence. The implementation should
prefer an append-only/unspendable evidence output. If reclamation is required,
it may occur only after all dependent challenge windows and settlement paths
have expired under an L1-enforced rule.

## Throughput principle

When choosing between:

- lower L2 transaction-size, execution-unit, script, value, or cardinality
  limits; and
- a larger but still bounded number of L1 proof transactions,

Midgard prefers preserving useful L2 transaction capacity and throughput.
Proof construction may use additional publication, certification, bisection,
or one-step transactions when that safely removes artificial consensus
restrictions.

This preference is bounded:

- every proof path has a compiled maximum transaction count;
- the complete worst-case dispute must finish well inside block maturity;
- every participant must be able to fund and submit the path under the
  documented operational assumptions;
- no branch may require an unbounded number of transactions;
- no accepted design may require tens of thousands of proof transactions for
  one challenged L2 transaction; and
- byte, execution-unit, fee, concurrency, and timing reports must cover the
  complete worst-case path, not only one step in isolation.

Proof-step minimality is therefore an optimization objective, not a reason to
impose otherwise unnecessary L2 consensus restrictions.

## Consequences

- The 9,215-byte general-field bound is no longer the intended architectural
  ceiling. Activation remains closed until the publish/reference protocol
  replaces it within canonical V1.
- The initial target for one state-token evidence UTxO is 14,848 raw evidence
  bytes under current mainnet/preprod parameters.
- Larger fields may be supported by ordered, independently authenticated
  evidence chunks rather than lowering the Midgard transaction limit.
- Normal and forced transactions must use the same field commitment and
  evidence interpretation.
- DA remains necessary for reconstruction and liveness; the evidence UTxO
  supplies L1 availability for the disputed field.
- Publication adds L1 fees and UTxO growth. Content-addressed deduplication,
  bounded retention, and safe garbage-collection rules must be addressed
  without weakening challenge availability.
- This changes proof witnesses, evidence identity, deployment scripts, SDK
  builders, node persistence, DA reconstruction, and release measurements, so
  it must land as one indivisible canonical V1 change before activation.

## Implementation acceptance criteria

Before activation:

1. implement and test the state-token minting policy and evidence validator;
2. measure the exact publication transaction with compiled validator hashes,
   final datum, real minimum ADA, final redeemer, collateral, reference script,
   and witnesses;
3. measure reference-input decoding and field use for every field family;
4. split hash certification into a separate transaction if the combined
   policy lacks the required execution reserve;
5. generate adversarial tests for wrong bytes/hash, token substitution,
   duplicate evidence, wrong script address, spending before maturity,
   malformed datum, and oversized data;
6. derive a maximum proof-transaction count and prove it fits the dispute
   schedule and maturity window;
7. update the canonical V1 transaction, DA, trace, manifest, and API surfaces
   atomically without introducing a second pre-launch version;
8. bind all measurements to the deployed validator hashes and protocol
   parameters; and
9. keep activation fail-closed until the resulting release-evidence digest is
   compiled in.
