# Authenticate carriage contents and measure signed transactions

Status: Accepted; records the implemented carriage decisions.

Recorded: 2026-09-07. Original decisions: 2026-08-09 through 2026-08-23.

## Context

A reconstruction benchmark does not measure publication capacity. Complete
Cardano transactions add fee, change, witnesses, datum framing, and Plutus Data
byte-string chunking. Publication capacity and the payload capacity of a proof
step are separate budgets.

Certified carriage introduces a second boundary: the consumer reads selected
chunks instead of hashing the whole preimage. Its certificate must therefore
bind the exact commitment extracted from the transaction being challenged,
including witness-set fields whose identity is not fixed by the body hash alone.

## Decision

Derive the carriage chunk size from the complete signed publication frontier
with a reliability reserve. Keep pure planning total over the admitted field
domain, and refuse oversized publication at construction. Do not silently
re-split a protocol chunk to make a transaction fit.

Treat the general inline-carriage threshold separately from complete-item direct
submission. The latter uses its actual pre-sign projection and falls back to the
supported reference route when necessary. Fixture-specific frontiers do not
change protocol constants by themselves.

The certificate mint binds the field hash and chunk digests to one reconstructed
preimage. A consumer compares the certificate's field hash with the commitment
from its anchored compact structures. Token naming or an anchored witness-set
hash without that comparison cannot substitute for content authentication.

## Consequences and verification

Changing the chunk size changes partition boundaries, codecs, certificates, and
cross-language vectors together. Certification must follow chunk publication
because reference inputs resolve against the pre-transaction UTxO set.

A byte-fitting field can still exceed execution limits through item traversal.
Use authenticated bounded continuations and measure full lifecycles; carriage
choice alone cannot make an unbounded absence or canonicality scan fit.

[MidgardTx §8](../../spec/midgard-tx.md#8-field-preimage-carriage-three-tiers)
owns the exact rules and current verifier inputs. The
[publication model](../../../demo/midgard-core/src/codec/native-tx-carriage.ts),
[certificate validator](../../../onchain/aiken/validators/field-preimage-certificate.ak),
and [field-access door](../../../onchain/aiken/lib/midgard/fraud-proofs/field-opening-v1.ak)
implement these boundaries. Superseded measurements and repair chronology belong
in Git; this ADR does not certify a release build.
