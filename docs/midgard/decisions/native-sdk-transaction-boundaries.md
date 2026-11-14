# Native SDK transaction boundaries

Status: Accepted; records the implemented SDK design.

Recorded and source reviewed: 2026-09-07.

## Context

A familiar Lucid-style API can obscure the difference between Cardano L1
transactions and Midgard-native L2 transactions, or make mutable provider state
silently change a transaction while it is being completed or signed.

## Decision

`lucid-midgard` builds the native L2 transaction domain. L1 protocol transactions
remain with Lucid and `midgard-sdk`. Signatures bind the native body hash;
Cardano transaction conversion is not a normal construction or import path.

Builders capture provider, wallet, and local-UTxO context. Composition and
completion preserve those snapshots, recompute derived commitments and script
indexes, and retain authored output order. These snapshots capture the selected
provider object and builder context; later provider queries still obtain current
protocol parameters and ledger facts. Fee convergence and value balance
must succeed before signing; user intent cannot be repaired by silently dropping
assets or changing signed bytes during retries.

Raw imports require canonical source material. Reference-input resolution is
exact and remains attached through signing and assembly. Script metadata can
identify a language/hash branch but cannot replace non-native executable bytes
or their CEK material. Witness assembly verifies signatures and required
ownership before exposing a submit-capable transaction.

Local preflight and the node consume `@al-ft/midgard-validation`. Local UTxO
presets and simulation reports remain advisory: node state determines admission
processing and acceptance. Durable submission is not validation or commitment.

## Consequences and implementation

These boundaries make failed completion, missing pre-state, and signing-domain
errors explicit. Browser wallets need an adapter that can sign the correct
native bytes; ordinary Cardano signing support does not establish that ability.

Implementation lives in [the builder](../../../demo/lucid-midgard/src/builder.ts),
[context snapshots](../../../demo/lucid-midgard/src/builder/context.ts), and
[raw import validation](../../../demo/lucid-midgard/src/builder/imported-tx.ts).
The package's composition, finalization, provider-switching, signing, and
submission suites exercise these boundaries. This record does not claim that
those suites or live acceptance were rerun during documentation maintenance.

Use the [package README](../../../demo/lucid-midgard/README.md) and
[maintained SDK documentation](../../../docs-site/content/docs/sdk/lucid-midgard)
for current APIs and executable examples. The delivered design-plan chapters
are retained in Git history under `docs/lucid-midgard/`.
